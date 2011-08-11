

-- UUAGC 0.9.38.6.5 (src-ag/Transform.ag)
module Transform where
{-# LINE 8 "src-ag/Transform.ag" #-}

import Control.Monad(mplus,mzero)
import Data.List (partition, elem, nub,intersperse, union)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set as Set (Set, member, union, toList, fromList, empty, singleton, member, unions, size, fold, intersection, difference, insert)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, empty, (><),fromList)
import Data.Foldable(toList)
import UU.Scanner.Position(noPos)

import ConcreteSyntax
import AbstractSyntax
import ErrorMessages
import Patterns (Patterns(..),Pattern(..))
import Expression (Expression(..))
import HsToken

import Options
import CommonTypes
import RhsCheck
{-# LINE 29 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 2 "src-ag/ConcreteSyntax.ag" #-}

import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes
{-# LINE 37 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 44 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}
{-# LINE 102 "src-ag/Transform.ag" #-}
type DefinedSets = Map Identifier (Set NontermIdent) 
{-# LINE 47 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 122 "src-ag/Transform.ag" #-}
type FieldMap  = [(Identifier, Type)] 
{-# LINE 51 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 123 "src-ag/Transform.ag" #-}
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap) 
{-# LINE 55 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 143 "src-ag/Transform.ag" #-}
type AttrName   = (Identifier,Identifier) 
{-# LINE 59 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 144 "src-ag/Transform.ag" #-}
type RuleInfo   = (Maybe Identifier, [AttrName]->Pattern, Expression, [AttrName], Bool, String) 
{-# LINE 63 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 145 "src-ag/Transform.ag" #-}
type SigInfo    = (Identifier,Type) 
{-# LINE 67 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 146 "src-ag/Transform.ag" #-}
type UniqueInfo = (Identifier,Identifier) 
{-# LINE 71 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 147 "src-ag/Transform.ag" #-}
type AugmentInfo = (Identifier,Expression)
{-# LINE 75 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 148 "src-ag/Transform.ag" #-}
type AroundInfo  = (Identifier,Expression)
{-# LINE 79 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 149 "src-ag/Transform.ag" #-}
type MergeInfo   = (Identifier, Identifier, [Identifier], Expression)
{-# LINE 83 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 203 "src-ag/Transform.ag" #-}


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

foldErrors f e xs = foldl g (e,Seq.empty) xs
  where g ~(e,es) x = let (e',es') = f x e
                      in (e', es >< es')


checkForDuplicates :: (Identifier -> Identifier -> Error)  ->  [Identifier]  ->  [Error]
checkForDuplicates err [] = []
checkForDuplicates err (x:xs) = let (same,other) = partition (equalId x) xs
                                in  map (err x) same ++ checkForDuplicates err other

equalId :: Identifier -> Identifier -> Bool
equalId x y = getName x == getName y

{-# LINE 114 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 339 "src-ag/Transform.ag" #-}

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

checkRules :: Map NontermIdent (Attributes, Attributes) -> Map NontermIdent (Map ConstructorIdent FieldMap) ->
              Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [SigInfo]) ->
              Map NontermIdent (Map ConstructorIdent [MergeInfo]) ->
              NontermIdent -> ConstructorIdent -> [RuleInfo] -> RulesAndErrors
checkRules attributes fields allinsts allsigs allmerges nt con rs
  = let fieldmap :: FieldMap
        fieldmap = (_LHS,NT nt undefined) : (_LOC,NT undefined undefined) : (_INST, NT undefined undefined) : (_FIRST, NT undefined undefined) : (_LAST, NT undefined undefined)
                 : Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fields)
                 ++ mapMaybe (\instNm -> lookup instNm sigs >>= \tp -> return (instNm, tp)) (Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allinsts))
                 --   merged children are not allowed to have any inherited attrs defined: do not include

        sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allsigs)

        hasAttrib f tp attr  = Map.member attr (f (Map.findWithDefault (Map.empty,Map.empty) tp attributes))

        checkRule :: RuleInfo -> AccumRuleCheck -> AccumRuleCheck
        checkRule (mbNm, pat,exp,as,owrt,str) ((r1,e1),m1)
          = let (e2,m2,u2,b2) = foldr (checkDefi owrt) (e1,m1,[],[]) as
            in  ( (Rule mbNm (pat u2) exp owrt str True : r1, e2), m2)

        checkDefi :: Bool -> AttrName -> AccumDefiCheck -> AccumDefiCheck
        checkDefi owrt fa@(field,attr) (e,m,u,bs)
         = case lookup field fieldmap
            of  Just (NT tp _) ->
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
    checkRule (Just nm,_,_,_,_,_) (errs, nms)
      | nm `Set.member` nms = (DupRuleName nt con nm Seq.<| errs, nms)
      | otherwise           = (errs, Set.insert nm nms)
    checkRule (Nothing,_,_,_,_,_) inp = inp

checkSigs :: NontermIdent -> ConstructorIdent -> [SigInfo] -> SigsAndErrors
checkSigs nt con sis
  = let checkSig (ide,typ) (sigs,errs)
         = if   ide `elem` map (\(TypeSig n t)-> n) sigs
           then (sigs, ((Seq.<|)) (DupSig nt con ide) errs)
           -- else if not (ide `elem` locattrdefs)
           -- then (sigs, ((Seq.<|)) (SupSig nt con ide) errs)
           else (TypeSig ide typ:sigs, errs)
    in  foldr checkSig ([],Seq.empty) sis

checkInsts :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [SigInfo]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [Identifier] -> InstsAndErrors
checkInsts allNts sigMap fieldMap nt con
  = foldr (\inst (insts, errs) ->
              maybe (insts, Seq.singleton (MissingInstSig nt con inst) >< errs)
                    (\info@(k, NT nm _) ->
                      case findInst k insts of
                        Just k' -> (insts, Seq.singleton (DupChild nt con k k') >< errs)
                        Nothing -> case nm `Set.member` allNts of
                                             True  -> (info : insts, errs)
                                             False | take 2 (getName nm) == "T_" -> let nm' = Ident (drop 2 (getName nm)) (getPos nm)
                                                                                    in case nm' `Set.member` allNts of
                                                                                         True  -> (info : insts, errs)
                                                                                         False -> (insts, Seq.singleton (UndefNont nm') >< errs)
                                                   | otherwise                   -> (insts, Seq.singleton (UndefNont nm) >< errs)
                    )
                  $ findSig inst
          ) ([], Seq.empty)
  where
    sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt sigMap)

    findSig name
      = do tp@(NT _ _) <- lookup name sigs
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
checkAugments allAttrs nt con augments
  = let checkAugment (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else if Map.member ident syns
                 then (Map.insert ident [expr] as, errs)
                 else (as, ((Seq.<|)) (MissingSyn nt ident) errs)

        (inhs,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkAugment (Map.empty, Seq.empty) augments

checkArounds :: Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [AroundInfo] -> AroundsAndErrors
checkArounds fieldMap nt con arounds
  = let checkAround (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else case lookup ident fields of
                   Just (NT _ _) -> (Map.insert ident [expr] as, errs)
                   _             -> (as, ((Seq.<|)) (UndefChild nt con ident) errs)
        fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fieldMap)
    in foldr checkAround (Map.empty, Seq.empty) arounds

checkMerges :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [MergeInfo] -> MergesAndErrors
checkMerges allNts allInsts fieldMap nt con merges
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

unionunionplusplus = Map.unionWith (Map.unionWith (++))
{-# LINE 269 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 494 "src-ag/Transform.ag" #-}

mkUniqueRules :: Options -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Attributes,Attributes) -> NontermIdent -> ConstructorIdent -> Map Identifier Identifier -> [Rule]
mkUniqueRules opts allFields allInsts allAttrDecls nt con usMap
  = map apply groups
  where
    fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allFields)
             ++ Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allInsts)
             -- may have duplicates

    groups = Map.assocs $ Map.foldrWithKey (\i r m -> Map.insertWith (++) r [i] m) Map.empty usMap
    apply (ref,us) = mkRule ref (findOutField ref) us
    findOutField ref = case [ chld | (chld,NT tp _) <- fields, tp `hasSyn` ref] of
                         []    -> _LHS
                         (x:_) -> x
    hasSyn tp ref = Map.member ref $ snd $ Map.findWithDefault (Map.empty,Map.empty) tp allAttrDecls
    mkRule ref outFld locAttrs
      = let pat = Product noPos (attr outFld ref : [attr _LOC u | u <- locAttrs ])
            rhs = Expression noPos $ wrap ref $ foldr gencase (finalout locAttrs) locAttrs
                     -- [HsToken ("mkUniques" ++ show (length locAttrs) ++ " ") noPos, AGField _LHS ref noPos Nothing]
        in Rule Nothing pat rhs False "-- generated by the unique rule mechanism." False
    attr fld a = Alias fld a (Underscore noPos) []
    gencase nm outp
      = h ("case " ++ uniqueDispenser opts ++ " __cont of { (__cont, " ++ getName nm ++ ") -> ") ++ outp ++ h "}"
    h s = [HsToken s noPos]
    finalout us = h ("(__cont, " ++ concat (intersperse "," (map getName us)) ++ ")")
    wrap ref inp = h "let __cont = " ++ [AGField _LHS ref noPos Nothing] ++ h " in seq __cont ( " ++ inp ++ h " )"
{-# LINE 298 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 692 "src-ag/Transform.ag" #-}

flattenDatas :: DataTypes -> Map NontermIdent (Set NontermIdent)
flattenDatas ds = Map.map flatten ds
  where flatten cs =  Set.fromList [ nt | (_,NT nt _) <- concatMap snd (Map.toList cs)]

reachableFrom :: Map NontermIdent (Set NontermIdent) -> Set NontermIdent -> Set NontermIdent
reachableFrom table nts = reach nts
  where reach nts = let nts' = Set.unions (nts : [ ns  | nt <- Set.toList nts
                                                 , let ns = Map.findWithDefault Set.empty nt table ])
                    in if Set.size nts' > Set.size nts
                          then reach nts'
                          else nts
invert :: Map NontermIdent (Set NontermIdent) -> Map NontermIdent (Set NontermIdent)
invert m = foldr inv Map.empty (Map.toList m)
  where inv (x,ns) m = fold (\n m -> Map.insertWith Set.union n (Set.singleton x) m) m ns

path :: Map NontermIdent (Set NontermIdent) -> NontermIdent -> NontermIdent -> Set NontermIdent
path table from to = let children = Map.findWithDefault Set.empty from table
                         forward  = reachableFrom table children
                         backward = reachableFrom (invert table)
                                                  (Set.singleton to)
                     in  Set.intersection forward backward
{-# LINE 323 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 812 "src-ag/Transform.ag" #-}

pragmaMapUnion :: PragmaMap -> PragmaMap -> PragmaMap
pragmaMapUnion = Map.unionWith (Map.unionWith Set.union)

pragmaMapSingle :: NontermIdent -> ConstructorIdent -> Set Identifier -> PragmaMap
pragmaMapSingle nt con nms = Map.singleton nt (Map.singleton con nms)
{-# LINE 332 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 844 "src-ag/Transform.ag" #-}

orderMapUnion :: AttrOrderMap -> AttrOrderMap -> AttrOrderMap
orderMapUnion = Map.unionWith (Map.unionWith Set.union)

orderMapSingle :: NontermIdent -> ConstructorIdent -> Set Dependency -> AttrOrderMap
orderMapSingle nt con deps = Map.singleton nt (Map.singleton con deps)
{-# LINE 341 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 870 "src-ag/Transform.ag" #-}

mergeParams :: ParamMap -> ParamMap -> ParamMap
mergeParams = Map.unionWith (++)
{-# LINE 347 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 893 "src-ag/Transform.ag" #-}

mergeCtx :: ContextMap -> ContextMap -> ContextMap
mergeCtx
  = Map.unionWith nubconcat
  where nubconcat a b = nub (a ++ b)
{-# LINE 355 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 912 "src-ag/Transform.ag" #-}

mergeQuant :: QuantMap -> QuantMap -> QuantMap
mergeQuant = Map.unionWith (++)
{-# LINE 361 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 923 "src-ag/Transform.ag" #-}

mergeDerivings m1 m2 = foldr (\(n,cs) m -> Map.insertWith Set.union n cs m) m2 (Map.toList m1)
{-# LINE 366 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 934 "src-ag/Transform.ag" #-}

merge x y = foldr f y (Map.toList x)
 where f ~(k,v) m = Map.insertWith (Map.union) k v m
{-# LINE 372 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 976 "src-ag/Transform.ag" #-}

checkAttrs allFields nts inherited synthesized decls = foldErrors check decls nts where
  check nt decls | not (nt `Map.member` allFields) = (decls,Seq.singleton(UndefNont nt))
                 | otherwise = let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt decls
                                   (inh',einh) = checkDuplicates (DupInhAttr nt) inherited   inh
                                   (syn',esyn) = checkDuplicates (DupSynAttr nt) synthesized syn
                               in (Map.insert nt (inh',syn') decls,einh >< esyn)
{-# LINE 382 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 987 "src-ag/Transform.ag" #-}

addSelf name atMap = let (eInh,eSyn) = Map.findWithDefault(Map.empty,Map.empty) name atMap
                     in  Map.insert name (eInh, Map.insert (Ident "self" noPos) (NT _SELF []) eSyn)atMap
{-# LINE 388 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 1128 "src-ag/Transform.ag" #-}

makeType :: Set NontermIdent -> Type -> Type
makeType nts tp@(NT x _) | x == _SELF       = tp
                         | Set.member x nts = tp
                         | otherwise        = Haskell (typeToHaskellString Nothing [] tp)
makeType _   tp                             = tp
{-# LINE 397 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 1135 "src-ag/Transform.ag" #-}

constructGrammar ::    Set NontermIdent
                    -> ParamMap
                    -> DataTypes
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
                    -> Grammar

constructGrammar nts ntParams gram attrs uses derivings wrappers allrules tsigs allinsts tsyns pragmaMap orderMap contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap =
   let gr = [ (nt,Map.toList alts) | (nt,alts) <- Map.toList gram]
       nonts = map nont gr
       nont (nt,alts) =  let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt attrs
                             rmap      = Map.findWithDefault Map.empty             nt allrules
                             tsmap     = Map.findWithDefault Map.empty             nt tsigs
                             instsmap  = Map.findWithDefault Map.empty             nt allinsts
                             params    = Map.findWithDefault []                    nt ntParams
                             mergemap  = Map.findWithDefault Map.empty             nt mergeMap
                             alt (con,flds) =
                                   let rules = maybe [] id (Map.lookup con rmap)
                                       tsigs = maybe [] id (Map.lookup con tsmap)
                                       insts = maybe [] id (Map.lookup con instsmap)
                                       merges = [ (n, NT t []) | (n, (t, _, _)) <- Map.assocs $ maybe Map.empty id (Map.lookup con mergemap) ]

                                       -- important: keep order of children
                                       cldrn = map child (flds ++ filter (not . existsAsField) insts ++ merges)
                                       child (nm, tp) =
                                          let tpI = if existsAsInst nm
                                                    then fromJust $ lookup nm insts
                                                    else tp
                                              (inh,syn) = case tpI of
                                                 NT nt _ -> let nt' = maybe nt id (deforestedNt nt)
                                                            in Map.findWithDefault (Map.empty,Map.empty) nt' attrs
                                                 _       -> (Map.empty,Map.empty)
                                              virt = if existsAsInst nm
                                                     then case lookup nm flds of
                                                            Just tp' -> Just (Just tp')
                                                            Nothing  -> Just Nothing
                                                     else if existsAsMerge nm
                                                          then (Just Nothing)
                                                          else Nothing
                                          in Child nm tpI inh syn virt
                                       existsAsInst nm = maybe False (const True) (lookup nm insts)
                                       existsAsField (nm,_) = maybe False (const True) (lookup nm flds)
                                       existsAsMerge nm = maybe False (const True) (lookup nm merges)
                                   in Production con cldrn rules tsigs
                            in Nonterminal nt params inh syn (map alt alts)
   in Grammar tsyns uses derivings wrappers nonts pragmaMap orderMap ntParams contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap
{-# LINE 461 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}

{-# LINE 1198 "src-ag/Transform.ag" #-}

mapUnionWithSetUnion = Map.unionWith Set.union
mapUnionWithPlusPlus = Map.unionWith (++)
{-# LINE 467 "dist/build/uuagc/uuagc-tmp/Transform.hs" #-}
-- AG ----------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         agi                  : (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))
         blocks               : Blocks
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         output               : Grammar
         pragmas              : Options -> Options
   alternatives:
      alternative AG:
         child elems          : Elems 
         visit 0:
            local allFields   : _
            local allConstrs  : _
            local allRules    : _
            local allSigs     : _
            local allInsts    : _
            local allUniques  : _
            local allAugments : _
            local allArounds  : _
            local allMerges   : _
            local augmentSigs : _
            local allRulesErrs : _
            local allNamesErrs : _
            local allSigsErrs : _
            local allInstsErrs : _
            local allUniquesErrs : _
            local allAugmentErrs : _
            local allAroundsErrs : _
            local allMergesErrs : _
            local checkedRulesPre : _
            local checkedSigs : _
            local checkedInsts : _
            local checkedUniques : _
            local checkedAugments : _
            local checkedArounds : _
            local checkedRules : _
            local checkedMerges : _
            local errs1       : _
            local errs2       : _
            local errs3       : _
            local errs4       : _
            local errs5       : _
            local errs6       : _
            local errs7       : _
            local errs8       : _
            local errs9       : _
            local errs10      : _
            local errs11      : _
            local allNonterminals : _
            local allAttrDecls : _
            local allAttrs    : _
-}
-- cata
sem_AG :: AG  ->
          T_AG 
sem_AG (AG _elems )  =
    (sem_AG_AG (sem_Elems _elems ) )
-- semantic domain
newtype T_AG  = T_AG (Options ->
                      ( ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))),Blocks,(Seq Error),(Maybe (String,String,String)),Grammar,(Options -> Options)))
data Inh_AG  = Inh_AG {options_Inh_AG :: !(Options)}
data Syn_AG  = Syn_AG {agi_Syn_AG :: !(((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes)))),blocks_Syn_AG :: !(Blocks),errors_Syn_AG :: !((Seq Error)),moduleDecl_Syn_AG :: !((Maybe (String,String,String))),output_Syn_AG :: !(Grammar),pragmas_Syn_AG :: !((Options -> Options))}
wrap_AG :: T_AG  ->
           Inh_AG  ->
           Syn_AG 
wrap_AG (T_AG sem ) (Inh_AG _lhsIoptions )  =
    (let ( _lhsOagi,_lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas) = sem _lhsIoptions 
     in  (Syn_AG _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas ))
sem_AG_AG :: T_Elems  ->
             T_AG 
sem_AG_AG (T_Elems elems_ )  =
    (T_AG (\ _lhsIoptions ->
               (let _lhsOoutput :: Grammar
                    _lhsOerrors :: (Seq Error)
                    _elemsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsOdefinedSets :: DefinedSets
                    _elemsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _lhsOagi :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes)))
                    _elemsOattrs :: (Map NontermIdent (Attributes, Attributes))
                    _lhsOblocks :: Blocks
                    _lhsOmoduleDecl :: (Maybe (String,String,String))
                    _lhsOpragmas :: (Options -> Options)
                    _elemsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                    _elemsOallFields :: DataTypes
                    _elemsOallNonterminals :: (Set NontermIdent)
                    _elemsOoptions :: Options
                    _elemsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsIattrOrderCollect :: AttrOrderMap
                    _elemsIattrs :: (Map NontermIdent (Attributes, Attributes))
                    _elemsIblocks :: Blocks
                    _elemsIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                    _elemsIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                    _elemsIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                    _elemsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                    _elemsIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                    _elemsIcollectedNames :: (Set Identifier)
                    _elemsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                    _elemsIcollectedSetNames :: (Set Identifier)
                    _elemsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                    _elemsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                    _elemsIctxCollect :: ContextMap
                    _elemsIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsIderivings :: Derivings
                    _elemsIerrors :: (Seq Error)
                    _elemsImoduleDecl :: (Maybe (String,String,String))
                    _elemsIparamsCollect :: ParamMap
                    _elemsIpragmas :: (Options -> Options)
                    _elemsIquantCollect :: QuantMap
                    _elemsIsemPragmasCollect :: PragmaMap
                    _elemsItypeSyns :: TypeSyns
                    _elemsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                    _elemsIwrappers :: (Set NontermIdent)
                    -- "src-ag/Transform.ag"(line 52, column 8)
                    _lhsOoutput =
                        ({-# LINE 52 "src-ag/Transform.ag" #-}
                         constructGrammar _allNonterminals
                                          _elemsIparamsCollect
                                          _allFields
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
                         {-# LINE 610 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 254, column 10)
                    _allFields =
                        ({-# LINE 254 "src-ag/Transform.ag" #-}
                         let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                         in  foldr f (Map.empty) _elemsIcollectedFields
                         {-# LINE 617 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 257, column 10)
                    _allConstrs =
                        ({-# LINE 257 "src-ag/Transform.ag" #-}
                         let f (nt,con,_) = Map.insertWith (++) nt [con]
                         in  foldr f (Map.empty) _elemsIcollectedFields
                         {-# LINE 624 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 260, column 10)
                    _allRules =
                        ({-# LINE 260 "src-ag/Transform.ag" #-}
                         let f (nt,con,r) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [r])
                         in  foldr f (Map.empty) _elemsIcollectedRules
                         {-# LINE 631 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 263, column 10)
                    _allSigs =
                        ({-# LINE 263 "src-ag/Transform.ag" #-}
                         let f (nt,con,t) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [t])
                             typeof nt r = Map.findWithDefault (Haskell "<unknown>") r $ fst $ Map.findWithDefault (Map.empty,Map.empty) nt _allAttrDecls
                         in  foldr f (Map.empty) ( _elemsIcollectedSigs
                                                 ++ [ (nt, con, (ident,typeof nt ref))  | (nt, con, us) <- _elemsIcollectedUniques, (ident,ref) <- us ]
                                                 )
                         {-# LINE 641 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 269, column 10)
                    _allInsts =
                        ({-# LINE 269 "src-ag/Transform.ag" #-}
                         let f (nt,con,is) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con is)
                         in  foldr f (Map.empty) _elemsIcollectedInsts
                         {-# LINE 648 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 272, column 10)
                    _allUniques =
                        ({-# LINE 272 "src-ag/Transform.ag" #-}
                         let f (nt,con,us) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con us)
                         in foldr f (Map.empty) _elemsIcollectedUniques
                         {-# LINE 655 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 274, column 10)
                    _allAugments =
                        ({-# LINE 274 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                         in foldr f Map.empty _elemsIcollectedAugments
                         {-# LINE 662 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 276, column 10)
                    _allArounds =
                        ({-# LINE 276 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                         in foldr f Map.empty _elemsIcollectedArounds
                         {-# LINE 669 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 278, column 10)
                    _allMerges =
                        ({-# LINE 278 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                          in foldr f Map.empty _elemsIcollectedMerges
                         {-# LINE 676 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 281, column 10)
                    _augmentSigs =
                        ({-# LINE 281 "src-ag/Transform.ag" #-}
                         let gen mp = []
                         in Map.map (Map.map gen) _allAugments
                         {-# LINE 683 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 284, column 10)
                    _allRulesErrs =
                        ({-# LINE 284 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkRules _allAttrDecls _allFields _allInsts _allSigs     _allMerges    )) _allRules
                         {-# LINE 689 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 285, column 10)
                    _allNamesErrs =
                        ({-# LINE 285 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . checkRuleNames) _allRules
                         {-# LINE 695 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 286, column 10)
                    _allSigsErrs =
                        ({-# LINE 286 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkSigs                                                 )) _allSigs
                         {-# LINE 701 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 287, column 10)
                    _allInstsErrs =
                        ({-# LINE 287 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkInsts _allNonterminals     _allSigs     _allFields   )) _allInsts
                         {-# LINE 707 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 288, column 10)
                    _allUniquesErrs =
                        ({-# LINE 288 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkUniques _allAttrDecls                                )) _allUniques
                         {-# LINE 713 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 289, column 10)
                    _allAugmentErrs =
                        ({-# LINE 289 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkAugments _allAttrDecls                               )) _allAugments
                         {-# LINE 719 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 290, column 10)
                    _allAroundsErrs =
                        ({-# LINE 290 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkArounds _allFields    )) _allArounds
                         {-# LINE 725 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 291, column 10)
                    _allMergesErrs =
                        ({-# LINE 291 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkMerges _allNonterminals     _allInsts     _allFields    )) _allMerges
                         {-# LINE 731 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 293, column 10)
                    _checkedRulesPre =
                        ({-# LINE 293 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allRulesErrs
                         {-# LINE 737 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 294, column 10)
                    _checkedSigs =
                        ({-# LINE 294 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allSigsErrs     `unionunionplusplus` _augmentSigs
                         {-# LINE 743 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 295, column 10)
                    _checkedInsts =
                        ({-# LINE 295 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allInstsErrs
                         {-# LINE 749 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 296, column 10)
                    _checkedUniques =
                        ({-# LINE 296 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allUniquesErrs
                         {-# LINE 755 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 297, column 10)
                    _checkedAugments =
                        ({-# LINE 297 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allAugmentErrs
                         {-# LINE 761 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 298, column 10)
                    _checkedArounds =
                        ({-# LINE 298 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allAroundsErrs
                         {-# LINE 767 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 299, column 10)
                    _checkedRules =
                        ({-# LINE 299 "src-ag/Transform.ag" #-}
                         Map.unionWith (Map.unionWith (++)) _checkedRulesPre     (Map.mapWithKey (Map.mapWithKey . (mkUniqueRules _lhsIoptions _allFields     _checkedInsts     _allAttrDecls    )) _checkedUniques    )
                         {-# LINE 773 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 300, column 10)
                    _checkedMerges =
                        ({-# LINE 300 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allMergesErrs
                         {-# LINE 779 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 302, column 10)
                    _errs1 =
                        ({-# LINE 302 "src-ag/Transform.ag" #-}
                         let f = checkForDuplicates (DupSynonym)
                         in  Seq.fromList . f . map fst $ _elemsItypeSyns
                         {-# LINE 786 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 305, column 10)
                    _errs2 =
                        ({-# LINE 305 "src-ag/Transform.ag" #-}
                         let g nt (con,fm) = checkForDuplicates (DupChild nt con) (map fst fm)
                             f (nt,cfm)    = concat . map (g nt) . Map.toList $ cfm
                         in  Seq.fromList . concat . map f . Map.toList $ _allFields
                         {-# LINE 794 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 309, column 10)
                    _errs3 =
                        ({-# LINE 309 "src-ag/Transform.ag" #-}
                         let f (nt,cons) = checkForDuplicates (DupAlt nt) cons
                         in   Seq.empty
                         {-# LINE 801 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 313, column 10)
                    _errs4 =
                        ({-# LINE 313 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allRulesErrs
                         {-# LINE 808 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 316, column 10)
                    _errs5 =
                        ({-# LINE 316 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allSigsErrs
                         {-# LINE 815 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 319, column 10)
                    _errs6 =
                        ({-# LINE 319 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allInstsErrs
                         {-# LINE 822 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 322, column 10)
                    _errs7 =
                        ({-# LINE 322 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allUniquesErrs
                         {-# LINE 829 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 325, column 10)
                    _errs8 =
                        ({-# LINE 325 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allAugmentErrs
                         {-# LINE 836 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 328, column 10)
                    _errs9 =
                        ({-# LINE 328 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allAroundsErrs
                         {-# LINE 843 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 331, column 10)
                    _errs10 =
                        ({-# LINE 331 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><)) s m
                         in Map.fold f Seq.empty _allNamesErrs
                         {-# LINE 850 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 334, column 10)
                    _errs11 =
                        ({-# LINE 334 "src-ag/Transform.ag" #-}
                         let f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allMergesErrs
                         {-# LINE 857 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 337, column 10)
                    _lhsOerrors =
                        ({-# LINE 337 "src-ag/Transform.ag" #-}
                         _elemsIerrors >< _errs1 >< _errs2 >< _errs3 >< _errs4 >< _errs5 >< _errs6 >< _errs7 >< _errs8 >< _errs9 >< _errs10 >< _errs11
                         {-# LINE 863 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 561, column 10)
                    _allNonterminals =
                        ({-# LINE 561 "src-ag/Transform.ag" #-}
                         _elemsIcollectedNames `Set.difference` _elemsIcollectedSetNames
                         {-# LINE 869 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 581, column 8)
                    _elemsOallConstructors =
                        ({-# LINE 581 "src-ag/Transform.ag" #-}
                         _elemsIcollectedConstructorsMap
                         {-# LINE 875 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 654, column 8)
                    _elemsOdefSets =
                        ({-# LINE 654 "src-ag/Transform.ag" #-}
                         Map.fromList (map (\x->(x,(Set.singleton x, Set.empty))) (Set.toList _allNonterminals    ))
                         {-# LINE 881 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 655, column 8)
                    _elemsOdefinedSets =
                        ({-# LINE 655 "src-ag/Transform.ag" #-}
                         Map.map fst _elemsIdefSets
                         {-# LINE 887 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 940, column 8)
                    _elemsOattrDecls =
                        ({-# LINE 940 "src-ag/Transform.ag" #-}
                         Map.empty
                         {-# LINE 893 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 994, column 9)
                    _allAttrDecls =
                        ({-# LINE 994 "src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                          then foldr addSelf _elemsIattrDecls (Set.toList _allNonterminals    )
                          else               _elemsIattrDecls
                         {-# LINE 901 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1213, column 8)
                    _lhsOagi =
                        ({-# LINE 1213 "src-ag/Transform.ag" #-}
                         (_allNonterminals    ,_allFields    ,_allAttrs    )
                         {-# LINE 907 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1215, column 8)
                    _allAttrs =
                        ({-# LINE 1215 "src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrs (Set.toList _allNonterminals    )
                              else               _elemsIattrs
                         {-# LINE 915 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1223, column 9)
                    _elemsOattrs =
                        ({-# LINE 1223 "src-ag/Transform.ag" #-}
                         Map.empty
                         {-# LINE 921 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 45, column 19)
                    _lhsOblocks =
                        ({-# LINE 45 "src-ag/Transform.ag" #-}
                         _elemsIblocks
                         {-# LINE 927 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                    _lhsOmoduleDecl =
                        ({-# LINE 1118 "src-ag/Transform.ag" #-}
                         _elemsImoduleDecl
                         {-# LINE 933 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 747, column 34)
                    _lhsOpragmas =
                        ({-# LINE 747 "src-ag/Transform.ag" #-}
                         _elemsIpragmas
                         {-# LINE 939 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallAttrDecls =
                        ({-# LINE 825 "src-ag/Transform.ag" #-}
                         _allAttrDecls
                         {-# LINE 945 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallAttrs =
                        ({-# LINE 1210 "src-ag/Transform.ag" #-}
                         _allAttrs
                         {-# LINE 951 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallFields =
                        ({-# LINE 129 "src-ag/Transform.ag" #-}
                         _allFields
                         {-# LINE 957 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallNonterminals =
                        ({-# LINE 89 "src-ag/Transform.ag" #-}
                         _allNonterminals
                         {-# LINE 963 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (down)
                    _elemsOoptions =
                        ({-# LINE 39 "src-ag/Transform.ag" #-}
                         _lhsIoptions
                         {-# LINE 969 "src-ag/Transform.hs" #-}
                         )
                    ( _elemsIattrDecls,_elemsIattrOrderCollect,_elemsIattrs,_elemsIblocks,_elemsIcollectedArounds,_elemsIcollectedAugments,_elemsIcollectedConstructorsMap,_elemsIcollectedFields,_elemsIcollectedInsts,_elemsIcollectedMerges,_elemsIcollectedNames,_elemsIcollectedRules,_elemsIcollectedSetNames,_elemsIcollectedSigs,_elemsIcollectedUniques,_elemsIctxCollect,_elemsIdefSets,_elemsIderivings,_elemsIerrors,_elemsImoduleDecl,_elemsIparamsCollect,_elemsIpragmas,_elemsIquantCollect,_elemsIsemPragmasCollect,_elemsItypeSyns,_elemsIuseMap,_elemsIwrappers) =
                        elems_ _elemsOallAttrDecls _elemsOallAttrs _elemsOallConstructors _elemsOallFields _elemsOallNonterminals _elemsOattrDecls _elemsOattrs _elemsOdefSets _elemsOdefinedSets _elemsOoptions 
                in  ( _lhsOagi,_lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas))) )
-- Alt ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
   alternatives:
      alternative Alt:
         child pos            : {Pos}
         child names          : ConstructorSet 
         child fields         : {Fields}
-}
-- cata
sem_Alt :: Alt  ->
           T_Alt 
sem_Alt (Alt _pos _names _fields )  =
    (sem_Alt_Alt _pos (sem_ConstructorSet _names ) _fields )
-- semantic domain
newtype T_Alt  = T_Alt ((Map NontermIdent (Set ConstructorIdent)) ->
                        (Set NontermIdent) ->
                        (Set NontermIdent) ->
                        ( (Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)])))
data Inh_Alt  = Inh_Alt {allConstructors_Inh_Alt :: !((Map NontermIdent (Set ConstructorIdent))),allNonterminals_Inh_Alt :: !((Set NontermIdent)),nts_Inh_Alt :: !((Set NontermIdent))}
data Syn_Alt  = Syn_Alt {collectedConstructorNames_Syn_Alt :: !((Set ConstructorIdent)),collectedFields_Syn_Alt :: !(([(NontermIdent, ConstructorIdent, FieldMap)]))}
wrap_Alt :: T_Alt  ->
            Inh_Alt  ->
            Syn_Alt 
wrap_Alt (T_Alt sem ) (Inh_Alt _lhsIallConstructors _lhsIallNonterminals _lhsInts )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOcollectedFields) = sem _lhsIallConstructors _lhsIallNonterminals _lhsInts 
     in  (Syn_Alt _lhsOcollectedConstructorNames _lhsOcollectedFields ))
sem_Alt_Alt :: Pos ->
               T_ConstructorSet  ->
               Fields ->
               T_Alt 
sem_Alt_Alt pos_ (T_ConstructorSet names_ ) fields_  =
    (T_Alt (\ _lhsIallConstructors
              _lhsIallNonterminals
              _lhsInts ->
                (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                     _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                     _namesIcollectedConstructorNames :: (Set ConstructorIdent)
                     _namesIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                     _namesIerrors :: (Seq Error)
                     -- "src-ag/Transform.ag"(line 239, column 10)
                     _lhsOcollectedFields =
                         ({-# LINE 239 "src-ag/Transform.ag" #-}
                          let fieldTable =
                               [ (attr, makeType _lhsIallNonterminals tp)
                               | (attr, tp) <- fields_
                               ]
                          in   [ (nt, con, fieldTable)
                               | nt  <- Set.toList _lhsInts
                               , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                               ]
                          {-# LINE 1032 "src-ag/Transform.hs" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 94, column 62)
                     _lhsOcollectedConstructorNames =
                         ({-# LINE 94 "src-ag/Transform.ag" #-}
                          _namesIcollectedConstructorNames
                          {-# LINE 1038 "src-ag/Transform.hs" #-}
                          )
                     ( _namesIcollectedConstructorNames,_namesIconstructors,_namesIerrors) =
                         names_ 
                 in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
-- Alts --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
   alternatives:
      alternative Cons:
         child hd             : Alt 
         child tl             : Alts 
      alternative Nil:
-}
-- cata
sem_Alts :: Alts  ->
            T_Alts 
sem_Alts list  =
    (Prelude.foldr sem_Alts_Cons sem_Alts_Nil (Prelude.map sem_Alt list) )
-- semantic domain
newtype T_Alts  = T_Alts ((Map NontermIdent (Set ConstructorIdent)) ->
                          (Set NontermIdent) ->
                          (Set NontermIdent) ->
                          ( (Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)])))
data Inh_Alts  = Inh_Alts {allConstructors_Inh_Alts :: !((Map NontermIdent (Set ConstructorIdent))),allNonterminals_Inh_Alts :: !((Set NontermIdent)),nts_Inh_Alts :: !((Set NontermIdent))}
data Syn_Alts  = Syn_Alts {collectedConstructorNames_Syn_Alts :: !((Set ConstructorIdent)),collectedFields_Syn_Alts :: !(([(NontermIdent, ConstructorIdent, FieldMap)]))}
wrap_Alts :: T_Alts  ->
             Inh_Alts  ->
             Syn_Alts 
wrap_Alts (T_Alts sem ) (Inh_Alts _lhsIallConstructors _lhsIallNonterminals _lhsInts )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOcollectedFields) = sem _lhsIallConstructors _lhsIallNonterminals _lhsInts 
     in  (Syn_Alts _lhsOcollectedConstructorNames _lhsOcollectedFields ))
sem_Alts_Cons :: T_Alt  ->
                 T_Alts  ->
                 T_Alts 
sem_Alts_Cons (T_Alt hd_ ) (T_Alts tl_ )  =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _hdOallNonterminals :: (Set NontermIdent)
                      _hdOnts :: (Set NontermIdent)
                      _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _tlOallNonterminals :: (Set NontermIdent)
                      _tlOnts :: (Set NontermIdent)
                      _hdIcollectedConstructorNames :: (Set ConstructorIdent)
                      _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _tlIcollectedConstructorNames :: (Set ConstructorIdent)
                      _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- use rule "src-ag/Transform.ag"(line 94, column 62)
                      _lhsOcollectedConstructorNames =
                          ({-# LINE 94 "src-ag/Transform.ag" #-}
                           _hdIcollectedConstructorNames `Set.union` _tlIcollectedConstructorNames
                           {-# LINE 1100 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           _hdIcollectedFields ++ _tlIcollectedFields
                           {-# LINE 1106 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _hdOallConstructors =
                          ({-# LINE 97 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 1112 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _hdOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1118 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _hdOnts =
                          ({-# LINE 168 "src-ag/Transform.ag" #-}
                           _lhsInts
                           {-# LINE 1124 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _tlOallConstructors =
                          ({-# LINE 97 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 1130 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _tlOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1136 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _tlOnts =
                          ({-# LINE 168 "src-ag/Transform.ag" #-}
                           _lhsInts
                           {-# LINE 1142 "src-ag/Transform.hs" #-}
                           )
                      ( _hdIcollectedConstructorNames,_hdIcollectedFields) =
                          hd_ _hdOallConstructors _hdOallNonterminals _hdOnts 
                      ( _tlIcollectedConstructorNames,_tlIcollectedFields) =
                          tl_ _tlOallConstructors _tlOallNonterminals _tlOnts 
                  in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
sem_Alts_Nil :: T_Alts 
sem_Alts_Nil  =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- use rule "src-ag/Transform.ag"(line 94, column 62)
                      _lhsOcollectedConstructorNames =
                          ({-# LINE 94 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 1160 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1166 "src-ag/Transform.hs" #-}
                           )
                  in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
-- Attrs -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
      synthesized attributes:
         errors               : Seq Error
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
   alternatives:
      alternative Attrs:
         child pos            : {Pos}
         child inh            : {AttrNames}
         child chn            : {AttrNames}
         child syn            : {AttrNames}
         visit 0:
            local _tup1       : _
            local attrDecls   : _
            local errors      : _
            local _tup2       : _
            local inherited   : _
            local synthesized : _
            local useMap      : _
            local errors1     : _
-}
-- cata
sem_Attrs :: Attrs  ->
             T_Attrs 
sem_Attrs (Attrs _pos _inh _chn _syn )  =
    (sem_Attrs_Attrs _pos _inh _chn _syn )
-- semantic domain
newtype T_Attrs  = T_Attrs (DataTypes ->
                            (Set NontermIdent) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Set NontermIdent) ->
                            Options ->
                            ( (Map NontermIdent (Attributes, Attributes)),(Map NontermIdent (Attributes, Attributes)),(Seq Error),(Map NontermIdent (Map Identifier (String,String,String)))))
data Inh_Attrs  = Inh_Attrs {allFields_Inh_Attrs :: !(DataTypes),allNonterminals_Inh_Attrs :: !((Set NontermIdent)),attrDecls_Inh_Attrs :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Attrs :: !((Map NontermIdent (Attributes, Attributes))),nts_Inh_Attrs :: !((Set NontermIdent)),options_Inh_Attrs :: !(Options)}
data Syn_Attrs  = Syn_Attrs {attrDecls_Syn_Attrs :: !((Map NontermIdent (Attributes, Attributes))),attrs_Syn_Attrs :: !((Map NontermIdent (Attributes, Attributes))),errors_Syn_Attrs :: !((Seq Error)),useMap_Syn_Attrs :: !((Map NontermIdent (Map Identifier (String,String,String))))}
wrap_Attrs :: T_Attrs  ->
              Inh_Attrs  ->
              Syn_Attrs 
wrap_Attrs (T_Attrs sem ) (Inh_Attrs _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions )  =
    (let ( _lhsOattrDecls,_lhsOattrs,_lhsOerrors,_lhsOuseMap) = sem _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions 
     in  (Syn_Attrs _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap ))
sem_Attrs_Attrs :: Pos ->
                   AttrNames ->
                   AttrNames ->
                   AttrNames ->
                   T_Attrs 
sem_Attrs_Attrs pos_ inh_ chn_ syn_  =
    (T_Attrs (\ _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsInts
                _lhsIoptions ->
                  (let _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOerrors :: (Seq Error)
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       -- "src-ag/Transform.ag"(line 949, column 15)
                       __tup1 =
                           ({-# LINE 949 "src-ag/Transform.ag" #-}
                            checkAttrs _lhsIallFields (Set.toList _lhsInts) _inherited _synthesized _lhsIattrDecls
                            {-# LINE 1240 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 949, column 15)
                       (_attrDecls,_) =
                           ({-# LINE 949 "src-ag/Transform.ag" #-}
                            __tup1
                            {-# LINE 1246 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 949, column 15)
                       (_,_errors) =
                           ({-# LINE 949 "src-ag/Transform.ag" #-}
                            __tup1
                            {-# LINE 1252 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 951, column 15)
                       __tup2 =
                           ({-# LINE 951 "src-ag/Transform.ag" #-}
                            let splitAttrs xs = unzip [ ((n,makeType _lhsIallNonterminals t),(n,ud))
                                                      | (n,t,ud) <- xs
                                                      ]
                                (inh,_)     = splitAttrs inh_
                                (chn,uses1) = splitAttrs chn_
                                (syn,uses2) = splitAttrs syn_
                                isUse (n,(e1,e2,_)) = not (null e1 || null e2)
                            in (inh++chn,chn++syn, Map.fromList (Prelude.filter isUse (uses1++uses2)))
                            {-# LINE 1265 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 951, column 15)
                       (_inherited,_,_) =
                           ({-# LINE 951 "src-ag/Transform.ag" #-}
                            __tup2
                            {-# LINE 1271 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 951, column 15)
                       (_,_synthesized,_) =
                           ({-# LINE 951 "src-ag/Transform.ag" #-}
                            __tup2
                            {-# LINE 1277 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 951, column 15)
                       (_,_,_useMap) =
                           ({-# LINE 951 "src-ag/Transform.ag" #-}
                            __tup2
                            {-# LINE 1283 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 959, column 11)
                       _lhsOuseMap =
                           ({-# LINE 959 "src-ag/Transform.ag" #-}
                            Map.fromList (zip (Set.toList _lhsInts) (repeat _useMap))
                            {-# LINE 1289 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 961, column 11)
                       _errors1 =
                           ({-# LINE 961 "src-ag/Transform.ag" #-}
                            if checkParseTy _lhsIoptions
                            then let attrs  = inh_ ++ syn_ ++ chn_
                                     items = map (\(ident,tp,_) -> (getPos ident, tp)) attrs
                                     errs  = map check items
                                     check (pos,Haskell s) =
                                       let exp = Expression pos tks
                                           tks = [tk]
                                           tk  = HsToken s pos
                                       in Seq.fromList $ checkTy exp
                                     check _ = Seq.empty
                                 in foldr (Seq.><) Seq.empty errs
                            else Seq.empty
                            {-# LINE 1306 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 973, column 11)
                       _lhsOerrors =
                           ({-# LINE 973 "src-ag/Transform.ag" #-}
                            _errors     Seq.>< _errors1
                            {-# LINE 1312 "src-ag/Transform.hs" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1227, column 11)
                       _lhsOattrs =
                           ({-# LINE 1227 "src-ag/Transform.ag" #-}
                            let insert decls nt = if Map.member nt decls
                                                    then  Map.update (\(inh,syn) -> Just ( Map.union inh $ Map.fromList _inherited
                                                                                         , Map.union syn $ Map.fromList _synthesized)) nt decls
                                                    else  Map.insert nt (Map.fromList _inherited, Map.fromList _synthesized) decls
                            in  foldl insert _lhsIattrs (Set.toList _lhsInts)
                            {-# LINE 1322 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (from local)
                       _lhsOattrDecls =
                           ({-# LINE 137 "src-ag/Transform.ag" #-}
                            _attrDecls
                            {-# LINE 1328 "src-ag/Transform.hs" #-}
                            )
                   in  ( _lhsOattrDecls,_lhsOattrs,_lhsOerrors,_lhsOuseMap))) )
-- ConstructorSet ----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         constructors         : (Set ConstructorIdent->Set ConstructorIdent)
         errors               : Seq Error
   alternatives:
      alternative CAll:
      alternative CDifference:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CName:
         child name           : {ConstructorIdent}
      alternative CUnion:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
-}
-- cata
sem_ConstructorSet :: ConstructorSet  ->
                      T_ConstructorSet 
sem_ConstructorSet (CAll )  =
    (sem_ConstructorSet_CAll )
sem_ConstructorSet (CDifference _set1 _set2 )  =
    (sem_ConstructorSet_CDifference (sem_ConstructorSet _set1 ) (sem_ConstructorSet _set2 ) )
sem_ConstructorSet (CName _name )  =
    (sem_ConstructorSet_CName _name )
sem_ConstructorSet (CUnion _set1 _set2 )  =
    (sem_ConstructorSet_CUnion (sem_ConstructorSet _set1 ) (sem_ConstructorSet _set2 ) )
-- semantic domain
newtype T_ConstructorSet  = T_ConstructorSet (( (Set ConstructorIdent),((Set ConstructorIdent->Set ConstructorIdent)),(Seq Error)))
data Inh_ConstructorSet  = Inh_ConstructorSet {}
data Syn_ConstructorSet  = Syn_ConstructorSet {collectedConstructorNames_Syn_ConstructorSet :: !((Set ConstructorIdent)),constructors_Syn_ConstructorSet :: !(((Set ConstructorIdent->Set ConstructorIdent))),errors_Syn_ConstructorSet :: !((Seq Error))}
wrap_ConstructorSet :: T_ConstructorSet  ->
                       Inh_ConstructorSet  ->
                       Syn_ConstructorSet 
wrap_ConstructorSet (T_ConstructorSet sem ) (Inh_ConstructorSet )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors) = sem 
     in  (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors ))
sem_ConstructorSet_CAll :: T_ConstructorSet 
sem_ConstructorSet_CAll  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 725, column 17)
                           _lhsOconstructors =
                               ({-# LINE 725 "src-ag/Transform.ag" #-}
                                \ds -> ds
                                {-# LINE 1379 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 94, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 94 "src-ag/Transform.ag" #-}
                                Set.empty
                                {-# LINE 1385 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 43, column 19)
                           _lhsOerrors =
                               ({-# LINE 43 "src-ag/Transform.ag" #-}
                                Seq.empty
                                {-# LINE 1391 "src-ag/Transform.hs" #-}
                                )
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CDifference :: T_ConstructorSet  ->
                                  T_ConstructorSet  ->
                                  T_ConstructorSet 
sem_ConstructorSet_CDifference (T_ConstructorSet set1_ ) (T_ConstructorSet set2_ )  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 724, column 17)
                           _lhsOconstructors =
                               ({-# LINE 724 "src-ag/Transform.ag" #-}
                                \ds -> _set1Iconstructors ds `Set.difference` _set2Iconstructors ds
                                {-# LINE 1411 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 94, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 94 "src-ag/Transform.ag" #-}
                                _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                                {-# LINE 1417 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 43, column 19)
                           _lhsOerrors =
                               ({-# LINE 43 "src-ag/Transform.ag" #-}
                                _set1Ierrors Seq.>< _set2Ierrors
                                {-# LINE 1423 "src-ag/Transform.hs" #-}
                                )
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               set1_ 
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               set2_ 
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CName :: ConstructorIdent ->
                            T_ConstructorSet 
sem_ConstructorSet_CName name_  =
    (T_ConstructorSet (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOerrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 569, column 11)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 569 "src-ag/Transform.ag" #-}
                                Set.singleton name_
                                {-# LINE 1440 "src-ag/Transform.hs" #-}
                                )
                           -- "src-ag/Transform.ag"(line 722, column 17)
                           _lhsOconstructors =
                               ({-# LINE 722 "src-ag/Transform.ag" #-}
                                \ds -> Set.singleton name_
                                {-# LINE 1446 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 43, column 19)
                           _lhsOerrors =
                               ({-# LINE 43 "src-ag/Transform.ag" #-}
                                Seq.empty
                                {-# LINE 1452 "src-ag/Transform.hs" #-}
                                )
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CUnion :: T_ConstructorSet  ->
                             T_ConstructorSet  ->
                             T_ConstructorSet 
sem_ConstructorSet_CUnion (T_ConstructorSet set1_ ) (T_ConstructorSet set2_ )  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 723, column 17)
                           _lhsOconstructors =
                               ({-# LINE 723 "src-ag/Transform.ag" #-}
                                \ds -> _set1Iconstructors ds `Set.union`      _set2Iconstructors ds
                                {-# LINE 1472 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 94, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 94 "src-ag/Transform.ag" #-}
                                _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                                {-# LINE 1478 "src-ag/Transform.hs" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 43, column 19)
                           _lhsOerrors =
                               ({-# LINE 43 "src-ag/Transform.ag" #-}
                                _set1Ierrors Seq.>< _set2Ierrors
                                {-# LINE 1484 "src-ag/Transform.hs" #-}
                                )
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               set1_ 
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               set2_ 
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
-- Elem --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         quantCollect         : QuantMap
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Attr:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child quants         : {[String]}
         child attrs          : Attrs 
      alternative Data:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child params         : {[Identifier]}
         child attrs          : Attrs 
         child alts           : Alts 
         child ext            : {Bool}
      alternative Deriving:
         child pos            : {Pos}
         child set            : NontSet 
         child classes        : {[NontermIdent]}
      alternative Module:
         child pos            : {Pos}
         child name           : {String}
         child exports        : {String}
         child imports        : {String}
      alternative Nocatas:
         child pos            : {Pos}
         child set            : NontSet 
      alternative Pragma:
         child pos            : {Pos}
         child names          : {[NontermIdent]}
      alternative Sem:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
         child quants         : {[String]}
         child alts           : SemAlts 
      alternative Set:
         child pos            : {Pos}
         child name           : {NontermIdent}
         child merge          : {Bool}
         child set            : NontSet 
         visit 0:
            local _tup3       : _
            local defSets2    : _
            local errs        : _
      alternative Txt:
         child pos            : {Pos}
         child name           : {Identifier}
         child mbNt           : {Maybe NontermIdent}
         child lines          : {[String]}
         visit 0:
            local blockInfo   : _
            local blockValue  : _
      alternative Type:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child name           : {NontermIdent}
         child params         : {[Identifier]}
         child type           : {ComplexType}
         visit 0:
            local expanded    : _
            local argType     : _
      alternative Wrapper:
         child pos            : {Pos}
         child set            : NontSet 
-}
-- cata
sem_Elem :: Elem  ->
            T_Elem 
sem_Elem (Attr _pos _ctx _names _quants _attrs )  =
    (sem_Elem_Attr _pos _ctx (sem_NontSet _names ) _quants (sem_Attrs _attrs ) )
sem_Elem (Data _pos _ctx _names _params _attrs _alts _ext )  =
    (sem_Elem_Data _pos _ctx (sem_NontSet _names ) _params (sem_Attrs _attrs ) (sem_Alts _alts ) _ext )
sem_Elem (Deriving _pos _set _classes )  =
    (sem_Elem_Deriving _pos (sem_NontSet _set ) _classes )
sem_Elem (Module _pos _name _exports _imports )  =
    (sem_Elem_Module _pos _name _exports _imports )
sem_Elem (Nocatas _pos _set )  =
    (sem_Elem_Nocatas _pos (sem_NontSet _set ) )
sem_Elem (Pragma _pos _names )  =
    (sem_Elem_Pragma _pos _names )
sem_Elem (Sem _pos _ctx _names _attrs _quants _alts )  =
    (sem_Elem_Sem _pos _ctx (sem_NontSet _names ) (sem_Attrs _attrs ) _quants (sem_SemAlts _alts ) )
sem_Elem (Set _pos _name _merge _set )  =
    (sem_Elem_Set _pos _name _merge (sem_NontSet _set ) )
sem_Elem (Txt _pos _name _mbNt _lines )  =
    (sem_Elem_Txt _pos _name _mbNt _lines )
sem_Elem (Type _pos _ctx _name _params _type )  =
    (sem_Elem_Type _pos _ctx _name _params _type )
sem_Elem (Wrapper _pos _set )  =
    (sem_Elem_Wrapper _pos (sem_NontSet _set ) )
-- semantic domain
newtype T_Elem  = T_Elem ((Map NontermIdent (Attributes, Attributes)) ->
                          (Map NontermIdent (Attributes, Attributes)) ->
                          (Map NontermIdent (Set ConstructorIdent)) ->
                          DataTypes ->
                          (Set NontermIdent) ->
                          (Map NontermIdent (Attributes, Attributes)) ->
                          (Map NontermIdent (Attributes, Attributes)) ->
                          (Map Identifier (Set NontermIdent,Set Identifier)) ->
                          DefinedSets ->
                          Options ->
                          ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,(Map NontermIdent (Attributes, Attributes)),Blocks,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),QuantMap,PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elem  = Inh_Elem {allAttrDecls_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),allConstructors_Inh_Elem :: !((Map NontermIdent (Set ConstructorIdent))),allFields_Inh_Elem :: !(DataTypes),allNonterminals_Inh_Elem :: !((Set NontermIdent)),attrDecls_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),defSets_Inh_Elem :: !((Map Identifier (Set NontermIdent,Set Identifier))),definedSets_Inh_Elem :: !(DefinedSets),options_Inh_Elem :: !(Options)}
data Syn_Elem  = Syn_Elem {attrDecls_Syn_Elem :: !((Map NontermIdent (Attributes, Attributes))),attrOrderCollect_Syn_Elem :: !(AttrOrderMap),attrs_Syn_Elem :: !((Map NontermIdent (Attributes, Attributes))),blocks_Syn_Elem :: !(Blocks),collectedArounds_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedConstructorsMap_Syn_Elem :: !((Map NontermIdent (Set ConstructorIdent))),collectedFields_Syn_Elem :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedInsts_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedNames_Syn_Elem :: !((Set Identifier)),collectedRules_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSetNames_Syn_Elem :: !((Set Identifier)),collectedSigs_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),ctxCollect_Syn_Elem :: !(ContextMap),defSets_Syn_Elem :: !((Map Identifier (Set NontermIdent,Set Identifier))),derivings_Syn_Elem :: !(Derivings),errors_Syn_Elem :: !((Seq Error)),moduleDecl_Syn_Elem :: !((Maybe (String,String,String))),paramsCollect_Syn_Elem :: !(ParamMap),pragmas_Syn_Elem :: !((Options -> Options)),quantCollect_Syn_Elem :: !(QuantMap),semPragmasCollect_Syn_Elem :: !(PragmaMap),typeSyns_Syn_Elem :: !(TypeSyns),useMap_Syn_Elem :: !((Map NontermIdent (Map Identifier (String,String,String)))),wrappers_Syn_Elem :: !((Set NontermIdent))}
wrap_Elem :: T_Elem  ->
             Inh_Elem  ->
             Syn_Elem 
wrap_Elem (T_Elem sem ) (Inh_Elem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions )  =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions 
     in  (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers ))
sem_Elem_Attr :: Pos ->
                 ClassContext ->
                 T_NontSet  ->
                 ([String]) ->
                 T_Attrs  ->
                 T_Elem 
sem_Elem_Attr pos_ ctx_ (T_NontSet names_ ) quants_ (T_Attrs attrs_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOctxCollect :: ContextMap
                      _lhsOquantCollect :: QuantMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      -- "src-ag/Transform.ag"(line 883, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 883 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 1709 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 908, column 7)
                      _lhsOquantCollect =
                          ({-# LINE 908 "src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 1717 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 944, column 10)
                      _attrsOnts =
                          ({-# LINE 944 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 1723 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1729 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1735 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1741 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1747 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1753 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1759 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1765 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1771 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 1777 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1783 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 1789 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1795 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1801 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1807 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors
                           {-# LINE 1813 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 1819 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1825 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 1831 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1837 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1843 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 1849 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 1855 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 1861 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 1867 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 1873 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 1879 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1885 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 1891 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 1897 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1903 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 1909 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 1915 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 39 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 1921 "src-ag/Transform.hs" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets 
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Data :: Pos ->
                 ClassContext ->
                 T_NontSet  ->
                 ([Identifier]) ->
                 T_Attrs  ->
                 T_Alts  ->
                 Bool ->
                 T_Elem 
sem_Elem_Data pos_ ctx_ (T_NontSet names_ ) params_ (T_Attrs attrs_ ) (T_Alts alts_ ) ext_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _altsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _altsOallNonterminals :: (Set NontermIdent)
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIcollectedConstructorNames :: (Set ConstructorIdent)
                      _altsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- "src-ag/Transform.ag"(line 171, column 10)
                      _altsOnts =
                          ({-# LINE 171 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 1999 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 575, column 11)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 575 "src-ag/Transform.ag" #-}
                           Map.fromList
                           [ (n, _altsIcollectedConstructorNames)
                           | n <- Set.toList _namesInontSet
                           ]
                           {-# LINE 2008 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 860, column 7)
                      _lhsOparamsCollect =
                          ({-# LINE 860 "src-ag/Transform.ag" #-}
                           if null params_
                           then Map.empty
                           else Map.fromList [(nt, params_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2016 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 883, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 883 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2024 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 943, column 10)
                      _attrsOnts =
                          ({-# LINE 943 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 2030 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2036 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2042 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2048 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2054 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           _altsIcollectedFields
                           {-# LINE 2060 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2066 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2072 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 2078 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2084 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2090 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2096 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2102 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2108 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors
                           {-# LINE 2114 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 2120 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2126 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2132 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2138 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2144 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 2150 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2156 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 2162 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 2168 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2174 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2180 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2186 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2192 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2198 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2204 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2210 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2216 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 39 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 2222 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOallConstructors =
                          ({-# LINE 97 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 2228 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2234 "src-ag/Transform.hs" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets 
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions 
                      ( _altsIcollectedConstructorNames,_altsIcollectedFields) =
                          alts_ _altsOallConstructors _altsOallNonterminals _altsOnts 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Deriving :: Pos ->
                     T_NontSet  ->
                     ([NontermIdent]) ->
                     T_Elem 
sem_Elem_Deriving pos_ (T_NontSet set_ ) classes_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOderivings :: Derivings
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 928, column 14)
                      _lhsOderivings =
                          ({-# LINE 928 "src-ag/Transform.ag" #-}
                           Map.fromList [(nt,Set.fromList classes_) | nt <- Set.toList _setInontSet]
                           {-# LINE 2295 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2301 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2307 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2313 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2319 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2325 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2331 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2337 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2343 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 2349 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2355 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2361 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2367 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2373 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2379 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 2385 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 2391 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2397 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2403 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2409 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2415 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2421 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2427 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2433 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2439 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2445 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2451 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2457 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2463 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2469 "src-ag/Transform.hs" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Module :: Pos ->
                   String ->
                   String ->
                   String ->
                   T_Elem 
sem_Elem_Module pos_ name_ exports_ imports_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 1122, column 7)
                      _lhsOmoduleDecl =
                          ({-# LINE 1122 "src-ag/Transform.ag" #-}
                           Just (name_, exports_, imports_)
                           {-# LINE 2521 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2527 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2533 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2539 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2545 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2551 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2557 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2563 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2569 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2575 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2581 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2587 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2593 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2599 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2605 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2611 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 2617 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2623 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2629 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2635 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2641 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2647 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2653 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2659 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2665 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2671 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2677 "src-ag/Transform.hs" #-}
                           )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Nocatas :: Pos ->
                    T_NontSet  ->
                    T_Elem 
sem_Elem_Nocatas pos_ (T_NontSet set_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOpragmas :: (Options -> Options)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 741, column 14)
                      _lhsOpragmas =
                          ({-# LINE 741 "src-ag/Transform.ag" #-}
                           \o -> o { nocatas = _setInontSet `Set.union` nocatas o }
                           {-# LINE 2731 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2737 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2743 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2749 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2755 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2761 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2767 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2773 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2779 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 2785 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2791 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2797 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2803 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2809 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2815 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2821 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 2827 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 2833 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2839 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2845 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2851 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2857 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2863 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2869 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2875 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2881 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2887 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2893 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2899 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2905 "src-ag/Transform.hs" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Pragma :: Pos ->
                   ([NontermIdent]) ->
                   T_Elem 
sem_Elem_Pragma pos_ names_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOpragmas :: (Options -> Options)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 750, column 13)
                      _lhsOpragmas =
                          ({-# LINE 750 "src-ag/Transform.ag" #-}
                           let mk n o = case getName n of
                                          "gencatas"     -> o { folds       = True  }
                                          "nogencatas"   -> o { folds       = False }
                                          "gendatas"     -> o { dataTypes   = True  }
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
                                          "sepsemmods"   -> o { sepSemMods = True }
                                          "genlinepragmas" -> o { genLinePragmas = True }
                                          "newtypes"       -> o { newtypes = True }
                                          "nonewtypes"     -> o { newtypes = False }
                                          "nooptimizations" -> o { noOptimizations = True }
                                          "kennedywarren"   -> o { kennedyWarren = True }
                                          "rename"          -> o { rename = True }
                                          _              -> o
                           in \o -> foldr mk o names_
                           {-# LINE 2999 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3005 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3011 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3017 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3023 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3029 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3035 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3041 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3047 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3053 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3059 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3065 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3071 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3077 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3083 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3089 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 3095 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3101 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3107 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3113 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3119 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3125 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3131 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3137 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3143 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3149 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3155 "src-ag/Transform.hs" #-}
                           )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Sem :: Pos ->
                ClassContext ->
                T_NontSet  ->
                T_Attrs  ->
                ([String]) ->
                T_SemAlts  ->
                T_Elem 
sem_Elem_Sem pos_ ctx_ (T_NontSet names_ ) (T_Attrs attrs_ ) quants_ (T_SemAlts alts_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOctxCollect :: ContextMap
                      _lhsOquantCollect :: QuantMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _altsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallFields :: DataTypes
                      _altsOoptions :: Options
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIattrOrderCollect :: AttrOrderMap
                      _altsIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _altsIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _altsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _altsIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _altsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _altsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _altsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _altsIerrors :: (Seq Error)
                      _altsIsemPragmasCollect :: PragmaMap
                      -- "src-ag/Transform.ag"(line 172, column 10)
                      _altsOnts =
                          ({-# LINE 172 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 3238 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 883, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 883 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 3246 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 908, column 7)
                      _lhsOquantCollect =
                          ({-# LINE 908 "src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 3254 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 945, column 10)
                      _attrsOnts =
                          ({-# LINE 945 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 3260 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           _altsIattrOrderCollect
                           {-# LINE 3266 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3272 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           _altsIcollectedArounds
                           {-# LINE 3278 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           _altsIcollectedAugments
                           {-# LINE 3284 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3290 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3296 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           _altsIcollectedInsts
                           {-# LINE 3302 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           _altsIcollectedMerges
                           {-# LINE 3308 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 3314 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           _altsIcollectedRules
                           {-# LINE 3320 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3326 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           _altsIcollectedSigs
                           {-# LINE 3332 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           _altsIcollectedUniques
                           {-# LINE 3338 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3344 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors Seq.>< _altsIerrors
                           {-# LINE 3350 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3356 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3362 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 3368 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           _altsIsemPragmasCollect
                           {-# LINE 3374 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3380 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 3386 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3392 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 3398 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 3404 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3410 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3416 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3422 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 3428 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3434 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3440 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3446 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3452 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 39 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 3458 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOallAttrDecls =
                          ({-# LINE 825 "src-ag/Transform.ag" #-}
                           _lhsIallAttrDecls
                           {-# LINE 3464 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOallAttrs =
                          ({-# LINE 1210 "src-ag/Transform.ag" #-}
                           _lhsIallAttrs
                           {-# LINE 3470 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3476 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _altsOoptions =
                          ({-# LINE 39 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 3482 "src-ag/Transform.hs" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets 
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions 
                      ( _altsIattrOrderCollect,_altsIcollectedArounds,_altsIcollectedAugments,_altsIcollectedInsts,_altsIcollectedMerges,_altsIcollectedRules,_altsIcollectedSigs,_altsIcollectedUniques,_altsIerrors,_altsIsemPragmasCollect) =
                          alts_ _altsOallAttrDecls _altsOallAttrs _altsOallFields _altsOnts _altsOoptions 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Set :: Pos ->
                NontermIdent ->
                Bool ->
                T_NontSet  ->
                T_Elem 
sem_Elem_Set pos_ name_ merge_ (T_NontSet set_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _lhsOerrors :: (Seq Error)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 552, column 10)
                      _lhsOcollectedSetNames =
                          ({-# LINE 552 "src-ag/Transform.ag" #-}
                           Set.singleton name_
                           {-# LINE 3544 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 659, column 13)
                      __tup3 =
                          ({-# LINE 659 "src-ag/Transform.ag" #-}
                           let allUsedNames = Set.unions [ maybe (Set.singleton n)
                                                                 snd
                                                                 (Map.lookup n _lhsIdefSets)
                                                         | n <- Set.toList _setIcollectedNames
                                                         ]
                               (nontSet,e1) | Set.member name_ allUsedNames
                                                        = (Set.empty, Seq.singleton(CyclicSet name_))
                                            | otherwise = (_setInontSet, Seq.empty)
                               (res, e2) = let toAdd = (nontSet,Set.insert name_ allUsedNames)
                                               union (a,b) (c,d) = (a `Set.union` c, b `Set.union` d)
                                           in if Set.member name_ _lhsIallNonterminals || not merge_
                                              then checkDuplicate DupSet name_ toAdd _lhsIdefSets
                                              else (Map.insertWith union name_ toAdd _lhsIdefSets, Seq.empty)
                           in (res, e1 Seq.>< e2)
                           {-# LINE 3563 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 659, column 13)
                      (_defSets2,_) =
                          ({-# LINE 659 "src-ag/Transform.ag" #-}
                           __tup3
                           {-# LINE 3569 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 659, column 13)
                      (_,_errs) =
                          ({-# LINE 659 "src-ag/Transform.ag" #-}
                           __tup3
                           {-# LINE 3575 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 673, column 9)
                      _lhsOdefSets =
                          ({-# LINE 673 "src-ag/Transform.ag" #-}
                           _defSets2
                           {-# LINE 3581 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 673, column 9)
                      _lhsOerrors =
                          ({-# LINE 674 "src-ag/Transform.ag" #-}
                           _errs >< _setIerrors
                           {-# LINE 3587 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3593 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3599 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3605 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3611 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3617 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3623 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3629 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3635 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 3641 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3647 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3653 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3659 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3665 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3671 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3677 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3683 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 3689 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3695 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3701 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3707 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3713 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3719 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3725 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3731 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3737 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3743 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 3749 "src-ag/Transform.hs" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Txt :: Pos ->
                Identifier ->
                (Maybe NontermIdent) ->
                ([String]) ->
                T_Elem 
sem_Elem_Txt pos_ name_ mbNt_ lines_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOblocks :: Blocks
                      _lhsOerrors :: (Seq Error)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 181, column 10)
                      _blockInfo =
                          ({-# LINE 181 "src-ag/Transform.ag" #-}
                           ( let nm = getName name_
                             in if nm == "imports"
                                then BlockImport
                                else if nm == "optpragmas"
                                     then BlockPragma
                                     else BlockOther
                           , mbNt_
                           )
                           {-# LINE 3808 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 189, column 10)
                      _blockValue =
                          ({-# LINE 189 "src-ag/Transform.ag" #-}
                           [(lines_, pos_)]
                           {-# LINE 3814 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 190, column 10)
                      _lhsOblocks =
                          ({-# LINE 190 "src-ag/Transform.ag" #-}
                           Map.singleton _blockInfo     _blockValue
                           {-# LINE 3820 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 191, column 10)
                      _lhsOerrors =
                          ({-# LINE 191 "src-ag/Transform.ag" #-}
                           if checkParseBlock _lhsIoptions
                           then let exp = Expression pos_ tks
                                    tks = [tk]
                                    tk  = HsToken (unlines lines_) pos_
                                in Seq.fromList $ checkBlock $ exp
                           else Seq.empty
                           {-# LINE 3831 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3837 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3843 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3849 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3855 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3861 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3867 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3873 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3879 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3885 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3891 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3897 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3903 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3909 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3915 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3921 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3927 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 3933 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3939 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3945 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3951 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3957 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3963 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3969 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3975 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3981 "src-ag/Transform.hs" #-}
                           )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Type :: Pos ->
                 ClassContext ->
                 NontermIdent ->
                 ([Identifier]) ->
                 ComplexType ->
                 T_Elem 
sem_Elem_Type pos_ ctx_ name_ params_ type_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 249, column 10)
                      _lhsOcollectedFields =
                          ({-# LINE 249 "src-ag/Transform.ag" #-}
                           map (\(x,y)->(name_, x, y)) _expanded
                           {-# LINE 4032 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 555, column 11)
                      _lhsOcollectedNames =
                          ({-# LINE 555 "src-ag/Transform.ag" #-}
                           Set.singleton name_
                           {-# LINE 4038 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 609, column 11)
                      _expanded =
                          ({-# LINE 609 "src-ag/Transform.ag" #-}
                           case _argType of
                                   List tp -> [(Ident "Cons" pos_, [(Ident "hd" pos_, tp)
                                                                   ,(Ident "tl" pos_, NT name_ (map getName params_))
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
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_))
                                                                         ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   IntMap tp   -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, Haskell "Int")
                                                                         , (Ident "val" pos_, tp)
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_))
                                                                         ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   Tuple xs -> [(Ident "Tuple" pos_, xs)]
                           {-# LINE 4072 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 638, column 11)
                      _argType =
                          ({-# LINE 638 "src-ag/Transform.ag" #-}
                           case type_ of
                            Maybe tp       -> Maybe  (  makeType _lhsIallNonterminals tp)
                            Either tp1 tp2 -> Either (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                            List tp        -> List   (  makeType _lhsIallNonterminals tp)
                            Tuple xs       -> Tuple [(f,makeType _lhsIallNonterminals tp) | (f,tp) <- xs]
                            Map tp1 tp2    -> Map    (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                            IntMap tp      -> IntMap (  makeType _lhsIallNonterminals tp)
                           {-# LINE 4084 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 645, column 11)
                      _lhsOtypeSyns =
                          ({-# LINE 645 "src-ag/Transform.ag" #-}
                           [(name_,_argType)]
                           {-# LINE 4090 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 866, column 7)
                      _lhsOparamsCollect =
                          ({-# LINE 866 "src-ag/Transform.ag" #-}
                           if null params_
                           then Map.empty
                           else Map.singleton name_ params_
                           {-# LINE 4098 "src-ag/Transform.hs" #-}
                           )
                      -- "src-ag/Transform.ag"(line 889, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 889 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.singleton name_ ctx_
                           {-# LINE 4106 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4112 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4118 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4124 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4130 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4136 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4142 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4148 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4154 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4160 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4166 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4172 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4178 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 4184 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 4190 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4196 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4202 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4208 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4214 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 731, column 32)
                      _lhsOwrappers =
                          ({-# LINE 731 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4220 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4226 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4232 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 4238 "src-ag/Transform.hs" #-}
                           )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Wrapper :: Pos ->
                    T_NontSet  ->
                    T_Elem 
sem_Elem_Wrapper pos_ (T_NontSet set_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 734, column 13)
                      _lhsOwrappers =
                          ({-# LINE 734 "src-ag/Transform.ag" #-}
                           _setInontSet
                           {-# LINE 4292 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 824, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 824 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4298 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 45, column 19)
                      _lhsOblocks =
                          ({-# LINE 45 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4304 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 157, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 157 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4310 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 156, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 156 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4316 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 95, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 95 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4322 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 126, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 126 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4328 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 154, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 154 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4334 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 158, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 158 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4340 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 87, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 87 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 4346 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 152, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 152 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4352 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 86, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 86 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4358 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 153, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 153 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4364 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 155, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 155 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4370 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 879, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 879 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4376 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 921, column 33)
                      _lhsOderivings =
                          ({-# LINE 921 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4382 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 43, column 19)
                      _lhsOerrors =
                          ({-# LINE 43 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 4388 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1118 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 4394 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 856, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 856 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4400 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 747, column 34)
                      _lhsOpragmas =
                          ({-# LINE 747 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4406 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 904, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 904 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4412 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 796, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 796 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4418 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 596, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 596 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4424 "src-ag/Transform.hs" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 138, column 15)
                      _lhsOuseMap =
                          ({-# LINE 138 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4430 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 137 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4436 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1220 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4442 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 105 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 4448 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 129 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 4454 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 89 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 4460 "src-ag/Transform.hs" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 108 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 4466 "src-ag/Transform.hs" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets 
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
-- Elems -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         quantCollect         : QuantMap
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Cons:
         child hd             : Elem 
         child tl             : Elems 
      alternative Nil:
-}
-- cata
sem_Elems :: Elems  ->
             T_Elems 
sem_Elems list  =
    (Prelude.foldr sem_Elems_Cons sem_Elems_Nil (Prelude.map sem_Elem list) )
-- semantic domain
newtype T_Elems  = T_Elems ((Map NontermIdent (Attributes, Attributes)) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Map NontermIdent (Set ConstructorIdent)) ->
                            DataTypes ->
                            (Set NontermIdent) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Map Identifier (Set NontermIdent,Set Identifier)) ->
                            DefinedSets ->
                            Options ->
                            ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,(Map NontermIdent (Attributes, Attributes)),Blocks,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),QuantMap,PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elems  = Inh_Elems {allAttrDecls_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),allConstructors_Inh_Elems :: !((Map NontermIdent (Set ConstructorIdent))),allFields_Inh_Elems :: !(DataTypes),allNonterminals_Inh_Elems :: !((Set NontermIdent)),attrDecls_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),defSets_Inh_Elems :: !((Map Identifier (Set NontermIdent,Set Identifier))),definedSets_Inh_Elems :: !(DefinedSets),options_Inh_Elems :: !(Options)}
data Syn_Elems  = Syn_Elems {attrDecls_Syn_Elems :: !((Map NontermIdent (Attributes, Attributes))),attrOrderCollect_Syn_Elems :: !(AttrOrderMap),attrs_Syn_Elems :: !((Map NontermIdent (Attributes, Attributes))),blocks_Syn_Elems :: !(Blocks),collectedArounds_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedConstructorsMap_Syn_Elems :: !((Map NontermIdent (Set ConstructorIdent))),collectedFields_Syn_Elems :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedInsts_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedNames_Syn_Elems :: !((Set Identifier)),collectedRules_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSetNames_Syn_Elems :: !((Set Identifier)),collectedSigs_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),ctxCollect_Syn_Elems :: !(ContextMap),defSets_Syn_Elems :: !((Map Identifier (Set NontermIdent,Set Identifier))),derivings_Syn_Elems :: !(Derivings),errors_Syn_Elems :: !((Seq Error)),moduleDecl_Syn_Elems :: !((Maybe (String,String,String))),paramsCollect_Syn_Elems :: !(ParamMap),pragmas_Syn_Elems :: !((Options -> Options)),quantCollect_Syn_Elems :: !(QuantMap),semPragmasCollect_Syn_Elems :: !(PragmaMap),typeSyns_Syn_Elems :: !(TypeSyns),useMap_Syn_Elems :: !((Map NontermIdent (Map Identifier (String,String,String)))),wrappers_Syn_Elems :: !((Set NontermIdent))}
wrap_Elems :: T_Elems  ->
              Inh_Elems  ->
              Syn_Elems 
wrap_Elems (T_Elems sem ) (Inh_Elems _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions )  =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions 
     in  (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers ))
sem_Elems_Cons :: T_Elem  ->
                  T_Elems  ->
                  T_Elems 
sem_Elems_Cons (T_Elem hd_ ) (T_Elems tl_ )  =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallAttrs
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsIdefSets
                _lhsIdefinedSets
                _lhsIoptions ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOquantCollect :: QuantMap
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _hdOallFields :: DataTypes
                       _hdOallNonterminals :: (Set NontermIdent)
                       _hdOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOdefinedSets :: DefinedSets
                       _hdOoptions :: Options
                       _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _tlOallFields :: DataTypes
                       _tlOallNonterminals :: (Set NontermIdent)
                       _tlOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlOdefinedSets :: DefinedSets
                       _tlOoptions :: Options
                       _hdIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdIattrOrderCollect :: AttrOrderMap
                       _hdIattrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdIblocks :: Blocks
                       _hdIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _hdIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _hdIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _hdIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _hdIcollectedNames :: (Set Identifier)
                       _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _hdIcollectedSetNames :: (Set Identifier)
                       _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _hdIctxCollect :: ContextMap
                       _hdIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdIderivings :: Derivings
                       _hdIerrors :: (Seq Error)
                       _hdImoduleDecl :: (Maybe (String,String,String))
                       _hdIparamsCollect :: ParamMap
                       _hdIpragmas :: (Options -> Options)
                       _hdIquantCollect :: QuantMap
                       _hdIsemPragmasCollect :: PragmaMap
                       _hdItypeSyns :: TypeSyns
                       _hdIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _hdIwrappers :: (Set NontermIdent)
                       _tlIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlIattrOrderCollect :: AttrOrderMap
                       _tlIattrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlIblocks :: Blocks
                       _tlIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _tlIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _tlIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _tlIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _tlIcollectedNames :: (Set Identifier)
                       _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _tlIcollectedSetNames :: (Set Identifier)
                       _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _tlIctxCollect :: ContextMap
                       _tlIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlIderivings :: Derivings
                       _tlIerrors :: (Seq Error)
                       _tlImoduleDecl :: (Maybe (String,String,String))
                       _tlIparamsCollect :: ParamMap
                       _tlIpragmas :: (Options -> Options)
                       _tlIquantCollect :: QuantMap
                       _tlIsemPragmasCollect :: PragmaMap
                       _tlItypeSyns :: TypeSyns
                       _tlIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _tlIwrappers :: (Set NontermIdent)
                       -- use rule "src-ag/Transform.ag"(line 824, column 55)
                       _lhsOattrOrderCollect =
                           ({-# LINE 824 "src-ag/Transform.ag" #-}
                            _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                            {-# LINE 4661 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 45, column 19)
                       _lhsOblocks =
                           ({-# LINE 45 "src-ag/Transform.ag" #-}
                            _hdIblocks `mapUnionWithPlusPlus` _tlIblocks
                            {-# LINE 4667 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 157, column 32)
                       _lhsOcollectedArounds =
                           ({-# LINE 157 "src-ag/Transform.ag" #-}
                            _hdIcollectedArounds ++ _tlIcollectedArounds
                            {-# LINE 4673 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 156, column 32)
                       _lhsOcollectedAugments =
                           ({-# LINE 156 "src-ag/Transform.ag" #-}
                            _hdIcollectedAugments ++ _tlIcollectedAugments
                            {-# LINE 4679 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 95, column 48)
                       _lhsOcollectedConstructorsMap =
                           ({-# LINE 95 "src-ag/Transform.ag" #-}
                            _hdIcollectedConstructorsMap `mapUnionWithSetUnion` _tlIcollectedConstructorsMap
                            {-# LINE 4685 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 126, column 28)
                       _lhsOcollectedFields =
                           ({-# LINE 126 "src-ag/Transform.ag" #-}
                            _hdIcollectedFields ++ _tlIcollectedFields
                            {-# LINE 4691 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 154, column 32)
                       _lhsOcollectedInsts =
                           ({-# LINE 154 "src-ag/Transform.ag" #-}
                            _hdIcollectedInsts ++ _tlIcollectedInsts
                            {-# LINE 4697 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 158, column 32)
                       _lhsOcollectedMerges =
                           ({-# LINE 158 "src-ag/Transform.ag" #-}
                            _hdIcollectedMerges ++ _tlIcollectedMerges
                            {-# LINE 4703 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 87, column 50)
                       _lhsOcollectedNames =
                           ({-# LINE 87 "src-ag/Transform.ag" #-}
                            _hdIcollectedNames `Set.union` _tlIcollectedNames
                            {-# LINE 4709 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 152, column 32)
                       _lhsOcollectedRules =
                           ({-# LINE 152 "src-ag/Transform.ag" #-}
                            _hdIcollectedRules ++ _tlIcollectedRules
                            {-# LINE 4715 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 86, column 50)
                       _lhsOcollectedSetNames =
                           ({-# LINE 86 "src-ag/Transform.ag" #-}
                            _hdIcollectedSetNames `Set.union` _tlIcollectedSetNames
                            {-# LINE 4721 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 153, column 32)
                       _lhsOcollectedSigs =
                           ({-# LINE 153 "src-ag/Transform.ag" #-}
                            _hdIcollectedSigs ++ _tlIcollectedSigs
                            {-# LINE 4727 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 155, column 32)
                       _lhsOcollectedUniques =
                           ({-# LINE 155 "src-ag/Transform.ag" #-}
                            _hdIcollectedUniques ++ _tlIcollectedUniques
                            {-# LINE 4733 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 879, column 34)
                       _lhsOctxCollect =
                           ({-# LINE 879 "src-ag/Transform.ag" #-}
                            _hdIctxCollect `mergeCtx` _tlIctxCollect
                            {-# LINE 4739 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 921, column 33)
                       _lhsOderivings =
                           ({-# LINE 921 "src-ag/Transform.ag" #-}
                            _hdIderivings `mergeDerivings` _tlIderivings
                            {-# LINE 4745 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 43, column 19)
                       _lhsOerrors =
                           ({-# LINE 43 "src-ag/Transform.ag" #-}
                            _hdIerrors Seq.>< _tlIerrors
                            {-# LINE 4751 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                       _lhsOmoduleDecl =
                           ({-# LINE 1118 "src-ag/Transform.ag" #-}
                            _hdImoduleDecl `mplus` _tlImoduleDecl
                            {-# LINE 4757 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 856, column 37)
                       _lhsOparamsCollect =
                           ({-# LINE 856 "src-ag/Transform.ag" #-}
                            _hdIparamsCollect `mergeParams` _tlIparamsCollect
                            {-# LINE 4763 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 747, column 34)
                       _lhsOpragmas =
                           ({-# LINE 747 "src-ag/Transform.ag" #-}
                            _hdIpragmas . _tlIpragmas
                            {-# LINE 4769 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 904, column 36)
                       _lhsOquantCollect =
                           ({-# LINE 904 "src-ag/Transform.ag" #-}
                            _hdIquantCollect `mergeQuant` _tlIquantCollect
                            {-# LINE 4775 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 796, column 56)
                       _lhsOsemPragmasCollect =
                           ({-# LINE 796 "src-ag/Transform.ag" #-}
                            _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                            {-# LINE 4781 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 596, column 32)
                       _lhsOtypeSyns =
                           ({-# LINE 596 "src-ag/Transform.ag" #-}
                            _hdItypeSyns ++ _tlItypeSyns
                            {-# LINE 4787 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 138, column 15)
                       _lhsOuseMap =
                           ({-# LINE 138 "src-ag/Transform.ag" #-}
                            _hdIuseMap `merge` _tlIuseMap
                            {-# LINE 4793 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 731, column 32)
                       _lhsOwrappers =
                           ({-# LINE 731 "src-ag/Transform.ag" #-}
                            _hdIwrappers `Set.union` _tlIwrappers
                            {-# LINE 4799 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (up)
                       _lhsOattrDecls =
                           ({-# LINE 137 "src-ag/Transform.ag" #-}
                            _tlIattrDecls
                            {-# LINE 4805 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (up)
                       _lhsOattrs =
                           ({-# LINE 1220 "src-ag/Transform.ag" #-}
                            _tlIattrs
                            {-# LINE 4811 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (up)
                       _lhsOdefSets =
                           ({-# LINE 105 "src-ag/Transform.ag" #-}
                            _tlIdefSets
                            {-# LINE 4817 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallAttrDecls =
                           ({-# LINE 825 "src-ag/Transform.ag" #-}
                            _lhsIallAttrDecls
                            {-# LINE 4823 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallAttrs =
                           ({-# LINE 1210 "src-ag/Transform.ag" #-}
                            _lhsIallAttrs
                            {-# LINE 4829 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallConstructors =
                           ({-# LINE 97 "src-ag/Transform.ag" #-}
                            _lhsIallConstructors
                            {-# LINE 4835 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallFields =
                           ({-# LINE 129 "src-ag/Transform.ag" #-}
                            _lhsIallFields
                            {-# LINE 4841 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOallNonterminals =
                           ({-# LINE 89 "src-ag/Transform.ag" #-}
                            _lhsIallNonterminals
                            {-# LINE 4847 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOattrDecls =
                           ({-# LINE 137 "src-ag/Transform.ag" #-}
                            _lhsIattrDecls
                            {-# LINE 4853 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOattrs =
                           ({-# LINE 1220 "src-ag/Transform.ag" #-}
                            _lhsIattrs
                            {-# LINE 4859 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOdefSets =
                           ({-# LINE 105 "src-ag/Transform.ag" #-}
                            _lhsIdefSets
                            {-# LINE 4865 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOdefinedSets =
                           ({-# LINE 108 "src-ag/Transform.ag" #-}
                            _lhsIdefinedSets
                            {-# LINE 4871 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOoptions =
                           ({-# LINE 39 "src-ag/Transform.ag" #-}
                            _lhsIoptions
                            {-# LINE 4877 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallAttrDecls =
                           ({-# LINE 825 "src-ag/Transform.ag" #-}
                            _lhsIallAttrDecls
                            {-# LINE 4883 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallAttrs =
                           ({-# LINE 1210 "src-ag/Transform.ag" #-}
                            _lhsIallAttrs
                            {-# LINE 4889 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallConstructors =
                           ({-# LINE 97 "src-ag/Transform.ag" #-}
                            _lhsIallConstructors
                            {-# LINE 4895 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallFields =
                           ({-# LINE 129 "src-ag/Transform.ag" #-}
                            _lhsIallFields
                            {-# LINE 4901 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOallNonterminals =
                           ({-# LINE 89 "src-ag/Transform.ag" #-}
                            _lhsIallNonterminals
                            {-# LINE 4907 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _tlOattrDecls =
                           ({-# LINE 137 "src-ag/Transform.ag" #-}
                            _hdIattrDecls
                            {-# LINE 4913 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _tlOattrs =
                           ({-# LINE 1220 "src-ag/Transform.ag" #-}
                            _hdIattrs
                            {-# LINE 4919 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _tlOdefSets =
                           ({-# LINE 105 "src-ag/Transform.ag" #-}
                            _hdIdefSets
                            {-# LINE 4925 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOdefinedSets =
                           ({-# LINE 108 "src-ag/Transform.ag" #-}
                            _lhsIdefinedSets
                            {-# LINE 4931 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOoptions =
                           ({-# LINE 39 "src-ag/Transform.ag" #-}
                            _lhsIoptions
                            {-# LINE 4937 "src-ag/Transform.hs" #-}
                            )
                       ( _hdIattrDecls,_hdIattrOrderCollect,_hdIattrs,_hdIblocks,_hdIcollectedArounds,_hdIcollectedAugments,_hdIcollectedConstructorsMap,_hdIcollectedFields,_hdIcollectedInsts,_hdIcollectedMerges,_hdIcollectedNames,_hdIcollectedRules,_hdIcollectedSetNames,_hdIcollectedSigs,_hdIcollectedUniques,_hdIctxCollect,_hdIdefSets,_hdIderivings,_hdIerrors,_hdImoduleDecl,_hdIparamsCollect,_hdIpragmas,_hdIquantCollect,_hdIsemPragmasCollect,_hdItypeSyns,_hdIuseMap,_hdIwrappers) =
                           hd_ _hdOallAttrDecls _hdOallAttrs _hdOallConstructors _hdOallFields _hdOallNonterminals _hdOattrDecls _hdOattrs _hdOdefSets _hdOdefinedSets _hdOoptions 
                       ( _tlIattrDecls,_tlIattrOrderCollect,_tlIattrs,_tlIblocks,_tlIcollectedArounds,_tlIcollectedAugments,_tlIcollectedConstructorsMap,_tlIcollectedFields,_tlIcollectedInsts,_tlIcollectedMerges,_tlIcollectedNames,_tlIcollectedRules,_tlIcollectedSetNames,_tlIcollectedSigs,_tlIcollectedUniques,_tlIctxCollect,_tlIdefSets,_tlIderivings,_tlIerrors,_tlImoduleDecl,_tlIparamsCollect,_tlIpragmas,_tlIquantCollect,_tlIsemPragmasCollect,_tlItypeSyns,_tlIuseMap,_tlIwrappers) =
                           tl_ _tlOallAttrDecls _tlOallAttrs _tlOallConstructors _tlOallFields _tlOallNonterminals _tlOattrDecls _tlOattrs _tlOdefSets _tlOdefinedSets _tlOoptions 
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elems_Nil :: T_Elems 
sem_Elems_Nil  =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallAttrs
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsIdefSets
                _lhsIdefinedSets
                _lhsIoptions ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOquantCollect :: QuantMap
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       -- use rule "src-ag/Transform.ag"(line 824, column 55)
                       _lhsOattrOrderCollect =
                           ({-# LINE 824 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 4987 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 45, column 19)
                       _lhsOblocks =
                           ({-# LINE 45 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 4993 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 157, column 32)
                       _lhsOcollectedArounds =
                           ({-# LINE 157 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 4999 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 156, column 32)
                       _lhsOcollectedAugments =
                           ({-# LINE 156 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5005 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 95, column 48)
                       _lhsOcollectedConstructorsMap =
                           ({-# LINE 95 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5011 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 126, column 28)
                       _lhsOcollectedFields =
                           ({-# LINE 126 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5017 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 154, column 32)
                       _lhsOcollectedInsts =
                           ({-# LINE 154 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5023 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 158, column 32)
                       _lhsOcollectedMerges =
                           ({-# LINE 158 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5029 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 87, column 50)
                       _lhsOcollectedNames =
                           ({-# LINE 87 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5035 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 152, column 32)
                       _lhsOcollectedRules =
                           ({-# LINE 152 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5041 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 86, column 50)
                       _lhsOcollectedSetNames =
                           ({-# LINE 86 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5047 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 153, column 32)
                       _lhsOcollectedSigs =
                           ({-# LINE 153 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5053 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 155, column 32)
                       _lhsOcollectedUniques =
                           ({-# LINE 155 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5059 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 879, column 34)
                       _lhsOctxCollect =
                           ({-# LINE 879 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5065 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 921, column 33)
                       _lhsOderivings =
                           ({-# LINE 921 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5071 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 43, column 19)
                       _lhsOerrors =
                           ({-# LINE 43 "src-ag/Transform.ag" #-}
                            Seq.empty
                            {-# LINE 5077 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1118, column 37)
                       _lhsOmoduleDecl =
                           ({-# LINE 1118 "src-ag/Transform.ag" #-}
                            mzero
                            {-# LINE 5083 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 856, column 37)
                       _lhsOparamsCollect =
                           ({-# LINE 856 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5089 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 747, column 34)
                       _lhsOpragmas =
                           ({-# LINE 747 "src-ag/Transform.ag" #-}
                            id
                            {-# LINE 5095 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 904, column 36)
                       _lhsOquantCollect =
                           ({-# LINE 904 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5101 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 796, column 56)
                       _lhsOsemPragmasCollect =
                           ({-# LINE 796 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5107 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 596, column 32)
                       _lhsOtypeSyns =
                           ({-# LINE 596 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5113 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 138, column 15)
                       _lhsOuseMap =
                           ({-# LINE 138 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5119 "src-ag/Transform.hs" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 731, column 32)
                       _lhsOwrappers =
                           ({-# LINE 731 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5125 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOattrDecls =
                           ({-# LINE 137 "src-ag/Transform.ag" #-}
                            _lhsIattrDecls
                            {-# LINE 5131 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOattrs =
                           ({-# LINE 1220 "src-ag/Transform.ag" #-}
                            _lhsIattrs
                            {-# LINE 5137 "src-ag/Transform.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOdefSets =
                           ({-# LINE 105 "src-ag/Transform.ag" #-}
                            _lhsIdefSets
                            {-# LINE 5143 "src-ag/Transform.hs" #-}
                            )
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
-- NontSet -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
      synthesized attributes:
         collectedNames       : Set Identifier
         errors               : Seq Error
         nontSet              : Set NontermIdent
   alternatives:
      alternative All:
      alternative Difference:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Intersect:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative NamedSet:
         child name           : {NontermIdent}
         visit 0:
            local _tup4       : _
            local nontSet     : _
            local errors      : _
      alternative Path:
         child from           : {NontermIdent}
         child to             : {NontermIdent}
      alternative Union:
         child set1           : NontSet 
         child set2           : NontSet 
-}
-- cata
sem_NontSet :: NontSet  ->
               T_NontSet 
sem_NontSet (All )  =
    (sem_NontSet_All )
sem_NontSet (Difference _set1 _set2 )  =
    (sem_NontSet_Difference (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
sem_NontSet (Intersect _set1 _set2 )  =
    (sem_NontSet_Intersect (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
sem_NontSet (NamedSet _name )  =
    (sem_NontSet_NamedSet _name )
sem_NontSet (Path _from _to )  =
    (sem_NontSet_Path _from _to )
sem_NontSet (Union _set1 _set2 )  =
    (sem_NontSet_Union (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
-- semantic domain
newtype T_NontSet  = T_NontSet (DataTypes ->
                                (Set NontermIdent) ->
                                DefinedSets ->
                                ( (Set Identifier),(Seq Error),(Set NontermIdent)))
data Inh_NontSet  = Inh_NontSet {allFields_Inh_NontSet :: !(DataTypes),allNonterminals_Inh_NontSet :: !((Set NontermIdent)),definedSets_Inh_NontSet :: !(DefinedSets)}
data Syn_NontSet  = Syn_NontSet {collectedNames_Syn_NontSet :: !((Set Identifier)),errors_Syn_NontSet :: !((Seq Error)),nontSet_Syn_NontSet :: !((Set NontermIdent))}
wrap_NontSet :: T_NontSet  ->
                Inh_NontSet  ->
                Syn_NontSet 
wrap_NontSet (T_NontSet sem ) (Inh_NontSet _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets )  =
    (let ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet) = sem _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets 
     in  (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet ))
sem_NontSet_All :: T_NontSet 
sem_NontSet_All  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         -- "src-ag/Transform.ag"(line 677, column 16)
                         _lhsOnontSet =
                             ({-# LINE 677 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5218 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 87, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 87 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 5224 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 5230 "src-ag/Transform.hs" #-}
                              )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Difference :: T_NontSet  ->
                          T_NontSet  ->
                          T_NontSet 
sem_NontSet_Difference (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 683, column 16)
                         _lhsOnontSet =
                             ({-# LINE 683 "src-ag/Transform.ag" #-}
                              Set.difference    _set1InontSet _set2InontSet
                              {-# LINE 5259 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 87, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 87 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 5265 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 5271 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5277 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5283 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5289 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5295 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5301 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5307 "src-ag/Transform.hs" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets 
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets 
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Intersect :: T_NontSet  ->
                         T_NontSet  ->
                         T_NontSet 
sem_NontSet_Intersect (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 682, column 16)
                         _lhsOnontSet =
                             ({-# LINE 682 "src-ag/Transform.ag" #-}
                              Set.intersection  _set1InontSet _set2InontSet
                              {-# LINE 5340 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 87, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 87 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 5346 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 5352 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5358 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5364 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5370 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5376 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5382 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5388 "src-ag/Transform.hs" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets 
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets 
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_NamedSet :: NontermIdent ->
                        T_NontSet 
sem_NontSet_NamedSet name_  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _lhsOnontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 558, column 14)
                         _lhsOcollectedNames =
                             ({-# LINE 558 "src-ag/Transform.ag" #-}
                              Set.singleton name_
                              {-# LINE 5408 "src-ag/Transform.hs" #-}
                              )
                         -- "src-ag/Transform.ag"(line 678, column 20)
                         __tup4 =
                             ({-# LINE 678 "src-ag/Transform.ag" #-}
                              case Map.lookup name_ _lhsIdefinedSets of
                                           Nothing  -> (Set.empty, Seq.singleton (UndefNont name_))
                                           Just set -> (set, Seq.empty)
                              {-# LINE 5416 "src-ag/Transform.hs" #-}
                              )
                         -- "src-ag/Transform.ag"(line 678, column 20)
                         (_nontSet,_) =
                             ({-# LINE 678 "src-ag/Transform.ag" #-}
                              __tup4
                              {-# LINE 5422 "src-ag/Transform.hs" #-}
                              )
                         -- "src-ag/Transform.ag"(line 678, column 20)
                         (_,_errors) =
                             ({-# LINE 678 "src-ag/Transform.ag" #-}
                              __tup4
                              {-# LINE 5428 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _errors
                              {-# LINE 5434 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (from local)
                         _lhsOnontSet =
                             ({-# LINE 113 "src-ag/Transform.ag" #-}
                              _nontSet
                              {-# LINE 5440 "src-ag/Transform.hs" #-}
                              )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Path :: NontermIdent ->
                    NontermIdent ->
                    T_NontSet 
sem_NontSet_Path from_ to_  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOerrors :: (Seq Error)
                         _lhsOcollectedNames :: (Set Identifier)
                         -- "src-ag/Transform.ag"(line 684, column 16)
                         _lhsOnontSet =
                             ({-# LINE 684 "src-ag/Transform.ag" #-}
                              let table = flattenDatas _lhsIallFields
                              in path table from_ to_
                              {-# LINE 5458 "src-ag/Transform.hs" #-}
                              )
                         -- "src-ag/Transform.ag"(line 686, column 16)
                         _lhsOerrors =
                             ({-# LINE 686 "src-ag/Transform.ag" #-}
                              let check name | Set.member name _lhsIallNonterminals
                                                         = Seq.empty
                                             | otherwise = Seq.singleton (UndefNont name)
                              in check from_ >< check to_
                              {-# LINE 5467 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 87, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 87 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 5473 "src-ag/Transform.hs" #-}
                              )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Union :: T_NontSet  ->
                     T_NontSet  ->
                     T_NontSet 
sem_NontSet_Union (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 681, column 16)
                         _lhsOnontSet =
                             ({-# LINE 681 "src-ag/Transform.ag" #-}
                              Set.union         _set1InontSet _set2InontSet
                              {-# LINE 5502 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 87, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 87 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 5508 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 5514 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5520 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5526 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5532 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5538 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 89 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5544 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 108 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5550 "src-ag/Transform.hs" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets 
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets 
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Pattern
         stpos                : Pos
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern (Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern (Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern (Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern (Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( Pattern ,([AttrName]),([Identifier]),([AttrName]->Pattern),Pos))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern ),definedAttrs_Syn_Pattern :: !(([AttrName])),definedInsts_Syn_Pattern :: !(([Identifier])),patunder_Syn_Pattern :: !(([AttrName]->Pattern)),stpos_Syn_Pattern :: !(Pos)}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos) = sem 
     in  (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOstpos :: Pos
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    _partsIcopy :: Patterns 
                    _partsIdefinedAttrs :: ([AttrName])
                    _partsIdefinedInsts :: ([Identifier])
                    _partsIpatunder :: ([AttrName]->Patterns)
                    -- "src-ag/Transform.ag"(line 1094, column 11)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1094 "src-ag/Transform.ag" #-}
                         (field_, attr_) : _patIdefinedAttrs
                         {-# LINE 5640 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1095, column 11)
                    _lhsOpatunder =
                        ({-# LINE 1095 "src-ag/Transform.ag" #-}
                         \us -> if ((field_,attr_) `elem` us) then Underscore noPos else _copy
                         {-# LINE 5646 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1096, column 11)
                    _lhsOdefinedInsts =
                        ({-# LINE 1096 "src-ag/Transform.ag" #-}
                         (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                         {-# LINE 5652 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1111, column 16)
                    _lhsOstpos =
                        ({-# LINE 1111 "src-ag/Transform.ag" #-}
                         getPos field_
                         {-# LINE 5658 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy _partsIcopy
                         {-# LINE 5664 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 5670 "src-ag/Transform.hs" #-}
                         )
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        pat_ 
                    ( _partsIcopy,_partsIdefinedAttrs,_partsIdefinedInsts,_partsIpatunder) =
                        parts_ 
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "src-ag/Transform.ag"(line 1098, column 12)
                    _lhsOpatunder =
                        ({-# LINE 1098 "src-ag/Transform.ag" #-}
                         \us -> Constr name_ (_patsIpatunder us)
                         {-# LINE 5694 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1109, column 16)
                    _lhsOstpos =
                        ({-# LINE 1109 "src-ag/Transform.ag" #-}
                         getPos name_
                         {-# LINE 5700 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1089 "src-ag/Transform.ag" #-}
                         _patsIdefinedAttrs
                         {-# LINE 5706 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1088 "src-ag/Transform.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 5712 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 5718 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 5724 "src-ag/Transform.hs" #-}
                         )
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern 
                    _lhsOstpos :: Pos
                    _patIcopy :: Pattern 
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    -- "src-ag/Transform.ag"(line 1100, column 17)
                    _lhsOpatunder =
                        ({-# LINE 1100 "src-ag/Transform.ag" #-}
                         \us -> Irrefutable (_patIpatunder us)
                         {-# LINE 5746 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1089 "src-ag/Transform.ag" #-}
                         _patIdefinedAttrs
                         {-# LINE 5752 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1088 "src-ag/Transform.ag" #-}
                         _patIdefinedInsts
                         {-# LINE 5758 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 5764 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 5770 "src-ag/Transform.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOstpos =
                        ({-# LINE 1106 "src-ag/Transform.ag" #-}
                         _patIstpos
                         {-# LINE 5776 "src-ag/Transform.hs" #-}
                         )
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "src-ag/Transform.ag"(line 1099, column 13)
                    _lhsOpatunder =
                        ({-# LINE 1099 "src-ag/Transform.ag" #-}
                         \us -> Product pos_ (_patsIpatunder us)
                         {-# LINE 5798 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1110, column 16)
                    _lhsOstpos =
                        ({-# LINE 1110 "src-ag/Transform.ag" #-}
                         pos_
                         {-# LINE 5804 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1089 "src-ag/Transform.ag" #-}
                         _patsIdefinedAttrs
                         {-# LINE 5810 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1088 "src-ag/Transform.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 5816 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 5822 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 5828 "src-ag/Transform.hs" #-}
                         )
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern 
                    -- "src-ag/Transform.ag"(line 1097, column 16)
                    _lhsOpatunder =
                        ({-# LINE 1097 "src-ag/Transform.ag" #-}
                         \us -> _copy
                         {-# LINE 5845 "src-ag/Transform.hs" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1112, column 16)
                    _lhsOstpos =
                        ({-# LINE 1112 "src-ag/Transform.ag" #-}
                         pos_
                         {-# LINE 5851 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1089 "src-ag/Transform.ag" #-}
                         []
                         {-# LINE 5857 "src-ag/Transform.hs" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1088 "src-ag/Transform.ag" #-}
                         []
                         {-# LINE 5863 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 5869 "src-ag/Transform.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 5875 "src-ag/Transform.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Patterns
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns ,([AttrName]),([Identifier]),([AttrName]->Patterns)))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns ),definedAttrs_Syn_Patterns :: !(([AttrName])),definedInsts_Syn_Patterns :: !(([Identifier])),patunder_Syn_Patterns :: !(([AttrName]->Patterns))}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder) = sem 
     in  (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns 
                     _hdIcopy :: Pattern 
                     _hdIdefinedAttrs :: ([AttrName])
                     _hdIdefinedInsts :: ([Identifier])
                     _hdIpatunder :: ([AttrName]->Pattern)
                     _hdIstpos :: Pos
                     _tlIcopy :: Patterns 
                     _tlIdefinedAttrs :: ([AttrName])
                     _tlIdefinedInsts :: ([Identifier])
                     _tlIpatunder :: ([AttrName]->Patterns)
                     -- "src-ag/Transform.ag"(line 1104, column 10)
                     _lhsOpatunder =
                         ({-# LINE 1104 "src-ag/Transform.ag" #-}
                          \us -> (_hdIpatunder us) : (_tlIpatunder us)
                          {-# LINE 5932 "src-ag/Transform.hs" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                     _lhsOdefinedAttrs =
                         ({-# LINE 1089 "src-ag/Transform.ag" #-}
                          _hdIdefinedAttrs ++ _tlIdefinedAttrs
                          {-# LINE 5938 "src-ag/Transform.hs" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 1088 "src-ag/Transform.ag" #-}
                          _hdIdefinedInsts ++ _tlIdefinedInsts
                          {-# LINE 5944 "src-ag/Transform.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 5950 "src-ag/Transform.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 5956 "src-ag/Transform.hs" #-}
                          )
                     ( _hdIcopy,_hdIdefinedAttrs,_hdIdefinedInsts,_hdIpatunder,_hdIstpos) =
                         hd_ 
                     ( _tlIcopy,_tlIdefinedAttrs,_tlIdefinedInsts,_tlIpatunder) =
                         tl_ 
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns 
                     -- "src-ag/Transform.ag"(line 1103, column 9)
                     _lhsOpatunder =
                         ({-# LINE 1103 "src-ag/Transform.ag" #-}
                          \us ->  []
                          {-# LINE 5973 "src-ag/Transform.hs" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1089, column 42)
                     _lhsOdefinedAttrs =
                         ({-# LINE 1089 "src-ag/Transform.ag" #-}
                          []
                          {-# LINE 5979 "src-ag/Transform.hs" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 1088 "src-ag/Transform.ag" #-}
                          []
                          {-# LINE 5985 "src-ag/Transform.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 5991 "src-ag/Transform.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 5997 "src-ag/Transform.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)) )
-- SemAlt ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
         options              : Options
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative SemAlt:
         child pos            : {Pos}
         child constructorSet : ConstructorSet 
         child rules          : SemDefs 
         visit 0:
            local pragmaNames : _
            local attrOrders  : _
            local coninfo     : _
-}
-- cata
sem_SemAlt :: SemAlt  ->
              T_SemAlt 
sem_SemAlt (SemAlt _pos _constructorSet _rules )  =
    (sem_SemAlt_SemAlt _pos (sem_ConstructorSet _constructorSet ) (sem_SemDefs _rules ) )
-- semantic domain
newtype T_SemAlt  = T_SemAlt ((Map NontermIdent (Attributes, Attributes)) ->
                              (Map NontermIdent (Attributes, Attributes)) ->
                              DataTypes ->
                              (Set NontermIdent) ->
                              Options ->
                              ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlt  = Inh_SemAlt {allAttrDecls_Inh_SemAlt :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_SemAlt :: !((Map NontermIdent (Attributes, Attributes))),allFields_Inh_SemAlt :: !(DataTypes),nts_Inh_SemAlt :: !((Set NontermIdent)),options_Inh_SemAlt :: !(Options)}
data Syn_SemAlt  = Syn_SemAlt {attrOrderCollect_Syn_SemAlt :: !(AttrOrderMap),collectedArounds_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedInsts_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedRules_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSigs_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),errors_Syn_SemAlt :: !((Seq Error)),semPragmasCollect_Syn_SemAlt :: !(PragmaMap)}
wrap_SemAlt :: T_SemAlt  ->
               Inh_SemAlt  ->
               Syn_SemAlt 
wrap_SemAlt (T_SemAlt sem ) (Inh_SemAlt _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions )  =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions 
     in  (Syn_SemAlt _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect ))
sem_SemAlt_SemAlt :: Pos ->
                     T_ConstructorSet  ->
                     T_SemDefs  ->
                     T_SemAlt 
sem_SemAlt_SemAlt pos_ (T_ConstructorSet constructorSet_ ) (T_SemDefs rules_ )  =
    (T_SemAlt (\ _lhsIallAttrDecls
                 _lhsIallAttrs
                 _lhsIallFields
                 _lhsInts
                 _lhsIoptions ->
                   (let _lhsOsemPragmasCollect :: PragmaMap
                        _lhsOattrOrderCollect :: AttrOrderMap
                        _lhsOerrors :: (Seq Error)
                        _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                        _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                        _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                        _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                        _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                        _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                        _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                        _rulesOoptions :: Options
                        _constructorSetIcollectedConstructorNames :: (Set ConstructorIdent)
                        _constructorSetIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                        _constructorSetIerrors :: (Seq Error)
                        _rulesIaroundInfos :: ([AroundInfo])
                        _rulesIaugmentInfos :: ([AugmentInfo])
                        _rulesIdefinedInsts :: ([Identifier])
                        _rulesIerrors :: (Seq Error)
                        _rulesImergeInfos :: ([MergeInfo])
                        _rulesIorderDepsCollect :: (Set Dependency)
                        _rulesIpragmaNamesCollect :: ([Identifier])
                        _rulesIruleInfos :: ([RuleInfo])
                        _rulesIsigInfos :: ([SigInfo])
                        _rulesIuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 800, column 7)
                        _pragmaNames =
                            ({-# LINE 800 "src-ag/Transform.ag" #-}
                             Set.fromList _rulesIpragmaNamesCollect
                             {-# LINE 6088 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 801, column 7)
                        _lhsOsemPragmasCollect =
                            ({-# LINE 801 "src-ag/Transform.ag" #-}
                             foldr pragmaMapUnion Map.empty [ pragmaMapSingle nt con _pragmaNames
                                                            | (nt, conset, _) <- _coninfo
                                                            , con <- Set.toList conset
                                                            ]
                             {-# LINE 6097 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 829, column 7)
                        _attrOrders =
                            ({-# LINE 829 "src-ag/Transform.ag" #-}
                             [ orderMapSingle nt con _rulesIorderDepsCollect
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6106 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 835, column 7)
                        _lhsOattrOrderCollect =
                            ({-# LINE 835 "src-ag/Transform.ag" #-}
                             foldr orderMapUnion Map.empty _attrOrders
                             {-# LINE 6112 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1013, column 12)
                        _coninfo =
                            ({-# LINE 1013 "src-ag/Transform.ag" #-}
                             [ (nt, conset, conkeys)
                             | nt  <- Set.toList _lhsInts
                             , let conmap = Map.findWithDefault Map.empty nt _lhsIallFields
                             , let conkeys = Set.fromList (Map.keys conmap)
                             , let conset  = _constructorSetIconstructors conkeys
                             ]
                             {-# LINE 6123 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1020, column 12)
                        _lhsOerrors =
                            ({-# LINE 1020 "src-ag/Transform.ag" #-}
                             Seq.fromList
                                [ UndefAlt nt con
                                | (nt, conset, conkeys) <- _coninfo
                                , con <- Set.toList (Set.difference conset conkeys)
                                ]
                             Seq.>< _rulesIerrors
                             {-# LINE 6134 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1026, column 12)
                        _lhsOcollectedRules =
                            ({-# LINE 1026 "src-ag/Transform.ag" #-}
                             [ (nt,con,r)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             , r <- _rulesIruleInfos
                             ]
                             {-# LINE 6144 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1032, column 12)
                        _lhsOcollectedSigs =
                            ({-# LINE 1032 "src-ag/Transform.ag" #-}
                             [ (nt,con,ts)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             , ts <- _rulesIsigInfos
                             ]
                             {-# LINE 6154 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1039, column 12)
                        _lhsOcollectedInsts =
                            ({-# LINE 1039 "src-ag/Transform.ag" #-}
                             [ (nt,con,_rulesIdefinedInsts)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6163 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1045, column 12)
                        _lhsOcollectedUniques =
                            ({-# LINE 1045 "src-ag/Transform.ag" #-}
                             [ (nt,con,_rulesIuniqueInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6172 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1051, column 12)
                        _lhsOcollectedAugments =
                            ({-# LINE 1051 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesIaugmentInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6181 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1057, column 12)
                        _lhsOcollectedArounds =
                            ({-# LINE 1057 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesIaroundInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6190 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1063, column 12)
                        _lhsOcollectedMerges =
                            ({-# LINE 1063 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesImergeInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6199 "src-ag/Transform.hs" #-}
                             )
                        -- copy rule (down)
                        _rulesOoptions =
                            ({-# LINE 39 "src-ag/Transform.ag" #-}
                             _lhsIoptions
                             {-# LINE 6205 "src-ag/Transform.hs" #-}
                             )
                        ( _constructorSetIcollectedConstructorNames,_constructorSetIconstructors,_constructorSetIerrors) =
                            constructorSet_ 
                        ( _rulesIaroundInfos,_rulesIaugmentInfos,_rulesIdefinedInsts,_rulesIerrors,_rulesImergeInfos,_rulesIorderDepsCollect,_rulesIpragmaNamesCollect,_rulesIruleInfos,_rulesIsigInfos,_rulesIuniqueInfos) =
                            rules_ _rulesOoptions 
                    in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
-- SemAlts -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
         options              : Options
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative Cons:
         child hd             : SemAlt 
         child tl             : SemAlts 
      alternative Nil:
-}
-- cata
sem_SemAlts :: SemAlts  ->
               T_SemAlts 
sem_SemAlts list  =
    (Prelude.foldr sem_SemAlts_Cons sem_SemAlts_Nil (Prelude.map sem_SemAlt list) )
-- semantic domain
newtype T_SemAlts  = T_SemAlts ((Map NontermIdent (Attributes, Attributes)) ->
                                (Map NontermIdent (Attributes, Attributes)) ->
                                DataTypes ->
                                (Set NontermIdent) ->
                                Options ->
                                ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlts  = Inh_SemAlts {allAttrDecls_Inh_SemAlts :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_SemAlts :: !((Map NontermIdent (Attributes, Attributes))),allFields_Inh_SemAlts :: !(DataTypes),nts_Inh_SemAlts :: !((Set NontermIdent)),options_Inh_SemAlts :: !(Options)}
data Syn_SemAlts  = Syn_SemAlts {attrOrderCollect_Syn_SemAlts :: !(AttrOrderMap),collectedArounds_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedInsts_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedRules_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSigs_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),errors_Syn_SemAlts :: !((Seq Error)),semPragmasCollect_Syn_SemAlts :: !(PragmaMap)}
wrap_SemAlts :: T_SemAlts  ->
                Inh_SemAlts  ->
                Syn_SemAlts 
wrap_SemAlts (T_SemAlts sem ) (Inh_SemAlts _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions )  =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions 
     in  (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect ))
sem_SemAlts_Cons :: T_SemAlt  ->
                    T_SemAlts  ->
                    T_SemAlts 
sem_SemAlts_Cons (T_SemAlt hd_ ) (T_SemAlts tl_ )  =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallAttrs
                  _lhsIallFields
                  _lhsInts
                  _lhsIoptions ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _hdOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                         _hdOallFields :: DataTypes
                         _hdOnts :: (Set NontermIdent)
                         _hdOoptions :: Options
                         _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _tlOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                         _tlOallFields :: DataTypes
                         _tlOnts :: (Set NontermIdent)
                         _tlOoptions :: Options
                         _hdIattrOrderCollect :: AttrOrderMap
                         _hdIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _hdIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _hdIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _hdIerrors :: (Seq Error)
                         _hdIsemPragmasCollect :: PragmaMap
                         _tlIattrOrderCollect :: AttrOrderMap
                         _tlIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _tlIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _tlIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _tlIerrors :: (Seq Error)
                         _tlIsemPragmasCollect :: PragmaMap
                         -- use rule "src-ag/Transform.ag"(line 824, column 55)
                         _lhsOattrOrderCollect =
                             ({-# LINE 824 "src-ag/Transform.ag" #-}
                              _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                              {-# LINE 6311 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 157, column 32)
                         _lhsOcollectedArounds =
                             ({-# LINE 157 "src-ag/Transform.ag" #-}
                              _hdIcollectedArounds ++ _tlIcollectedArounds
                              {-# LINE 6317 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 156, column 32)
                         _lhsOcollectedAugments =
                             ({-# LINE 156 "src-ag/Transform.ag" #-}
                              _hdIcollectedAugments ++ _tlIcollectedAugments
                              {-# LINE 6323 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 154, column 32)
                         _lhsOcollectedInsts =
                             ({-# LINE 154 "src-ag/Transform.ag" #-}
                              _hdIcollectedInsts ++ _tlIcollectedInsts
                              {-# LINE 6329 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 158, column 32)
                         _lhsOcollectedMerges =
                             ({-# LINE 158 "src-ag/Transform.ag" #-}
                              _hdIcollectedMerges ++ _tlIcollectedMerges
                              {-# LINE 6335 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 152, column 32)
                         _lhsOcollectedRules =
                             ({-# LINE 152 "src-ag/Transform.ag" #-}
                              _hdIcollectedRules ++ _tlIcollectedRules
                              {-# LINE 6341 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 153, column 32)
                         _lhsOcollectedSigs =
                             ({-# LINE 153 "src-ag/Transform.ag" #-}
                              _hdIcollectedSigs ++ _tlIcollectedSigs
                              {-# LINE 6347 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 155, column 32)
                         _lhsOcollectedUniques =
                             ({-# LINE 155 "src-ag/Transform.ag" #-}
                              _hdIcollectedUniques ++ _tlIcollectedUniques
                              {-# LINE 6353 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _hdIerrors Seq.>< _tlIerrors
                              {-# LINE 6359 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 796, column 56)
                         _lhsOsemPragmasCollect =
                             ({-# LINE 796 "src-ag/Transform.ag" #-}
                              _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                              {-# LINE 6365 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOallAttrDecls =
                             ({-# LINE 825 "src-ag/Transform.ag" #-}
                              _lhsIallAttrDecls
                              {-# LINE 6371 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOallAttrs =
                             ({-# LINE 1210 "src-ag/Transform.ag" #-}
                              _lhsIallAttrs
                              {-# LINE 6377 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 6383 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOnts =
                             ({-# LINE 168 "src-ag/Transform.ag" #-}
                              _lhsInts
                              {-# LINE 6389 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOoptions =
                             ({-# LINE 39 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 6395 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOallAttrDecls =
                             ({-# LINE 825 "src-ag/Transform.ag" #-}
                              _lhsIallAttrDecls
                              {-# LINE 6401 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOallAttrs =
                             ({-# LINE 1210 "src-ag/Transform.ag" #-}
                              _lhsIallAttrs
                              {-# LINE 6407 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOallFields =
                             ({-# LINE 129 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 6413 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOnts =
                             ({-# LINE 168 "src-ag/Transform.ag" #-}
                              _lhsInts
                              {-# LINE 6419 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOoptions =
                             ({-# LINE 39 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 6425 "src-ag/Transform.hs" #-}
                              )
                         ( _hdIattrOrderCollect,_hdIcollectedArounds,_hdIcollectedAugments,_hdIcollectedInsts,_hdIcollectedMerges,_hdIcollectedRules,_hdIcollectedSigs,_hdIcollectedUniques,_hdIerrors,_hdIsemPragmasCollect) =
                             hd_ _hdOallAttrDecls _hdOallAttrs _hdOallFields _hdOnts _hdOoptions 
                         ( _tlIattrOrderCollect,_tlIcollectedArounds,_tlIcollectedAugments,_tlIcollectedInsts,_tlIcollectedMerges,_tlIcollectedRules,_tlIcollectedSigs,_tlIcollectedUniques,_tlIerrors,_tlIsemPragmasCollect) =
                             tl_ _tlOallAttrDecls _tlOallAttrs _tlOallFields _tlOnts _tlOoptions 
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
sem_SemAlts_Nil :: T_SemAlts 
sem_SemAlts_Nil  =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallAttrs
                  _lhsIallFields
                  _lhsInts
                  _lhsIoptions ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         -- use rule "src-ag/Transform.ag"(line 824, column 55)
                         _lhsOattrOrderCollect =
                             ({-# LINE 824 "src-ag/Transform.ag" #-}
                              Map.empty
                              {-# LINE 6453 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 157, column 32)
                         _lhsOcollectedArounds =
                             ({-# LINE 157 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6459 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 156, column 32)
                         _lhsOcollectedAugments =
                             ({-# LINE 156 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6465 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 154, column 32)
                         _lhsOcollectedInsts =
                             ({-# LINE 154 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6471 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 158, column 32)
                         _lhsOcollectedMerges =
                             ({-# LINE 158 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6477 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 152, column 32)
                         _lhsOcollectedRules =
                             ({-# LINE 152 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6483 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 153, column 32)
                         _lhsOcollectedSigs =
                             ({-# LINE 153 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6489 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 155, column 32)
                         _lhsOcollectedUniques =
                             ({-# LINE 155 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 6495 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 6501 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 796, column 56)
                         _lhsOsemPragmasCollect =
                             ({-# LINE 796 "src-ag/Transform.ag" #-}
                              Map.empty
                              {-# LINE 6507 "src-ag/Transform.hs" #-}
                              )
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
-- SemDef ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         aroundInfos          : [AroundInfo]
         augmentInfos         : [AugmentInfo]
         definedInsts         : [Identifier]
         errors               : Seq Error
         mergeInfos           : [MergeInfo]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative AroundDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative AttrOrderBefore:
         child before         : {[Occurrence]}
         child after          : {[Occurrence]}
         visit 0:
            local dependency  : _
      alternative AugmentDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative Def:
         child pos            : {Pos}
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
      alternative MergeDef:
         child target         : {Identifier}
         child nt             : {Identifier}
         child sources        : {[Identifier]}
         child rhs            : {Expression}
      alternative SemPragma:
         child names          : {[NontermIdent]}
      alternative TypeDef:
         child pos            : {Pos}
         child ident          : {Identifier}
         child tp             : {Type}
      alternative UniqueDef:
         child ident          : {Identifier}
         child ref            : {Identifier}
-}
-- cata
sem_SemDef :: SemDef  ->
              T_SemDef 
sem_SemDef (AroundDef _ident _rhs )  =
    (sem_SemDef_AroundDef _ident _rhs )
sem_SemDef (AttrOrderBefore _before _after )  =
    (sem_SemDef_AttrOrderBefore _before _after )
sem_SemDef (AugmentDef _ident _rhs )  =
    (sem_SemDef_AugmentDef _ident _rhs )
sem_SemDef (Def _pos _mbName _pattern _rhs _owrt )  =
    (sem_SemDef_Def _pos _mbName (sem_Pattern _pattern ) _rhs _owrt )
sem_SemDef (MergeDef _target _nt _sources _rhs )  =
    (sem_SemDef_MergeDef _target _nt _sources _rhs )
sem_SemDef (SemPragma _names )  =
    (sem_SemDef_SemPragma _names )
sem_SemDef (TypeDef _pos _ident _tp )  =
    (sem_SemDef_TypeDef _pos _ident _tp )
sem_SemDef (UniqueDef _ident _ref )  =
    (sem_SemDef_UniqueDef _ident _ref )
-- semantic domain
newtype T_SemDef  = T_SemDef (Options ->
                              ( ([AroundInfo]),([AugmentInfo]),([Identifier]),(Seq Error),([MergeInfo]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDef  = Inh_SemDef {options_Inh_SemDef :: !(Options)}
data Syn_SemDef  = Syn_SemDef {aroundInfos_Syn_SemDef :: !(([AroundInfo])),augmentInfos_Syn_SemDef :: !(([AugmentInfo])),definedInsts_Syn_SemDef :: !(([Identifier])),errors_Syn_SemDef :: !((Seq Error)),mergeInfos_Syn_SemDef :: !(([MergeInfo])),orderDepsCollect_Syn_SemDef :: !((Set Dependency)),pragmaNamesCollect_Syn_SemDef :: !(([Identifier])),ruleInfos_Syn_SemDef :: !(([RuleInfo])),sigInfos_Syn_SemDef :: !(([SigInfo])),uniqueInfos_Syn_SemDef :: !(([UniqueInfo]))}
wrap_SemDef :: T_SemDef  ->
               Inh_SemDef  ->
               Syn_SemDef 
wrap_SemDef (T_SemDef sem ) (Inh_SemDef _lhsIoptions )  =
    (let ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) = sem _lhsIoptions 
     in  (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos ))
sem_SemDef_AroundDef :: Identifier ->
                        Expression ->
                        T_SemDef 
sem_SemDef_AroundDef ident_ rhs_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 1082, column 17)
                        _lhsOaroundInfos =
                            ({-# LINE 1082 "src-ag/Transform.ag" #-}
                             [ (ident_, rhs_) ]
                             {-# LINE 6608 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6614 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6620 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 43, column 19)
                        _lhsOerrors =
                            ({-# LINE 43 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 6626 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6632 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 6638 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6644 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6650 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6656 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6662 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_AttrOrderBefore :: ([Occurrence]) ->
                              ([Occurrence]) ->
                              T_SemDef 
sem_SemDef_AttrOrderBefore before_ after_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 841, column 7)
                        _dependency =
                            ({-# LINE 841 "src-ag/Transform.ag" #-}
                             [ Dependency b a | b <- before_, a <- after_ ]
                             {-# LINE 6684 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 842, column 7)
                        _lhsOorderDepsCollect =
                            ({-# LINE 842 "src-ag/Transform.ag" #-}
                             Set.fromList _dependency
                             {-# LINE 6690 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6696 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6702 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6708 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 43, column 19)
                        _lhsOerrors =
                            ({-# LINE 43 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 6714 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6720 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6726 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6732 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6738 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6744 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_AugmentDef :: Identifier ->
                         Expression ->
                         T_SemDef 
sem_SemDef_AugmentDef ident_ rhs_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 1079, column 17)
                        _lhsOaugmentInfos =
                            ({-# LINE 1079 "src-ag/Transform.ag" #-}
                             [ (ident_, rhs_) ]
                             {-# LINE 6766 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6772 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6778 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 43, column 19)
                        _lhsOerrors =
                            ({-# LINE 43 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 6784 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6790 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 6796 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6802 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6808 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6814 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6820 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_Def :: Pos ->
                  (Maybe Identifier) ->
                  T_Pattern  ->
                  Expression ->
                  Bool ->
                  T_SemDef 
sem_SemDef_Def pos_ mbName_ (T_Pattern pattern_ ) rhs_ owrt_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        _patternIcopy :: Pattern 
                        _patternIdefinedAttrs :: ([AttrName])
                        _patternIdefinedInsts :: ([Identifier])
                        _patternIpatunder :: ([AttrName]->Pattern)
                        _patternIstpos :: Pos
                        -- "src-ag/Transform.ag"(line 528, column 3)
                        _lhsOerrors =
                            ({-# LINE 528 "src-ag/Transform.ag" #-}
                             if checkParseRhs _lhsIoptions
                             then Seq.fromList $ checkRhs rhs_
                             else Seq.empty
                             {-# LINE 6852 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1070, column 10)
                        _lhsOruleInfos =
                            ({-# LINE 1070 "src-ag/Transform.ag" #-}
                             [ (mbName_, _patternIpatunder, rhs_, _patternIdefinedAttrs, owrt_, show _patternIstpos) ]
                             {-# LINE 6858 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6864 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6870 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             _patternIdefinedInsts
                             {-# LINE 6876 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6882 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 6888 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6894 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6900 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6906 "src-ag/Transform.hs" #-}
                             )
                        ( _patternIcopy,_patternIdefinedAttrs,_patternIdefinedInsts,_patternIpatunder,_patternIstpos) =
                            pattern_ 
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_MergeDef :: Identifier ->
                       Identifier ->
                       ([Identifier]) ->
                       Expression ->
                       T_SemDef 
sem_SemDef_MergeDef target_ nt_ sources_ rhs_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 528, column 3)
                        _lhsOerrors =
                            ({-# LINE 528 "src-ag/Transform.ag" #-}
                             if checkParseRhs _lhsIoptions
                             then Seq.fromList $ checkRhs rhs_
                             else Seq.empty
                             {-# LINE 6934 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1085, column 17)
                        _lhsOmergeInfos =
                            ({-# LINE 1085 "src-ag/Transform.ag" #-}
                             [ (target_, nt_, sources_, rhs_) ]
                             {-# LINE 6940 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6946 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6952 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6958 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 6964 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6970 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6976 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6982 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 6988 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_SemPragma :: ([NontermIdent]) ->
                        T_SemDef 
sem_SemDef_SemPragma names_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 810, column 7)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 810 "src-ag/Transform.ag" #-}
                             names_
                             {-# LINE 7009 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7015 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7021 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7027 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 43, column 19)
                        _lhsOerrors =
                            ({-# LINE 43 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7033 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7039 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7045 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7051 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7057 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7063 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_TypeDef :: Pos ->
                      Identifier ->
                      Type ->
                      T_SemDef 
sem_SemDef_TypeDef pos_ ident_ tp_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 535, column 3)
                        _lhsOerrors =
                            ({-# LINE 535 "src-ag/Transform.ag" #-}
                             if checkParseTy _lhsIoptions
                             then case tp_ of
                                    Haskell s -> let exp = Expression pos_ tks
                                                     tks = [tk]
                                                     tk  = HsToken s pos_
                                                 in Seq.fromList $ checkTy exp
                                    _ -> Seq.empty
                             else Seq.empty
                             {-# LINE 7093 "src-ag/Transform.hs" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1073, column 14)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             [ (ident_, tp_) ]
                             {-# LINE 7099 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7105 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7111 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7117 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7123 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7129 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7135 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7141 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1005 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7147 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDef_UniqueDef :: Identifier ->
                        Identifier ->
                        T_SemDef 
sem_SemDef_UniqueDef ident_ ref_  =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOuniqueInfos :: ([UniqueInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        -- "src-ag/Transform.ag"(line 1076, column 16)
                        _lhsOuniqueInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             [ (ident_, ref_) ]
                             {-# LINE 7169 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1007 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7175 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1006 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7181 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1088 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7187 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 43, column 19)
                        _lhsOerrors =
                            ({-# LINE 43 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7193 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1008 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7199 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 837, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 837 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7205 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 806, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 806 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7211 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1003 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7217 "src-ag/Transform.hs" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1004 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7223 "src-ag/Transform.hs" #-}
                             )
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
-- SemDefs -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         aroundInfos          : [AroundInfo]
         augmentInfos         : [AugmentInfo]
         definedInsts         : [Identifier]
         errors               : Seq Error
         mergeInfos           : [MergeInfo]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative Cons:
         child hd             : SemDef 
         child tl             : SemDefs 
      alternative Nil:
-}
-- cata
sem_SemDefs :: SemDefs  ->
               T_SemDefs 
sem_SemDefs list  =
    (Prelude.foldr sem_SemDefs_Cons sem_SemDefs_Nil (Prelude.map sem_SemDef list) )
-- semantic domain
newtype T_SemDefs  = T_SemDefs (Options ->
                                ( ([AroundInfo]),([AugmentInfo]),([Identifier]),(Seq Error),([MergeInfo]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDefs  = Inh_SemDefs {options_Inh_SemDefs :: !(Options)}
data Syn_SemDefs  = Syn_SemDefs {aroundInfos_Syn_SemDefs :: !(([AroundInfo])),augmentInfos_Syn_SemDefs :: !(([AugmentInfo])),definedInsts_Syn_SemDefs :: !(([Identifier])),errors_Syn_SemDefs :: !((Seq Error)),mergeInfos_Syn_SemDefs :: !(([MergeInfo])),orderDepsCollect_Syn_SemDefs :: !((Set Dependency)),pragmaNamesCollect_Syn_SemDefs :: !(([Identifier])),ruleInfos_Syn_SemDefs :: !(([RuleInfo])),sigInfos_Syn_SemDefs :: !(([SigInfo])),uniqueInfos_Syn_SemDefs :: !(([UniqueInfo]))}
wrap_SemDefs :: T_SemDefs  ->
                Inh_SemDefs  ->
                Syn_SemDefs 
wrap_SemDefs (T_SemDefs sem ) (Inh_SemDefs _lhsIoptions )  =
    (let ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) = sem _lhsIoptions 
     in  (Syn_SemDefs _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos ))
sem_SemDefs_Cons :: T_SemDef  ->
                    T_SemDefs  ->
                    T_SemDefs 
sem_SemDefs_Cons (T_SemDef hd_ ) (T_SemDefs tl_ )  =
    (T_SemDefs (\ _lhsIoptions ->
                    (let _lhsOaroundInfos :: ([AroundInfo])
                         _lhsOaugmentInfos :: ([AugmentInfo])
                         _lhsOdefinedInsts :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOmergeInfos :: ([MergeInfo])
                         _lhsOorderDepsCollect :: (Set Dependency)
                         _lhsOpragmaNamesCollect :: ([Identifier])
                         _lhsOruleInfos :: ([RuleInfo])
                         _lhsOsigInfos :: ([SigInfo])
                         _lhsOuniqueInfos :: ([UniqueInfo])
                         _hdOoptions :: Options
                         _tlOoptions :: Options
                         _hdIaroundInfos :: ([AroundInfo])
                         _hdIaugmentInfos :: ([AugmentInfo])
                         _hdIdefinedInsts :: ([Identifier])
                         _hdIerrors :: (Seq Error)
                         _hdImergeInfos :: ([MergeInfo])
                         _hdIorderDepsCollect :: (Set Dependency)
                         _hdIpragmaNamesCollect :: ([Identifier])
                         _hdIruleInfos :: ([RuleInfo])
                         _hdIsigInfos :: ([SigInfo])
                         _hdIuniqueInfos :: ([UniqueInfo])
                         _tlIaroundInfos :: ([AroundInfo])
                         _tlIaugmentInfos :: ([AugmentInfo])
                         _tlIdefinedInsts :: ([Identifier])
                         _tlIerrors :: (Seq Error)
                         _tlImergeInfos :: ([MergeInfo])
                         _tlIorderDepsCollect :: (Set Dependency)
                         _tlIpragmaNamesCollect :: ([Identifier])
                         _tlIruleInfos :: ([RuleInfo])
                         _tlIsigInfos :: ([SigInfo])
                         _tlIuniqueInfos :: ([UniqueInfo])
                         -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                         _lhsOaroundInfos =
                             ({-# LINE 1007 "src-ag/Transform.ag" #-}
                              _hdIaroundInfos ++ _tlIaroundInfos
                              {-# LINE 7305 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                         _lhsOaugmentInfos =
                             ({-# LINE 1006 "src-ag/Transform.ag" #-}
                              _hdIaugmentInfos ++ _tlIaugmentInfos
                              {-# LINE 7311 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                         _lhsOdefinedInsts =
                             ({-# LINE 1088 "src-ag/Transform.ag" #-}
                              _hdIdefinedInsts ++ _tlIdefinedInsts
                              {-# LINE 7317 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              _hdIerrors Seq.>< _tlIerrors
                              {-# LINE 7323 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                         _lhsOmergeInfos =
                             ({-# LINE 1008 "src-ag/Transform.ag" #-}
                              _hdImergeInfos ++ _tlImergeInfos
                              {-# LINE 7329 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 837, column 44)
                         _lhsOorderDepsCollect =
                             ({-# LINE 837 "src-ag/Transform.ag" #-}
                              _hdIorderDepsCollect `Set.union` _tlIorderDepsCollect
                              {-# LINE 7335 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 806, column 46)
                         _lhsOpragmaNamesCollect =
                             ({-# LINE 806 "src-ag/Transform.ag" #-}
                              _hdIpragmaNamesCollect ++ _tlIpragmaNamesCollect
                              {-# LINE 7341 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                         _lhsOruleInfos =
                             ({-# LINE 1003 "src-ag/Transform.ag" #-}
                              _hdIruleInfos ++ _tlIruleInfos
                              {-# LINE 7347 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                         _lhsOsigInfos =
                             ({-# LINE 1004 "src-ag/Transform.ag" #-}
                              _hdIsigInfos ++ _tlIsigInfos
                              {-# LINE 7353 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                         _lhsOuniqueInfos =
                             ({-# LINE 1005 "src-ag/Transform.ag" #-}
                              _hdIuniqueInfos ++ _tlIuniqueInfos
                              {-# LINE 7359 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOoptions =
                             ({-# LINE 39 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 7365 "src-ag/Transform.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOoptions =
                             ({-# LINE 39 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 7371 "src-ag/Transform.hs" #-}
                              )
                         ( _hdIaroundInfos,_hdIaugmentInfos,_hdIdefinedInsts,_hdIerrors,_hdImergeInfos,_hdIorderDepsCollect,_hdIpragmaNamesCollect,_hdIruleInfos,_hdIsigInfos,_hdIuniqueInfos) =
                             hd_ _hdOoptions 
                         ( _tlIaroundInfos,_tlIaugmentInfos,_tlIdefinedInsts,_tlIerrors,_tlImergeInfos,_tlIorderDepsCollect,_tlIpragmaNamesCollect,_tlIruleInfos,_tlIsigInfos,_tlIuniqueInfos) =
                             tl_ _tlOoptions 
                     in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )
sem_SemDefs_Nil :: T_SemDefs 
sem_SemDefs_Nil  =
    (T_SemDefs (\ _lhsIoptions ->
                    (let _lhsOaroundInfos :: ([AroundInfo])
                         _lhsOaugmentInfos :: ([AugmentInfo])
                         _lhsOdefinedInsts :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOmergeInfos :: ([MergeInfo])
                         _lhsOorderDepsCollect :: (Set Dependency)
                         _lhsOpragmaNamesCollect :: ([Identifier])
                         _lhsOruleInfos :: ([RuleInfo])
                         _lhsOsigInfos :: ([SigInfo])
                         _lhsOuniqueInfos :: ([UniqueInfo])
                         -- use rule "src-ag/Transform.ag"(line 1007, column 40)
                         _lhsOaroundInfos =
                             ({-# LINE 1007 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7395 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1006, column 40)
                         _lhsOaugmentInfos =
                             ({-# LINE 1006 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7401 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1088, column 55)
                         _lhsOdefinedInsts =
                             ({-# LINE 1088 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7407 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 43, column 19)
                         _lhsOerrors =
                             ({-# LINE 43 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 7413 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1008, column 40)
                         _lhsOmergeInfos =
                             ({-# LINE 1008 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7419 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 837, column 44)
                         _lhsOorderDepsCollect =
                             ({-# LINE 837 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 7425 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 806, column 46)
                         _lhsOpragmaNamesCollect =
                             ({-# LINE 806 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7431 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1003, column 40)
                         _lhsOruleInfos =
                             ({-# LINE 1003 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7437 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1004, column 40)
                         _lhsOsigInfos =
                             ({-# LINE 1004 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7443 "src-ag/Transform.hs" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1005, column 40)
                         _lhsOuniqueInfos =
                             ({-# LINE 1005 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7449 "src-ag/Transform.hs" #-}
                              )
                     in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))) )