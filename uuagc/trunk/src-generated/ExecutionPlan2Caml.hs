{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionPlan2Caml where
{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 10 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 16 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 23 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/ExecutionPlan.ag" #-}

-- VisitSyntax.ag imports
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
import ErrorMessages

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
{-# LINE 37 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 32 "./src-ag/ExecutionPlan2Caml.ag" #-}

import ExecutionPlan
import Pretty
import PPUtil
import Options
import Data.Monoid(mappend,mempty)
import Data.Maybe
import Data.Graph
import Debug.Trace
import System.IO
import System.Directory
import System.FilePath
import UU.Scanner.Position

import TokenDef
import HsToken
import ErrorMessages

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Data.Foldable(toList)
{-# LINE 65 "dist/build/ExecutionPlan2Caml.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 175 "./src-ag/ExecutionPlan2Caml.ag" #-}

ppRecordTp :: PP a => [a] -> PP_Doc
ppRecordTp es
  | null es   = text "unit"
  | otherwise = pp_block "{" "}" "; " (map pp es)

ppRecordVal :: PP a => [a] -> PP_Doc
ppRecordVal es
  | null es   = text "()"
  | otherwise = pp_block "{" "}" "; " (map pp es)

ppFieldsVal :: Bool -> [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)] -> PP_Doc
ppFieldsVal record fields
  | null fields = text "()"
  | record      = ppRecordVal [ r >#< "=" >#< x | (r,x,_,_) <- fields ]
  | otherwise   = pp_block "(" ")" "," [ x | (_,x,_,_) <- fields ]

ppFieldsType :: Bool -> Bool -> [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)] -> PP_Doc
ppFieldsType record defor fields
  | null fields = text "unit"
  | record      = ppRecordTp [ r >#< ":" >#< (if defor then d else f) | (r,_,d,f) <- fields ]
  | otherwise   = pp_block "(" ")" "*" [ if defor then d else f | (_,_,d,f) <- fields ]
{-# LINE 91 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 288 "./src-ag/ExecutionPlan2Caml.ag" #-}

ppTp :: Type -> PP_Doc
ppTp tp = case tp of
  Haskell t -> pp t   -- ocaml type
  NT nt tps deforested
    | nt == _SELF -> pp "?SELF?"
    | null tps    -> ppNontTp nt deforested
    | otherwise   -> pp_parens (ppSpaced (map pp_parens tps) >#< ppNontTp nt deforested)
  Self -> pp "?SELF?"

ppNontTp :: NontermIdent -> Bool -> PP_Doc
ppNontTp nt True  = pp "t_" >|< pp nt
ppNontTp nt False = pp nt

-- multiple type parameters go into a tuple
ppTypeParams :: PP a => [a] -> PP_Doc
ppTypeParams []  = empty
ppTypeParams [x] = pp x
ppTypeParams xs  = pp_block "(" ")" "," (map pp xs)
{-# LINE 113 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 361 "./src-ag/ExecutionPlan2Caml.ag" #-}

-- convention for nonterminals to module names
modName :: NontermIdent -> PP_Doc
modName nt = pp "M_" >|< pp nt

ppFunDecl :: Bool -> PP_Doc -> [(PP_Doc,PP_Doc)] -> PP_Doc -> PP_Doc -> PP_Doc
ppFunDecl gensigs nm args resSig expr = body where
  body = nm >#< ppSpaced (map arg args) >#< ppRes >#< "="
         >-< indent 2 expr
  arg (arg,tp) = ppArg gensigs arg tp
  ppRes
    | gensigs  = ":" >#< resSig
    | otherwise = empty

ppArg :: Bool -> PP_Doc -> PP_Doc -> PP_Doc
ppArg gensigs arg tp
  | gensigs   = pp_parens (arg >#< ":" >#< tp)
  | otherwise = arg

{-# LINE 135 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 428 "./src-ag/ExecutionPlan2Caml.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 139 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 464 "./src-ag/ExecutionPlan2Caml.ag" #-}

--
-- conventions
--

-- type of the state of a node: a closure containing the children states and attributes,
-- with code of type 'type_nt_sem' that represents the subsequent visits to successor states.
type_nt_state nt st = "s_" >|< nt >|< "_" >|< st

-- type of a visit to a node (the initial, and when in a given state)
-- an instance of this type is called the "semantics"
type_nt_sem_top nt = "t_" >|< nt
type_nt_sem nt st = type_nt_sem_top nt >|< "_s" >|< st

-- type of a caller (contains visit selection + inputs + continuation)
type_caller nt st = "c_" >|< nt >|< "_s" >|< st

-- names of records
nm_attach nt = "attach_">|< nt
nm_invoke nt st = "inv_" >|< nt >|< "_s" >|< st

-- name of the type variable representing the result type of the continuation
cont_tvar = text "'cont__"


-- order states in reverse topological order so that successor states are
-- earlier in the resulting list.
orderStates :: StateIdentifier -> [VisitStateState] -> [StateIdentifier]
orderStates initial edges = res where
  source  = Map.singleton initial Set.empty  -- ensures that the initial state is in graph even when there are no edges
  targets = [ Map.singleton t Set.empty | (_,_,t) <- edges ]
  deps    = [ Map.singleton f (Set.singleton t) | (_,f,t) <- edges ]

  mp  = Map.unionsWith Set.union (source : (targets ++ deps))
  es  = [ (f,f,Set.toList ts) | (f,ts) <- Map.toList mp ]
  cps = stronglyConnComp es
  res = flattenSCCs cps
{-# LINE 179 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 521 "./src-ag/ExecutionPlan2Caml.ag" #-}

type_caller_visit nt v = "c_" >|< nt >|< "_v" >|< v
con_visit nt v = "C_" >|< nt >|< "_v" >|< v

-- field names
nm_inh nt v  = "inh_" >|< nt >|< "_v" >|< v
nm_cont nt v = "cont_" >|< nt >|< "_v" >|< v
{-# LINE 189 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 567 "./src-ag/ExecutionPlan2Caml.ag" #-}

-- more naming conventions
nm_inarg nm nt v = "i_" >|< nm >|< "_" >|< nt >|< "_v" >|< v
nm_outarg nm nt v = "o_" >|< nm >|< "_" >|< nt >|< "_v" >|< v
nm_outarg_cont = nm_outarg "_cont"

conNmTVisit nt vId      = "t_" >|< nt >|< "_v"    >|< vId
conNmTVisitIn nt vId    = "t_" >|< nt >|< "_vIn"  >|< vId
conNmTVisitOut nt vId   = "t_" >|< nt >|< "_vOut" >|< vId

-- todo: remove ppMonadType
ppMonadType :: Options -> PP_Doc
ppMonadType opts
  | parallelInvoke opts = text "IO"
  | otherwise           = text "Identity"
{-# LINE 207 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 780 "./src-ag/ExecutionPlan2Caml.ag" #-}

nm_visit v = "__v" >|< v
nm_k st = "__k" >|< st
nm_st st = "__st" >|< st

mklets :: (PP b, PP c) => [b] -> c -> PP_Doc
mklets defs body = res where
  ppLet def = "let" >#< def >#< "in"
  res = vlist (map ppLet defs) >-< body
{-# LINE 219 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 822 "./src-ag/ExecutionPlan2Caml.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 228 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 963 "./src-ag/ExecutionPlan2Caml.ag" #-}

stname :: Identifier -> Int -> String
stname child st = "_" ++ getName child ++ "X" ++ show st

-- should actually return some conversion info
compatibleAttach :: VisitKind -> NontermIdent -> Options -> Bool
compatibleAttach _ _ _ = True
{-# LINE 238 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1030 "./src-ag/ExecutionPlan2Caml.ag" #-}

dummyPat :: Options -> Bool -> PP_Doc
dummyPat opts noArgs
  | not noArgs            = empty
  | strictDummyToken opts = text "()"
  | otherwise             = text "(_ : unit)"

dummyArg :: Options -> Bool -> PP_Doc
dummyArg opts noArgs
  | not noArgs            = empty
  | otherwise             = text "()"

dummyType :: Options -> Bool -> PP_Doc
dummyType opts noArgs
  | not noArgs            = empty
  | otherwise             = text "unit"
{-# LINE 257 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1106 "./src-ag/ExecutionPlan2Caml.ag" #-}

data NonLocalAttr
  = AttrInh Identifier Identifier
  | AttrSyn Identifier Identifier deriving Show

mkNonLocalAttr :: Bool -> Identifier -> Identifier -> NonLocalAttr
mkNonLocalAttr True  = AttrInh  -- True: inherited attr
mkNonLocalAttr False = AttrSyn

lookupAttrType :: NonLocalAttr -> Map Identifier Attributes -> Map Identifier Attributes -> Map Identifier Type -> Maybe PP_Doc
lookupAttrType (AttrInh child name) inhs _ = lookupType child name inhs
lookupAttrType (AttrSyn child name) _ syns = lookupType child name syns

-- Note: if the child takes type parameters, the type of an attribute of this child may refer to these parameters. This means that
-- the actual type of the attribute needs to have its type parameters substituted with the actual type argument of the child.
-- However, for now we simply decide to return Nothing in this case, which skips the type annotation.
lookupType :: Identifier -> Identifier -> Map Identifier Attributes -> Map Identifier Type -> Maybe PP_Doc
lookupType child name attrMp childMp
  | noParameters childTp = Just ppDoc
  | otherwise            = Nothing
  where
    attrTp     = Map.findWithDefault (error "lookupType: the attribute is not in the attrs of the child") name childAttrs
    childAttrs = Map.findWithDefault (error "lookupType: the attributes of the nonterm are not in the map") nonterm attrMp
    nonterm    = extractNonterminal childTp
    childTp    = Map.findWithDefault (error ("lookupType: the child " ++ show child ++ "is not in the appropriate map")) child childMp
    ppDoc      = ppTp attrTp

noParameters :: Type -> Bool
noParameters (Haskell _)   = True
noParameters (NT _ args _) = null args
{-# LINE 290 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1204 "./src-ag/ExecutionPlan2Caml.ag" #-}

contNm = text "__cont_"
inpsNm = text "__inps_"

-- a `compatibleKind` b  means: can kind b be invoked from a
compatibleKind :: VisitKind -> VisitKind -> Bool
compatibleKind _              _             = True

compatibleRule :: VisitKind -> Bool -> Bool
compatibleRule (VisitPure _) False = False
compatibleRule _             _     = True
{-# LINE 304 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1229 "./src-ag/ExecutionPlan2Caml.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 309 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1252 "./src-ag/ExecutionPlan2Caml.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 318 "dist/build/ExecutionPlan2Caml.hs" #-}
-- EChild ------------------------------------------------------
-- wrapper
data Inh_EChild  = Inh_EChild { allInitStates_Inh_EChild :: (Map NontermIdent Int), con_Inh_EChild :: (ConstructorIdent), mainFile_Inh_EChild :: (String), mainName_Inh_EChild :: (String), nt_Inh_EChild :: (NontermIdent), options_Inh_EChild :: (Options) }
data Syn_EChild  = Syn_EChild { argnamesw_Syn_EChild :: ( PP_Doc ), childTypes_Syn_EChild :: (Map Identifier Type), childintros_Syn_EChild :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), sigs_Syn_EChild :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]), terminaldefs_Syn_EChild :: (Set String) }
{-# INLINABLE wrap_EChild #-}
wrap_EChild :: T_EChild  -> Inh_EChild  -> (Syn_EChild )
wrap_EChild (T_EChild act) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions
        (T_EChild_vOut1 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs) <- return (inv_EChild_s2 sem arg)
        return (Syn_EChild _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs)
   )

-- cata
{-# NOINLINE sem_EChild #-}
sem_EChild :: EChild  -> T_EChild 
sem_EChild ( EChild name_ tp_ kind_ hasAround_ merges_ isMerged_ ) = sem_EChild_EChild name_ tp_ kind_ hasAround_ merges_ isMerged_
sem_EChild ( ETerm name_ tp_ ) = sem_EChild_ETerm name_ tp_

-- semantic domain
newtype T_EChild  = T_EChild {
                             attach_T_EChild :: Identity (T_EChild_s2 )
                             }
newtype T_EChild_s2  = C_EChild_s2 {
                                   inv_EChild_s2 :: (T_EChild_v1 )
                                   }
data T_EChild_s3  = C_EChild_s3
type T_EChild_v1  = (T_EChild_vIn1 ) -> (T_EChild_vOut1 )
data T_EChild_vIn1  = T_EChild_vIn1 (Map NontermIdent Int) (ConstructorIdent) (String) (String) (NontermIdent) (Options)
data T_EChild_vOut1  = T_EChild_vOut1 ( PP_Doc ) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) (Set String)
{-# NOINLINE sem_EChild_EChild #-}
sem_EChild_EChild :: (Identifier) -> (Type) -> (ChildKind) -> (Bool) -> (Maybe [Identifier]) -> (Bool) -> T_EChild 
sem_EChild_EChild arg_name_ arg_tp_ arg_kind_ arg_hasAround_ _ _ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) -> ( let
         _tpDocFor = rule0 arg_tp_
         _tpDocDefor = rule1 arg_tp_
         _fieldNm = rule2 _lhsIcon _lhsInt arg_name_
         _childNm = rule3 arg_name_
         _field = rule4 _childNm _fieldNm _tpDocDefor _tpDocFor
         _lhsOsigs :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         _lhsOsigs = rule5 _field arg_kind_
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule6 _lhsIoptions _nt arg_kind_ arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule7 _introcode arg_name_
         _isDefor = rule8 arg_tp_
         _valcode = rule9 _isDefor _lhsIoptions _nt arg_kind_ arg_name_
         _aroundcode = rule10 _lhsIoptions arg_hasAround_ arg_name_
         _introcode = rule11 _aroundcode _initSt _isDefor _lhsIoptions _nt _valcode arg_hasAround_ arg_kind_ arg_name_
         _nt = rule12 arg_tp_
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule13 arg_name_ arg_tp_
         _initSt = rule14 _lhsIallInitStates _nt
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule15  ()
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule0 = \ tp_ ->
                         {-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         ppTp $ removeDeforested tp_
                         {-# LINE 386 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule1 = \ tp_ ->
                         {-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         ppTp $ forceDeforested tp_
                         {-# LINE 392 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule2 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                         {-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         text $ recordFieldname _lhsInt _lhsIcon name_
                         {-# LINE 398 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 281 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule3 = \ name_ ->
                         {-# LINE 281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         text (fieldname name_)
                         {-# LINE 404 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 282 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule4 = \ _childNm _fieldNm _tpDocDefor _tpDocFor ->
                     {-# LINE 282 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     (_fieldNm    , _childNm    , _tpDocDefor    , _tpDocFor    )
                     {-# LINE 410 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 283 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule5 = \ _field kind_ ->
                         {-# LINE 283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         case kind_ of
                           ChildAttr -> []
                           _         -> [_field    ]
                         {-# LINE 418 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 396 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule6 = \ ((_lhsIoptions) :: Options) _nt kind_ name_ ->
                             {-# LINE 396 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             case kind_ of
                               ChildSyntax     -> "(" >#< prefix _lhsIoptions >|< _nt     >#< name_ >|< "_" >#< ")"
                               ChildAttr       -> empty
                               ChildReplace tp -> "(" >#< prefix _lhsIoptions >|< extractNonterminal tp >#< name_ >|< "_" >#< ")"
                             {-# LINE 427 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 923 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule7 = \ _introcode name_ ->
                               {-# LINE 923 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               Map.singleton name_ _introcode
                               {-# LINE 433 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 924 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule8 = \ tp_ ->
                               {-# LINE 924 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               case tp_ of
                                 NT _ _ defor -> defor
                                 _            -> False
                               {-# LINE 441 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 927 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule9 = \ _isDefor ((_lhsIoptions) :: Options) _nt kind_ name_ ->
                               {-# LINE 927 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               case kind_ of
                                 ChildSyntax -> name_ >|< "_"
                                 ChildAttr   ->
                                                let head | not _isDefor     = if lateHigherOrderBinding _lhsIoptions
                                                                              then lateSemNtLabel _nt     >#< lhsname _lhsIoptions True idLateBindingAttr
                                                                              else prefix _lhsIoptions >|< _nt
                                                         | otherwise        = empty
                                                in pp_parens (head >#< instname name_)
                                 ChildReplace _ ->
                                                   pp_parens (instname name_ >#< name_ >|< "_")
                               {-# LINE 456 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 938 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule10 = \ ((_lhsIoptions) :: Options) hasAround_ name_ ->
                               {-# LINE 938 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               if hasAround_
                               then locname _lhsIoptions name_ >|< "_around"
                               else empty
                               {-# LINE 464 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 941 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule11 = \ _aroundcode _initSt _isDefor ((_lhsIoptions) :: Options) _nt _valcode hasAround_ kind_ name_ ->
                               {-# LINE 941 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               \kind -> let pat    = text $ stname name_ _initSt
                                            attach = pp_parens (_aroundcode     >#< _valcode    ) >|< "." >|< nm_attach _nt     >#< "()"
                                            decl   = pat >#< "=" >#< attach
                                        in if compatibleAttach kind _nt     _lhsIoptions
                                           then Right ( "let" >#< decl >#< "in"
                                                      , Set.singleton (stname name_ _initSt    )
                                                      , case kind_ of
                                                          ChildAttr   -> Map.insert (instname name_) Nothing $
                                                                           ( if _isDefor     || not (lateHigherOrderBinding _lhsIoptions)
                                                                             then id
                                                                             else Map.insert (lhsname _lhsIoptions True idLateBindingAttr) (Just $ AttrInh _LHS idLateBindingAttr)
                                                                           ) $
                                                                           ( if hasAround_
                                                                             then Map.insert (locname _lhsIoptions (name_) ++ "_around") Nothing
                                                                             else id
                                                                           ) $ Map.empty
                                                          ChildReplace _ -> Map.singleton (instname name_) Nothing
                                                          ChildSyntax    -> Map.empty
                                                      )
                                           else Left $ IncompatibleAttachKind name_ kind
                               {-# LINE 489 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 961 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule12 = \ tp_ ->
                      {-# LINE 961 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      extractNonterminal tp_
                      {-# LINE 495 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 1424 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule13 = \ name_ tp_ ->
                     {-# LINE 1424 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 501 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 1468 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule14 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) _nt ->
                 {-# LINE 1468 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                 {-# LINE 507 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule15 #-}
   rule15 = \  (_ :: ()) ->
     Set.empty
{-# NOINLINE sem_EChild_ETerm #-}
sem_EChild_ETerm :: (Identifier) -> (Type) -> T_EChild 
sem_EChild_ETerm arg_name_ arg_tp_ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) -> ( let
         _tpDocFor = rule16 arg_tp_
         _tpDocDefor = rule17 arg_tp_
         _fieldNm = rule18 _lhsIcon _lhsInt arg_name_
         _childNm = rule19 arg_name_
         _field = rule20 _childNm _fieldNm _tpDocDefor _tpDocFor
         _lhsOsigs :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         _lhsOsigs = rule21 _field
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule22 arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule23 arg_name_
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule24 arg_name_
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule25 arg_name_ arg_tp_
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule16 #-}
   {-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule16 = \ tp_ ->
                         {-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         ppTp $ removeDeforested tp_
                         {-# LINE 541 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule17 = \ tp_ ->
                         {-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         ppTp $ forceDeforested tp_
                         {-# LINE 547 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule18 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                         {-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         text $ recordFieldname _lhsInt _lhsIcon name_
                         {-# LINE 553 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 281 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule19 = \ name_ ->
                         {-# LINE 281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         text (fieldname name_)
                         {-# LINE 559 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 282 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule20 = \ _childNm _fieldNm _tpDocDefor _tpDocFor ->
                     {-# LINE 282 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     (_fieldNm    , _childNm    , _tpDocDefor    , _tpDocFor    )
                     {-# LINE 565 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 286 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule21 = \ _field ->
                         {-# LINE 286 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         [_field    ]
                         {-# LINE 571 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 400 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule22 = \ name_ ->
                             {-# LINE 400 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text $ fieldname name_
                             {-# LINE 577 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 922 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule23 = \ name_ ->
                               {-# LINE 922 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               Map.singleton name_ (\_ -> Right (empty, Set.empty, Map.empty))
                               {-# LINE 583 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 1266 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule24 = \ name_ ->
                       {-# LINE 1266 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       Set.singleton $ fieldname name_
                       {-# LINE 589 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 1424 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule25 = \ name_ tp_ ->
                     {-# LINE 1424 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 595 "dist/build/ExecutionPlan2Caml.hs"#-}

-- EChildren ---------------------------------------------------
-- wrapper
data Inh_EChildren  = Inh_EChildren { allInitStates_Inh_EChildren :: (Map NontermIdent Int), con_Inh_EChildren :: (ConstructorIdent), mainFile_Inh_EChildren :: (String), mainName_Inh_EChildren :: (String), nt_Inh_EChildren :: (NontermIdent), options_Inh_EChildren :: (Options) }
data Syn_EChildren  = Syn_EChildren { argnamesw_Syn_EChildren :: ([PP_Doc]), childTypes_Syn_EChildren :: (Map Identifier Type), childintros_Syn_EChildren :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), sigs_Syn_EChildren :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]), terminaldefs_Syn_EChildren :: (Set String) }
{-# INLINABLE wrap_EChildren #-}
wrap_EChildren :: T_EChildren  -> Inh_EChildren  -> (Syn_EChildren )
wrap_EChildren (T_EChildren act) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions
        (T_EChildren_vOut4 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs) <- return (inv_EChildren_s5 sem arg)
        return (Syn_EChildren _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs)
   )

-- cata
{-# NOINLINE sem_EChildren #-}
sem_EChildren :: EChildren  -> T_EChildren 
sem_EChildren list = Prelude.foldr sem_EChildren_Cons sem_EChildren_Nil (Prelude.map sem_EChild list)

-- semantic domain
newtype T_EChildren  = T_EChildren {
                                   attach_T_EChildren :: Identity (T_EChildren_s5 )
                                   }
newtype T_EChildren_s5  = C_EChildren_s5 {
                                         inv_EChildren_s5 :: (T_EChildren_v4 )
                                         }
data T_EChildren_s6  = C_EChildren_s6
type T_EChildren_v4  = (T_EChildren_vIn4 ) -> (T_EChildren_vOut4 )
data T_EChildren_vIn4  = T_EChildren_vIn4 (Map NontermIdent Int) (ConstructorIdent) (String) (String) (NontermIdent) (Options)
data T_EChildren_vOut4  = T_EChildren_vOut4 ([PP_Doc]) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) (Set String)
{-# NOINLINE sem_EChildren_Cons #-}
sem_EChildren_Cons :: T_EChild  -> T_EChildren  -> T_EChildren 
sem_EChildren_Cons arg_hd_ arg_tl_ = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_EChild (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_tl_))
         (T_EChild_vOut1 _hdIargnamesw _hdIchildTypes _hdIchildintros _hdIsigs _hdIterminaldefs) = inv_EChild_s2 _hdX2 (T_EChild_vIn1 _hdOallInitStates _hdOcon _hdOmainFile _hdOmainName _hdOnt _hdOoptions)
         (T_EChildren_vOut4 _tlIargnamesw _tlIchildTypes _tlIchildintros _tlIsigs _tlIterminaldefs) = inv_EChildren_s5 _tlX5 (T_EChildren_vIn4 _tlOallInitStates _tlOcon _tlOmainFile _tlOmainName _tlOnt _tlOoptions)
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule26 _hdIargnamesw _tlIargnamesw
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule27 _hdIchildTypes _tlIchildTypes
         _lhsOchildintros :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule28 _hdIchildintros _tlIchildintros
         _lhsOsigs :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         _lhsOsigs = rule29 _hdIsigs _tlIsigs
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule30 _hdIterminaldefs _tlIterminaldefs
         _hdOallInitStates = rule31 _lhsIallInitStates
         _hdOcon = rule32 _lhsIcon
         _hdOmainFile = rule33 _lhsImainFile
         _hdOmainName = rule34 _lhsImainName
         _hdOnt = rule35 _lhsInt
         _hdOoptions = rule36 _lhsIoptions
         _tlOallInitStates = rule37 _lhsIallInitStates
         _tlOcon = rule38 _lhsIcon
         _tlOmainFile = rule39 _lhsImainFile
         _tlOmainName = rule40 _lhsImainName
         _tlOnt = rule41 _lhsInt
         _tlOoptions = rule42 _lhsIoptions
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule26 #-}
   rule26 = \ ((_hdIargnamesw) ::  PP_Doc ) ((_tlIargnamesw) :: [PP_Doc]) ->
     _hdIargnamesw : _tlIargnamesw
   {-# INLINE rule27 #-}
   rule27 = \ ((_hdIchildTypes) :: Map Identifier Type) ((_tlIchildTypes) :: Map Identifier Type) ->
     _hdIchildTypes `mappend` _tlIchildTypes
   {-# INLINE rule28 #-}
   rule28 = \ ((_hdIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ((_tlIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _hdIchildintros `Map.union` _tlIchildintros
   {-# INLINE rule29 #-}
   rule29 = \ ((_hdIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) ((_tlIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) ->
     _hdIsigs ++ _tlIsigs
   {-# INLINE rule30 #-}
   rule30 = \ ((_hdIterminaldefs) :: Set String) ((_tlIterminaldefs) :: Set String) ->
     _hdIterminaldefs `Set.union` _tlIterminaldefs
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_EChildren_Nil #-}
sem_EChildren_Nil ::  T_EChildren 
sem_EChildren_Nil  = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) -> ( let
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule43  ()
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule44  ()
         _lhsOchildintros :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule45  ()
         _lhsOsigs :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         _lhsOsigs = rule46  ()
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule47  ()
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule43 #-}
   rule43 = \  (_ :: ()) ->
     []
   {-# INLINE rule44 #-}
   rule44 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule45 #-}
   rule45 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule46 #-}
   rule46 = \  (_ :: ()) ->
     []
   {-# INLINE rule47 #-}
   rule47 = \  (_ :: ()) ->
     Set.empty

-- ENonterminal ------------------------------------------------
-- wrapper
data Inh_ENonterminal  = Inh_ENonterminal { allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminal :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminal :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminal :: (String), mainName_Inh_ENonterminal :: (String), options_Inh_ENonterminal :: (Options), synmap_Inh_ENonterminal :: (Map NontermIdent Attributes), typeSyns_Inh_ENonterminal :: (TypeSyns), wrappers_Inh_ENonterminal :: (Set NontermIdent) }
data Syn_ENonterminal  = Syn_ENonterminal { childvisit_Syn_ENonterminal :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), code_Syn_ENonterminal :: (PP_Doc), datas_Syn_ENonterminal :: (PP_Doc), errors_Syn_ENonterminal :: (Seq Error), fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)), initStates_Syn_ENonterminal :: (Map NontermIdent Int), modules_Syn_ENonterminal :: (PP_Doc), semFunBndDefs_Syn_ENonterminal :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminal :: (Seq PP_Doc), visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminal #-}
wrap_ENonterminal :: T_ENonterminal  -> Inh_ENonterminal  -> (Syn_ENonterminal )
wrap_ENonterminal (T_ENonterminal act) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers
        (T_ENonterminal_vOut7 _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminal_s8 sem arg)
        return (Syn_ENonterminal _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_ENonterminal #-}
sem_ENonterminal :: ENonterminal  -> T_ENonterminal 
sem_ENonterminal ( ENonterminal nt_ params_ classCtxs_ initial_ initialv_ nextVisits_ prevVisits_ prods_ recursive_ hoInfo_ ) = sem_ENonterminal_ENonterminal nt_ params_ classCtxs_ initial_ initialv_ nextVisits_ prevVisits_ ( sem_EProductions prods_ ) recursive_ hoInfo_

-- semantic domain
newtype T_ENonterminal  = T_ENonterminal {
                                         attach_T_ENonterminal :: Identity (T_ENonterminal_s8 )
                                         }
newtype T_ENonterminal_s8  = C_ENonterminal_s8 {
                                               inv_ENonterminal_s8 :: (T_ENonterminal_v7 )
                                               }
data T_ENonterminal_s9  = C_ENonterminal_s9
type T_ENonterminal_v7  = (T_ENonterminal_vIn7 ) -> (T_ENonterminal_vOut7 )
data T_ENonterminal_vIn7  = T_ENonterminal_vIn7 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (Options) (Map NontermIdent Attributes) (TypeSyns) (Set NontermIdent)
data T_ENonterminal_vOut7  = T_ENonterminal_vOut7 (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (PP_Doc) (PP_Doc) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminal_ENonterminal #-}
sem_ENonterminal_ENonterminal :: (NontermIdent) -> ([Identifier]) -> (ClassContext) -> (StateIdentifier) -> (Maybe VisitIdentifier) -> (Map StateIdentifier StateCtx) -> (Map StateIdentifier StateCtx) -> T_EProductions  -> (Bool) -> (HigherOrderInfo) -> T_ENonterminal 
sem_ENonterminal_ENonterminal arg_nt_ arg_params_ _ arg_initial_ arg_initialv_ arg_nextVisits_ arg_prevVisits_ arg_prods_ _ _ = T_ENonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_ENonterminal_v7 
      v7 = \ (T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_prods_))
         (T_EProductions_vOut16 _prodsIallvisits _prodsIchildvisit _prodsIcount _prodsIdatatype _prodsIdatatype_call _prodsIdatatype_con _prodsIerrors _prodsIfromToStates _prodsIsemFunBndDefs _prodsIsemFunBndTps _prodsIsem_nt _prodsIsem_prod _prodsIt_visits _prodsIvisitKinds _prodsIvisitdefs _prodsIvisituses) = inv_EProductions_s17 _prodsX17 (T_EProductions_vIn16 _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOprevVisits _prodsOrename _prodsOsynmap)
         _prodsOrename = rule48 _lhsIoptions
         _prodsOnt = rule49 arg_nt_
         _prodsOparams = rule50 arg_params_
         _lhsOdatas :: PP_Doc
         _lhsOdatas = rule51 _c_states _datatypeNt _datatypeProds _hasWrapper _lhsIoptions _prodsIt_visits _t_init _t_states _wr_inh _wr_syn arg_nt_
         _lhsOcode :: PP_Doc
         _lhsOcode = rule52 _datatypeCon _hasWrapper _lhsIoptions _prodsIsem_prod _sem_nt _wrapper arg_nt_
         _lhsOmodules :: PP_Doc
         _lhsOmodules = rule53 _moduleDecl
         _hasWrapper = rule54 _lhsIwrappers arg_nt_
         _t_params = rule55 arg_params_
         _aliasPre = rule56 _t_params arg_nt_
         _aliasMod = rule57 _aliasPre arg_nt_
         _datatypeNt = rule58 _aliasMod _aliasPre _lhsItypeSyns _prodsIdatatype _prodsIdatatype_call _t_params arg_nt_
         _datatypeCon = rule59 _lhsItypeSyns _prodsIdatatype_con arg_nt_
         _moduleDecl = rule60 _lhsItypeSyns arg_nt_
         _datatypeProds = rule61 _prodsIdatatype
         _fsemname = rule62 _lhsIoptions
         _semname = rule63 _fsemname arg_nt_
         _frecarg = rule64 _fsemname
         _sem_param_tp = rule65 _t_params arg_nt_
         _sem_res_tp = rule66 _t_params _t_type
         _sem_tp = rule67 _sem_param_tp _sem_res_tp
         _o_sigs = rule68 _lhsIoptions
         _sem_nt_body = rule69 _prodsIsem_nt
         _sem_nt = rule70 _frecarg _fsemname _lhsItypeSyns _o_sigs _sem_nt_body _sem_param_tp _sem_res_tp _semname arg_nt_
         (Just _prodsOinhmap) = rule71 _lhsIinhmap arg_nt_
         (Just _prodsOsynmap) = rule72 _lhsIsynmap arg_nt_
         _prodsOallInhmap = rule73 _lhsIinhmap
         _prodsOallSynmap = rule74 _lhsIsynmap
         _allstates = rule75 _prodsIallvisits arg_initial_
         _stvisits = rule76 _prodsIallvisits
         _t_type = rule77 arg_nt_
         _t_c_params = rule78 arg_params_
         _t_init = rule79 _t_params _t_type arg_initial_ arg_nt_
         _t_states = rule80 _allstates _t_c_params _t_params arg_nextVisits_ arg_nt_
         _c_states = rule81 _allstates _prodsIallvisits _t_c_params arg_nextVisits_ arg_nt_
         _wr_inh = rule82 _genwrap _wr_inhs1
         _wr_syn = rule83 _genwrap _wr_syns
         _genwrap = rule84 _t_params arg_nt_
         _inhAttrs = rule85 _lhsIinhmap arg_nt_
         _wr_inhs = rule86 _inhAttrs _wr_filter
         _wr_inhs1 = rule87 _inhAttrs
         _wr_filter = rule88 _lhsIoptions
         _wr_syns = rule89 _lhsIsynmap arg_nt_
         _wrapname = rule90 arg_nt_
         _inhname = rule91 arg_nt_
         _synname = rule92 arg_nt_
         _firstVisitInfo = rule93 arg_initial_ arg_nextVisits_
         _wrapArgSemTp = rule94 _t_params _t_type
         _wrapArgInhTp = rule95 _inhname _t_params
         _wrapArgPats = rule96 _lhsIoptions _wr_inhs1 arg_nt_
         _wrapResTp = rule97 _synname _t_params
         _wrapper = rule98 _o_sigs _wrapArgInhTp _wrapArgPats _wrapArgSemTp _wrapResTp _wrapname _wrapperPreamble
         _wrapperPreamble = rule99 _lhsImainName _lhsIoptions _wrapperBody
         _wrapperBody = rule100 _firstVisitInfo _lhsIoptions _wr_inhs _wr_syns arg_initial_ arg_initialv_ arg_nt_
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule101 _prodsIsemFunBndDefs _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule102 _prodsIsemFunBndTps _semFunBndTp
         _semFunBndDef = rule103 _semFunBndNm _semname
         _semFunBndTp = rule104 _semFunBndNm _sem_tp
         _semFunBndNm = rule105 arg_nt_
         _prodsOinitial = rule106 arg_initial_
         _prodsOallstates = rule107 _allstates
         _prodsOnextVisits = rule108 arg_nextVisits_
         _prodsOprevVisits = rule109 arg_prevVisits_
         _prodsOlocalAttrTypes = rule110 _lhsIlocalAttrTypes arg_nt_
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule111 arg_initial_ arg_nt_
         _ntType = rule112 arg_nt_ arg_params_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule113 _prodsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule114 _prodsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule115 _prodsIfromToStates
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule116 _prodsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule117 _prodsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule118 _prodsIvisituses
         _prodsOallFromToStates = rule119 _lhsIallFromToStates
         _prodsOallInitStates = rule120 _lhsIallInitStates
         _prodsOallVisitKinds = rule121 _lhsIallVisitKinds
         _prodsOallchildvisit = rule122 _lhsIallchildvisit
         _prodsOavisitdefs = rule123 _lhsIavisitdefs
         _prodsOavisituses = rule124 _lhsIavisituses
         _prodsOmainFile = rule125 _lhsImainFile
         _prodsOmainName = rule126 _lhsImainName
         _prodsOntType = rule127 _ntType
         _prodsOoptions = rule128 _lhsIoptions
         __result_ = T_ENonterminal_vOut7 _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminal_s8 v7
   {-# INLINE rule48 #-}
   {-# LINE 78 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule48 = \ ((_lhsIoptions) :: Options) ->
                   {-# LINE 78 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   rename _lhsIoptions
                   {-# LINE 890 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule49 #-}
   {-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule49 = \ nt_ ->
               {-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
               nt_
               {-# LINE 896 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule50 #-}
   {-# LINE 96 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule50 = \ params_ ->
                   {-# LINE 96 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   params_
                   {-# LINE 902 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 115 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule51 = \ _c_states _datatypeNt _datatypeProds _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIt_visits) :: PP_Doc) _t_init _t_states _wr_inh _wr_syn nt_ ->
                {-# LINE 115 "./src-ag/ExecutionPlan2Caml.ag" #-}
                (    text ""
                 >-< "(* *** " ++ getName nt_ ++ " *** [data] *)")
                 >-< (if dataTypes _lhsIoptions
                      then pp "(* data *)"
                           >-< _datatypeNt
                           >-< _datatypeProds
                           >-< ""
                      else empty)
                 >-< (if _hasWrapper
                       then pp "(* wrapper *)"
                            >-< _wr_inh
                            >-< _wr_syn
                            >-< ""
                       else empty)
                 >-< (if semfuns _lhsIoptions
                      then pp "(* semantic domain *)"
                           >-< _t_init
                           >-< _t_states
                           >-< _c_states
                           >-< _prodsIt_visits
                           >-< ""
                      else empty)
                {-# LINE 929 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule52 #-}
   {-# LINE 138 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule52 = \ _datatypeCon _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIsem_prod) :: PP_Doc) _sem_nt _wrapper nt_ ->
                 {-# LINE 138 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 (    text ""
                  >-< "(* *** " ++ getName nt_ ++ " *** [code] *)")
                  >-< (if dataTypes _lhsIoptions
                      then pp "(* constructor functions *)"
                           >-< _datatypeCon
                      else empty)
                  >-< (if _hasWrapper
                       then pp "(* wrapper *)"
                            >-< _wrapper
                            >-< ""
                       else empty)
                  >-< (if folds _lhsIoptions
                       then "(* cata *)"
                            >-< _sem_nt
                            >-< ""
                       else empty)
                  >-< (if semfuns _lhsIoptions
                       then "(* semantic domain *)"
                            >-< _prodsIsem_prod
                            >-< ""
                       else empty)
                 {-# LINE 955 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule53 #-}
   {-# LINE 163 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule53 = \ _moduleDecl ->
                  {-# LINE 163 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  _moduleDecl
                  {-# LINE 961 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule54 #-}
   {-# LINE 165 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule54 = \ ((_lhsIwrappers) :: Set NontermIdent) nt_ ->
                     {-# LINE 165 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     nt_ `Set.member` _lhsIwrappers
                     {-# LINE 967 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule55 #-}
   {-# LINE 216 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule55 = \ params_ ->
                   {-# LINE 216 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   ppTypeParams params_
                   {-# LINE 973 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule56 #-}
   {-# LINE 217 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule56 = \ _t_params nt_ ->
                   {-# LINE 217 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   "and" >#< _t_params     >#< nt_ >#< "="
                   {-# LINE 979 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule57 #-}
   {-# LINE 218 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule57 = \ _aliasPre nt_ ->
                   {-# LINE 218 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   _aliasPre     >#< modName nt_ >|< ".t"
                   {-# LINE 985 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule58 #-}
   {-# LINE 220 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule58 = \ _aliasMod _aliasPre ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype) :: [PP_Doc]) ((_prodsIdatatype_call) :: [PP_Doc]) _t_params nt_ ->
        {-# LINE 220 "./src-ag/ExecutionPlan2Caml.ag" #-}
        case lookup nt_ _lhsItypeSyns of
          Just (List t)     -> _aliasPre     >#< ppTp t >#< "list"
          Just (Maybe t)    -> _aliasPre     >#< ppTp t >#< "option"
          Just (Tuple ts)   -> _aliasPre     >#< (pp_block "(" ")" " * " $ map (ppTp . snd) ts)
          Just (Map k v)    -> _aliasMod
          Just (IntMap t)   -> _aliasMod
          Just (OrdSet t)   -> _aliasMod
          Just IntSet       -> _aliasMod
          _ -> "and" >#< _t_params     >#< nt_ >#< "="
               >-< ( if null _prodsIdatatype
                     then pp "unit"
                     else indent 2 $ vlist _prodsIdatatype_call
                   )
        {-# LINE 1003 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule59 #-}
   {-# LINE 239 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule59 = \ ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype_con) :: [PP_Doc]) nt_ ->
        {-# LINE 239 "./src-ag/ExecutionPlan2Caml.ag" #-}
        case lookup nt_ _lhsItypeSyns of
          Just _  -> empty
          Nothing -> vlist _prodsIdatatype_con
        {-# LINE 1011 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule60 #-}
   {-# LINE 244 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule60 = \ ((_lhsItypeSyns) :: TypeSyns) nt_ ->
        {-# LINE 244 "./src-ag/ExecutionPlan2Caml.ag" #-}
        let ppModule :: PP a => a -> PP_Doc
            ppModule expr = "module" >#< modName nt_ >#< "="
        in case lookup nt_ _lhsItypeSyns of
             Just (Map k _)  -> ppModule ("Map.Make" >#< pp_parens (ppTp k))
             Just (IntMap _) -> ppModule ("Map.Make ()")
             Just (OrdSet t) -> ppModule ("Set.Make" >#< pp_parens (ppTp t))
             Just IntSet     -> ppModule ("Set.Make (struct  type t = int  let compare = Pervasives.compare  end)")
             _               -> empty
        {-# LINE 1024 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 253 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule61 = \ ((_prodsIdatatype) :: [PP_Doc]) ->
                        {-# LINE 253 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        vlist _prodsIdatatype
                        {-# LINE 1030 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 313 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule62 = \ ((_lhsIoptions) :: Options) ->
                   {-# LINE 313 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   \x -> prefix _lhsIoptions ++ show x
                   {-# LINE 1036 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 314 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule63 = \ _fsemname nt_ ->
                  {-# LINE 314 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  _fsemname     nt_
                  {-# LINE 1042 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 315 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule64 = \ _fsemname ->
                  {-# LINE 315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  \t x -> case t of
                    NT nt _ _ -> pp_parens (_fsemname nt >#< x)
                    _         -> x
                  {-# LINE 1050 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule65 #-}
   {-# LINE 319 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule65 = \ _t_params nt_ ->
                       {-# LINE 319 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       _t_params     >#< nt_
                       {-# LINE 1056 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 320 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule66 = \ _t_params _t_type ->
                       {-# LINE 320 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       _t_params     >#< _t_type
                       {-# LINE 1062 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 321 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule67 = \ _sem_param_tp _sem_res_tp ->
                       {-# LINE 321 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       _sem_param_tp     >#< "->" >#< _sem_res_tp
                       {-# LINE 1068 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 323 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule68 = \ ((_lhsIoptions) :: Options) ->
                  {-# LINE 323 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  typeSigs _lhsIoptions
                  {-# LINE 1074 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 324 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule69 = \ ((_prodsIsem_nt) :: PP_Doc) ->
                      {-# LINE 324 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      "match arg with" >-< (indent 2 $ _prodsIsem_nt)
                      {-# LINE 1080 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 325 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule70 = \ _frecarg _fsemname ((_lhsItypeSyns) :: TypeSyns) _o_sigs _sem_nt_body _sem_param_tp _sem_res_tp _semname nt_ ->
                  {-# LINE 325 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  let genSem :: PP a => a -> PP_Doc -> PP_Doc
                      genSem nm body = "and" >#< ppFunDecl _o_sigs     (pp _semname    ) [(pp nm, _sem_param_tp    )] _sem_res_tp     body
                      genAlias alts = genSem (pp "arg") (pp "match arg with" >-< (indent 2 $ vlist $ map (pp "|" >#<) alts))
                      genMap v = let body = modName nt_ >|< ".fold" >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil" >#< els
                                     els  = case v of
                                       NT nt _ _ -> pp_parens (modName nt_ >|< ".map" >#< _fsemname     nt >#< "m")
                                       _         -> pp "m"
                                 in genSem "m" body
                      genSet mbNt = let body = "List.fold_right" >#< _semname     >|< "_Entry" >#<
                                                els (pp_parens (modName nt_ >|< ".elements" >#< "s")) >#< _semname     >|< "_Nil"
                                        els r = maybe r (\nt -> pp_parens ("List.map" >#< _fsemname     nt >#< r)) mbNt
                                    in genSem "s" body
                  in case lookup nt_ _lhsItypeSyns of
                       Just (List t) -> let body = "List.fold_right" >#< _semname     >|< "_Cons" >#< els >#< _semname     >|< "_Nil"
                                            els  = case t of
                                              NT nt _ _ -> pp_parens ("List.map" >#< _fsemname     nt >#< "list")
                                              _         -> pp "list"
                                        in genSem "list" body
                       Just (Tuple ts) -> let pat = pp_parens (ppCommas $ map fst ts)
                                              body = _semname     >|< "_Tuple" >#< ppSpaced (map (\t -> _frecarg     (snd t) (pp $ fst t)) ts)
                                          in genSem pat body
                       Just (Map _ v) -> genMap v
                       Just (IntMap v) -> genMap v
                       Just (Maybe t) -> genAlias
                           [ "None" >#< "->" >#< "=" >#< _semname     >|< "_Nothing"
                           , "Some" >#< "just" >#< "->" >#< _semname     >|< "_Just" >#< _frecarg t (pp "just")
                           ]
                       Just (OrdSet t) -> genSet $ case t of
                                            NT nt _ _ -> Just nt
                                            _         -> Nothing
                       Just (IntSet) -> genSet Nothing
                       _ -> genSem "arg" _sem_nt_body
                  {-# LINE 1117 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 420 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule71 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 420 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                         Map.lookup nt_ _lhsIinhmap
                                         {-# LINE 1123 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 421 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule72 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 421 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                         Map.lookup nt_ _lhsIsynmap
                                         {-# LINE 1129 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 422 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule73 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 422 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                     _lhsIinhmap
                                     {-# LINE 1135 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 423 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule74 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 423 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                     _lhsIsynmap
                                     {-# LINE 1141 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 444 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule75 = \ ((_prodsIallvisits) :: [VisitStateState]) initial_ ->
                    {-# LINE 444 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    orderStates initial_ _prodsIallvisits
                    {-# LINE 1147 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 445 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule76 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                    {-# LINE 445 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                    {-# LINE 1153 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 446 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule77 = \ nt_ ->
                    {-# LINE 446 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    type_nt_sem_top nt_
                    {-# LINE 1159 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 447 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule78 = \ params_ ->
                     {-# LINE 447 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     ppTypeParams (cont_tvar : map pp params_)
                     {-# LINE 1165 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 450 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule79 = \ _t_params _t_type initial_ nt_ ->
                    {-# LINE 450 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    "and" >#< _t_params     >#< _t_type     >#< "=" >#< pp_braces ( nm_attach nt_ >#< ":" >#< "unit" >#< "->" >#< _t_params     >#< type_nt_sem nt_ initial_)
                    {-# LINE 1171 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 453 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule80 = \ _allstates _t_c_params _t_params nextVisits_ nt_ ->
                    {-# LINE 453 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    vlist $ map (\st ->
                      let s_st = type_nt_state nt_ st
                          t_st  = type_nt_sem nt_ st
                          c_st  = type_caller nt_ st
                          nextVisits = Map.findWithDefault ManyVis st nextVisits_
                          decl = "and" >#< _t_params     >#< t_st >#< "="
                      in case nextVisits of
                           NoneVis    -> decl >#< "unit"
                           _          -> decl >#< ppRecordVal [ nm_invoke nt_ st >#< ":" >#< cont_tvar >#< "." >#< _t_c_params     >#< c_st >#< "->" >#< cont_tvar ]
                     ) _allstates
                    {-# LINE 1186 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 506 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule81 = \ _allstates ((_prodsIallvisits) :: [VisitStateState]) _t_c_params nextVisits_ nt_ ->
                   {-# LINE 506 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   vlist $ map (\st ->
                     let nt_st = type_nt_state nt_ st
                         c_st  = type_caller nt_ st
                         outg  = filter (\(_,f,_) -> f == st) _prodsIallvisits
                         nextVisits = Map.findWithDefault ManyVis st nextVisits_
                         declHead = "and" >#< _t_c_params     >#< c_st >#< "="
                         visitcons = vlist $ map (\(v,_,_) ->
                           "|" >#< con_visit nt_ v >#< "of" >#< _t_c_params     >#< type_caller_visit nt_ v
                          ) outg
                     in case nextVisits of
                          NoneVis  -> empty
                          OneVis v -> declHead >#< _t_c_params     >#< type_caller_visit nt_ v
                          ManyVis  -> declHead >-< indent 3 visitcons
                    ) _allstates
                   {-# LINE 1205 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 588 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule82 = \ _genwrap _wr_inhs1 ->
                   {-# LINE 588 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   _genwrap     "inh" _wr_inhs1
                   {-# LINE 1211 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 589 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule83 = \ _genwrap _wr_syns ->
                   {-# LINE 589 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   _genwrap     "syn" _wr_syns
                   {-# LINE 1217 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 590 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule84 = \ _t_params nt_ ->
                   {-# LINE 590 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   \nm attrs ->
                     "and" >#< _t_params     >#< nm >|< "_" >|< nt_ >#< "=" >#< ppRecordTp
                       [ i >|< "_" >|< nm >|< "_" >|< nt_ >#< ":" >#< ppTp t | (i,t) <- attrs ]
                   {-# LINE 1225 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 594 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule85 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                   {-# LINE 594 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   fromJust $ Map.lookup nt_ _lhsIinhmap
                   {-# LINE 1231 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 595 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule86 = \ _inhAttrs _wr_filter ->
                   {-# LINE 595 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   Map.toList $ _wr_filter     $ _inhAttrs
                   {-# LINE 1237 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 596 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule87 = \ _inhAttrs ->
                   {-# LINE 596 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   Map.toList _inhAttrs
                   {-# LINE 1243 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 597 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule88 = \ ((_lhsIoptions) :: Options) ->
                    {-# LINE 597 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                    then Map.delete idLateBindingAttr
                    else id
                    {-# LINE 1251 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 600 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule89 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                   {-# LINE 600 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                   {-# LINE 1257 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 602 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule90 = \ nt_ ->
                   {-# LINE 602 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   text ("wrap_" ++ show nt_)
                   {-# LINE 1263 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 603 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule91 = \ nt_ ->
                   {-# LINE 603 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   text ("inh_" ++ show nt_)
                   {-# LINE 1269 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 604 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule92 = \ nt_ ->
                   {-# LINE 604 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   text ("syn_" ++ show nt_)
                   {-# LINE 1275 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 605 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule93 = \ initial_ nextVisits_ ->
                         {-# LINE 605 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.findWithDefault ManyVis initial_ nextVisits_
                         {-# LINE 1281 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 607 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule94 = \ _t_params _t_type ->
                       {-# LINE 607 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       _t_params     >#< _t_type
                       {-# LINE 1287 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 608 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule95 = \ _inhname _t_params ->
                       {-# LINE 608 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       _t_params     >#< _inhname
                       {-# LINE 1293 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 609 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule96 = \ ((_lhsIoptions) :: Options) _wr_inhs1 nt_ ->
                       {-# LINE 609 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       ppRecordVal [ i >|< "_inh_" >|< nt_ >#< "=" >#< lhsname _lhsIoptions True i | (i,_) <- _wr_inhs1     ]
                       {-# LINE 1299 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 610 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule97 = \ _synname _t_params ->
                    {-# LINE 610 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    _t_params     >#< _synname
                    {-# LINE 1305 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 611 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule98 = \ _o_sigs _wrapArgInhTp _wrapArgPats _wrapArgSemTp _wrapResTp _wrapname _wrapperPreamble ->
                   {-# LINE 611 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   "and" >#< ppFunDecl _o_sigs     _wrapname     [(pp "act", _wrapArgSemTp    ), (_wrapArgPats    , _wrapArgInhTp    )] _wrapResTp     _wrapperPreamble
                   {-# LINE 1311 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 613 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule99 = \ ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) _wrapperBody ->
        {-# LINE 613 "./src-ag/ExecutionPlan2Caml.ag" #-}
        ( if lateHigherOrderBinding _lhsIoptions
          then "let" >#< lhsname _lhsIoptions True idLateBindingAttr >#< "=" >#< lateBindingFieldNm _lhsImainName >#< "in"
          else empty
        )
        >-< _wrapperBody
        {-# LINE 1321 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 619 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule100 = \ _firstVisitInfo ((_lhsIoptions) :: Options) _wr_inhs _wr_syns initial_ initialv_ nt_ ->
        {-# LINE 619 "./src-ag/ExecutionPlan2Caml.ag" #-}
        case initialv_ of
          Nothing -> text "{ }"
          Just initv ->
            let attach  = "let" >#< "sem" >#< "=" >#< "act." >|< nm_attach nt_ >#< "()" >#< "in"
                pat     = ppRecordVal [ nm_outarg i nt_ initv >#< "=" >#< lhsname _lhsIoptions False i | (i,_) <- _wr_syns     ]
                bld     = ppRecordVal [ i >|< "_syn_" >|< nt_ >#< "=" >#< lhsname _lhsIoptions False i | (i,_) <- _wr_syns     ]
                res     = "let res = function" >#< pat >#< "->" >#< bld >#< "in"
                inps    = "let" >#< "inps" >#< "=" >#< ppRecordVal [ nm_inarg i nt_ initv >#< "=" >#< lhsname _lhsIoptions True i | (i,_) <- _wr_inhs     ] >#< "in"
                arg     = "let" >#< "arg" >#< "=" >#< argcon >#< argrec >#< "in"
                argcon  = case _firstVisitInfo     of
                            ManyVis -> con_visit nt_ initv
                            _       -> empty
                argrec  = ppRecordVal
                            [ nm_inh nt_ initv >#< "=" >#<  "inps"
                            , nm_cont nt_ initv >#< "=" >#< "res"
                            ]
                invoke  = "sem." >|< nm_invoke nt_ initial_ >#< "arg"
            in attach >-< res >-< inps >-< arg >-< invoke
        {-# LINE 1344 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 648 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule101 = \ ((_prodsIsemFunBndDefs) :: Seq PP_Doc) _semFunBndDef ->
                        {-# LINE 648 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                        {-# LINE 1350 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule102 #-}
   {-# LINE 649 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule102 = \ ((_prodsIsemFunBndTps) :: Seq PP_Doc) _semFunBndTp ->
                        {-# LINE 649 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                        {-# LINE 1356 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 650 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule103 = \ _semFunBndNm _semname ->
                        {-# LINE 650 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 1362 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 651 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule104 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 651 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndNm     >#< ":" >#< _sem_tp
                        {-# LINE 1368 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 652 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule105 = \ nt_ ->
                        {-# LINE 652 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        lateSemNtLabel nt_
                        {-# LINE 1374 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule106 #-}
   {-# LINE 682 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule106 = \ initial_ ->
                      {-# LINE 682 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      initial_
                      {-# LINE 1380 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 683 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule107 = \ _allstates ->
                      {-# LINE 683 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      _allstates
                      {-# LINE 1386 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule108 #-}
   {-# LINE 1390 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule108 = \ nextVisits_ ->
                       {-# LINE 1390 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       nextVisits_
                       {-# LINE 1392 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule109 #-}
   {-# LINE 1391 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule109 = \ prevVisits_ ->
                       {-# LINE 1391 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       prevVisits_
                       {-# LINE 1398 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule110 #-}
   {-# LINE 1435 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule110 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) nt_ ->
                           {-# LINE 1435 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                           {-# LINE 1404 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule111 #-}
   {-# LINE 1462 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule111 = \ initial_ nt_ ->
                     {-# LINE 1462 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton nt_ initial_
                     {-# LINE 1410 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule112 #-}
   {-# LINE 1476 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule112 = \ nt_ params_ ->
                 {-# LINE 1476 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 NT nt_ (map show params_) False
                 {-# LINE 1416 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule113 #-}
   rule113 = \ ((_prodsIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _prodsIchildvisit
   {-# INLINE rule114 #-}
   rule114 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule115 #-}
   rule115 = \ ((_prodsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _prodsIfromToStates
   {-# INLINE rule116 #-}
   rule116 = \ ((_prodsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _prodsIvisitKinds
   {-# INLINE rule117 #-}
   rule117 = \ ((_prodsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisitdefs
   {-# INLINE rule118 #-}
   rule118 = \ ((_prodsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisituses
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule127 #-}
   rule127 = \ _ntType ->
     _ntType
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- ENonterminals -----------------------------------------------
-- wrapper
data Inh_ENonterminals  = Inh_ENonterminals { allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminals :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminals :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminals :: (String), mainName_Inh_ENonterminals :: (String), options_Inh_ENonterminals :: (Options), synmap_Inh_ENonterminals :: (Map NontermIdent Attributes), typeSyns_Inh_ENonterminals :: (TypeSyns), wrappers_Inh_ENonterminals :: (Set NontermIdent) }
data Syn_ENonterminals  = Syn_ENonterminals { childvisit_Syn_ENonterminals :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), code_Syn_ENonterminals :: (PP_Doc), datas_Syn_ENonterminals :: (PP_Doc), errors_Syn_ENonterminals :: (Seq Error), fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)), initStates_Syn_ENonterminals :: (Map NontermIdent Int), modules_Syn_ENonterminals :: (PP_Doc), semFunBndDefs_Syn_ENonterminals :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminals :: (Seq PP_Doc), visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminals #-}
wrap_ENonterminals :: T_ENonterminals  -> Inh_ENonterminals  -> (Syn_ENonterminals )
wrap_ENonterminals (T_ENonterminals act) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers
        (T_ENonterminals_vOut10 _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminals_s11 sem arg)
        return (Syn_ENonterminals _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_ENonterminals #-}
sem_ENonterminals :: ENonterminals  -> T_ENonterminals 
sem_ENonterminals list = Prelude.foldr sem_ENonterminals_Cons sem_ENonterminals_Nil (Prelude.map sem_ENonterminal list)

-- semantic domain
newtype T_ENonterminals  = T_ENonterminals {
                                           attach_T_ENonterminals :: Identity (T_ENonterminals_s11 )
                                           }
newtype T_ENonterminals_s11  = C_ENonterminals_s11 {
                                                   inv_ENonterminals_s11 :: (T_ENonterminals_v10 )
                                                   }
data T_ENonterminals_s12  = C_ENonterminals_s12
type T_ENonterminals_v10  = (T_ENonterminals_vIn10 ) -> (T_ENonterminals_vOut10 )
data T_ENonterminals_vIn10  = T_ENonterminals_vIn10 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (Options) (Map NontermIdent Attributes) (TypeSyns) (Set NontermIdent)
data T_ENonterminals_vOut10  = T_ENonterminals_vOut10 (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (PP_Doc) (PP_Doc) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminals_Cons #-}
sem_ENonterminals_Cons :: T_ENonterminal  -> T_ENonterminals  -> T_ENonterminals 
sem_ENonterminals_Cons arg_hd_ arg_tl_ = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_ENonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_tl_))
         (T_ENonterminal_vOut7 _hdIchildvisit _hdIcode _hdIdatas _hdIerrors _hdIfromToStates _hdIinitStates _hdImodules _hdIsemFunBndDefs _hdIsemFunBndTps _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_ENonterminal_s8 _hdX8 (T_ENonterminal_vIn7 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOoptions _hdOsynmap _hdOtypeSyns _hdOwrappers)
         (T_ENonterminals_vOut10 _tlIchildvisit _tlIcode _tlIdatas _tlIerrors _tlIfromToStates _tlIinitStates _tlImodules _tlIsemFunBndDefs _tlIsemFunBndTps _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_ENonterminals_s11 _tlX11 (T_ENonterminals_vIn10 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOoptions _tlOsynmap _tlOtypeSyns _tlOwrappers)
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule129 _hdIchildvisit _tlIchildvisit
         _lhsOcode :: PP_Doc
         _lhsOcode = rule130 _hdIcode _tlIcode
         _lhsOdatas :: PP_Doc
         _lhsOdatas = rule131 _hdIdatas _tlIdatas
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule132 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule133 _hdIfromToStates _tlIfromToStates
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule134 _hdIinitStates _tlIinitStates
         _lhsOmodules :: PP_Doc
         _lhsOmodules = rule135 _hdImodules _tlImodules
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule136 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule137 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule138 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule139 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule140 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule141 _lhsIallFromToStates
         _hdOallInitStates = rule142 _lhsIallInitStates
         _hdOallVisitKinds = rule143 _lhsIallVisitKinds
         _hdOallchildvisit = rule144 _lhsIallchildvisit
         _hdOavisitdefs = rule145 _lhsIavisitdefs
         _hdOavisituses = rule146 _lhsIavisituses
         _hdOinhmap = rule147 _lhsIinhmap
         _hdOlocalAttrTypes = rule148 _lhsIlocalAttrTypes
         _hdOmainFile = rule149 _lhsImainFile
         _hdOmainName = rule150 _lhsImainName
         _hdOoptions = rule151 _lhsIoptions
         _hdOsynmap = rule152 _lhsIsynmap
         _hdOtypeSyns = rule153 _lhsItypeSyns
         _hdOwrappers = rule154 _lhsIwrappers
         _tlOallFromToStates = rule155 _lhsIallFromToStates
         _tlOallInitStates = rule156 _lhsIallInitStates
         _tlOallVisitKinds = rule157 _lhsIallVisitKinds
         _tlOallchildvisit = rule158 _lhsIallchildvisit
         _tlOavisitdefs = rule159 _lhsIavisitdefs
         _tlOavisituses = rule160 _lhsIavisituses
         _tlOinhmap = rule161 _lhsIinhmap
         _tlOlocalAttrTypes = rule162 _lhsIlocalAttrTypes
         _tlOmainFile = rule163 _lhsImainFile
         _tlOmainName = rule164 _lhsImainName
         _tlOoptions = rule165 _lhsIoptions
         _tlOsynmap = rule166 _lhsIsynmap
         _tlOtypeSyns = rule167 _lhsItypeSyns
         _tlOwrappers = rule168 _lhsIwrappers
         __result_ = T_ENonterminals_vOut10 _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule129 #-}
   rule129 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule130 #-}
   rule130 = \ ((_hdIcode) :: PP_Doc) ((_tlIcode) :: PP_Doc) ->
     _hdIcode >-< _tlIcode
   {-# INLINE rule131 #-}
   rule131 = \ ((_hdIdatas) :: PP_Doc) ((_tlIdatas) :: PP_Doc) ->
     _hdIdatas >-< _tlIdatas
   {-# INLINE rule132 #-}
   rule132 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule133 #-}
   rule133 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule134 #-}
   rule134 = \ ((_hdIinitStates) :: Map NontermIdent Int) ((_tlIinitStates) :: Map NontermIdent Int) ->
     _hdIinitStates `mappend` _tlIinitStates
   {-# INLINE rule135 #-}
   rule135 = \ ((_hdImodules) :: PP_Doc) ((_tlImodules) :: PP_Doc) ->
     _hdImodules >-< _tlImodules
   {-# INLINE rule136 #-}
   rule136 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule137 #-}
   rule137 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule138 #-}
   rule138 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule139 #-}
   rule139 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule140 #-}
   rule140 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_ENonterminals_Nil #-}
sem_ENonterminals_Nil ::  T_ENonterminals 
sem_ENonterminals_Nil  = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) -> ( let
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule169  ()
         _lhsOcode :: PP_Doc
         _lhsOcode = rule170  ()
         _lhsOdatas :: PP_Doc
         _lhsOdatas = rule171  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule172  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule173  ()
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule174  ()
         _lhsOmodules :: PP_Doc
         _lhsOmodules = rule175  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule176  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule177  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule178  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule179  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule180  ()
         __result_ = T_ENonterminals_vOut10 _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule169 #-}
   rule169 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule170 #-}
   rule170 = \  (_ :: ()) ->
     empty
   {-# INLINE rule171 #-}
   rule171 = \  (_ :: ()) ->
     empty
   {-# INLINE rule172 #-}
   rule172 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule173 #-}
   rule173 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule174 #-}
   rule174 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule175 #-}
   rule175 = \  (_ :: ()) ->
     empty
   {-# INLINE rule176 #-}
   rule176 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule177 #-}
   rule177 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule178 #-}
   rule178 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule179 #-}
   rule179 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule180 #-}
   rule180 = \  (_ :: ()) ->
     Map.empty

-- EProduction -------------------------------------------------
-- wrapper
data Inh_EProduction  = Inh_EProduction { allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProduction :: (Map NontermIdent Attributes), allInitStates_Inh_EProduction :: (Map NontermIdent Int), allSynmap_Inh_EProduction :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProduction :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), allstates_Inh_EProduction :: ([StateIdentifier]), avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), inhmap_Inh_EProduction :: (Attributes), initial_Inh_EProduction :: (StateIdentifier), localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProduction :: (String), mainName_Inh_EProduction :: (String), nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), nt_Inh_EProduction :: (NontermIdent), ntType_Inh_EProduction :: (Type), options_Inh_EProduction :: (Options), params_Inh_EProduction :: ([Identifier]), prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), rename_Inh_EProduction :: (Bool), synmap_Inh_EProduction :: (Attributes) }
data Syn_EProduction  = Syn_EProduction { allvisits_Syn_EProduction :: ([VisitStateState]), childvisit_Syn_EProduction :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), count_Syn_EProduction :: (Int), datatype_Syn_EProduction :: (PP_Doc), datatype_call_Syn_EProduction :: (PP_Doc), datatype_con_Syn_EProduction :: (PP_Doc), errors_Syn_EProduction :: (Seq Error), fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProduction :: (Seq PP_Doc), semFunBndTps_Syn_EProduction :: (Seq PP_Doc), sem_nt_Syn_EProduction :: (PP_Doc), sem_prod_Syn_EProduction :: (PP_Doc), t_visits_Syn_EProduction :: (PP_Doc), visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProduction #-}
wrap_EProduction :: T_EProduction  -> Inh_EProduction  -> (Syn_EProduction )
wrap_EProduction (T_EProduction act) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap
        (T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProduction_s14 sem arg)
        return (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_EProduction #-}
sem_EProduction :: EProduction  -> T_EProduction 
sem_EProduction ( EProduction con_ params_ constraints_ rules_ children_ visits_ ) = sem_EProduction_EProduction con_ params_ constraints_ ( sem_ERules rules_ ) ( sem_EChildren children_ ) ( sem_Visits visits_ )

-- semantic domain
newtype T_EProduction  = T_EProduction {
                                       attach_T_EProduction :: Identity (T_EProduction_s14 )
                                       }
newtype T_EProduction_s14  = C_EProduction_s14 {
                                               inv_EProduction_s14 :: (T_EProduction_v13 )
                                               }
data T_EProduction_s15  = C_EProduction_s15
type T_EProduction_v13  = (T_EProduction_vIn13 ) -> (T_EProduction_vOut13 )
data T_EProduction_vIn13  = T_EProduction_vIn13 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ([StateIdentifier]) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Bool) (Attributes)
data T_EProduction_vOut13  = T_EProduction_vOut13 ([VisitStateState]) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Int) (PP_Doc) (PP_Doc) (PP_Doc) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProduction_EProduction #-}
sem_EProduction_EProduction :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_ERules  -> T_EChildren  -> T_Visits  -> T_EProduction 
sem_EProduction_EProduction arg_con_ arg_params_ _ arg_rules_ arg_children_ arg_visits_ = T_EProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_EProduction_v13 
      v13 = \ (T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) -> ( let
         _rulesX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_rules_))
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_children_))
         _visitsX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_visits_))
         (T_ERules_vOut22 _rulesIerrors _rulesImrules _rulesIruledefs _rulesIruleuses _rulesIsem_rules) = inv_ERules_s23 _rulesX23 (T_ERules_vIn22 _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOnt _rulesOoptions _rulesOruleKinds _rulesOsynmap _rulesOusageInfo)
         (T_EChildren_vOut4 _childrenIargnamesw _childrenIchildTypes _childrenIchildintros _childrenIsigs _childrenIterminaldefs) = inv_EChildren_s5 _childrenX5 (T_EChildren_vIn4 _childrenOallInitStates _childrenOcon _childrenOmainFile _childrenOmainName _childrenOnt _childrenOoptions)
         (T_Visits_vOut55 _visitsIallvisits _visitsIchildvisit _visitsIerrors _visitsIfromToStates _visitsIintramap _visitsIlazyIntras _visitsIruleKinds _visitsIruleUsage _visitsIsem_visit _visitsIt_visits _visitsIvisitKinds _visitsIvisitdefs _visitsIvisituses) = inv_Visits_s56 _visitsX56 (T_Visits_vIn55 _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOchildintros _visitsOcon _visitsOinhmap _visitsOmrules _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs)
         _childrenOcon = rule181 arg_con_
         _rulesOcon = rule182 arg_con_
         _visitsOcon = rule183 arg_con_
         _o_records = rule184 _lhsIoptions
         _t_params = rule185 _lhsIparams
         _t_c_params = rule186 arg_params_
         _conname = rule187 _lhsInt _lhsIrename arg_con_
         _recname = rule188 _conname
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule189 _childrenIsigs _o_records _recname _t_params
         _lhsOdatatype_call :: PP_Doc
         _lhsOdatatype_call = rule190 _conname _recname _t_params
         _lhsOdatatype_con :: PP_Doc
         _lhsOdatatype_con = rule191 _childrenIsigs _conname _lhsInt _o_records _o_sigs _t_params arg_con_
         _lhsOcount :: Int
         _lhsOcount = rule192  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule193 _childrenIargnamesw _childrenIsigs _lhsInt _lhsIoptions _lhsIrename _o_records arg_con_
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule194 _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule195 _semFunBndTp
         _semFunBndDef = rule196 _semFunBndNm _semname
         _semFunBndTp = rule197 _semFunBndNm _sem_tp
         _semFunBndNm = rule198 _lhsInt arg_con_
         _o_sigs = rule199 _lhsIoptions
         _t_type = rule200 _lhsInt
         _semname = rule201 _lhsInt _lhsIoptions arg_con_
         _sem_res_tp = rule202 _t_params _t_type
         _sem_tp = rule203 _childrenIsigs _sem_res_tp
         _initializer = rule204  ()
         _sem_prod = rule205 _childrenIsigs _o_sigs _prod_body _sem_res_tp _semname
         _prod_body = rule206 _initializer _lhsIinitial _lhsInt _rulesIsem_rules _statefuns arg_con_
         _statefuns = rule207 _genstfn _lhsIallstates
         _genstfn = rule208 _lhsIinitial _lhsInextVisits _lhsInt _stargs _stks _stvs
         _stargs = rule209 _childTypes _lhsIallInhmap _lhsIallSynmap _lhsIoptions _localAttrTypes _visitsIintramap
         _stvisits = rule210 _visitsIallvisits
         _stks = rule211 _lhsInextVisits _lhsInt _stvisits _t_c_params arg_con_
         _stvs = rule212 _visitsIsem_visit
         _visitsOmrules = rule213 _rulesImrules
         _visitsOchildintros = rule214 _childrenIchildintros
         _rulesOusageInfo = rule215 _visitsIruleUsage
         _rulesOruleKinds = rule216 _visitsIruleKinds
         _visitsOallintramap = rule217 _visitsIintramap
         _visitsOterminaldefs = rule218 _childrenIterminaldefs
         _visitsOruledefs = rule219 _rulesIruledefs
         _visitsOruleuses = rule220 _rulesIruleuses
         _lazyIntras = rule221 _visitsIlazyIntras
         _childTypes = rule222 _childrenIchildTypes _lhsIntType
         _localAttrTypes = rule223 _lhsIlocalAttrTypes arg_con_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule224 _visitsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule225 _rulesIerrors _visitsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule226 _visitsIfromToStates
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule227 _visitsIt_visits
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule228 _visitsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule229 _visitsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule230 _visitsIvisituses
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule231 _visitsIallvisits
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule232 _sem_prod
         _rulesOallInhmap = rule233 _lhsIallInhmap
         _rulesOallSynmap = rule234 _lhsIallSynmap
         _rulesOchildTypes = rule235 _childTypes
         _rulesOinhmap = rule236 _lhsIinhmap
         _rulesOlazyIntras = rule237 _lazyIntras
         _rulesOlocalAttrTypes = rule238 _localAttrTypes
         _rulesOmainFile = rule239 _lhsImainFile
         _rulesOmainName = rule240 _lhsImainName
         _rulesOnt = rule241 _lhsInt
         _rulesOoptions = rule242 _lhsIoptions
         _rulesOsynmap = rule243 _lhsIsynmap
         _childrenOallInitStates = rule244 _lhsIallInitStates
         _childrenOmainFile = rule245 _lhsImainFile
         _childrenOmainName = rule246 _lhsImainName
         _childrenOnt = rule247 _lhsInt
         _childrenOoptions = rule248 _lhsIoptions
         _visitsOallFromToStates = rule249 _lhsIallFromToStates
         _visitsOallInhmap = rule250 _lhsIallInhmap
         _visitsOallInitStates = rule251 _lhsIallInitStates
         _visitsOallSynmap = rule252 _lhsIallSynmap
         _visitsOallVisitKinds = rule253 _lhsIallVisitKinds
         _visitsOallchildvisit = rule254 _lhsIallchildvisit
         _visitsOavisitdefs = rule255 _lhsIavisitdefs
         _visitsOavisituses = rule256 _lhsIavisituses
         _visitsOchildTypes = rule257 _childTypes
         _visitsOinhmap = rule258 _lhsIinhmap
         _visitsOnextVisits = rule259 _lhsInextVisits
         _visitsOnt = rule260 _lhsInt
         _visitsOoptions = rule261 _lhsIoptions
         _visitsOparams = rule262 _lhsIparams
         _visitsOprevVisits = rule263 _lhsIprevVisits
         _visitsOsynmap = rule264 _lhsIsynmap
         __result_ = T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProduction_s14 v13
   {-# INLINE rule181 #-}
   {-# LINE 90 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule181 = \ con_ ->
                                               {-# LINE 90 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                               con_
                                               {-# LINE 1904 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule182 #-}
   {-# LINE 91 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule182 = \ con_ ->
                   {-# LINE 91 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   con_
                   {-# LINE 1910 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule183 #-}
   {-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule183 = \ con_ ->
                   {-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   con_
                   {-# LINE 1916 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule184 #-}
   {-# LINE 259 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule184 = \ ((_lhsIoptions) :: Options) ->
                    {-# LINE 259 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    dataRecords _lhsIoptions
                    {-# LINE 1922 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule185 #-}
   {-# LINE 260 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule185 = \ ((_lhsIparams) :: [Identifier]) ->
                   {-# LINE 260 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   ppTypeParams _lhsIparams
                   {-# LINE 1928 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule186 #-}
   {-# LINE 261 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule186 = \ params_ ->
                     {-# LINE 261 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     ppTypeParams (cont_tvar : map pp params_)
                     {-# LINE 1934 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule187 #-}
   {-# LINE 262 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule187 = \ ((_lhsInt) :: NontermIdent) ((_lhsIrename) :: Bool) con_ ->
                  {-# LINE 262 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  conname _lhsIrename _lhsInt con_
                  {-# LINE 1940 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule188 #-}
   {-# LINE 263 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule188 = \ _conname ->
                  {-# LINE 263 "./src-ag/ExecutionPlan2Caml.ag" #-}
                  pp "fields_" >|< _conname
                  {-# LINE 1946 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule189 #-}
   {-# LINE 264 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule189 = \ ((_childrenIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) _o_records _recname _t_params ->
                   {-# LINE 264 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   "and" >#< _t_params     >#< _recname     >#< "="
                   >#< ppFieldsType _o_records     False _childrenIsigs
                   {-# LINE 1953 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule190 #-}
   {-# LINE 266 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule190 = \ _conname _recname _t_params ->
                        {-# LINE 266 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        pp "|" >#< _conname     >#< "of" >#< pp_parens (_t_params     >#< _recname    )
                        {-# LINE 1959 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule191 #-}
   {-# LINE 268 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule191 = \ ((_childrenIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) _conname ((_lhsInt) :: NontermIdent) _o_records _o_sigs _t_params con_ ->
        {-# LINE 268 "./src-ag/ExecutionPlan2Caml.ag" #-}
        let funNm  = _lhsInt >|< "_" >|< con_
            decl   = "and" >#< ppFunDecl _o_sigs     funNm params (_t_params     >#< _lhsInt) body
            params = [ (x, t) | (_,x,_,t) <- _childrenIsigs ]
            body   = _conname     >#< ppFieldsVal _o_records     _childrenIsigs
        in decl
        {-# LINE 1969 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule192 #-}
   {-# LINE 384 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule192 = \  (_ :: ()) ->
                                              {-# LINE 384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                              1
                                              {-# LINE 1975 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule193 #-}
   {-# LINE 389 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule193 = \ ((_childrenIargnamesw) :: [PP_Doc]) ((_childrenIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIrename) :: Bool) _o_records con_ ->
                 {-# LINE 389 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 "|" >#< conname _lhsIrename _lhsInt con_ >#< ppFieldsVal _o_records     _childrenIsigs >#< "->" >#<
                   prefix _lhsIoptions >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                 {-# LINE 1982 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule194 #-}
   {-# LINE 655 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule194 = \ _semFunBndDef ->
                        {-# LINE 655 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        Seq.singleton _semFunBndDef
                        {-# LINE 1988 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule195 #-}
   {-# LINE 656 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule195 = \ _semFunBndTp ->
                        {-# LINE 656 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        Seq.singleton _semFunBndTp
                        {-# LINE 1994 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule196 #-}
   {-# LINE 657 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule196 = \ _semFunBndNm _semname ->
                        {-# LINE 657 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 2000 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule197 #-}
   {-# LINE 658 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule197 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 658 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        _semFunBndNm     >#< ":" >#< _sem_tp
                        {-# LINE 2006 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule198 #-}
   {-# LINE 659 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule198 = \ ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 659 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        lateSemConLabel _lhsInt con_
                        {-# LINE 2012 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule199 #-}
   {-# LINE 686 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule199 = \ ((_lhsIoptions) :: Options) ->
                     {-# LINE 686 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     typeSigs _lhsIoptions
                     {-# LINE 2018 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule200 #-}
   {-# LINE 687 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule200 = \ ((_lhsInt) :: NontermIdent) ->
                     {-# LINE 687 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     type_nt_sem_top _lhsInt
                     {-# LINE 2024 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule201 #-}
   {-# LINE 688 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule201 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) con_ ->
                     {-# LINE 688 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     prefix _lhsIoptions >|< _lhsInt >|< "_" >|< con_
                     {-# LINE 2030 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule202 #-}
   {-# LINE 689 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule202 = \ _t_params _t_type ->
                     {-# LINE 689 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     _t_params     >#< _t_type
                     {-# LINE 2036 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule203 #-}
   {-# LINE 690 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule203 = \ ((_childrenIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) _sem_res_tp ->
                     {-# LINE 690 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     pp_block "" "" "->" [ d | (_,_,d,_) <- _childrenIsigs ] >#< "->" >#< _sem_res_tp
                     {-# LINE 2042 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule204 #-}
   {-# LINE 693 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule204 = \  (_ :: ()) ->
        {-# LINE 693 "./src-ag/ExecutionPlan2Caml.ag" #-}
        empty
        {-# LINE 2048 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule205 #-}
   {-# LINE 699 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule205 = \ ((_childrenIsigs) :: [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]) _o_sigs _prod_body _sem_res_tp _semname ->
                    {-# LINE 699 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    "and" >#< ppFunDecl _o_sigs     _semname     [ (x,d) | (_,x,d,_) <- _childrenIsigs ] _sem_res_tp     _prod_body
                    {-# LINE 2054 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule206 #-}
   {-# LINE 701 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule206 = \ _initializer ((_lhsIinitial) :: StateIdentifier) ((_lhsInt) :: NontermIdent) ((_rulesIsem_rules) :: PP_Doc) _statefuns con_ ->
        {-# LINE 701 "./src-ag/ExecutionPlan2Caml.ag" #-}
        _initializer
        >-< "{" >#< nm_attach _lhsInt >#< "=" >#< "function () ->"
        >-< indent 2
            (   "(* rules of production" >#< con_ >#< "*)"
            >-< _rulesIsem_rules
            >-< "(* states of production" >#< con_ >#< "*)"
            >-< vlist _statefuns
            >-< nm_st _lhsIinitial
            )
        >#< "}"
        {-# LINE 2069 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule207 #-}
   {-# LINE 717 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule207 = \ _genstfn ((_lhsIallstates) :: [StateIdentifier]) ->
                    {-# LINE 717 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    map _genstfn     _lhsIallstates
                    {-# LINE 2075 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule208 #-}
   {-# LINE 719 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule208 = \ ((_lhsIinitial) :: StateIdentifier) ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ((_lhsInt) :: NontermIdent) _stargs _stks _stvs ->
        {-# LINE 719 "./src-ag/ExecutionPlan2Caml.ag" #-}
        \st -> let nextVisitInfo = Map.findWithDefault ManyVis st _lhsInextVisits
                   stNm = nm_st st
                   stDef body = "let" >#< stNm >#< (if st == _lhsIinitial then empty else _stargs     st) >#< "="
                                >-< indent 2 body >#< "in"
               in case nextVisitInfo of
                    NoneVis ->
                               if st == _lhsIinitial
                               then stDef (pp "unit")
                               else empty
                    _       -> stDef $ mklets (_stvs     st ++ _stks     st) $ ppRecordVal
                                 [ nm_invoke _lhsInt st >#< "=" >#< nm_k st ]
        {-# LINE 2091 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule209 #-}
   {-# LINE 739 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule209 = \ _childTypes ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIoptions) :: Options) _localAttrTypes ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
        {-# LINE 739 "./src-ag/ExecutionPlan2Caml.ag" #-}
        \st -> let attrs = maybe Map.empty id $ Map.lookup st _visitsIintramap in ppSpaced
                 [ case mbAttr of
                     Just (AttrSyn child nm) | child == _LOC && not (noPerStateTypeSigs _lhsIoptions) ->
                       case Map.lookup nm _localAttrTypes     of
                         Just tp -> pp_parens (strNm >#< ":" >#< ppTp tp)
                         Nothing -> pp strNm
                     Just attr | not (noPerStateTypeSigs _lhsIoptions) ->
                       case lookupAttrType attr _lhsIallInhmap _lhsIallSynmap _childTypes     of
                         Just tpDoc -> pp_parens (strNm >#< ":" >#< tpDoc)
                         Nothing    -> pp strNm
                     _ -> pp strNm
                 | (strNm, mbAttr) <- Map.assocs attrs
                 ] >#< dummyPat _lhsIoptions (Map.null attrs)
        {-# LINE 2109 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule210 #-}
   {-# LINE 754 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule210 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
                   {-# LINE 754 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   \st -> filter (\(_,f,_) -> f == st) _visitsIallvisits
                   {-# LINE 2115 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule211 #-}
   {-# LINE 756 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule211 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ((_lhsInt) :: NontermIdent) _stvisits _t_c_params con_ ->
        {-# LINE 756 "./src-ag/ExecutionPlan2Caml.ag" #-}
        \st -> let stvisits = _stvisits     st
                   def = ppFunDecl False                   (pp $ nm_k st)
                           [(pp "arg", _t_c_params     >#< type_caller _lhsInt st)] (pp cont_tvar) body
                   nextVisitInfo = Map.findWithDefault ManyVis st _lhsInextVisits
                   body = case nextVisitInfo of
                     NoneVis  -> text "?no next visit?"
                     OneVis v -> dispatch "arg" v
                     ManyVis  -> let alt (v,_,_) = "|" >#< con_visit _lhsInt v >#< "chosen" >#< "->" >-< indent 2 (dispatch "chosen" v)
                                 in "match arg with" >-< (indent 2 $ vlist $ map alt stvisits)
                   dispatch nm v = "let" >#< ppRecordVal
                                     [ nm_inh _lhsInt v >#< "=" >#< "inp"
                                     , nm_cont _lhsInt v >#< "=" >#< "cont" ]
                                   >#< "=" >#< pp nm
                                   >-< "in" >#< "cont" >#< pp_parens (nm_visit v >#< "inp")
               in if null stvisits
                  then []
                  else [ "(* k-function for production" >#< con_ >#< " *)" >-< def ]
        {-# LINE 2137 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule212 #-}
   {-# LINE 777 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule212 = \ ((_visitsIsem_visit) ::  [(StateIdentifier,PP_Doc)] ) ->
               {-# LINE 777 "./src-ag/ExecutionPlan2Caml.ag" #-}
               \st -> [ppf | (f,ppf) <- _visitsIsem_visit, f == st]
               {-# LINE 2143 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule213 #-}
   {-# LINE 778 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule213 = \ ((_rulesImrules) :: Map Identifier (VisitKind -> Either Error PP_Doc)) ->
                    {-# LINE 778 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    _rulesImrules
                    {-# LINE 2149 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule214 #-}
   {-# LINE 919 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule214 = \ ((_childrenIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                         {-# LINE 919 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         _childrenIchildintros
                         {-# LINE 2155 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule215 #-}
   {-# LINE 1225 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule215 = \ ((_visitsIruleUsage) :: Map Identifier Int) ->
                                                   {-# LINE 1225 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                                   _visitsIruleUsage
                                                   {-# LINE 2161 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule216 #-}
   {-# LINE 1240 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule216 = \ ((_visitsIruleKinds) :: Map Identifier (Set VisitKind)) ->
                      {-# LINE 1240 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      _visitsIruleKinds
                      {-# LINE 2167 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule217 #-}
   {-# LINE 1269 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule217 = \ ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                          {-# LINE 1269 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          _visitsIintramap
                          {-# LINE 2173 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule218 #-}
   {-# LINE 1270 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule218 = \ ((_childrenIterminaldefs) :: Set String) ->
                          {-# LINE 1270 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          _childrenIterminaldefs
                          {-# LINE 2179 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule219 #-}
   {-# LINE 1294 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule219 = \ ((_rulesIruledefs) :: Map Identifier (Set String)) ->
                                    {-# LINE 1294 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _rulesIruledefs
                                    {-# LINE 2185 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule220 #-}
   {-# LINE 1295 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule220 = \ ((_rulesIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                    {-# LINE 1295 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _rulesIruleuses
                                    {-# LINE 2191 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule221 #-}
   {-# LINE 1349 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule221 = \ ((_visitsIlazyIntras) :: Set String) ->
                     {-# LINE 1349 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     _visitsIlazyIntras
                     {-# LINE 2197 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule222 #-}
   {-# LINE 1421 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule222 = \ ((_childrenIchildTypes) :: Map Identifier Type) ((_lhsIntType) :: Type) ->
                     {-# LINE 1421 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                     {-# LINE 2203 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule223 #-}
   {-# LINE 1438 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule223 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) con_ ->
                           {-# LINE 1438 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                           {-# LINE 2209 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule224 #-}
   rule224 = \ ((_visitsIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _visitsIchildvisit
   {-# INLINE rule225 #-}
   rule225 = \ ((_rulesIerrors) :: Seq Error) ((_visitsIerrors) :: Seq Error) ->
     _rulesIerrors Seq.>< _visitsIerrors
   {-# INLINE rule226 #-}
   rule226 = \ ((_visitsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _visitsIfromToStates
   {-# INLINE rule227 #-}
   rule227 = \ ((_visitsIt_visits) :: PP_Doc) ->
     _visitsIt_visits
   {-# INLINE rule228 #-}
   rule228 = \ ((_visitsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _visitsIvisitKinds
   {-# INLINE rule229 #-}
   rule229 = \ ((_visitsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisitdefs
   {-# INLINE rule230 #-}
   rule230 = \ ((_visitsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisituses
   {-# INLINE rule231 #-}
   rule231 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
     _visitsIallvisits
   {-# INLINE rule232 #-}
   rule232 = \ _sem_prod ->
     _sem_prod
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule235 #-}
   rule235 = \ _childTypes ->
     _childTypes
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule237 #-}
   rule237 = \ _lazyIntras ->
     _lazyIntras
   {-# INLINE rule238 #-}
   rule238 = \ _localAttrTypes ->
     _localAttrTypes
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule257 #-}
   rule257 = \ _childTypes ->
     _childTypes
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap

-- EProductions ------------------------------------------------
-- wrapper
data Inh_EProductions  = Inh_EProductions { allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProductions :: (Map NontermIdent Attributes), allInitStates_Inh_EProductions :: (Map NontermIdent Int), allSynmap_Inh_EProductions :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProductions :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), allstates_Inh_EProductions :: ([StateIdentifier]), avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), inhmap_Inh_EProductions :: (Attributes), initial_Inh_EProductions :: (StateIdentifier), localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProductions :: (String), mainName_Inh_EProductions :: (String), nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), nt_Inh_EProductions :: (NontermIdent), ntType_Inh_EProductions :: (Type), options_Inh_EProductions :: (Options), params_Inh_EProductions :: ([Identifier]), prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), rename_Inh_EProductions :: (Bool), synmap_Inh_EProductions :: (Attributes) }
data Syn_EProductions  = Syn_EProductions { allvisits_Syn_EProductions :: ([VisitStateState]), childvisit_Syn_EProductions :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), count_Syn_EProductions :: (Int), datatype_Syn_EProductions :: ([PP_Doc]), datatype_call_Syn_EProductions :: ([PP_Doc]), datatype_con_Syn_EProductions :: ([PP_Doc]), errors_Syn_EProductions :: (Seq Error), fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProductions :: (Seq PP_Doc), semFunBndTps_Syn_EProductions :: (Seq PP_Doc), sem_nt_Syn_EProductions :: (PP_Doc), sem_prod_Syn_EProductions :: (PP_Doc), t_visits_Syn_EProductions :: (PP_Doc), visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProductions #-}
wrap_EProductions :: T_EProductions  -> Inh_EProductions  -> (Syn_EProductions )
wrap_EProductions (T_EProductions act) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap
        (T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProductions_s17 sem arg)
        return (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_EProductions #-}
sem_EProductions :: EProductions  -> T_EProductions 
sem_EProductions list = Prelude.foldr sem_EProductions_Cons sem_EProductions_Nil (Prelude.map sem_EProduction list)

-- semantic domain
newtype T_EProductions  = T_EProductions {
                                         attach_T_EProductions :: Identity (T_EProductions_s17 )
                                         }
newtype T_EProductions_s17  = C_EProductions_s17 {
                                                 inv_EProductions_s17 :: (T_EProductions_v16 )
                                                 }
data T_EProductions_s18  = C_EProductions_s18
type T_EProductions_v16  = (T_EProductions_vIn16 ) -> (T_EProductions_vOut16 )
data T_EProductions_vIn16  = T_EProductions_vIn16 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ([StateIdentifier]) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Bool) (Attributes)
data T_EProductions_vOut16  = T_EProductions_vOut16 ([VisitStateState]) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Int) ([PP_Doc]) ([PP_Doc]) ([PP_Doc]) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProductions_Cons #-}
sem_EProductions_Cons :: T_EProduction  -> T_EProductions  -> T_EProductions 
sem_EProductions_Cons arg_hd_ arg_tl_ = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_EProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_tl_))
         (T_EProduction_vOut13 _hdIallvisits _hdIchildvisit _hdIcount _hdIdatatype _hdIdatatype_call _hdIdatatype_con _hdIerrors _hdIfromToStates _hdIsemFunBndDefs _hdIsemFunBndTps _hdIsem_nt _hdIsem_prod _hdIt_visits _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_EProduction_s14 _hdX14 (T_EProduction_vIn13 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOprevVisits _hdOrename _hdOsynmap)
         (T_EProductions_vOut16 _tlIallvisits _tlIchildvisit _tlIcount _tlIdatatype _tlIdatatype_call _tlIdatatype_con _tlIerrors _tlIfromToStates _tlIsemFunBndDefs _tlIsemFunBndTps _tlIsem_nt _tlIsem_prod _tlIt_visits _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_EProductions_s17 _tlX17 (T_EProductions_vIn16 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOprevVisits _tlOrename _tlOsynmap)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule265 _hdIallvisits
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule266 _hdIt_visits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule267 _hdIchildvisit _tlIchildvisit
         _lhsOcount :: Int
         _lhsOcount = rule268 _hdIcount _tlIcount
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule269 _hdIdatatype _tlIdatatype
         _lhsOdatatype_call :: [PP_Doc]
         _lhsOdatatype_call = rule270 _hdIdatatype_call _tlIdatatype_call
         _lhsOdatatype_con :: [PP_Doc]
         _lhsOdatatype_con = rule271 _hdIdatatype_con _tlIdatatype_con
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule272 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule273 _hdIfromToStates _tlIfromToStates
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule274 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule275 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule276 _hdIsem_nt _tlIsem_nt
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule277 _hdIsem_prod _tlIsem_prod
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule278 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule279 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule280 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule281 _lhsIallFromToStates
         _hdOallInhmap = rule282 _lhsIallInhmap
         _hdOallInitStates = rule283 _lhsIallInitStates
         _hdOallSynmap = rule284 _lhsIallSynmap
         _hdOallVisitKinds = rule285 _lhsIallVisitKinds
         _hdOallchildvisit = rule286 _lhsIallchildvisit
         _hdOallstates = rule287 _lhsIallstates
         _hdOavisitdefs = rule288 _lhsIavisitdefs
         _hdOavisituses = rule289 _lhsIavisituses
         _hdOinhmap = rule290 _lhsIinhmap
         _hdOinitial = rule291 _lhsIinitial
         _hdOlocalAttrTypes = rule292 _lhsIlocalAttrTypes
         _hdOmainFile = rule293 _lhsImainFile
         _hdOmainName = rule294 _lhsImainName
         _hdOnextVisits = rule295 _lhsInextVisits
         _hdOnt = rule296 _lhsInt
         _hdOntType = rule297 _lhsIntType
         _hdOoptions = rule298 _lhsIoptions
         _hdOparams = rule299 _lhsIparams
         _hdOprevVisits = rule300 _lhsIprevVisits
         _hdOrename = rule301 _lhsIrename
         _hdOsynmap = rule302 _lhsIsynmap
         _tlOallFromToStates = rule303 _lhsIallFromToStates
         _tlOallInhmap = rule304 _lhsIallInhmap
         _tlOallInitStates = rule305 _lhsIallInitStates
         _tlOallSynmap = rule306 _lhsIallSynmap
         _tlOallVisitKinds = rule307 _lhsIallVisitKinds
         _tlOallchildvisit = rule308 _lhsIallchildvisit
         _tlOallstates = rule309 _lhsIallstates
         _tlOavisitdefs = rule310 _lhsIavisitdefs
         _tlOavisituses = rule311 _lhsIavisituses
         _tlOinhmap = rule312 _lhsIinhmap
         _tlOinitial = rule313 _lhsIinitial
         _tlOlocalAttrTypes = rule314 _lhsIlocalAttrTypes
         _tlOmainFile = rule315 _lhsImainFile
         _tlOmainName = rule316 _lhsImainName
         _tlOnextVisits = rule317 _lhsInextVisits
         _tlOnt = rule318 _lhsInt
         _tlOntType = rule319 _lhsIntType
         _tlOoptions = rule320 _lhsIoptions
         _tlOparams = rule321 _lhsIparams
         _tlOprevVisits = rule322 _lhsIprevVisits
         _tlOrename = rule323 _lhsIrename
         _tlOsynmap = rule324 _lhsIsynmap
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule265 #-}
   {-# LINE 439 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule265 = \ ((_hdIallvisits) :: [VisitStateState]) ->
                           {-# LINE 439 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           _hdIallvisits
                           {-# LINE 2459 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule266 #-}
   {-# LINE 534 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule266 = \ ((_hdIt_visits) :: PP_Doc) ->
                   {-# LINE 534 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   _hdIt_visits
                   {-# LINE 2465 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule267 #-}
   rule267 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule268 #-}
   rule268 = \ ((_hdIcount) :: Int) ((_tlIcount) :: Int) ->
     _hdIcount + _tlIcount
   {-# INLINE rule269 #-}
   rule269 = \ ((_hdIdatatype) :: PP_Doc) ((_tlIdatatype) :: [PP_Doc]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule270 #-}
   rule270 = \ ((_hdIdatatype_call) :: PP_Doc) ((_tlIdatatype_call) :: [PP_Doc]) ->
     _hdIdatatype_call : _tlIdatatype_call
   {-# INLINE rule271 #-}
   rule271 = \ ((_hdIdatatype_con) :: PP_Doc) ((_tlIdatatype_con) :: [PP_Doc]) ->
     _hdIdatatype_con : _tlIdatatype_con
   {-# INLINE rule272 #-}
   rule272 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule273 #-}
   rule273 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule274 #-}
   rule274 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule275 #-}
   rule275 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule276 #-}
   rule276 = \ ((_hdIsem_nt) :: PP_Doc) ((_tlIsem_nt) :: PP_Doc) ->
     _hdIsem_nt >-< _tlIsem_nt
   {-# INLINE rule277 #-}
   rule277 = \ ((_hdIsem_prod) :: PP_Doc) ((_tlIsem_prod) :: PP_Doc) ->
     _hdIsem_prod >-< _tlIsem_prod
   {-# INLINE rule278 #-}
   rule278 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule279 #-}
   rule279 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule280 #-}
   rule280 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIallstates) :: [StateIdentifier]) ->
     _lhsIallstates
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule309 #-}
   rule309 = \ ((_lhsIallstates) :: [StateIdentifier]) ->
     _lhsIallstates
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule317 #-}
   rule317 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule318 #-}
   rule318 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_EProductions_Nil #-}
sem_EProductions_Nil ::  T_EProductions 
sem_EProductions_Nil  = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule325  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule326  ()
         _lhsOcount :: Int
         _lhsOcount = rule327  ()
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule328  ()
         _lhsOdatatype_call :: [PP_Doc]
         _lhsOdatatype_call = rule329  ()
         _lhsOdatatype_con :: [PP_Doc]
         _lhsOdatatype_con = rule330  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule331  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule332  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule333  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule334  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule335  ()
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule336  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule337  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule338  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule339  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule340  ()
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule325 #-}
   {-# LINE 440 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule325 = \  (_ :: ()) ->
                           {-# LINE 440 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           error "Every nonterminal should have at least 1 production"
                           {-# LINE 2687 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule326 #-}
   rule326 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule327 #-}
   rule327 = \  (_ :: ()) ->
     0
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
     Seq.empty
   {-# INLINE rule332 #-}
   rule332 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule333 #-}
   rule333 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule334 #-}
   rule334 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule335 #-}
   rule335 = \  (_ :: ()) ->
     empty
   {-# INLINE rule336 #-}
   rule336 = \  (_ :: ()) ->
     empty
   {-# INLINE rule337 #-}
   rule337 = \  (_ :: ()) ->
     empty
   {-# INLINE rule338 #-}
   rule338 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule339 #-}
   rule339 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule340 #-}
   rule340 = \  (_ :: ()) ->
     Map.empty

-- ERule -------------------------------------------------------
-- wrapper
data Inh_ERule  = Inh_ERule { allInhmap_Inh_ERule :: (Map NontermIdent Attributes), allSynmap_Inh_ERule :: (Map NontermIdent Attributes), childTypes_Inh_ERule :: (Map Identifier Type), con_Inh_ERule :: (ConstructorIdent), inhmap_Inh_ERule :: (Attributes), lazyIntras_Inh_ERule :: (Set String), localAttrTypes_Inh_ERule :: (Map Identifier Type), mainFile_Inh_ERule :: (String), mainName_Inh_ERule :: (String), nt_Inh_ERule :: (NontermIdent), options_Inh_ERule :: (Options), ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)), synmap_Inh_ERule :: (Attributes), usageInfo_Inh_ERule :: (Map Identifier Int) }
data Syn_ERule  = Syn_ERule { errors_Syn_ERule :: (Seq Error), mrules_Syn_ERule :: (Map Identifier (VisitKind -> Either Error PP_Doc)), ruledefs_Syn_ERule :: (Map Identifier (Set String)), ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERule :: (PP_Doc) }
{-# INLINABLE wrap_ERule #-}
wrap_ERule :: T_ERule  -> Inh_ERule  -> (Syn_ERule )
wrap_ERule (T_ERule act) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo
        (T_ERule_vOut19 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules) <- return (inv_ERule_s20 sem arg)
        return (Syn_ERule _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules)
   )

-- cata
{-# INLINE sem_ERule #-}
sem_ERule :: ERule  -> T_ERule 
sem_ERule ( ERule name_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ mbError_ ) = sem_ERule_ERule name_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ mbError_

-- semantic domain
newtype T_ERule  = T_ERule {
                           attach_T_ERule :: Identity (T_ERule_s20 )
                           }
newtype T_ERule_s20  = C_ERule_s20 {
                                   inv_ERule_s20 :: (T_ERule_v19 )
                                   }
data T_ERule_s21  = C_ERule_s21
type T_ERule_v19  = (T_ERule_vIn19 ) -> (T_ERule_vOut19 )
data T_ERule_vIn19  = T_ERule_vIn19 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (Attributes) (Set String) (Map Identifier Type) (String) (String) (NontermIdent) (Options) (Map Identifier (Set VisitKind)) (Attributes) (Map Identifier Int)
data T_ERule_vOut19  = T_ERule_vOut19 (Seq Error) (Map Identifier (VisitKind -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc)
{-# NOINLINE sem_ERule_ERule #-}
sem_ERule_ERule :: (Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Maybe Error) -> T_ERule 
sem_ERule_ERule arg_name_ arg_pattern_ arg_rhs_ _ _ arg_explicit_ arg_pure_ arg_mbError_ = T_ERule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_ERule_v19 
      v19 = \ (T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX29 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut40 _patternIattrTypes _patternIattrs _patternIcopy _patternIextraDefs _patternIisUnderscore _patternIsem_lhs) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap)
         (T_Expression_vOut28 _rhsIattrs _rhsIpos _rhsIsemfunc _rhsItks) = inv_Expression_s29 _rhsX29 (T_Expression_vIn28 _rhsOoptions)
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule341 _rulecode _used
         _rulecode = rule342 _declHead _endpragma _genpragma _pragma _rhsIpos _rhsIsemfunc
         _pragma = rule343 _lhsIoptions _rhsIpos
         _endpragma = rule344 _lhsImainFile _lhsIoptions
         _genpragma = rule345 _haspos _lhsIoptions arg_explicit_
         _haspos = rule346 _rhsIpos
         _declHead = rule347 _argPats _lhsIoptions _rhsIattrs arg_name_
         _argPats = rule348 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIlocalAttrTypes _lhsIoptions _rhsIattrs
         _argExprs = rule349 _rhsIattrs
         _stepcode = rule350 _argExprs _lhsIoptions _patternIextraDefs _patternIsem_lhs _rhsIattrs arg_name_ arg_pure_
         _lhsOmrules :: Map Identifier (VisitKind -> Either Error PP_Doc)
         _lhsOmrules = rule351 _stepcode arg_name_
         _used = rule352 _lhsIusageInfo arg_name_
         _kinds = rule353 _lhsIruleKinds arg_name_
         _anyLazyKind = rule354 _kinds
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule355 _patternIattrs arg_name_
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule356 _rhsIattrs arg_name_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule357 _used arg_mbError_
         _patternOallInhmap = rule358 _lhsIallInhmap
         _patternOallSynmap = rule359 _lhsIallSynmap
         _patternOanyLazyKind = rule360 _anyLazyKind
         _patternOinhmap = rule361 _lhsIinhmap
         _patternOlocalAttrTypes = rule362 _lhsIlocalAttrTypes
         _patternOoptions = rule363 _lhsIoptions
         _patternOsynmap = rule364 _lhsIsynmap
         _rhsOoptions = rule365 _lhsIoptions
         __result_ = T_ERule_vOut19 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules
         in __result_ )
     in C_ERule_s20 v19
   {-# INLINE rule341 #-}
   {-# LINE 977 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule341 = \ _rulecode _used ->
                          {-# LINE 977 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          if _used     == 0
                          then empty
                          else _rulecode
                          {-# LINE 2815 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 980 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule342 = \ _declHead _endpragma _genpragma _pragma ((_rhsIpos) :: Pos) ((_rhsIsemfunc) :: PP_Doc) ->
                          {-# LINE 980 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          ( if _genpragma
                            then _pragma
                            else empty
                          )
                          >-< _declHead
                          >-< indent ((column _rhsIpos - 2) `max` 2)
                                ( if _genpragma
                                  then _pragma     >-< _rhsIsemfunc >-< _endpragma
                                  else _rhsIsemfunc
                                )
                          >#< "in"
                          {-# LINE 2831 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule343 #-}
   {-# LINE 993 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule343 = \ ((_lhsIoptions) :: Options) ((_rhsIpos) :: Pos) ->
                           {-# LINE 993 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           ppLinePragma _lhsIoptions (line _rhsIpos) (file _rhsIpos)
                           {-# LINE 2837 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule344 #-}
   {-# LINE 994 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule344 = \ ((_lhsImainFile) :: String) ((_lhsIoptions) :: Options) ->
                           {-# LINE 994 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           ppWithLineNr (\ln -> ppLinePragma _lhsIoptions (ln+1) _lhsImainFile)
                           {-# LINE 2843 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule345 #-}
   {-# LINE 995 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule345 = \ _haspos ((_lhsIoptions) :: Options) explicit_ ->
                           {-# LINE 995 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           genLinePragmas _lhsIoptions && explicit_ && _haspos
                           {-# LINE 2849 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule346 #-}
   {-# LINE 996 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule346 = \ ((_rhsIpos) :: Pos) ->
                           {-# LINE 996 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           line _rhsIpos > 0 && column _rhsIpos >= 0 && not (null (file _rhsIpos))
                           {-# LINE 2855 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule347 #-}
   {-# LINE 1000 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule347 = \ _argPats ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                       {-# LINE 1000 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       "let" >#< name_ >#< _argPats     >#< dummyPat _lhsIoptions (Map.null _rhsIattrs) >#< "="
                       {-# LINE 2861 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule348 #-}
   {-# LINE 1002 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule348 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIchildTypes) :: Map Identifier Type) ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
            {-# LINE 1002 "./src-ag/ExecutionPlan2Caml.ag" #-}
            ppSpaced
              [ case mbAttr of
                  Just (AttrSyn child nm) | child == _LOC && not (noPerStateTypeSigs _lhsIoptions) ->
                    case Map.lookup nm _lhsIlocalAttrTypes of
                      Just tp -> pp_parens (strNm >#< ":" >#< ppTp tp)
                      Nothing -> pp strNm
                  Just attr | not (noPerStateTypeSigs _lhsIoptions) ->
                    case lookupAttrType attr _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes of
                      Just tpDoc -> pp_parens (strNm >#< ":" >#< tpDoc)
                      Nothing    -> pp strNm
                  _ -> pp strNm
              | (strNm, mbAttr) <- Map.assocs _rhsIattrs
              ]
            {-# LINE 2879 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule349 #-}
   {-# LINE 1016 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule349 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                       {-# LINE 1016 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       ppSpaced $ Map.keys _rhsIattrs
                       {-# LINE 2885 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 1017 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule350 = \ _argExprs ((_lhsIoptions) :: Options) ((_patternIextraDefs) :: [(PP_Doc,PP_Doc)]) ((_patternIsem_lhs) ::  PP_Doc ) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ pure_ ->
                       {-# LINE 1017 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       \kind ->
                         let mkBind (pat,expr) = "let" >#< pat >#< "=" >#< expr >#< "in"
                         in if kind `compatibleRule` pure_
                            then Right $ mkBind (_patternIsem_lhs, name_ >#< _argExprs     >#< dummyArg _lhsIoptions (Map.null _rhsIattrs))
                                         >-< vlist (map mkBind _patternIextraDefs)
                            else Left $ IncompatibleRuleKind name_ kind
                       {-# LINE 2896 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 1024 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule351 = \ _stepcode name_ ->
                       {-# LINE 1024 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       Map.singleton name_ _stepcode
                       {-# LINE 2902 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule352 #-}
   {-# LINE 1227 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule352 = \ ((_lhsIusageInfo) :: Map Identifier Int) name_ ->
                                                 {-# LINE 1227 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                                 Map.findWithDefault 0 name_ _lhsIusageInfo
                                                 {-# LINE 2908 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule353 #-}
   {-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule353 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) name_ ->
                {-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                Map.findWithDefault Set.empty name_ _lhsIruleKinds
                {-# LINE 2914 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule354 #-}
   {-# LINE 1244 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule354 = \ _kinds ->
                      {-# LINE 1244 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      Set.fold (\k r -> isLazyKind k || r) False _kinds
                      {-# LINE 2920 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule355 #-}
   {-# LINE 1290 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule355 = \ ((_patternIattrs) :: Set String) name_ ->
                           {-# LINE 1290 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           Map.singleton name_ _patternIattrs
                           {-# LINE 2926 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule356 #-}
   {-# LINE 1291 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule356 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                           {-# LINE 1291 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           Map.singleton name_ _rhsIattrs
                           {-# LINE 2932 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule357 #-}
   {-# LINE 1485 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule357 = \ _used mbError_ ->
                 {-# LINE 1485 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 case mbError_ of
                   Just e | _used     > 0 -> Seq.singleton e
                   _                      -> Seq.empty
                 {-# LINE 2940 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule360 #-}
   rule360 = \ _anyLazyKind ->
     _anyLazyKind
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- ERules ------------------------------------------------------
-- wrapper
data Inh_ERules  = Inh_ERules { allInhmap_Inh_ERules :: (Map NontermIdent Attributes), allSynmap_Inh_ERules :: (Map NontermIdent Attributes), childTypes_Inh_ERules :: (Map Identifier Type), con_Inh_ERules :: (ConstructorIdent), inhmap_Inh_ERules :: (Attributes), lazyIntras_Inh_ERules :: (Set String), localAttrTypes_Inh_ERules :: (Map Identifier Type), mainFile_Inh_ERules :: (String), mainName_Inh_ERules :: (String), nt_Inh_ERules :: (NontermIdent), options_Inh_ERules :: (Options), ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)), synmap_Inh_ERules :: (Attributes), usageInfo_Inh_ERules :: (Map Identifier Int) }
data Syn_ERules  = Syn_ERules { errors_Syn_ERules :: (Seq Error), mrules_Syn_ERules :: (Map Identifier (VisitKind -> Either Error PP_Doc)), ruledefs_Syn_ERules :: (Map Identifier (Set String)), ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERules :: (PP_Doc) }
{-# INLINABLE wrap_ERules #-}
wrap_ERules :: T_ERules  -> Inh_ERules  -> (Syn_ERules )
wrap_ERules (T_ERules act) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo
        (T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules) <- return (inv_ERules_s23 sem arg)
        return (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules)
   )

-- cata
{-# NOINLINE sem_ERules #-}
sem_ERules :: ERules  -> T_ERules 
sem_ERules list = Prelude.foldr sem_ERules_Cons sem_ERules_Nil (Prelude.map sem_ERule list)

-- semantic domain
newtype T_ERules  = T_ERules {
                             attach_T_ERules :: Identity (T_ERules_s23 )
                             }
newtype T_ERules_s23  = C_ERules_s23 {
                                     inv_ERules_s23 :: (T_ERules_v22 )
                                     }
data T_ERules_s24  = C_ERules_s24
type T_ERules_v22  = (T_ERules_vIn22 ) -> (T_ERules_vOut22 )
data T_ERules_vIn22  = T_ERules_vIn22 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (Attributes) (Set String) (Map Identifier Type) (String) (String) (NontermIdent) (Options) (Map Identifier (Set VisitKind)) (Attributes) (Map Identifier Int)
data T_ERules_vOut22  = T_ERules_vOut22 (Seq Error) (Map Identifier (VisitKind -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc)
{-# NOINLINE sem_ERules_Cons #-}
sem_ERules_Cons :: T_ERule  -> T_ERules  -> T_ERules 
sem_ERules_Cons arg_hd_ arg_tl_ = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_ERule (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_tl_))
         (T_ERule_vOut19 _hdIerrors _hdImrules _hdIruledefs _hdIruleuses _hdIsem_rules) = inv_ERule_s20 _hdX20 (T_ERule_vIn19 _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnt _hdOoptions _hdOruleKinds _hdOsynmap _hdOusageInfo)
         (T_ERules_vOut22 _tlIerrors _tlImrules _tlIruledefs _tlIruleuses _tlIsem_rules) = inv_ERules_s23 _tlX23 (T_ERules_vIn22 _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnt _tlOoptions _tlOruleKinds _tlOsynmap _tlOusageInfo)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule366 _hdIerrors _tlIerrors
         _lhsOmrules :: Map Identifier (VisitKind -> Either Error PP_Doc)
         _lhsOmrules = rule367 _hdImrules _tlImrules
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule368 _hdIruledefs _tlIruledefs
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule369 _hdIruleuses _tlIruleuses
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule370 _hdIsem_rules _tlIsem_rules
         _hdOallInhmap = rule371 _lhsIallInhmap
         _hdOallSynmap = rule372 _lhsIallSynmap
         _hdOchildTypes = rule373 _lhsIchildTypes
         _hdOcon = rule374 _lhsIcon
         _hdOinhmap = rule375 _lhsIinhmap
         _hdOlazyIntras = rule376 _lhsIlazyIntras
         _hdOlocalAttrTypes = rule377 _lhsIlocalAttrTypes
         _hdOmainFile = rule378 _lhsImainFile
         _hdOmainName = rule379 _lhsImainName
         _hdOnt = rule380 _lhsInt
         _hdOoptions = rule381 _lhsIoptions
         _hdOruleKinds = rule382 _lhsIruleKinds
         _hdOsynmap = rule383 _lhsIsynmap
         _hdOusageInfo = rule384 _lhsIusageInfo
         _tlOallInhmap = rule385 _lhsIallInhmap
         _tlOallSynmap = rule386 _lhsIallSynmap
         _tlOchildTypes = rule387 _lhsIchildTypes
         _tlOcon = rule388 _lhsIcon
         _tlOinhmap = rule389 _lhsIinhmap
         _tlOlazyIntras = rule390 _lhsIlazyIntras
         _tlOlocalAttrTypes = rule391 _lhsIlocalAttrTypes
         _tlOmainFile = rule392 _lhsImainFile
         _tlOmainName = rule393 _lhsImainName
         _tlOnt = rule394 _lhsInt
         _tlOoptions = rule395 _lhsIoptions
         _tlOruleKinds = rule396 _lhsIruleKinds
         _tlOsynmap = rule397 _lhsIsynmap
         _tlOusageInfo = rule398 _lhsIusageInfo
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule366 #-}
   rule366 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule367 #-}
   rule367 = \ ((_hdImrules) :: Map Identifier (VisitKind -> Either Error PP_Doc)) ((_tlImrules) :: Map Identifier (VisitKind -> Either Error PP_Doc)) ->
     _hdImrules `Map.union` _tlImrules
   {-# INLINE rule368 #-}
   rule368 = \ ((_hdIruledefs) :: Map Identifier (Set String)) ((_tlIruledefs) :: Map Identifier (Set String)) ->
     _hdIruledefs `uwSetUnion` _tlIruledefs
   {-# INLINE rule369 #-}
   rule369 = \ ((_hdIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ((_tlIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _hdIruleuses `uwMapUnion` _tlIruleuses
   {-# INLINE rule370 #-}
   rule370 = \ ((_hdIsem_rules) :: PP_Doc) ((_tlIsem_rules) :: PP_Doc) ->
     _hdIsem_rules >-< _tlIsem_rules
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
{-# NOINLINE sem_ERules_Nil #-}
sem_ERules_Nil ::  T_ERules 
sem_ERules_Nil  = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule399  ()
         _lhsOmrules :: Map Identifier (VisitKind -> Either Error PP_Doc)
         _lhsOmrules = rule400  ()
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule401  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule402  ()
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule403  ()
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule399 #-}
   rule399 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule400 #-}
   rule400 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule401 #-}
   rule401 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule402 #-}
   rule402 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule403 #-}
   rule403 = \  (_ :: ()) ->
     empty

-- ExecutionPlan -----------------------------------------------
-- wrapper
data Inh_ExecutionPlan  = Inh_ExecutionPlan { inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ExecutionPlan :: (String), mainName_Inh_ExecutionPlan :: (String), options_Inh_ExecutionPlan :: (Options), synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes) }
data Syn_ExecutionPlan  = Syn_ExecutionPlan { code_Syn_ExecutionPlan :: (PP_Doc), datas_Syn_ExecutionPlan :: (PP_Doc), errors_Syn_ExecutionPlan :: (Seq Error), modules_Syn_ExecutionPlan :: (PP_Doc) }
{-# INLINABLE wrap_ExecutionPlan #-}
wrap_ExecutionPlan :: T_ExecutionPlan  -> Inh_ExecutionPlan  -> (Syn_ExecutionPlan )
wrap_ExecutionPlan (T_ExecutionPlan act) (Inh_ExecutionPlan _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ExecutionPlan_vIn25 _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap
        (T_ExecutionPlan_vOut25 _lhsOcode _lhsOdatas _lhsOerrors _lhsOmodules) <- return (inv_ExecutionPlan_s26 sem arg)
        return (Syn_ExecutionPlan _lhsOcode _lhsOdatas _lhsOerrors _lhsOmodules)
   )

-- cata
{-# INLINE sem_ExecutionPlan #-}
sem_ExecutionPlan :: ExecutionPlan  -> T_ExecutionPlan 
sem_ExecutionPlan ( ExecutionPlan nonts_ typeSyns_ wrappers_ derivings_ ) = sem_ExecutionPlan_ExecutionPlan ( sem_ENonterminals nonts_ ) typeSyns_ wrappers_ derivings_

-- semantic domain
newtype T_ExecutionPlan  = T_ExecutionPlan {
                                           attach_T_ExecutionPlan :: Identity (T_ExecutionPlan_s26 )
                                           }
newtype T_ExecutionPlan_s26  = C_ExecutionPlan_s26 {
                                                   inv_ExecutionPlan_s26 :: (T_ExecutionPlan_v25 )
                                                   }
data T_ExecutionPlan_s27  = C_ExecutionPlan_s27
type T_ExecutionPlan_v25  = (T_ExecutionPlan_vIn25 ) -> (T_ExecutionPlan_vOut25 )
data T_ExecutionPlan_vIn25  = T_ExecutionPlan_vIn25 (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (Options) (Map NontermIdent Attributes)
data T_ExecutionPlan_vOut25  = T_ExecutionPlan_vOut25 (PP_Doc) (PP_Doc) (Seq Error) (PP_Doc)
{-# NOINLINE sem_ExecutionPlan_ExecutionPlan #-}
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals  -> (TypeSyns) -> (Set NontermIdent) -> (Derivings) -> T_ExecutionPlan 
sem_ExecutionPlan_ExecutionPlan arg_nonts_ arg_typeSyns_ arg_wrappers_ _ = T_ExecutionPlan (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_ExecutionPlan_v25 
      v25 = \ (T_ExecutionPlan_vIn25 _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_nonts_))
         (T_ENonterminals_vOut10 _nontsIchildvisit _nontsIcode _nontsIdatas _nontsIerrors _nontsIfromToStates _nontsIinitStates _nontsImodules _nontsIsemFunBndDefs _nontsIsemFunBndTps _nontsIvisitKinds _nontsIvisitdefs _nontsIvisituses) = inv_ENonterminals_s11 _nontsX11 (T_ENonterminals_vIn10 _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOoptions _nontsOsynmap _nontsOtypeSyns _nontsOwrappers)
         _lhsOcode :: PP_Doc
         _lhsOcode = rule404 _nontsIcode _wrappersExtra
         _lhsOdatas :: PP_Doc
         _lhsOdatas = rule405 _commonExtra _nontsIdatas
         _nontsOwrappers = rule406 arg_wrappers_
         _nontsOtypeSyns = rule407 arg_typeSyns_
         _wrappersExtra = rule408 _lateSemBndDef _lhsIoptions
         _commonExtra = rule409 _lateSemBndTp _lhsIoptions
         _lateSemBndTp = rule410 _lhsImainName _nontsIsemFunBndTps
         _lateSemBndDef = rule411 _lhsImainName _nontsIsemFunBndDefs
         _nontsOallchildvisit = rule412 _nontsIchildvisit
         _nontsOavisitdefs = rule413 _nontsIvisitdefs
         _nontsOavisituses = rule414 _nontsIvisituses
         _nontsOallFromToStates = rule415 _nontsIfromToStates
         _nontsOallVisitKinds = rule416 _nontsIvisitKinds
         _nontsOallInitStates = rule417 _nontsIinitStates
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule418 _nontsIerrors
         _lhsOmodules :: PP_Doc
         _lhsOmodules = rule419 _nontsImodules
         _nontsOinhmap = rule420 _lhsIinhmap
         _nontsOlocalAttrTypes = rule421 _lhsIlocalAttrTypes
         _nontsOmainFile = rule422 _lhsImainFile
         _nontsOmainName = rule423 _lhsImainName
         _nontsOoptions = rule424 _lhsIoptions
         _nontsOsynmap = rule425 _lhsIsynmap
         __result_ = T_ExecutionPlan_vOut25 _lhsOcode _lhsOdatas _lhsOerrors _lhsOmodules
         in __result_ )
     in C_ExecutionPlan_s26 v25
   {-# INLINE rule404 #-}
   {-# LINE 105 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule404 = \ ((_nontsIcode) :: PP_Doc) _wrappersExtra ->
                 {-# LINE 105 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 _nontsIcode  >-< _wrappersExtra
                 {-# LINE 3256 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule405 #-}
   {-# LINE 106 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule405 = \ _commonExtra ((_nontsIdatas) :: PP_Doc) ->
                 {-# LINE 106 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 _nontsIdatas >-< _commonExtra
                 {-# LINE 3262 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule406 #-}
   {-# LINE 112 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule406 = \ wrappers_ ->
                     {-# LINE 112 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     wrappers_
                     {-# LINE 3268 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule407 #-}
   {-# LINE 173 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule407 = \ typeSyns_ ->
                     {-# LINE 173 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     typeSyns_
                     {-# LINE 3274 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule408 #-}
   {-# LINE 663 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule408 = \ _lateSemBndDef ((_lhsIoptions) :: Options) ->
                        {-# LINE 663 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndDef
                        else empty
                        {-# LINE 3282 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule409 #-}
   {-# LINE 666 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule409 = \ _lateSemBndTp ((_lhsIoptions) :: Options) ->
                        {-# LINE 666 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndTp
                        else empty
                        {-# LINE 3290 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule410 #-}
   {-# LINE 669 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule410 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndTps) :: Seq PP_Doc) ->
                       {-# LINE 669 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       "and" >#< lateBindingTypeNm _lhsImainName >#< "=" >#< ppRecordTp (toList _nontsIsemFunBndTps)
                       {-# LINE 3296 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule411 #-}
   {-# LINE 670 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule411 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndDefs) :: Seq PP_Doc) ->
                        {-# LINE 670 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        "and" >#< lateBindingFieldNm _lhsImainName >#< ":" >#< lateBindingTypeNm _lhsImainName >#< "="
                        >-< (indent 2 $ ppRecordVal $ toList _nontsIsemFunBndDefs)
                        {-# LINE 3303 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule412 #-}
   {-# LINE 1157 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule412 = \ ((_nontsIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                          {-# LINE 1157 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                          _nontsIchildvisit
                                          {-# LINE 3309 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule413 #-}
   {-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule413 = \ ((_nontsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                       _nontsIvisitdefs
                                       {-# LINE 3315 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule414 #-}
   {-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule414 = \ ((_nontsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                       _nontsIvisituses
                                       {-# LINE 3321 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule415 #-}
   {-# LINE 1407 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule415 = \ ((_nontsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
                            {-# LINE 1407 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _nontsIfromToStates
                            {-# LINE 3327 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule416 #-}
   {-# LINE 1451 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule416 = \ ((_nontsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
                          {-# LINE 1451 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          _nontsIvisitKinds
                          {-# LINE 3333 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule417 #-}
   {-# LINE 1465 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule417 = \ ((_nontsIinitStates) :: Map NontermIdent Int) ->
                          {-# LINE 1465 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          _nontsIinitStates
                          {-# LINE 3339 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule418 #-}
   rule418 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule419 #-}
   rule419 = \ ((_nontsImodules) :: PP_Doc) ->
     _nontsImodules
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { options_Inh_Expression :: (Options) }
data Syn_Expression  = Syn_Expression { attrs_Syn_Expression :: (Map String (Maybe NonLocalAttr)), pos_Syn_Expression :: (Pos), semfunc_Syn_Expression :: (PP_Doc), tks_Syn_Expression :: ([HsToken]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn28 _lhsIoptions
        (T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks) <- return (inv_Expression_s29 sem arg)
        return (Syn_Expression _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s29 )
                                     }
newtype T_Expression_s29  = C_Expression_s29 {
                                             inv_Expression_s29 :: (T_Expression_v28 )
                                             }
data T_Expression_s30  = C_Expression_s30
type T_Expression_v28  = (T_Expression_vIn28 ) -> (T_Expression_vOut28 )
data T_Expression_vIn28  = T_Expression_vIn28 (Options)
data T_Expression_vOut28  = T_Expression_vOut28 (Map String (Maybe NonLocalAttr)) (Pos) (PP_Doc) ([HsToken])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Expression_v28 
      v28 = \ (T_Expression_vIn28 _lhsIoptions) -> ( let
         _lhsOtks :: [HsToken]
         _lhsOtks = rule426 arg_tks_
         _lhsOpos :: Pos
         _lhsOpos = rule427 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule428 _inhhstoken arg_tks_
         _lhsOsemfunc :: PP_Doc
         _lhsOsemfunc = rule429 _inhhstoken arg_tks_
         _inhhstoken = rule430 _lhsIoptions
         __result_ = T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks
         in __result_ )
     in C_Expression_s29 v28
   {-# INLINE rule426 #-}
   {-# LINE 1028 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule426 = \ tks_ ->
                           {-# LINE 1028 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           tks_
                           {-# LINE 3419 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule427 #-}
   {-# LINE 1049 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule427 = \ pos_ ->
                                        {-# LINE 1049 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                        pos_
                                        {-# LINE 3425 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule428 #-}
   {-# LINE 1141 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule428 = \ _inhhstoken tks_ ->
                               {-# LINE 1141 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               Map.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) _inhhstoken    )) tks_
                               {-# LINE 3431 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule429 #-}
   {-# LINE 1142 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule429 = \ _inhhstoken tks_ ->
                               {-# LINE 1142 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) _inhhstoken    )) tks_
                               {-# LINE 3437 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule430 #-}
   {-# LINE 1143 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule430 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 1143 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  Inh_HsToken _lhsIoptions
                                  {-# LINE 3443 "dist/build/ExecutionPlan2Caml.hs"#-}

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken { options_Inh_HsToken :: (Options) }
data Syn_HsToken  = Syn_HsToken { attrs_Syn_HsToken :: (Map String (Maybe NonLocalAttr)), tok_Syn_HsToken :: ((Pos,String)) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsToken_vIn31 _lhsIoptions
        (T_HsToken_vOut31 _lhsOattrs _lhsOtok) <- return (inv_HsToken_s32 sem arg)
        return (Syn_HsToken _lhsOattrs _lhsOtok)
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
                               attach_T_HsToken :: Identity (T_HsToken_s32 )
                               }
newtype T_HsToken_s32  = C_HsToken_s32 {
                                       inv_HsToken_s32 :: (T_HsToken_v31 )
                                       }
data T_HsToken_s33  = C_HsToken_s33
type T_HsToken_v31  = (T_HsToken_vIn31 ) -> (T_HsToken_vOut31 )
data T_HsToken_vIn31  = T_HsToken_vIn31 (Options)
data T_HsToken_vOut31  = T_HsToken_vOut31 (Map String (Maybe NonLocalAttr)) ((Pos,String))
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ arg_pos_ _ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule431 arg_var_
         _tok = rule432 arg_pos_ arg_var_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule433 _tok
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule431 #-}
   {-# LINE 1100 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule431 = \ var_ ->
                              {-# LINE 1100 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              Map.singleton (fieldname var_) Nothing
                              {-# LINE 3500 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule432 #-}
   {-# LINE 1363 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule432 = \ pos_ var_ ->
                          {-# LINE 1363 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          (pos_,fieldname var_)
                          {-# LINE 3506 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule433 #-}
   rule433 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _mbAttr = rule434 arg_attr_ arg_field_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule435 _lhsIoptions _mbAttr arg_attr_ arg_field_
         _addTrace = rule436 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule437 _addTrace _lhsIoptions arg_attr_ arg_field_ arg_pos_
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule434 #-}
   {-# LINE 1101 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule434 = \ attr_ field_ ->
                              {-# LINE 1101 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              if field_ == _INST || field_ == _FIELD || field_ == _INST'
                              then Nothing
                              else Just $ mkNonLocalAttr (field_ == _LHS) field_ attr_
                              {-# LINE 3533 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule435 #-}
   {-# LINE 1104 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule435 = \ ((_lhsIoptions) :: Options) _mbAttr attr_ field_ ->
                              {-# LINE 1104 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              Map.singleton (attrname _lhsIoptions True field_ attr_) _mbAttr
                              {-# LINE 3539 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule436 #-}
   {-# LINE 1367 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule436 = \ attr_ field_ rdesc_ ->
                        {-# LINE 1367 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(prerr_endline " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ "; " ++ x ++ ")"
                          Nothing -> id
                        {-# LINE 3547 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule437 #-}
   {-# LINE 1370 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule437 = \ _addTrace ((_lhsIoptions) :: Options) attr_ field_ pos_ ->
                   {-# LINE 1370 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   (pos_, _addTrace     $ attrname _lhsIoptions True field_ attr_)
                   {-# LINE 3553 "dist/build/ExecutionPlan2Caml.hs"#-}
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule438 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule439  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule438 #-}
   {-# LINE 1372 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule438 = \ pos_ value_ ->
                         {-# LINE 1372 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, value_)
                         {-# LINE 3573 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule439 #-}
   rule439 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule440 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule441  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule440 #-}
   {-# LINE 1374 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule440 = \ pos_ value_ ->
                           {-# LINE 1374 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 3599 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule441 #-}
   rule441 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule442 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule443  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule442 #-}
   {-# LINE 1379 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule442 = \ pos_ value_ ->
                           {-# LINE 1379 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 3622 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule443 #-}
   rule443 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule444 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule445  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule444 #-}
   {-# LINE 1380 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule444 = \ pos_ ->
                           {-# LINE 1380 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           (pos_, "")
                           {-# LINE 3645 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule445 #-}
   rule445 = \  (_ :: ()) ->
     Map.empty

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens { options_Inh_HsTokens :: (Options) }
data Syn_HsTokens  = Syn_HsTokens { tks_Syn_HsTokens :: ([(Pos,String)]) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokens_vIn34 _lhsIoptions
        (T_HsTokens_vOut34 _lhsOtks) <- return (inv_HsTokens_s35 sem arg)
        return (Syn_HsTokens _lhsOtks)
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s35 )
                                 }
newtype T_HsTokens_s35  = C_HsTokens_s35 {
                                         inv_HsTokens_s35 :: (T_HsTokens_v34 )
                                         }
data T_HsTokens_s36  = C_HsTokens_s36
type T_HsTokens_v34  = (T_HsTokens_vIn34 ) -> (T_HsTokens_vOut34 )
data T_HsTokens_vIn34  = T_HsTokens_vIn34 (Options)
data T_HsTokens_vOut34  = T_HsTokens_vOut34 ([(Pos,String)])
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut31 _hdIattrs _hdItok) = inv_HsToken_s32 _hdX32 (T_HsToken_vIn31 _hdOoptions)
         (T_HsTokens_vOut34 _tlItks) = inv_HsTokens_s35 _tlX35 (T_HsTokens_vIn34 _tlOoptions)
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule446 _hdItok _tlItks
         _hdOoptions = rule447 _lhsIoptions
         _tlOoptions = rule448 _lhsIoptions
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule446 #-}
   {-# LINE 1359 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule446 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 1359 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 3703 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule447 #-}
   rule447 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule448 #-}
   rule448 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule449  ()
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule449 #-}
   {-# LINE 1360 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule449 = \  (_ :: ()) ->
                     {-# LINE 1360 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     []
                     {-# LINE 3727 "dist/build/ExecutionPlan2Caml.hs"#-}

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot { options_Inh_HsTokensRoot :: (Options) }
data Syn_HsTokensRoot  = Syn_HsTokensRoot {  }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokensRoot_vIn37 _lhsIoptions
        (T_HsTokensRoot_vOut37 ) <- return (inv_HsTokensRoot_s38 sem arg)
        return (Syn_HsTokensRoot )
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s38 )
                                         }
newtype T_HsTokensRoot_s38  = C_HsTokensRoot_s38 {
                                                 inv_HsTokensRoot_s38 :: (T_HsTokensRoot_v37 )
                                                 }
data T_HsTokensRoot_s39  = C_HsTokensRoot_s39
type T_HsTokensRoot_v37  = (T_HsTokensRoot_vIn37 ) -> (T_HsTokensRoot_vOut37 )
data T_HsTokensRoot_vIn37  = T_HsTokensRoot_vIn37 (Options)
data T_HsTokensRoot_vOut37  = T_HsTokensRoot_vOut37 
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_HsTokensRoot_v37 
      v37 = \ (T_HsTokensRoot_vIn37 _lhsIoptions) -> ( let
         _tokensX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut34 _tokensItks) = inv_HsTokens_s35 _tokensX35 (T_HsTokens_vIn34 _tokensOoptions)
         _tokensOoptions = rule450 _lhsIoptions
         __result_ = T_HsTokensRoot_vOut37 
         in __result_ )
     in C_HsTokensRoot_s38 v37
   {-# INLINE rule450 #-}
   rule450 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { allInhmap_Inh_Pattern :: (Map NontermIdent Attributes), allSynmap_Inh_Pattern :: (Map NontermIdent Attributes), anyLazyKind_Inh_Pattern :: (Bool), inhmap_Inh_Pattern :: (Attributes), localAttrTypes_Inh_Pattern :: (Map Identifier Type), options_Inh_Pattern :: (Options), synmap_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { attrTypes_Syn_Pattern :: (PP_Doc), attrs_Syn_Pattern :: (Set String), copy_Syn_Pattern :: (Pattern), extraDefs_Syn_Pattern :: ([(PP_Doc,PP_Doc)]), isUnderscore_Syn_Pattern :: (Bool), sem_lhs_Syn_Pattern :: ( PP_Doc ) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs)
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
data T_Pattern_vIn40  = T_Pattern_vIn40 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Bool) (Attributes) (Map Identifier Type) (Options) (Attributes)
data T_Pattern_vOut40  = T_Pattern_vOut40 (PP_Doc) (Set String) (Pattern) ([(PP_Doc,PP_Doc)]) (Bool) ( PP_Doc )
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrTypes _patsIattrs _patsIcopy _patsIextraDefs _patsIsem_lhs) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule451 _patsIsem_lhs arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule452  ()
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule453 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule454 _patsIattrs
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule455 _patsIextraDefs
         _copy = rule456 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule457 _copy
         _patsOallInhmap = rule458 _lhsIallInhmap
         _patsOallSynmap = rule459 _lhsIallSynmap
         _patsOanyLazyKind = rule460 _lhsIanyLazyKind
         _patsOinhmap = rule461 _lhsIinhmap
         _patsOlocalAttrTypes = rule462 _lhsIlocalAttrTypes
         _patsOoptions = rule463 _lhsIoptions
         _patsOsynmap = rule464 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule451 #-}
   {-# LINE 1066 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule451 = \ ((_patsIsem_lhs) :: [PP_Doc]) name_ ->
                                  {-# LINE 1066 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp_parens $ name_ >#< pp_block "(" ")" "," _patsIsem_lhs
                                  {-# LINE 3847 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 1075 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule452 = \  (_ :: ()) ->
                                    {-# LINE 1075 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    False
                                    {-# LINE 3853 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule453 #-}
   rule453 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule454 #-}
   rule454 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule455 #-}
   rule455 = \ ((_patsIextraDefs) :: [(PP_Doc,PP_Doc)]) ->
     _patsIextraDefs
   {-# INLINE rule456 #-}
   rule456 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule457 #-}
   rule457 = \ _copy ->
     _copy
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrTypes _patsIattrs _patsIcopy _patsIextraDefs _patsIsem_lhs) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule465 _patsIsem_lhs
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule466  ()
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule467 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule468 _patsIattrs
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule469 _patsIextraDefs
         _copy = rule470 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule471 _copy
         _patsOallInhmap = rule472 _lhsIallInhmap
         _patsOallSynmap = rule473 _lhsIallSynmap
         _patsOanyLazyKind = rule474 _lhsIanyLazyKind
         _patsOinhmap = rule475 _lhsIinhmap
         _patsOlocalAttrTypes = rule476 _lhsIlocalAttrTypes
         _patsOoptions = rule477 _lhsIoptions
         _patsOsynmap = rule478 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule465 #-}
   {-# LINE 1065 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule465 = \ ((_patsIsem_lhs) :: [PP_Doc]) ->
                                  {-# LINE 1065 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp_block "(" ")" "," _patsIsem_lhs
                                  {-# LINE 3927 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule466 #-}
   {-# LINE 1076 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule466 = \  (_ :: ()) ->
                                    {-# LINE 1076 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    False
                                    {-# LINE 3933 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule467 #-}
   rule467 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule468 #-}
   rule468 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule469 #-}
   rule469 = \ ((_patsIextraDefs) :: [(PP_Doc,PP_Doc)]) ->
     _patsIextraDefs
   {-# INLINE rule470 #-}
   rule470 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule471 #-}
   rule471 = \ _copy ->
     _copy
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule473 #-}
   rule473 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrTypes _patIattrs _patIcopy _patIextraDefs _patIisUnderscore _patIsem_lhs) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _var = rule479 _lhsIoptions arg_attr_ arg_field_
         _hasTp = rule480 _mbTp
         _o_sigs = rule481 _lhsIoptions
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule482 _hasTp _mbTp _o_sigs _var
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule483 _patIisUnderscore _patIsem_lhs _var
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule484  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule485 _lhsIoptions _patIattrs arg_attr_ arg_field_
         _mbTp = rule486 _lhsIlocalAttrTypes _lhsIsynmap arg_attr_ arg_field_
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule487 _lhsIoptions _mbTp _patIattrTypes arg_attr_ arg_field_
         _copy = rule488 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule489 _copy
         _patOallInhmap = rule490 _lhsIallInhmap
         _patOallSynmap = rule491 _lhsIallSynmap
         _patOanyLazyKind = rule492 _lhsIanyLazyKind
         _patOinhmap = rule493 _lhsIinhmap
         _patOlocalAttrTypes = rule494 _lhsIlocalAttrTypes
         _patOoptions = rule495 _lhsIoptions
         _patOsynmap = rule496 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule479 #-}
   {-# LINE 1057 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule479 = \ ((_lhsIoptions) :: Options) attr_ field_ ->
                                  {-# LINE 1057 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  text $ attrname _lhsIoptions False field_ attr_
                                  {-# LINE 4011 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule480 #-}
   {-# LINE 1058 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule480 = \ _mbTp ->
                                  {-# LINE 1058 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  isJust _mbTp
                                  {-# LINE 4017 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule481 #-}
   {-# LINE 1059 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule481 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 1059 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  typeSigs _lhsIoptions
                                  {-# LINE 4023 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule482 #-}
   {-# LINE 1061 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule482 = \ _hasTp _mbTp _o_sigs _var ->
                                  {-# LINE 1061 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  ppArg (_hasTp     && _o_sigs    ) _var     (maybe (text "?no type?") ppTp _mbTp    )
                                  {-# LINE 4029 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule483 #-}
   {-# LINE 1062 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule483 = \ ((_patIisUnderscore) :: Bool) ((_patIsem_lhs) ::  PP_Doc ) _var ->
                                  {-# LINE 1062 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  if _patIisUnderscore
                                  then []
                                  else [ (_patIsem_lhs, _var    ) ]
                                  {-# LINE 4037 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule484 #-}
   {-# LINE 1077 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule484 = \  (_ :: ()) ->
                                    {-# LINE 1077 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    False
                                    {-# LINE 4043 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule485 #-}
   {-# LINE 1083 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule485 = \ ((_lhsIoptions) :: Options) ((_patIattrs) :: Set String) attr_ field_ ->
                    {-# LINE 1083 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    Set.insert (attrname _lhsIoptions False field_ attr_) _patIattrs
                    {-# LINE 4049 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule486 #-}
   {-# LINE 1089 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule486 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIsynmap) :: Attributes) attr_ field_ ->
                    {-# LINE 1089 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    if field_ == _LHS
                    then Map.lookup attr_ _lhsIsynmap
                    else if field_ == _LOC
                         then Map.lookup attr_ _lhsIlocalAttrTypes
                         else Nothing
                    {-# LINE 4059 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule487 #-}
   {-# LINE 1094 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule487 = \ ((_lhsIoptions) :: Options) _mbTp ((_patIattrTypes) :: PP_Doc) attr_ field_ ->
                    {-# LINE 1094 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    maybe empty (\tp -> (attrname _lhsIoptions False field_ attr_) >#< "::" >#< ppTp tp) _mbTp
                    >-< _patIattrTypes
                    {-# LINE 4066 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule488 #-}
   rule488 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule489 #-}
   rule489 = \ _copy ->
     _copy
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrTypes _patIattrs _patIcopy _patIextraDefs _patIisUnderscore _patIsem_lhs) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule497 _patIsem_lhs
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule498 _patIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule499 _patIattrs
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule500 _patIextraDefs
         _copy = rule501 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule502 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule503 _patIisUnderscore
         _patOallInhmap = rule504 _lhsIallInhmap
         _patOallSynmap = rule505 _lhsIallSynmap
         _patOanyLazyKind = rule506 _lhsIanyLazyKind
         _patOinhmap = rule507 _lhsIinhmap
         _patOlocalAttrTypes = rule508 _lhsIlocalAttrTypes
         _patOoptions = rule509 _lhsIoptions
         _patOsynmap = rule510 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule497 #-}
   {-# LINE 1068 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule497 = \ ((_patIsem_lhs) ::  PP_Doc ) ->
                                  {-# LINE 1068 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp_parens (text "lazy" >#< _patIsem_lhs)
                                  {-# LINE 4131 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule498 #-}
   rule498 = \ ((_patIattrTypes) :: PP_Doc) ->
     _patIattrTypes
   {-# INLINE rule499 #-}
   rule499 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule500 #-}
   rule500 = \ ((_patIextraDefs) :: [(PP_Doc,PP_Doc)]) ->
     _patIextraDefs
   {-# INLINE rule501 #-}
   rule501 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule502 #-}
   rule502 = \ _copy ->
     _copy
   {-# INLINE rule503 #-}
   rule503 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule510 #-}
   rule510 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule511  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule512  ()
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule513  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule514  ()
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule515  ()
         _copy = rule516 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule517 _copy
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule511 #-}
   {-# LINE 1067 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule511 = \  (_ :: ()) ->
                                  {-# LINE 1067 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  text "_"
                                  {-# LINE 4199 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule512 #-}
   {-# LINE 1078 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule512 = \  (_ :: ()) ->
                                    {-# LINE 1078 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    True
                                    {-# LINE 4205 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule513 #-}
   rule513 = \  (_ :: ()) ->
     empty
   {-# INLINE rule514 #-}
   rule514 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule515 #-}
   rule515 = \  (_ :: ()) ->
     []
   {-# INLINE rule516 #-}
   rule516 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule517 #-}
   rule517 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { allInhmap_Inh_Patterns :: (Map NontermIdent Attributes), allSynmap_Inh_Patterns :: (Map NontermIdent Attributes), anyLazyKind_Inh_Patterns :: (Bool), inhmap_Inh_Patterns :: (Attributes), localAttrTypes_Inh_Patterns :: (Map Identifier Type), options_Inh_Patterns :: (Options), synmap_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { attrTypes_Syn_Patterns :: (PP_Doc), attrs_Syn_Patterns :: (Set String), copy_Syn_Patterns :: (Patterns), extraDefs_Syn_Patterns :: ([(PP_Doc,PP_Doc)]), sem_lhs_Syn_Patterns :: ([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOsem_lhs) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOsem_lhs)
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
data T_Patterns_vIn43  = T_Patterns_vIn43 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Bool) (Attributes) (Map Identifier Type) (Options) (Attributes)
data T_Patterns_vOut43  = T_Patterns_vOut43 (PP_Doc) (Set String) (Patterns) ([(PP_Doc,PP_Doc)]) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIattrTypes _hdIattrs _hdIcopy _hdIextraDefs _hdIisUnderscore _hdIsem_lhs) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdOallInhmap _hdOallSynmap _hdOanyLazyKind _hdOinhmap _hdOlocalAttrTypes _hdOoptions _hdOsynmap)
         (T_Patterns_vOut43 _tlIattrTypes _tlIattrs _tlIcopy _tlIextraDefs _tlIsem_lhs) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlOallInhmap _tlOallSynmap _tlOanyLazyKind _tlOinhmap _tlOlocalAttrTypes _tlOoptions _tlOsynmap)
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule518 _hdIattrTypes _tlIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule519 _hdIattrs _tlIattrs
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule520 _hdIextraDefs _tlIextraDefs
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule521 _hdIsem_lhs _tlIsem_lhs
         _copy = rule522 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule523 _copy
         _hdOallInhmap = rule524 _lhsIallInhmap
         _hdOallSynmap = rule525 _lhsIallSynmap
         _hdOanyLazyKind = rule526 _lhsIanyLazyKind
         _hdOinhmap = rule527 _lhsIinhmap
         _hdOlocalAttrTypes = rule528 _lhsIlocalAttrTypes
         _hdOoptions = rule529 _lhsIoptions
         _hdOsynmap = rule530 _lhsIsynmap
         _tlOallInhmap = rule531 _lhsIallInhmap
         _tlOallSynmap = rule532 _lhsIallSynmap
         _tlOanyLazyKind = rule533 _lhsIanyLazyKind
         _tlOinhmap = rule534 _lhsIinhmap
         _tlOlocalAttrTypes = rule535 _lhsIlocalAttrTypes
         _tlOoptions = rule536 _lhsIoptions
         _tlOsynmap = rule537 _lhsIsynmap
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule518 #-}
   rule518 = \ ((_hdIattrTypes) :: PP_Doc) ((_tlIattrTypes) :: PP_Doc) ->
     _hdIattrTypes >-< _tlIattrTypes
   {-# INLINE rule519 #-}
   rule519 = \ ((_hdIattrs) :: Set String) ((_tlIattrs) :: Set String) ->
     _hdIattrs `Set.union` _tlIattrs
   {-# INLINE rule520 #-}
   rule520 = \ ((_hdIextraDefs) :: [(PP_Doc,PP_Doc)]) ((_tlIextraDefs) :: [(PP_Doc,PP_Doc)]) ->
     _hdIextraDefs ++ _tlIextraDefs
   {-# INLINE rule521 #-}
   rule521 = \ ((_hdIsem_lhs) ::  PP_Doc ) ((_tlIsem_lhs) :: [PP_Doc]) ->
     _hdIsem_lhs : _tlIsem_lhs
   {-# INLINE rule522 #-}
   rule522 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule523 #-}
   rule523 = \ _copy ->
     _copy
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule529 #-}
   rule529 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule530 #-}
   rule530 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule532 #-}
   rule532 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule534 #-}
   rule534 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule535 #-}
   rule535 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule536 #-}
   rule536 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule537 #-}
   rule537 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule538  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule539  ()
         _lhsOextraDefs :: [(PP_Doc,PP_Doc)]
         _lhsOextraDefs = rule540  ()
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule541  ()
         _copy = rule542  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule543 _copy
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule538 #-}
   rule538 = \  (_ :: ()) ->
     empty
   {-# INLINE rule539 #-}
   rule539 = \  (_ :: ()) ->
     Set.empty
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
   rule543 = \ _copy ->
     _copy

-- Visit -------------------------------------------------------
-- wrapper
data Inh_Visit  = Inh_Visit { allFromToStates_Inh_Visit :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visit :: (Map NontermIdent Attributes), allInitStates_Inh_Visit :: (Map NontermIdent Int), allSynmap_Inh_Visit :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visit :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_Visit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), allintramap_Inh_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visit :: (Map Identifier Type), childintros_Inh_Visit :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), con_Inh_Visit :: (ConstructorIdent), inhmap_Inh_Visit :: (Attributes), mrules_Inh_Visit :: (Map Identifier (VisitKind ->  Either Error PP_Doc)), nextVisits_Inh_Visit :: (Map StateIdentifier StateCtx), nt_Inh_Visit :: (NontermIdent), options_Inh_Visit :: (Options), params_Inh_Visit :: ([Identifier]), prevVisits_Inh_Visit :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visit :: (Map Identifier (Set String)), ruleuses_Inh_Visit :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visit :: (Attributes), terminaldefs_Inh_Visit :: (Set String) }
data Syn_Visit  = Syn_Visit { allvisits_Syn_Visit :: ( VisitStateState ), childvisit_Syn_Visit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), errors_Syn_Visit :: (Seq Error), fromToStates_Syn_Visit :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visit :: (Set String), ruleKinds_Syn_Visit :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visit :: (Map Identifier Int), sem_visit_Syn_Visit :: (  (StateIdentifier,PP_Doc)  ), t_visits_Syn_Visit :: (PP_Doc), visitKinds_Syn_Visit :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visit :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visit :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visit #-}
wrap_Visit :: T_Visit  -> Inh_Visit  -> (Syn_Visit )
wrap_Visit (T_Visit act) (Inh_Visit _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visit_vOut46 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visit_s47 sem arg)
        return (Syn_Visit _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_Visit #-}
sem_Visit :: Visit  -> T_Visit 
sem_Visit ( Visit ident_ from_ to_ inh_ syn_ steps_ kind_ ) = sem_Visit_Visit ident_ from_ to_ inh_ syn_ ( sem_VisitSteps steps_ ) kind_

-- semantic domain
newtype T_Visit  = T_Visit {
                           attach_T_Visit :: Identity (T_Visit_s47 )
                           }
newtype T_Visit_s47  = C_Visit_s47 {
                                   inv_Visit_s47 :: (T_Visit_v46 )
                                   }
data T_Visit_s48  = C_Visit_s48
type T_Visit_v46  = (T_Visit_vIn46 ) -> (T_Visit_vOut46 )
data T_Visit_vIn46  = T_Visit_vIn46 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (ConstructorIdent) (Attributes) (Map Identifier (VisitKind ->  Either Error PP_Doc)) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visit_vOut46  = T_Visit_vOut46 ( VisitStateState ) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (  (StateIdentifier,PP_Doc)  ) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visit_Visit #-}
sem_Visit_Visit :: (VisitIdentifier) -> (StateIdentifier) -> (StateIdentifier) -> (Set Identifier) -> (Set Identifier) -> T_VisitSteps  -> (VisitKind) -> T_Visit 
sem_Visit_Visit arg_ident_ arg_from_ arg_to_ arg_inh_ arg_syn_ arg_steps_ arg_kind_ = T_Visit (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Visit_v46 
      v46 = \ (T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _lhsOallvisits ::  VisitStateState 
         _lhsOallvisits = rule544 arg_from_ arg_ident_ arg_to_
         _nameTIn_visit = rule545 _lhsInt arg_ident_
         _nameTOut_visit = rule546 _lhsInt arg_ident_
         _nameNextState = rule547 _lhsInt arg_to_
         _nameCaller_visit = rule548 _lhsInt arg_ident_
         _nextVisitInfo = rule549 _lhsInextVisits arg_to_
         _t_params = rule550 _lhsIparams
         _t_c_params = rule551 _lhsIparams
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule552 _contpart _inhpart _lhsInt _nameCaller_visit _nameTIn_visit _nameTOut_visit _synpart _t_c_params _t_params arg_ident_
         _contpart = rule553 _lhsInt _nameNextState _nextVisitInfo _t_params arg_ident_
         _inhpart = rule554 _lhsIinhmap _ppTypeList arg_inh_
         _synpart = rule555 _lhsIsynmap _ppTypeList arg_syn_
         _ppTypeList = rule556 _lhsInt arg_ident_
         _o_sigs = rule557 _lhsIoptions
         _lhsOsem_visit ::   (StateIdentifier,PP_Doc)  
         _lhsOsem_visit = rule558 _lhsInt _lhsIoptions _nameTIn_visit _nameTOut_visit _o_sigs _stepsIsem_steps _t_params arg_from_ arg_ident_ arg_inh_
         _stepsOfollow = rule559 _nextStBuild _resultval
         _nextArgsMp = rule560 _lhsIallintramap arg_to_
         _nextArgs = rule561 _nextArgsMp
         _nextStExp = rule562 _lhsIoptions _nextArgs _nextArgsMp arg_to_
         _resultval = rule563 _lhsInt _lhsIoptions _nextStRefExp arg_ident_ arg_syn_
         (_nextStBuild,_nextStRefExp) = rule564 _lhsInt _nextStExp _nextVisitInfo arg_ident_
         _stepsOkind = rule565 arg_kind_
         _stepsOindex = rule566  ()
         _stepsOprevMaxSimRefs = rule567  ()
         _stepsOuseParallel = rule568  ()
         _prevVisitInfo = rule569 _lhsInextVisits arg_from_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule570 _invokecode arg_ident_
         _invokecode = rule571 _lhsInt _lhsIoptions _nameTOut_visit _nextVisitInfo _o_sigs _prevVisitInfo arg_from_ arg_ident_ arg_inh_ arg_kind_ arg_syn_ arg_to_
         _thisintra = rule572 _defsAsMap _nextintra _uses
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule573 _thisintra arg_from_
         _nextintra = rule574 _lhsIallintramap arg_to_
         _uses = rule575 _lhsIoptions _stepsIuses arg_syn_
         _inhVarNms = rule576 _lhsIoptions arg_inh_
         _defs = rule577 _inhVarNms _lhsIterminaldefs _stepsIdefs
         _defsAsMap = rule578 _defs
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule579 arg_ident_ arg_syn_
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule580 arg_ident_ arg_inh_
         _lazyIntrasInh = rule581 _inhVarNms _stepsIdefs arg_kind_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule582 _lazyIntrasInh _stepsIlazyIntras
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule583 arg_from_ arg_ident_ arg_to_
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule584 arg_ident_ arg_kind_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule585 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule586 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule587 _stepsIruleUsage
         _stepsOallFromToStates = rule588 _lhsIallFromToStates
         _stepsOallInitStates = rule589 _lhsIallInitStates
         _stepsOallVisitKinds = rule590 _lhsIallVisitKinds
         _stepsOallchildvisit = rule591 _lhsIallchildvisit
         _stepsOavisitdefs = rule592 _lhsIavisitdefs
         _stepsOavisituses = rule593 _lhsIavisituses
         _stepsOchildTypes = rule594 _lhsIchildTypes
         _stepsOchildintros = rule595 _lhsIchildintros
         _stepsOmrules = rule596 _lhsImrules
         _stepsOoptions = rule597 _lhsIoptions
         _stepsOruledefs = rule598 _lhsIruledefs
         _stepsOruleuses = rule599 _lhsIruleuses
         __result_ = T_Visit_vOut46 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visit_s47 v46
   {-# INLINE rule544 #-}
   {-# LINE 436 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule544 = \ from_ ident_ to_ ->
                            {-# LINE 436 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 4507 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule545 #-}
   {-# LINE 539 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule545 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                          {-# LINE 539 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          conNmTVisitIn _lhsInt ident_
                          {-# LINE 4513 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule546 #-}
   {-# LINE 540 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule546 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                          {-# LINE 540 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          conNmTVisitOut _lhsInt ident_
                          {-# LINE 4519 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule547 #-}
   {-# LINE 541 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule547 = \ ((_lhsInt) :: NontermIdent) to_ ->
                           {-# LINE 541 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           type_nt_sem _lhsInt to_
                           {-# LINE 4525 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule548 #-}
   {-# LINE 542 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule548 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                           {-# LINE 542 "./src-ag/ExecutionPlan2Caml.ag" #-}
                           type_caller_visit _lhsInt ident_
                           {-# LINE 4531 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule549 #-}
   {-# LINE 544 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule549 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) to_ ->
                          {-# LINE 544 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          Map.findWithDefault ManyVis to_ _lhsInextVisits
                          {-# LINE 4537 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule550 #-}
   {-# LINE 546 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule550 = \ ((_lhsIparams) :: [Identifier]) ->
                    {-# LINE 546 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    ppTypeParams _lhsIparams
                    {-# LINE 4543 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule551 #-}
   {-# LINE 547 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule551 = \ ((_lhsIparams) :: [Identifier]) ->
                     {-# LINE 547 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     ppTypeParams (cont_tvar : map pp _lhsIparams)
                     {-# LINE 4549 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule552 #-}
   {-# LINE 551 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule552 = \ _contpart _inhpart ((_lhsInt) :: NontermIdent) _nameCaller_visit _nameTIn_visit _nameTOut_visit _synpart _t_c_params _t_params ident_ ->
                    {-# LINE 551 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    "and" >#< _t_c_params     >#< _nameCaller_visit     >#< "=" >#< ppRecordTp
                      [ nm_inh _lhsInt ident_ >#< ":" >#< _t_params     >#< conNmTVisitIn _lhsInt ident_
                      , nm_cont _lhsInt ident_ >#< ":" >#< _t_params     >#< conNmTVisitOut _lhsInt ident_ >#< "->" >#< cont_tvar
                      ]
                    >-< "and" >#< _t_params     >#< _nameTIn_visit      >#< "=" >#< ppRecordTp _inhpart
                    >-< "and" >#< _t_params     >#< _nameTOut_visit     >#< "=" >#< ppRecordTp (_synpart     ++ _contpart    )
                    {-# LINE 4560 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule553 #-}
   {-# LINE 558 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule553 = \ ((_lhsInt) :: NontermIdent) _nameNextState _nextVisitInfo _t_params ident_ ->
                   {-# LINE 558 "./src-ag/ExecutionPlan2Caml.ag" #-}
                   case _nextVisitInfo     of
                     NoneVis -> []
                     _       -> [ nm_outarg_cont _lhsInt ident_ >#< ":" >#< _t_params     >#< _nameNextState     ]
                   {-# LINE 4568 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule554 #-}
   {-# LINE 562 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule554 = \ ((_lhsIinhmap) :: Attributes) _ppTypeList inh_ ->
                    {-# LINE 562 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    _ppTypeList     nm_inarg inh_ _lhsIinhmap
                    {-# LINE 4574 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule555 #-}
   {-# LINE 563 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule555 = \ ((_lhsIsynmap) :: Attributes) _ppTypeList syn_ ->
                    {-# LINE 563 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    _ppTypeList     nm_outarg syn_ _lhsIsynmap
                    {-# LINE 4580 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule556 #-}
   {-# LINE 564 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule556 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                     {-# LINE 564 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     \f s m -> map (\i -> case Map.lookup i m of
                                            Just tp -> f i _lhsInt ident_ >#< ":" >#< ppTp tp ) $ Set.toList s
                     {-# LINE 4587 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule557 #-}
   {-# LINE 798 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule557 = \ ((_lhsIoptions) :: Options) ->
                 {-# LINE 798 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 typeSigs _lhsIoptions
                 {-# LINE 4593 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule558 #-}
   {-# LINE 799 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule558 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nameTIn_visit _nameTOut_visit _o_sigs ((_stepsIsem_steps) :: PP_Doc) _t_params from_ ident_ inh_ ->
                    {-# LINE 799 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    ( from_
                    , let resTp = _t_params     >#< _nameTOut_visit
                          argTp = _t_params     >#< _nameTIn_visit
                          argMatch = ppRecordVal [ nm_inarg i _lhsInt ident_ >#< "=" >#< lhsname _lhsIoptions True i | i <- Set.toList inh_ ]
                      in ppFunDecl _o_sigs     (nm_visit ident_) [(argMatch, argTp)] resTp _stepsIsem_steps
                    )
                    {-# LINE 4604 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule559 #-}
   {-# LINE 806 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule559 = \ _nextStBuild _resultval ->
                     {-# LINE 806 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     _nextStBuild     >-< _resultval
                     {-# LINE 4610 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule560 #-}
   {-# LINE 808 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule560 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                     {-# LINE 808 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                     {-# LINE 4616 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule561 #-}
   {-# LINE 809 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule561 = \ _nextArgsMp ->
                     {-# LINE 809 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     ppSpaced $ Map.keys $ _nextArgsMp
                     {-# LINE 4622 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule562 #-}
   {-# LINE 810 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule562 = \ ((_lhsIoptions) :: Options) _nextArgs _nextArgsMp to_ ->
                     {-# LINE 810 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     nm_st to_ >#< _nextArgs     >#< dummyArg _lhsIoptions (Map.null _nextArgsMp    )
                     {-# LINE 4628 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule563 #-}
   {-# LINE 812 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule563 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nextStRefExp ident_ syn_ ->
                    {-# LINE 812 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    ppRecordVal
                      (  [ nm_outarg i _lhsInt ident_ >#< "=" >#< lhsname _lhsIoptions False i | i <- Set.toList syn_ ]
                      ++ [ _nextStRefExp     ])
                    {-# LINE 4636 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule564 #-}
   {-# LINE 817 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule564 = \ ((_lhsInt) :: NontermIdent) _nextStExp _nextVisitInfo ident_ ->
         {-# LINE 817 "./src-ag/ExecutionPlan2Caml.ag" #-}
         case _nextVisitInfo     of
           NoneVis  -> (empty, empty)
           _        -> ( "let" >#< nextStName >#< "=" >#< _nextStExp     >#< "in"
                       , nm_outarg_cont _lhsInt ident_ >#< "=" >#< nextStName)
         {-# LINE 4645 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule565 #-}
   {-# LINE 832 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule565 = \ kind_ ->
                                  {-# LINE 832 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  kind_
                                  {-# LINE 4651 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule566 #-}
   {-# LINE 884 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule566 = \  (_ :: ()) ->
                                     {-# LINE 884 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                     0
                                     {-# LINE 4657 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule567 #-}
   {-# LINE 891 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule567 = \  (_ :: ()) ->
                                              {-# LINE 891 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                              0
                                              {-# LINE 4663 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule568 #-}
   {-# LINE 908 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule568 = \  (_ :: ()) ->
                                           {-# LINE 908 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                           False
                                           {-# LINE 4669 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule569 #-}
   {-# LINE 1165 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule569 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) from_ ->
                        {-# LINE 1165 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        Map.findWithDefault ManyVis from_ _lhsInextVisits
                        {-# LINE 4675 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule570 #-}
   {-# LINE 1166 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule570 = \ _invokecode ident_ ->
                     {-# LINE 1166 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton ident_ _invokecode
                     {-# LINE 4681 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule571 #-}
   {-# LINE 1168 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule571 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nameTOut_visit _nextVisitInfo _o_sigs _prevVisitInfo from_ ident_ inh_ kind_ syn_ to_ ->
        {-# LINE 1168 "./src-ag/ExecutionPlan2Caml.ag" #-}
        \chld childTp kind follow ->
          let code = cont >-< inps >-< call
              childNmTo   = text $ stname chld to_
              childNmFrom = text $ stname chld from_
              childTpArgs = case childTp of
                              NT _ args _ -> args
                              _           -> error "generate visit call: type of the child is not a nonterminal!"
              cont = "let" >#< contNm >#< ppArg _o_sigs     (ppRecordVal cont_in) cont_in_tp >#< "="
                     >-< indent 2 follow
                     >#< "in"
              cont_in = [ nm_outarg i _lhsInt ident_ >#< "=" >#< attrname _lhsIoptions True chld i | i <- Set.toList syn_ ]
                        ++ case _nextVisitInfo     of
                             NoneVis -> []
                             _       -> [ nm_outarg_cont _lhsInt ident_ >#< "=" >#< childNmTo ]
              cont_in_tp = ppTypeParams childTpArgs >#< _nameTOut_visit
              inps = "let" >#< inpsNm >#< "=" >#< ppRecordVal
                       [ nm_inh _lhsInt ident_ >#< "=" >#< ppRecordVal inps_in
                       , nm_cont _lhsInt ident_ >#< "=" >#< contNm
                       ] >#< "in"
              inps_in = [ nm_inarg i _lhsInt ident_ >#< "=" >#< attrname _lhsIoptions False chld i | i <- Set.toList inh_ ]
              call = childNmFrom >|< "." >|< nm_invoke _lhsInt from_ >#< arg
              arg = case _prevVisitInfo     of
                      NoneVis  -> error "error: invocation of a visit from a state that has no next visits"
                      OneVis _ -> pp inpsNm
                      ManyVis  -> pp_parens (con_visit _lhsInt ident_ >#< inpsNm)
          in if kind `compatibleKind` kind_
             then Right code
             else Left $ IncompatibleVisitKind chld ident_ kind kind_
        {-# LINE 4714 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule572 #-}
   {-# LINE 1273 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule572 = \ _defsAsMap _nextintra _uses ->
                            {-# LINE 1273 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            (_uses     `Map.union` _nextintra    ) `Map.difference` _defsAsMap
                            {-# LINE 4720 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule573 #-}
   {-# LINE 1274 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule573 = \ _thisintra from_ ->
                            {-# LINE 1274 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton from_ _thisintra
                            {-# LINE 4726 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule574 #-}
   {-# LINE 1275 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule574 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                            {-# LINE 1275 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 4732 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule575 #-}
   {-# LINE 1276 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule575 = \ ((_lhsIoptions) :: Options) ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) syn_ ->
                            {-# LINE 1276 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            let mp1 = _stepsIuses
                                mp2 = Map.fromList [ (lhsname _lhsIoptions False i, Just (AttrSyn _LHS i)) | i <- Set.elems syn_ ]
                            in mp1 `Map.union` mp2
                            {-# LINE 4740 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule576 #-}
   {-# LINE 1279 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule576 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 1279 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Set.map (lhsname _lhsIoptions True) inh_
                            {-# LINE 4746 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule577 #-}
   {-# LINE 1280 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule577 = \ _inhVarNms ((_lhsIterminaldefs) :: Set String) ((_stepsIdefs) :: Set String) ->
                            {-# LINE 1280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _stepsIdefs `Set.union` _inhVarNms     `Set.union` _lhsIterminaldefs
                            {-# LINE 4752 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule578 #-}
   {-# LINE 1281 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule578 = \ _defs ->
                            {-# LINE 1281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.fromList [ (a, Nothing) | a <- Set.elems _defs     ]
                            {-# LINE 4758 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule579 #-}
   {-# LINE 1305 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule579 = \ ident_ syn_ ->
                            {-# LINE 1305 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 4764 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule580 #-}
   {-# LINE 1306 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule580 = \ ident_ inh_ ->
                            {-# LINE 1306 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 4770 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule581 #-}
   {-# LINE 1338 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule581 = \ _inhVarNms ((_stepsIdefs) :: Set String) kind_ ->
                        {-# LINE 1338 "./src-ag/ExecutionPlan2Caml.ag" #-}
                        case kind_ of
                          VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                          _               -> Set.empty
                        {-# LINE 4778 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule582 #-}
   {-# LINE 1341 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule582 = \ _lazyIntrasInh ((_stepsIlazyIntras) :: Set String) ->
                     {-# LINE 1341 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                     {-# LINE 4784 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule583 #-}
   {-# LINE 1404 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule583 = \ from_ ident_ to_ ->
                       {-# LINE 1404 "./src-ag/ExecutionPlan2Caml.ag" #-}
                       Map.singleton ident_ (from_, to_)
                       {-# LINE 4790 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule584 #-}
   {-# LINE 1448 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule584 = \ ident_ kind_ ->
                     {-# LINE 1448 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     Map.singleton ident_ kind_
                     {-# LINE 4796 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule585 #-}
   rule585 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule586 #-}
   rule586 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule587 #-}
   rule587 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule589 #-}
   rule589 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule590 #-}
   rule590 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule591 #-}
   rule591 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule592 #-}
   rule592 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule593 #-}
   rule593 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule594 #-}
   rule594 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule595 #-}
   rule595 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses

-- VisitStep ---------------------------------------------------
-- wrapper
data Inh_VisitStep  = Inh_VisitStep { allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitStep :: (Map NontermIdent Int), allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitStep :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitStep :: (Map Identifier Type), childintros_Inh_VisitStep :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), follow_Inh_VisitStep :: (PP_Doc), index_Inh_VisitStep :: (Int), isLast_Inh_VisitStep :: (Bool), kind_Inh_VisitStep :: (VisitKind), mrules_Inh_VisitStep :: (Map Identifier (VisitKind ->  Either Error PP_Doc)), options_Inh_VisitStep :: (Options), prevMaxSimRefs_Inh_VisitStep :: (Int), ruledefs_Inh_VisitStep :: (Map Identifier (Set String)), ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))), useParallel_Inh_VisitStep :: (Bool) }
data Syn_VisitStep  = Syn_VisitStep { defs_Syn_VisitStep :: (Set String), errors_Syn_VisitStep :: (Seq Error), index_Syn_VisitStep :: (Int), isLast_Syn_VisitStep :: (Bool), lazyIntras_Syn_VisitStep :: (Set String), prevMaxSimRefs_Syn_VisitStep :: (Int), ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitStep :: (Map Identifier Int), sem_steps_Syn_VisitStep :: (PP_Doc), uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitStep #-}
wrap_VisitStep :: T_VisitStep  -> Inh_VisitStep  -> (Syn_VisitStep )
wrap_VisitStep (T_VisitStep act) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
        (T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds) <- return (inv_VisitStep_s50 sem arg)
        return (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds)
   )

-- cata
{-# NOINLINE sem_VisitStep #-}
sem_VisitStep :: VisitStep  -> T_VisitStep 
sem_VisitStep ( Sem name_ ) = sem_VisitStep_Sem name_
sem_VisitStep ( ChildVisit child_ nonterm_ visit_ ) = sem_VisitStep_ChildVisit child_ nonterm_ visit_
sem_VisitStep ( PureGroup steps_ ordered_ ) = sem_VisitStep_PureGroup ( sem_VisitSteps steps_ ) ordered_
sem_VisitStep ( Sim steps_ ) = sem_VisitStep_Sim ( sem_VisitSteps steps_ )
sem_VisitStep ( ChildIntro child_ ) = sem_VisitStep_ChildIntro child_

-- semantic domain
newtype T_VisitStep  = T_VisitStep {
                                   attach_T_VisitStep :: Identity (T_VisitStep_s50 )
                                   }
newtype T_VisitStep_s50  = C_VisitStep_s50 {
                                           inv_VisitStep_s50 :: (T_VisitStep_v49 )
                                           }
data T_VisitStep_s51  = C_VisitStep_s51
type T_VisitStep_v49  = (T_VisitStep_vIn49 ) -> (T_VisitStep_vOut49 )
data T_VisitStep_vIn49  = T_VisitStep_vIn49 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (PP_Doc) (Int) (Bool) (VisitKind) (Map Identifier (VisitKind ->  Either Error PP_Doc)) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Bool)
data T_VisitStep_vOut49  = T_VisitStep_vOut49 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitStep_Sem #-}
sem_VisitStep_Sem :: (Identifier) -> T_VisitStep 
sem_VisitStep_Sem arg_name_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _ruleItf = rule600 _lhsImrules arg_name_
         _lhsOerrors :: Seq Error
         (_lhsOerrors,_sem_steps) = rule601 _lhsIkind _ruleItf
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule602 _lhsIfollow _sem_steps
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule603 arg_name_
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule604 _lhsIkind arg_name_
         _lhsOdefs :: Set String
         _lhsOdefs = rule605 _lhsIruledefs arg_name_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule606 _lhsIruleuses arg_name_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule607  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule608  ()
         _lhsOindex :: Int
         _lhsOindex = rule609 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule610 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule611 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule600 #-}
   {-# LINE 849 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule600 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) name_ ->
                               {-# LINE 849 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               Map.findWithDefault (error $ "Rule "  ++ show name_  ++ " not found") name_ _lhsImrules
                               {-# LINE 4915 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule601 #-}
   {-# LINE 850 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule601 = \ ((_lhsIkind) :: VisitKind) _ruleItf ->
                                               {-# LINE 850 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                               case _ruleItf     _lhsIkind of
                                                 Left e     -> (Seq.singleton e, empty)
                                                 Right stmt -> (Seq.empty, stmt)
                                               {-# LINE 4923 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule602 #-}
   {-# LINE 853 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule602 = \ ((_lhsIfollow) :: PP_Doc) _sem_steps ->
                                 {-# LINE 853 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _sem_steps     >-< _lhsIfollow
                                 {-# LINE 4929 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule603 #-}
   {-# LINE 1226 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule603 = \ name_ ->
                                                 {-# LINE 1226 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                                 Map.singleton name_ 1
                                                 {-# LINE 4935 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule604 #-}
   {-# LINE 1236 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule604 = \ ((_lhsIkind) :: VisitKind) name_ ->
                    {-# LINE 1236 "./src-ag/ExecutionPlan2Caml.ag" #-}
                    Map.singleton name_ (Set.singleton _lhsIkind)
                    {-# LINE 4941 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule605 #-}
   {-# LINE 1321 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule605 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) name_ ->
                            {-# LINE 1321 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                            {-# LINE 4947 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule606 #-}
   {-# LINE 1322 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule606 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) name_ ->
                            {-# LINE 1322 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                            {-# LINE 4953 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule607 #-}
   rule607 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule608 #-}
   rule608 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule609 #-}
   rule609 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule610 #-}
   rule610 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule611 #-}
   rule611 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_ChildVisit #-}
sem_VisitStep_ChildVisit :: (Identifier) -> (NontermIdent) -> (VisitIdentifier) -> T_VisitStep 
sem_VisitStep_ChildVisit arg_child_ _ arg_visit_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _visitItf = rule612 _lhsIallchildvisit arg_visit_
         _childType = rule613 _lhsIchildTypes arg_child_
         _lhsOerrors :: Seq Error
         _lhsOsem_steps :: PP_Doc
         (_lhsOerrors,_lhsOsem_steps) = rule614 _childType _lhsIfollow _lhsIkind _visitItf arg_child_
         _lhsOdefs :: Set String
         _lhsOdefs = rule615 _lhsIavisitdefs _lhsIoptions _to arg_child_ arg_visit_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule616 _from _lhsIavisituses _lhsIoptions arg_child_ arg_visit_
         (_from,_to) = rule617 _lhsIallFromToStates arg_visit_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule618  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule619  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule620  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule621  ()
         _lhsOindex :: Int
         _lhsOindex = rule622 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule623 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule624 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule612 #-}
   {-# LINE 860 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule612 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) visit_ ->
                                {-# LINE 860 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error $ "Visit " ++ show visit_ ++ " not found") visit_ _lhsIallchildvisit
                                {-# LINE 5008 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule613 #-}
   {-# LINE 861 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule613 = \ ((_lhsIchildTypes) :: Map Identifier Type) child_ ->
                                 {-# LINE 861 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Map.findWithDefault (error ("type of child " ++ show child_ ++ " is not in the childTypes map! " ++ show _lhsIchildTypes)) child_ _lhsIchildTypes
                                 {-# LINE 5014 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule614 #-}
   {-# LINE 862 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule614 = \ _childType ((_lhsIfollow) :: PP_Doc) ((_lhsIkind) :: VisitKind) _visitItf child_ ->
                                               {-# LINE 862 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                               case _visitItf     child_ _childType     _lhsIkind _lhsIfollow of
                                                 Left e      -> (Seq.singleton e, empty)
                                                 Right steps -> (Seq.empty, steps)
                                               {-# LINE 5022 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule615 #-}
   {-# LINE 1323 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule615 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_lhsIoptions) :: Options) _to child_ visit_ ->
                            {-# LINE 1323 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Set.insert (stname child_ _to) $ maybe (error "Visit not found") (Set.map $ attrname _lhsIoptions True child_) $ Map.lookup visit_ _lhsIavisitdefs
                            {-# LINE 5028 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule616 #-}
   {-# LINE 1324 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule616 = \ _from ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ((_lhsIoptions) :: Options) child_ visit_ ->
                            {-# LINE 1324 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            let convert attrs = Map.fromList [ (attrname _lhsIoptions False child_ attr, Just $ mkNonLocalAttr True child_ attr) | attr <- Set.elems attrs ]
                            in Map.insert (stname child_ _from) Nothing $ convert $
                                 maybe (error "Visit not found") id $ Map.lookup visit_ _lhsIavisituses
                            {-# LINE 5036 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule617 #-}
   {-# LINE 1410 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule617 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) visit_ ->
                         {-# LINE 1410 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                         {-# LINE 5042 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule618 #-}
   rule618 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule619 #-}
   rule619 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule620 #-}
   rule620 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule621 #-}
   rule621 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule623 #-}
   rule623 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule624 #-}
   rule624 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_PureGroup #-}
sem_VisitStep_PureGroup :: T_VisitSteps  -> (Bool) -> T_VisitStep 
sem_VisitStep_PureGroup arg_steps_ arg_ordered_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _stepsOkind = rule625 arg_ordered_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule626 _stepsIdefs _stepsIlazyIntras arg_ordered_
         _lhsOdefs :: Set String
         _lhsOdefs = rule627 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule628 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule629 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule630 _stepsIruleUsage
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule631 _stepsIsem_steps
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule632 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule633 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule634 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule635 _stepsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule636 _stepsIprevMaxSimRefs
         _stepsOallFromToStates = rule637 _lhsIallFromToStates
         _stepsOallInitStates = rule638 _lhsIallInitStates
         _stepsOallVisitKinds = rule639 _lhsIallVisitKinds
         _stepsOallchildvisit = rule640 _lhsIallchildvisit
         _stepsOavisitdefs = rule641 _lhsIavisitdefs
         _stepsOavisituses = rule642 _lhsIavisituses
         _stepsOchildTypes = rule643 _lhsIchildTypes
         _stepsOchildintros = rule644 _lhsIchildintros
         _stepsOfollow = rule645 _lhsIfollow
         _stepsOindex = rule646 _lhsIindex
         _stepsOmrules = rule647 _lhsImrules
         _stepsOoptions = rule648 _lhsIoptions
         _stepsOprevMaxSimRefs = rule649 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule650 _lhsIruledefs
         _stepsOruleuses = rule651 _lhsIruleuses
         _stepsOuseParallel = rule652 _lhsIuseParallel
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule625 #-}
   {-# LINE 836 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule625 = \ ordered_ ->
                 {-# LINE 836 "./src-ag/ExecutionPlan2Caml.ag" #-}
                 VisitPure ordered_
                 {-# LINE 5120 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule626 #-}
   {-# LINE 1344 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule626 = \ ((_stepsIdefs) :: Set String) ((_stepsIlazyIntras) :: Set String) ordered_ ->
                     {-# LINE 1344 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     if ordered_
                     then _stepsIlazyIntras
                     else _stepsIdefs
                     {-# LINE 5128 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule627 #-}
   rule627 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule628 #-}
   rule628 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule629 #-}
   rule629 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule630 #-}
   rule630 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule631 #-}
   rule631 = \ ((_stepsIsem_steps) :: PP_Doc) ->
     _stepsIsem_steps
   {-# INLINE rule632 #-}
   rule632 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule633 #-}
   rule633 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule634 #-}
   rule634 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule635 #-}
   rule635 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule636 #-}
   rule636 = \ ((_stepsIprevMaxSimRefs) :: Int) ->
     _stepsIprevMaxSimRefs
   {-# INLINE rule637 #-}
   rule637 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule638 #-}
   rule638 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule641 #-}
   rule641 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule642 #-}
   rule642 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule643 #-}
   rule643 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule644 #-}
   rule644 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule645 #-}
   rule645 = \ ((_lhsIfollow) :: PP_Doc) ->
     _lhsIfollow
   {-# INLINE rule646 #-}
   rule646 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule647 #-}
   rule647 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule648 #-}
   rule648 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule649 #-}
   rule649 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule650 #-}
   rule650 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule651 #-}
   rule651 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule652 #-}
   rule652 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
{-# NOINLINE sem_VisitStep_Sim #-}
sem_VisitStep_Sim :: T_VisitSteps  -> T_VisitStep 
sem_VisitStep_Sim arg_steps_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _stepsOindex = rule653  ()
         _lhsOindex :: Int
         _lhsOindex = rule654 _lhsIindex
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule655 _lhsIprevMaxSimRefs _stepsIindex _useParallel
         _useParallel = rule656 _lhsIoptions _stepsIsize
         _lhsOdefs :: Set String
         _lhsOdefs = rule657 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule658 _stepsIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule659 _stepsIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule660 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule661 _stepsIruleUsage
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule662 _stepsIsem_steps
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule663 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule664 _stepsIvisitKinds
         _lhsOisLast :: Bool
         _lhsOisLast = rule665 _stepsIisLast
         _stepsOallFromToStates = rule666 _lhsIallFromToStates
         _stepsOallInitStates = rule667 _lhsIallInitStates
         _stepsOallVisitKinds = rule668 _lhsIallVisitKinds
         _stepsOallchildvisit = rule669 _lhsIallchildvisit
         _stepsOavisitdefs = rule670 _lhsIavisitdefs
         _stepsOavisituses = rule671 _lhsIavisituses
         _stepsOchildTypes = rule672 _lhsIchildTypes
         _stepsOchildintros = rule673 _lhsIchildintros
         _stepsOfollow = rule674 _lhsIfollow
         _stepsOkind = rule675 _lhsIkind
         _stepsOmrules = rule676 _lhsImrules
         _stepsOoptions = rule677 _lhsIoptions
         _stepsOprevMaxSimRefs = rule678 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule679 _lhsIruledefs
         _stepsOruleuses = rule680 _lhsIruleuses
         _stepsOuseParallel = rule681 _useParallel
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule653 #-}
   {-# LINE 885 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule653 = \  (_ :: ()) ->
                                     {-# LINE 885 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                     0
                                     {-# LINE 5264 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule654 #-}
   {-# LINE 886 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule654 = \ ((_lhsIindex) :: Int) ->
                                     {-# LINE 886 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                     _lhsIindex
                                     {-# LINE 5270 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule655 #-}
   {-# LINE 893 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule655 = \ ((_lhsIprevMaxSimRefs) :: Int) ((_stepsIindex) :: Int) _useParallel ->
                         {-# LINE 893 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         if _useParallel
                         then _lhsIprevMaxSimRefs `max` (_stepsIindex - 1)
                         else _lhsIprevMaxSimRefs
                         {-# LINE 5278 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule656 #-}
   {-# LINE 909 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule656 = \ ((_lhsIoptions) :: Options) ((_stepsIsize) :: Int) ->
                                         {-# LINE 909 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                         parallelInvoke _lhsIoptions && _stepsIsize > 1
                                         {-# LINE 5284 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule657 #-}
   rule657 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule658 #-}
   rule658 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule659 #-}
   rule659 = \ ((_stepsIlazyIntras) :: Set String) ->
     _stepsIlazyIntras
   {-# INLINE rule660 #-}
   rule660 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule661 #-}
   rule661 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule662 #-}
   rule662 = \ ((_stepsIsem_steps) :: PP_Doc) ->
     _stepsIsem_steps
   {-# INLINE rule663 #-}
   rule663 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule664 #-}
   rule664 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule665 #-}
   rule665 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule666 #-}
   rule666 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule667 #-}
   rule667 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule668 #-}
   rule668 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule669 #-}
   rule669 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule670 #-}
   rule670 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule671 #-}
   rule671 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule672 #-}
   rule672 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule673 #-}
   rule673 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule674 #-}
   rule674 = \ ((_lhsIfollow) :: PP_Doc) ->
     _lhsIfollow
   {-# INLINE rule675 #-}
   rule675 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule676 #-}
   rule676 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule677 #-}
   rule677 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule678 #-}
   rule678 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule679 #-}
   rule679 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule680 #-}
   rule680 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule681 #-}
   rule681 = \ _useParallel ->
     _useParallel
{-# NOINLINE sem_VisitStep_ChildIntro #-}
sem_VisitStep_ChildIntro :: (Identifier) -> T_VisitStep 
sem_VisitStep_ChildIntro arg_child_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _attachItf = rule682 _lhsIchildintros arg_child_
         _lhsOerrors :: Seq Error
         _lhsOdefs :: Set String
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         (_lhsOerrors,_sem_steps,_lhsOdefs,_lhsOuses) = rule683 _attachItf _lhsIkind
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule684 _lhsIfollow _sem_steps
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule685  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule686  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule687  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule688  ()
         _lhsOindex :: Int
         _lhsOindex = rule689 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule690 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule691 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule682 #-}
   {-# LINE 854 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule682 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) child_ ->
                                 {-# LINE 854 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Map.findWithDefault (error $ "Child " ++ show child_ ++ " not found") child_ _lhsIchildintros
                                 {-# LINE 5396 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule683 #-}
   {-# LINE 856 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule683 = \ _attachItf ((_lhsIkind) :: VisitKind) ->
                     {-# LINE 856 "./src-ag/ExecutionPlan2Caml.ag" #-}
                     case _attachItf     _lhsIkind of
                       Left e                   -> (Seq.singleton e, empty, Set.empty, Map.empty)
                       Right (code, defs, uses) -> (Seq.empty, code, defs, uses)
                     {-# LINE 5404 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule684 #-}
   {-# LINE 859 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule684 = \ ((_lhsIfollow) :: PP_Doc) _sem_steps ->
                                 {-# LINE 859 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _sem_steps     >-< _lhsIfollow
                                 {-# LINE 5410 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule685 #-}
   rule685 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule686 #-}
   rule686 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule687 #-}
   rule687 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule688 #-}
   rule688 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs

-- VisitSteps --------------------------------------------------
-- wrapper
data Inh_VisitSteps  = Inh_VisitSteps { allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitSteps :: (Map NontermIdent Int), allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitSteps :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitSteps :: (Map Identifier Type), childintros_Inh_VisitSteps :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), follow_Inh_VisitSteps :: (PP_Doc), index_Inh_VisitSteps :: (Int), kind_Inh_VisitSteps :: (VisitKind), mrules_Inh_VisitSteps :: (Map Identifier (VisitKind ->  Either Error PP_Doc)), options_Inh_VisitSteps :: (Options), prevMaxSimRefs_Inh_VisitSteps :: (Int), ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)), ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))), useParallel_Inh_VisitSteps :: (Bool) }
data Syn_VisitSteps  = Syn_VisitSteps { defs_Syn_VisitSteps :: (Set String), errors_Syn_VisitSteps :: (Seq Error), index_Syn_VisitSteps :: (Int), isLast_Syn_VisitSteps :: (Bool), lazyIntras_Syn_VisitSteps :: (Set String), prevMaxSimRefs_Syn_VisitSteps :: (Int), ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitSteps :: (Map Identifier Int), sem_steps_Syn_VisitSteps :: (PP_Doc), size_Syn_VisitSteps :: (Int), uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitSteps #-}
wrap_VisitSteps :: T_VisitSteps  -> Inh_VisitSteps  -> (Syn_VisitSteps )
wrap_VisitSteps (T_VisitSteps act) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
        (T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOuses _lhsOvisitKinds) <- return (inv_VisitSteps_s53 sem arg)
        return (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOuses _lhsOvisitKinds)
   )

-- cata
{-# NOINLINE sem_VisitSteps #-}
sem_VisitSteps :: VisitSteps  -> T_VisitSteps 
sem_VisitSteps list = Prelude.foldr sem_VisitSteps_Cons sem_VisitSteps_Nil (Prelude.map sem_VisitStep list)

-- semantic domain
newtype T_VisitSteps  = T_VisitSteps {
                                     attach_T_VisitSteps :: Identity (T_VisitSteps_s53 )
                                     }
newtype T_VisitSteps_s53  = C_VisitSteps_s53 {
                                             inv_VisitSteps_s53 :: (T_VisitSteps_v52 )
                                             }
data T_VisitSteps_s54  = C_VisitSteps_s54
type T_VisitSteps_v52  = (T_VisitSteps_vIn52 ) -> (T_VisitSteps_vOut52 )
data T_VisitSteps_vIn52  = T_VisitSteps_vIn52 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (PP_Doc) (Int) (VisitKind) (Map Identifier (VisitKind ->  Either Error PP_Doc)) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Bool)
data T_VisitSteps_vOut52  = T_VisitSteps_vOut52 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (Int) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitSteps_Cons #-}
sem_VisitSteps_Cons :: T_VisitStep  -> T_VisitSteps  -> T_VisitSteps 
sem_VisitSteps_Cons arg_hd_ arg_tl_ = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_VisitStep (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_tl_))
         (T_VisitStep_vOut49 _hdIdefs _hdIerrors _hdIindex _hdIisLast _hdIlazyIntras _hdIprevMaxSimRefs _hdIruleKinds _hdIruleUsage _hdIsem_steps _hdIuses _hdIvisitKinds) = inv_VisitStep_s50 _hdX50 (T_VisitStep_vIn49 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOfollow _hdOindex _hdOisLast _hdOkind _hdOmrules _hdOoptions _hdOprevMaxSimRefs _hdOruledefs _hdOruleuses _hdOuseParallel)
         (T_VisitSteps_vOut52 _tlIdefs _tlIerrors _tlIindex _tlIisLast _tlIlazyIntras _tlIprevMaxSimRefs _tlIruleKinds _tlIruleUsage _tlIsem_steps _tlIsize _tlIuses _tlIvisitKinds) = inv_VisitSteps_s53 _tlX53 (T_VisitSteps_vIn52 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOfollow _tlOindex _tlOkind _tlOmrules _tlOoptions _tlOprevMaxSimRefs _tlOruledefs _tlOruleuses _tlOuseParallel)
         _hdOfollow = rule692 _tlIsem_steps
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule693 _hdIsem_steps
         _lhsOsize :: Int
         _lhsOsize = rule694 _tlIsize
         _hdOindex = rule695 _lhsIindex
         _tlOindex = rule696 _lhsIindex
         _lhsOindex :: Int
         _lhsOindex = rule697 _tlIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule698  ()
         _hdOisLast = rule699 _tlIisLast
         _lhsOdefs :: Set String
         _lhsOdefs = rule700 _hdIdefs _tlIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule701 _hdIerrors _tlIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule702 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule703 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule704 _hdIruleUsage _tlIruleUsage
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule705 _hdIuses _tlIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule706 _hdIvisitKinds _tlIvisitKinds
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule707 _tlIprevMaxSimRefs
         _hdOallFromToStates = rule708 _lhsIallFromToStates
         _hdOallInitStates = rule709 _lhsIallInitStates
         _hdOallVisitKinds = rule710 _lhsIallVisitKinds
         _hdOallchildvisit = rule711 _lhsIallchildvisit
         _hdOavisitdefs = rule712 _lhsIavisitdefs
         _hdOavisituses = rule713 _lhsIavisituses
         _hdOchildTypes = rule714 _lhsIchildTypes
         _hdOchildintros = rule715 _lhsIchildintros
         _hdOkind = rule716 _lhsIkind
         _hdOmrules = rule717 _lhsImrules
         _hdOoptions = rule718 _lhsIoptions
         _hdOprevMaxSimRefs = rule719 _lhsIprevMaxSimRefs
         _hdOruledefs = rule720 _lhsIruledefs
         _hdOruleuses = rule721 _lhsIruleuses
         _hdOuseParallel = rule722 _lhsIuseParallel
         _tlOallFromToStates = rule723 _lhsIallFromToStates
         _tlOallInitStates = rule724 _lhsIallInitStates
         _tlOallVisitKinds = rule725 _lhsIallVisitKinds
         _tlOallchildvisit = rule726 _lhsIallchildvisit
         _tlOavisitdefs = rule727 _lhsIavisitdefs
         _tlOavisituses = rule728 _lhsIavisituses
         _tlOchildTypes = rule729 _lhsIchildTypes
         _tlOchildintros = rule730 _lhsIchildintros
         _tlOfollow = rule731 _lhsIfollow
         _tlOkind = rule732 _lhsIkind
         _tlOmrules = rule733 _lhsImrules
         _tlOoptions = rule734 _lhsIoptions
         _tlOprevMaxSimRefs = rule735 _hdIprevMaxSimRefs
         _tlOruledefs = rule736 _lhsIruledefs
         _tlOruleuses = rule737 _lhsIruleuses
         _tlOuseParallel = rule738 _lhsIuseParallel
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule692 #-}
   {-# LINE 844 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule692 = \ ((_tlIsem_steps) :: PP_Doc) ->
                            {-# LINE 844 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _tlIsem_steps
                            {-# LINE 5541 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule693 #-}
   {-# LINE 845 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule693 = \ ((_hdIsem_steps) :: PP_Doc) ->
                            {-# LINE 845 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _hdIsem_steps
                            {-# LINE 5547 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule694 #-}
   {-# LINE 876 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule694 = \ ((_tlIsize) :: Int) ->
                      {-# LINE 876 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      1 + _tlIsize
                      {-# LINE 5553 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule695 #-}
   {-# LINE 881 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule695 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 881 "./src-ag/ExecutionPlan2Caml.ag" #-}
                _lhsIindex
                {-# LINE 5559 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule696 #-}
   {-# LINE 882 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule696 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 882 "./src-ag/ExecutionPlan2Caml.ag" #-}
                1 + _lhsIindex
                {-# LINE 5565 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule697 #-}
   {-# LINE 883 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule697 = \ ((_tlIindex) :: Int) ->
                {-# LINE 883 "./src-ag/ExecutionPlan2Caml.ag" #-}
                _tlIindex
                {-# LINE 5571 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule698 #-}
   {-# LINE 902 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule698 = \  (_ :: ()) ->
                         {-# LINE 902 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         False
                         {-# LINE 5577 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule699 #-}
   {-# LINE 903 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule699 = \ ((_tlIisLast) :: Bool) ->
                         {-# LINE 903 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         _tlIisLast
                         {-# LINE 5583 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule700 #-}
   rule700 = \ ((_hdIdefs) :: Set String) ((_tlIdefs) :: Set String) ->
     _hdIdefs `Set.union` _tlIdefs
   {-# INLINE rule701 #-}
   rule701 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule702 #-}
   rule702 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule703 #-}
   rule703 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule704 #-}
   rule704 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule705 #-}
   rule705 = \ ((_hdIuses) :: Map String (Maybe NonLocalAttr)) ((_tlIuses) :: Map String (Maybe NonLocalAttr)) ->
     _hdIuses `Map.union` _tlIuses
   {-# INLINE rule706 #-}
   rule706 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule707 #-}
   rule707 = \ ((_tlIprevMaxSimRefs) :: Int) ->
     _tlIprevMaxSimRefs
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule716 #-}
   rule716 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule718 #-}
   rule718 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule721 #-}
   rule721 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule724 #-}
   rule724 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule725 #-}
   rule725 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule729 #-}
   rule729 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule730 #-}
   rule730 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule731 #-}
   rule731 = \ ((_lhsIfollow) :: PP_Doc) ->
     _lhsIfollow
   {-# INLINE rule732 #-}
   rule732 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule733 #-}
   rule733 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule734 #-}
   rule734 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule735 #-}
   rule735 = \ ((_hdIprevMaxSimRefs) :: Int) ->
     _hdIprevMaxSimRefs
   {-# INLINE rule736 #-}
   rule736 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule737 #-}
   rule737 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule738 #-}
   rule738 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
{-# NOINLINE sem_VisitSteps_Nil #-}
sem_VisitSteps_Nil ::  T_VisitSteps 
sem_VisitSteps_Nil  = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule739 _lhsIfollow
         _lhsOsize :: Int
         _lhsOsize = rule740  ()
         _lhsOisLast :: Bool
         _lhsOisLast = rule741  ()
         _lhsOdefs :: Set String
         _lhsOdefs = rule742  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule743  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule744  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule745  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule746  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule747  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule748  ()
         _lhsOindex :: Int
         _lhsOindex = rule749 _lhsIindex
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule750 _lhsIprevMaxSimRefs
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule739 #-}
   {-# LINE 846 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule739 = \ ((_lhsIfollow) :: PP_Doc) ->
                            {-# LINE 846 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIfollow
                            {-# LINE 5740 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule740 #-}
   {-# LINE 875 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule740 = \  (_ :: ()) ->
                      {-# LINE 875 "./src-ag/ExecutionPlan2Caml.ag" #-}
                      0
                      {-# LINE 5746 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule741 #-}
   {-# LINE 901 "./src-ag/ExecutionPlan2Caml.ag" #-}
   rule741 = \  (_ :: ()) ->
                         {-# LINE 901 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         True
                         {-# LINE 5752 "dist/build/ExecutionPlan2Caml.hs"#-}
   {-# INLINE rule742 #-}
   rule742 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule743 #-}
   rule743 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule744 #-}
   rule744 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule745 #-}
   rule745 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule746 #-}
   rule746 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule747 #-}
   rule747 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule748 #-}
   rule748 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule749 #-}
   rule749 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule750 #-}
   rule750 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs

-- Visits ------------------------------------------------------
-- wrapper
data Inh_Visits  = Inh_Visits { allFromToStates_Inh_Visits :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visits :: (Map NontermIdent Attributes), allInitStates_Inh_Visits :: (Map NontermIdent Int), allSynmap_Inh_Visits :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visits :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_Visits :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), allintramap_Inh_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visits :: (Map Identifier Type), childintros_Inh_Visits :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), con_Inh_Visits :: (ConstructorIdent), inhmap_Inh_Visits :: (Attributes), mrules_Inh_Visits :: (Map Identifier (VisitKind ->  Either Error PP_Doc)), nextVisits_Inh_Visits :: (Map StateIdentifier StateCtx), nt_Inh_Visits :: (NontermIdent), options_Inh_Visits :: (Options), params_Inh_Visits :: ([Identifier]), prevVisits_Inh_Visits :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visits :: (Map Identifier (Set String)), ruleuses_Inh_Visits :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visits :: (Attributes), terminaldefs_Inh_Visits :: (Set String) }
data Syn_Visits  = Syn_Visits { allvisits_Syn_Visits :: ([VisitStateState]), childvisit_Syn_Visits :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)), errors_Syn_Visits :: (Seq Error), fromToStates_Syn_Visits :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visits :: (Set String), ruleKinds_Syn_Visits :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visits :: (Map Identifier Int), sem_visit_Syn_Visits :: ( [(StateIdentifier,PP_Doc)] ), t_visits_Syn_Visits :: (PP_Doc), visitKinds_Syn_Visits :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visits :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visits :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visits #-}
wrap_Visits :: T_Visits  -> Inh_Visits  -> (Syn_Visits )
wrap_Visits (T_Visits act) (Inh_Visits _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visits_s56 sem arg)
        return (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_Visits #-}
sem_Visits :: Visits  -> T_Visits 
sem_Visits list = Prelude.foldr sem_Visits_Cons sem_Visits_Nil (Prelude.map sem_Visit list)

-- semantic domain
newtype T_Visits  = T_Visits {
                             attach_T_Visits :: Identity (T_Visits_s56 )
                             }
newtype T_Visits_s56  = C_Visits_s56 {
                                     inv_Visits_s56 :: (T_Visits_v55 )
                                     }
data T_Visits_s57  = C_Visits_s57
type T_Visits_v55  = (T_Visits_vIn55 ) -> (T_Visits_vOut55 )
data T_Visits_vIn55  = T_Visits_vIn55 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (ConstructorIdent) (Attributes) (Map Identifier (VisitKind ->  Either Error PP_Doc)) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visits_vOut55  = T_Visits_vOut55 ([VisitStateState]) (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) ( [(StateIdentifier,PP_Doc)] ) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visits_Cons #-}
sem_Visits_Cons :: T_Visit  -> T_Visits  -> T_Visits 
sem_Visits_Cons arg_hd_ arg_tl_ = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_Visit (arg_hd_))
         _tlX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_tl_))
         (T_Visit_vOut46 _hdIallvisits _hdIchildvisit _hdIerrors _hdIfromToStates _hdIintramap _hdIlazyIntras _hdIruleKinds _hdIruleUsage _hdIsem_visit _hdIt_visits _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_Visit_s47 _hdX47 (T_Visit_vIn46 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOcon _hdOinhmap _hdOmrules _hdOnextVisits _hdOnt _hdOoptions _hdOparams _hdOprevVisits _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs)
         (T_Visits_vOut55 _tlIallvisits _tlIchildvisit _tlIerrors _tlIfromToStates _tlIintramap _tlIlazyIntras _tlIruleKinds _tlIruleUsage _tlIsem_visit _tlIt_visits _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_Visits_s56 _tlX56 (T_Visits_vIn55 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOcon _tlOinhmap _tlOmrules _tlOnextVisits _tlOnt _tlOoptions _tlOparams _tlOprevVisits _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule751 _hdIallvisits _tlIallvisits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule752 _hdIchildvisit _tlIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule753 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule754 _hdIfromToStates _tlIfromToStates
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule755 _hdIintramap _tlIintramap
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule756 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule757 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule758 _hdIruleUsage _tlIruleUsage
         _lhsOsem_visit ::  [(StateIdentifier,PP_Doc)] 
         _lhsOsem_visit = rule759 _hdIsem_visit _tlIsem_visit
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule760 _hdIt_visits _tlIt_visits
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule761 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule762 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule763 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule764 _lhsIallFromToStates
         _hdOallInhmap = rule765 _lhsIallInhmap
         _hdOallInitStates = rule766 _lhsIallInitStates
         _hdOallSynmap = rule767 _lhsIallSynmap
         _hdOallVisitKinds = rule768 _lhsIallVisitKinds
         _hdOallchildvisit = rule769 _lhsIallchildvisit
         _hdOallintramap = rule770 _lhsIallintramap
         _hdOavisitdefs = rule771 _lhsIavisitdefs
         _hdOavisituses = rule772 _lhsIavisituses
         _hdOchildTypes = rule773 _lhsIchildTypes
         _hdOchildintros = rule774 _lhsIchildintros
         _hdOcon = rule775 _lhsIcon
         _hdOinhmap = rule776 _lhsIinhmap
         _hdOmrules = rule777 _lhsImrules
         _hdOnextVisits = rule778 _lhsInextVisits
         _hdOnt = rule779 _lhsInt
         _hdOoptions = rule780 _lhsIoptions
         _hdOparams = rule781 _lhsIparams
         _hdOprevVisits = rule782 _lhsIprevVisits
         _hdOruledefs = rule783 _lhsIruledefs
         _hdOruleuses = rule784 _lhsIruleuses
         _hdOsynmap = rule785 _lhsIsynmap
         _hdOterminaldefs = rule786 _lhsIterminaldefs
         _tlOallFromToStates = rule787 _lhsIallFromToStates
         _tlOallInhmap = rule788 _lhsIallInhmap
         _tlOallInitStates = rule789 _lhsIallInitStates
         _tlOallSynmap = rule790 _lhsIallSynmap
         _tlOallVisitKinds = rule791 _lhsIallVisitKinds
         _tlOallchildvisit = rule792 _lhsIallchildvisit
         _tlOallintramap = rule793 _lhsIallintramap
         _tlOavisitdefs = rule794 _lhsIavisitdefs
         _tlOavisituses = rule795 _lhsIavisituses
         _tlOchildTypes = rule796 _lhsIchildTypes
         _tlOchildintros = rule797 _lhsIchildintros
         _tlOcon = rule798 _lhsIcon
         _tlOinhmap = rule799 _lhsIinhmap
         _tlOmrules = rule800 _lhsImrules
         _tlOnextVisits = rule801 _lhsInextVisits
         _tlOnt = rule802 _lhsInt
         _tlOoptions = rule803 _lhsIoptions
         _tlOparams = rule804 _lhsIparams
         _tlOprevVisits = rule805 _lhsIprevVisits
         _tlOruledefs = rule806 _lhsIruledefs
         _tlOruleuses = rule807 _lhsIruleuses
         _tlOsynmap = rule808 _lhsIsynmap
         _tlOterminaldefs = rule809 _lhsIterminaldefs
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule751 #-}
   rule751 = \ ((_hdIallvisits) ::  VisitStateState ) ((_tlIallvisits) :: [VisitStateState]) ->
     _hdIallvisits : _tlIallvisits
   {-# INLINE rule752 #-}
   rule752 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule753 #-}
   rule753 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule754 #-}
   rule754 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule755 #-}
   rule755 = \ ((_hdIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ((_tlIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _hdIintramap `uwMapUnion` _tlIintramap
   {-# INLINE rule756 #-}
   rule756 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule757 #-}
   rule757 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule758 #-}
   rule758 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule759 #-}
   rule759 = \ ((_hdIsem_visit) ::   (StateIdentifier,PP_Doc)  ) ((_tlIsem_visit) ::  [(StateIdentifier,PP_Doc)] ) ->
     _hdIsem_visit : _tlIsem_visit
   {-# INLINE rule760 #-}
   rule760 = \ ((_hdIt_visits) :: PP_Doc) ((_tlIt_visits) :: PP_Doc) ->
     _hdIt_visits >-< _tlIt_visits
   {-# INLINE rule761 #-}
   rule761 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule762 #-}
   rule762 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule763 #-}
   rule763 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule764 #-}
   rule764 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule765 #-}
   rule765 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule766 #-}
   rule766 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule767 #-}
   rule767 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule768 #-}
   rule768 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule769 #-}
   rule769 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule770 #-}
   rule770 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule771 #-}
   rule771 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule772 #-}
   rule772 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule773 #-}
   rule773 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule774 #-}
   rule774 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule775 #-}
   rule775 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule776 #-}
   rule776 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule777 #-}
   rule777 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule778 #-}
   rule778 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule779 #-}
   rule779 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule780 #-}
   rule780 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule781 #-}
   rule781 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule782 #-}
   rule782 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule783 #-}
   rule783 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule784 #-}
   rule784 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule785 #-}
   rule785 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule786 #-}
   rule786 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
   {-# INLINE rule787 #-}
   rule787 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule788 #-}
   rule788 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule789 #-}
   rule789 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule790 #-}
   rule790 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule791 #-}
   rule791 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule792 #-}
   rule792 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
     _lhsIallchildvisit
   {-# INLINE rule793 #-}
   rule793 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule794 #-}
   rule794 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule795 #-}
   rule795 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule796 #-}
   rule796 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule797 #-}
   rule797 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule798 #-}
   rule798 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule799 #-}
   rule799 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule800 #-}
   rule800 = \ ((_lhsImrules) :: Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule801 #-}
   rule801 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule802 #-}
   rule802 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule803 #-}
   rule803 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule804 #-}
   rule804 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule805 #-}
   rule805 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule806 #-}
   rule806 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule807 #-}
   rule807 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule808 #-}
   rule808 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule809 #-}
   rule809 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
{-# NOINLINE sem_Visits_Nil #-}
sem_Visits_Nil ::  T_Visits 
sem_Visits_Nil  = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule810  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         _lhsOchildvisit = rule811  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule812  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule813  ()
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule814  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule815  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule816  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule817  ()
         _lhsOsem_visit ::  [(StateIdentifier,PP_Doc)] 
         _lhsOsem_visit = rule818  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule819  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule820  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule821  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule822  ()
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule810 #-}
   rule810 = \  (_ :: ()) ->
     []
   {-# INLINE rule811 #-}
   rule811 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule812 #-}
   rule812 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule813 #-}
   rule813 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule814 #-}
   rule814 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule815 #-}
   rule815 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule816 #-}
   rule816 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule817 #-}
   rule817 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule818 #-}
   rule818 = \  (_ :: ()) ->
     []
   {-# INLINE rule819 #-}
   rule819 = \  (_ :: ()) ->
     empty
   {-# INLINE rule820 #-}
   rule820 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule821 #-}
   rule821 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule822 #-}
   rule822 = \  (_ :: ()) ->
     Map.empty
