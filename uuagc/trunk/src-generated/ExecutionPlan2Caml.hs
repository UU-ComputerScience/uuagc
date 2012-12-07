

-- UUAGC 0.9.42.2 (src-ag/ExecutionPlan2Caml.ag)
module ExecutionPlan2Caml where
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
{-# LINE 32 "dist/build/ExecutionPlan2Caml.hs" #-}

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
{-# LINE 46 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 53 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 59 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 65 "dist/build/ExecutionPlan2Caml.hs" #-}
{-# LINE 173 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 89 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 286 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 111 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 359 "./src-ag/ExecutionPlan2Caml.ag" #-}

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

{-# LINE 133 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 426 "./src-ag/ExecutionPlan2Caml.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 137 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 462 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 177 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 519 "./src-ag/ExecutionPlan2Caml.ag" #-}

type_caller_visit nt v = "c_" >|< nt >|< "_v" >|< v
con_visit nt v = "C_" >|< nt >|< "_v" >|< v

-- field names
nm_inh nt v  = "inh_" >|< nt >|< "_v" >|< v
nm_cont nt v = "cont_" >|< nt >|< "_v" >|< v
{-# LINE 187 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 565 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 205 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 778 "./src-ag/ExecutionPlan2Caml.ag" #-}

nm_visit v = "__v" >|< v
nm_k st = "__k" >|< st
nm_st st = "__st" >|< st

mklets :: (PP b, PP c) => [b] -> c -> PP_Doc
mklets defs body = res where
  ppLet def = "let" >#< def >#< "in"
  res = vlist (map ppLet defs) >-< body
{-# LINE 217 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 820 "./src-ag/ExecutionPlan2Caml.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 226 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 961 "./src-ag/ExecutionPlan2Caml.ag" #-}

stname :: Identifier -> Int -> String
stname child st = "_" ++ getName child ++ "X" ++ show st

-- should actually return some conversion info
compatibleAttach :: VisitKind -> NontermIdent -> Options -> Bool
compatibleAttach _ _ _ = True
{-# LINE 236 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1028 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 255 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1104 "./src-ag/ExecutionPlan2Caml.ag" #-}

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
{-# LINE 288 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1201 "./src-ag/ExecutionPlan2Caml.ag" #-}

contNm = text "__cont_"
inpsNm = text "__inps_"

-- a `compatibleKind` b  means: can kind b be invoked from a
compatibleKind :: VisitKind -> VisitKind -> Bool
compatibleKind _              _             = True

compatibleRule :: VisitKind -> Bool -> Bool
compatibleRule (VisitPure _) False = False
compatibleRule _             _     = True
{-# LINE 302 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1226 "./src-ag/ExecutionPlan2Caml.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 307 "dist/build/ExecutionPlan2Caml.hs" #-}

{-# LINE 1249 "./src-ag/ExecutionPlan2Caml.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 316 "dist/build/ExecutionPlan2Caml.hs" #-}
-- EChild ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInitStates        : Map NontermIdent Int
         con                  : ConstructorIdent
         mainFile             : String
         mainName             : String
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         argnamesw            :  PP_Doc 
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         sigs                 : [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         terminaldefs         : Set String
   alternatives:
      alternative EChild:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         child hasAround      : {Bool}
         child merges         : {Maybe [Identifier]}
         child isMerged       : {Bool}
         visit 0:
            local tpDocFor    : _
            local tpDocDefor  : _
            local fieldNm     : _
            local childNm     : _
            local field       : _
            local isDefor     : _
            local valcode     : _
            local aroundcode  : _
            local introcode   : _
            local nt          : _
            local initSt      : _
      alternative ETerm:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local tpDocFor    : _
            local tpDocDefor  : _
            local fieldNm     : _
            local childNm     : _
            local field       : _
-}
-- cata
sem_EChild :: EChild ->
              T_EChild
sem_EChild (EChild _name _tp _kind _hasAround _merges _isMerged) =
    (sem_EChild_EChild _name _tp _kind _hasAround _merges _isMerged)
sem_EChild (ETerm _name _tp) =
    (sem_EChild_ETerm _name _tp)
-- semantic domain
newtype T_EChild = T_EChild ((Map NontermIdent Int) ->
                             ConstructorIdent ->
                             String ->
                             String ->
                             NontermIdent ->
                             Options ->
                             ( ( PP_Doc ),(Map Identifier Type),(Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]),(Set String)))
data Inh_EChild = Inh_EChild {allInitStates_Inh_EChild :: (Map NontermIdent Int),con_Inh_EChild :: ConstructorIdent,mainFile_Inh_EChild :: String,mainName_Inh_EChild :: String,nt_Inh_EChild :: NontermIdent,options_Inh_EChild :: Options}
data Syn_EChild = Syn_EChild {argnamesw_Syn_EChild :: ( PP_Doc ),childTypes_Syn_EChild :: (Map Identifier Type),childintros_Syn_EChild :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),sigs_Syn_EChild :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]),terminaldefs_Syn_EChild :: (Set String)}
wrap_EChild :: T_EChild ->
               Inh_EChild ->
               Syn_EChild
wrap_EChild (T_EChild sem) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) =
    (let ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs) = sem _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions
     in  (Syn_EChild _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs))
sem_EChild_EChild :: Identifier ->
                     Type ->
                     ChildKind ->
                     Bool ->
                     (Maybe [Identifier]) ->
                     Bool ->
                     T_EChild
sem_EChild_EChild name_ tp_ kind_ hasAround_ merges_ isMerged_ =
    (T_EChild (\ _lhsIallInitStates
                 _lhsIcon
                 _lhsImainFile
                 _lhsImainName
                 _lhsInt
                 _lhsIoptions ->
                   (let _lhsOsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                        _lhsOargnamesw :: ( PP_Doc )
                        _lhsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _lhsOchildTypes :: (Map Identifier Type)
                        _lhsOterminaldefs :: (Set String)
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 276, column 7)
                        _tpDocFor =
                            ({-# LINE 276 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             ppTp $ removeDeforested tp_
                             {-# LINE 409 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 277, column 7)
                        _tpDocDefor =
                            ({-# LINE 277 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             ppTp $ forceDeforested tp_
                             {-# LINE 415 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 278, column 7)
                        _fieldNm =
                            ({-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text $ recordFieldname _lhsInt _lhsIcon name_
                             {-# LINE 421 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 279, column 7)
                        _childNm =
                            ({-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text (fieldname name_)
                             {-# LINE 427 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 280, column 7)
                        _field =
                            ({-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             (_fieldNm    , _childNm    , _tpDocDefor    , _tpDocFor    )
                             {-# LINE 433 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 281, column 13)
                        _lhsOsigs =
                            ({-# LINE 281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             case kind_ of
                               ChildAttr -> []
                               _         -> [_field    ]
                             {-# LINE 441 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 394, column 12)
                        _lhsOargnamesw =
                            ({-# LINE 394 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             case kind_ of
                               ChildSyntax     -> "(" >#< prefix _lhsIoptions >|< _nt     >#< name_ >|< "_" >#< ")"
                               ChildAttr       -> empty
                               ChildReplace tp -> "(" >#< prefix _lhsIoptions >|< extractNonterminal tp >#< name_ >|< "_" >#< ")"
                             {-# LINE 450 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 921, column 12)
                        _lhsOchildintros =
                            ({-# LINE 921 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.singleton name_ _introcode
                             {-# LINE 456 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 922, column 12)
                        _isDefor =
                            ({-# LINE 922 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             case tp_ of
                               NT _ _ defor -> defor
                               _            -> False
                             {-# LINE 464 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 925, column 12)
                        _valcode =
                            ({-# LINE 925 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             case kind_ of
                               ChildSyntax -> name_ >|< "_"
                               ChildAttr   ->
                                              let head | not _isDefor     = if lateHigherOrderBinding _lhsIoptions
                                                                            then lateSemNtLabel _nt     >#< lhsname True idLateBindingAttr
                                                                            else prefix _lhsIoptions >|< _nt
                                                       | otherwise        = empty
                                              in pp_parens (head >#< instname name_)
                               ChildReplace _ ->
                                                 pp_parens (instname name_ >#< name_ >|< "_")
                             {-# LINE 479 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 936, column 12)
                        _aroundcode =
                            ({-# LINE 936 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             if hasAround_
                             then locname name_ >|< "_around"
                             else empty
                             {-# LINE 487 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 939, column 12)
                        _introcode =
                            ({-# LINE 939 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                                                           else Map.insert (lhsname True idLateBindingAttr) (Just $ AttrInh _LHS idLateBindingAttr)
                                                                         ) $
                                                                         ( if hasAround_
                                                                           then Map.insert (locname (name_) ++ "_around") Nothing
                                                                           else id
                                                                         ) $ Map.empty
                                                        ChildReplace _ -> Map.singleton (instname name_) Nothing
                                                        ChildSyntax    -> Map.empty
                                                    )
                                         else Left $ IncompatibleAttachKind name_ kind
                             {-# LINE 512 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 959, column 12)
                        _nt =
                            ({-# LINE 959 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             extractNonterminal tp_
                             {-# LINE 518 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 1421, column 3)
                        _lhsOchildTypes =
                            ({-# LINE 1421 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.singleton name_ tp_
                             {-# LINE 524 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 1465, column 3)
                        _initSt =
                            ({-# LINE 1465 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                             {-# LINE 530 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1260, column 42)
                        _lhsOterminaldefs =
                            ({-# LINE 1260 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Set.empty
                             {-# LINE 536 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                    in  ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs))))
sem_EChild_ETerm :: Identifier ->
                    Type ->
                    T_EChild
sem_EChild_ETerm name_ tp_ =
    (T_EChild (\ _lhsIallInitStates
                 _lhsIcon
                 _lhsImainFile
                 _lhsImainName
                 _lhsInt
                 _lhsIoptions ->
                   (let _lhsOsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                        _lhsOargnamesw :: ( PP_Doc )
                        _lhsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _lhsOterminaldefs :: (Set String)
                        _lhsOchildTypes :: (Map Identifier Type)
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 276, column 7)
                        _tpDocFor =
                            ({-# LINE 276 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             ppTp $ removeDeforested tp_
                             {-# LINE 558 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 277, column 7)
                        _tpDocDefor =
                            ({-# LINE 277 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             ppTp $ forceDeforested tp_
                             {-# LINE 564 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 278, column 7)
                        _fieldNm =
                            ({-# LINE 278 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text $ recordFieldname _lhsInt _lhsIcon name_
                             {-# LINE 570 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 279, column 7)
                        _childNm =
                            ({-# LINE 279 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text (fieldname name_)
                             {-# LINE 576 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 280, column 7)
                        _field =
                            ({-# LINE 280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             (_fieldNm    , _childNm    , _tpDocDefor    , _tpDocFor    )
                             {-# LINE 582 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 284, column 13)
                        _lhsOsigs =
                            ({-# LINE 284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             [_field    ]
                             {-# LINE 588 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 398, column 12)
                        _lhsOargnamesw =
                            ({-# LINE 398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             text $ fieldname name_
                             {-# LINE 594 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 920, column 12)
                        _lhsOchildintros =
                            ({-# LINE 920 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.singleton name_ (\_ -> Right (empty, Set.empty, Map.empty))
                             {-# LINE 600 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 1263, column 3)
                        _lhsOterminaldefs =
                            ({-# LINE 1263 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Set.singleton $ fieldname name_
                             {-# LINE 606 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- "./src-ag/ExecutionPlan2Caml.ag"(line 1421, column 3)
                        _lhsOchildTypes =
                            ({-# LINE 1421 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.singleton name_ tp_
                             {-# LINE 612 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                    in  ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs))))
-- EChildren ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInitStates        : Map NontermIdent Int
         con                  : ConstructorIdent
         mainFile             : String
         mainName             : String
         nt                   : NontermIdent
         options              : Options
      synthesized attributes:
         argnamesw            : [PP_Doc]
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         sigs                 : [(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]
         terminaldefs         : Set String
   alternatives:
      alternative Cons:
         child hd             : EChild 
         child tl             : EChildren 
      alternative Nil:
-}
-- cata
sem_EChildren :: EChildren ->
                 T_EChildren
sem_EChildren list =
    (Prelude.foldr sem_EChildren_Cons sem_EChildren_Nil (Prelude.map sem_EChild list))
-- semantic domain
newtype T_EChildren = T_EChildren ((Map NontermIdent Int) ->
                                   ConstructorIdent ->
                                   String ->
                                   String ->
                                   NontermIdent ->
                                   Options ->
                                   ( ([PP_Doc]),(Map Identifier Type),(Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]),(Set String)))
data Inh_EChildren = Inh_EChildren {allInitStates_Inh_EChildren :: (Map NontermIdent Int),con_Inh_EChildren :: ConstructorIdent,mainFile_Inh_EChildren :: String,mainName_Inh_EChildren :: String,nt_Inh_EChildren :: NontermIdent,options_Inh_EChildren :: Options}
data Syn_EChildren = Syn_EChildren {argnamesw_Syn_EChildren :: ([PP_Doc]),childTypes_Syn_EChildren :: (Map Identifier Type),childintros_Syn_EChildren :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),sigs_Syn_EChildren :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)]),terminaldefs_Syn_EChildren :: (Set String)}
wrap_EChildren :: T_EChildren ->
                  Inh_EChildren ->
                  Syn_EChildren
wrap_EChildren (T_EChildren sem) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions) =
    (let ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs) = sem _lhsIallInitStates _lhsIcon _lhsImainFile _lhsImainName _lhsInt _lhsIoptions
     in  (Syn_EChildren _lhsOargnamesw _lhsOchildTypes _lhsOchildintros _lhsOsigs _lhsOterminaldefs))
sem_EChildren_Cons :: T_EChild ->
                      T_EChildren ->
                      T_EChildren
sem_EChildren_Cons (T_EChild hd_) (T_EChildren tl_) =
    (T_EChildren (\ _lhsIallInitStates
                    _lhsIcon
                    _lhsImainFile
                    _lhsImainName
                    _lhsInt
                    _lhsIoptions ->
                      (let _lhsOargnamesw :: ([PP_Doc])
                           _lhsOchildTypes :: (Map Identifier Type)
                           _lhsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _lhsOsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                           _lhsOterminaldefs :: (Set String)
                           _hdOallInitStates :: (Map NontermIdent Int)
                           _hdOcon :: ConstructorIdent
                           _hdOmainFile :: String
                           _hdOmainName :: String
                           _hdOnt :: NontermIdent
                           _hdOoptions :: Options
                           _tlOallInitStates :: (Map NontermIdent Int)
                           _tlOcon :: ConstructorIdent
                           _tlOmainFile :: String
                           _tlOmainName :: String
                           _tlOnt :: NontermIdent
                           _tlOoptions :: Options
                           _hdIargnamesw :: ( PP_Doc )
                           _hdIchildTypes :: (Map Identifier Type)
                           _hdIchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _hdIsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                           _hdIterminaldefs :: (Set String)
                           _tlIargnamesw :: ([PP_Doc])
                           _tlIchildTypes :: (Map Identifier Type)
                           _tlIchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _tlIsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                           _tlIterminaldefs :: (Set String)
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 391, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 391 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _hdIargnamesw : _tlIargnamesw
                                {-# LINE 699 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1414, column 40)
                           _lhsOchildTypes =
                               ({-# LINE 1414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _hdIchildTypes `mappend` _tlIchildTypes
                                {-# LINE 705 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 911, column 21)
                           _lhsOchildintros =
                               ({-# LINE 911 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _hdIchildintros `Map.union` _tlIchildintros
                                {-# LINE 711 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 272, column 34)
                           _lhsOsigs =
                               ({-# LINE 272 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _hdIsigs ++ _tlIsigs
                                {-# LINE 717 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1260, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 1260 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _hdIterminaldefs `Set.union` _tlIterminaldefs
                                {-# LINE 723 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOallInitStates =
                               ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 729 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOcon =
                               ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIcon
                                {-# LINE 735 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOmainFile =
                               ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImainFile
                                {-# LINE 741 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOmainName =
                               ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImainName
                                {-# LINE 747 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOnt =
                               ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsInt
                                {-# LINE 753 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOoptions =
                               ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIoptions
                                {-# LINE 759 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOallInitStates =
                               ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 765 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOcon =
                               ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIcon
                                {-# LINE 771 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOmainFile =
                               ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImainFile
                                {-# LINE 777 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOmainName =
                               ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImainName
                                {-# LINE 783 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOnt =
                               ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsInt
                                {-# LINE 789 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOoptions =
                               ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIoptions
                                {-# LINE 795 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           ( _hdIargnamesw,_hdIchildTypes,_hdIchildintros,_hdIsigs,_hdIterminaldefs) =
                               hd_ _hdOallInitStates _hdOcon _hdOmainFile _hdOmainName _hdOnt _hdOoptions
                           ( _tlIargnamesw,_tlIchildTypes,_tlIchildintros,_tlIsigs,_tlIterminaldefs) =
                               tl_ _tlOallInitStates _tlOcon _tlOmainFile _tlOmainName _tlOnt _tlOoptions
                       in  ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs))))
sem_EChildren_Nil :: T_EChildren
sem_EChildren_Nil =
    (T_EChildren (\ _lhsIallInitStates
                    _lhsIcon
                    _lhsImainFile
                    _lhsImainName
                    _lhsInt
                    _lhsIoptions ->
                      (let _lhsOargnamesw :: ([PP_Doc])
                           _lhsOchildTypes :: (Map Identifier Type)
                           _lhsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _lhsOsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                           _lhsOterminaldefs :: (Set String)
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 391, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 391 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                []
                                {-# LINE 819 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1414, column 40)
                           _lhsOchildTypes =
                               ({-# LINE 1414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                mempty
                                {-# LINE 825 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 911, column 21)
                           _lhsOchildintros =
                               ({-# LINE 911 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.empty
                                {-# LINE 831 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 272, column 34)
                           _lhsOsigs =
                               ({-# LINE 272 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                []
                                {-# LINE 837 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1260, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 1260 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Set.empty
                                {-# LINE 843 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                       in  ( _lhsOargnamesw,_lhsOchildTypes,_lhsOchildintros,_lhsOsigs,_lhsOterminaldefs))))
-- ENonterminal ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainFile             : String
         mainName             : String
         options              : Options
         synmap               : Map NontermIdent Attributes
         typeSyns             : TypeSyns
         wrappers             : Set NontermIdent
      synthesized attributes:
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         code                 : PP_Doc
         datas                : PP_Doc
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         initStates           : Map NontermIdent Int
         modules              : PP_Doc
         semFunBndDefs        : Seq PP_Doc
         semFunBndTps         : Seq PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative ENonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child classCtxs      : {ClassContext}
         child initial        : {StateIdentifier}
         child initialv       : {Maybe VisitIdentifier}
         child nextVisits     : {Map StateIdentifier StateCtx}
         child prevVisits     : {Map StateIdentifier StateCtx}
         child prods          : EProductions 
         child recursive      : {Bool}
         child hoInfo         : {HigherOrderInfo}
         visit 0:
            local hasWrapper  : _
            local t_params    : _
            local aliasPre    : _
            local aliasMod    : _
            local datatypeNt  : _
            local datatypeCon : _
            local moduleDecl  : _
            local datatypeProds : _
            local fsemname    : _
            local semname     : _
            local frecarg     : _
            local sem_param_tp : _
            local sem_res_tp  : _
            local sem_tp      : _
            local o_sigs      : _
            local sem_nt_body : _
            local sem_nt      : _
            local allstates   : _
            local stvisits    : _
            local t_type      : _
            local t_c_params  : _
            local t_init      : _
            local t_states    : _
            local c_states    : _
            local wr_inh      : _
            local wr_syn      : _
            local genwrap     : _
            local inhAttrs    : _
            local wr_inhs     : _
            local wr_inhs1    : _
            local wr_filter   : _
            local wr_syns     : _
            local wrapname    : _
            local inhname     : _
            local synname     : _
            local firstVisitInfo : _
            local wrapArgSemTp : _
            local wrapArgInhTp : _
            local wrapArgPats : _
            local wrapResTp   : _
            local wrapper     : _
            local wrapperPreamble : _
            local wrapperBody : _
            local semFunBndDef : _
            local semFunBndTp : _
            local semFunBndNm : _
            local ntType      : _
-}
-- cata
sem_ENonterminal :: ENonterminal ->
                    T_ENonterminal
sem_ENonterminal (ENonterminal _nt _params _classCtxs _initial _initialv _nextVisits _prevVisits _prods _recursive _hoInfo) =
    (sem_ENonterminal_ENonterminal _nt _params _classCtxs _initial _initialv _nextVisits _prevVisits (sem_EProductions _prods) _recursive _hoInfo)
-- semantic domain
newtype T_ENonterminal = T_ENonterminal ((Map VisitIdentifier (Int,Int)) ->
                                         (Map NontermIdent Int) ->
                                         (Map VisitIdentifier VisitKind) ->
                                         (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         (Map NontermIdent Attributes) ->
                                         (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                         String ->
                                         String ->
                                         Options ->
                                         (Map NontermIdent Attributes) ->
                                         TypeSyns ->
                                         (Set NontermIdent) ->
                                         ( (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),PP_Doc,PP_Doc,(Seq Error),(Map VisitIdentifier (Int,Int)),(Map NontermIdent Int),PP_Doc,(Seq PP_Doc),(Seq PP_Doc),(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_ENonterminal = Inh_ENonterminal {allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_ENonterminal :: (Map NontermIdent Int),allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_ENonterminal :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)),inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes),localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainFile_Inh_ENonterminal :: String,mainName_Inh_ENonterminal :: String,options_Inh_ENonterminal :: Options,synmap_Inh_ENonterminal :: (Map NontermIdent Attributes),typeSyns_Inh_ENonterminal :: TypeSyns,wrappers_Inh_ENonterminal :: (Set NontermIdent)}
data Syn_ENonterminal = Syn_ENonterminal {childvisit_Syn_ENonterminal :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),code_Syn_ENonterminal :: PP_Doc,datas_Syn_ENonterminal :: PP_Doc,errors_Syn_ENonterminal :: (Seq Error),fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)),initStates_Syn_ENonterminal :: (Map NontermIdent Int),modules_Syn_ENonterminal :: PP_Doc,semFunBndDefs_Syn_ENonterminal :: (Seq PP_Doc),semFunBndTps_Syn_ENonterminal :: (Seq PP_Doc),visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind),visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier))}
wrap_ENonterminal :: T_ENonterminal ->
                     Inh_ENonterminal ->
                     Syn_ENonterminal
wrap_ENonterminal (T_ENonterminal sem) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) =
    (let ( _lhsOchildvisit,_lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOfromToStates,_lhsOinitStates,_lhsOmodules,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers
     in  (Syn_ENonterminal _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_ENonterminal_ENonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 ClassContext ->
                                 StateIdentifier ->
                                 (Maybe VisitIdentifier) ->
                                 (Map StateIdentifier StateCtx) ->
                                 (Map StateIdentifier StateCtx) ->
                                 T_EProductions ->
                                 Bool ->
                                 HigherOrderInfo ->
                                 T_ENonterminal
sem_ENonterminal_ENonterminal nt_ params_ classCtxs_ initial_ initialv_ nextVisits_ prevVisits_ (T_EProductions prods_) recursive_ hoInfo_ =
    (T_ENonterminal (\ _lhsIallFromToStates
                       _lhsIallInitStates
                       _lhsIallVisitKinds
                       _lhsIallchildvisit
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIinhmap
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsIoptions
                       _lhsIsynmap
                       _lhsItypeSyns
                       _lhsIwrappers ->
                         (let _prodsOrename :: Bool
                              _prodsOnt :: NontermIdent
                              _prodsOparams :: ([Identifier])
                              _lhsOdatas :: PP_Doc
                              _lhsOcode :: PP_Doc
                              _lhsOmodules :: PP_Doc
                              _prodsOinhmap :: Attributes
                              _prodsOsynmap :: Attributes
                              _prodsOallInhmap :: (Map NontermIdent Attributes)
                              _prodsOallSynmap :: (Map NontermIdent Attributes)
                              _lhsOsemFunBndDefs :: (Seq PP_Doc)
                              _lhsOsemFunBndTps :: (Seq PP_Doc)
                              _prodsOinitial :: StateIdentifier
                              _prodsOallstates :: ([StateIdentifier])
                              _prodsOnextVisits :: (Map StateIdentifier StateCtx)
                              _prodsOprevVisits :: (Map StateIdentifier StateCtx)
                              _prodsOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _lhsOinitStates :: (Map NontermIdent Int)
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                              _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                              _prodsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                              _prodsOallInitStates :: (Map NontermIdent Int)
                              _prodsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                              _prodsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _prodsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _prodsOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _prodsOmainFile :: String
                              _prodsOmainName :: String
                              _prodsOntType :: Type
                              _prodsOoptions :: Options
                              _prodsIallvisits :: ([VisitStateState])
                              _prodsIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _prodsIcount :: Int
                              _prodsIdatatype :: ([PP_Doc])
                              _prodsIdatatype_call :: ([PP_Doc])
                              _prodsIdatatype_con :: ([PP_Doc])
                              _prodsIerrors :: (Seq Error)
                              _prodsIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _prodsIsemFunBndDefs :: (Seq PP_Doc)
                              _prodsIsemFunBndTps :: (Seq PP_Doc)
                              _prodsIsem_nt :: PP_Doc
                              _prodsIsem_prod :: PP_Doc
                              _prodsIt_visits :: PP_Doc
                              _prodsIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _prodsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _prodsIvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 76, column 3)
                              _prodsOrename =
                                  ({-# LINE 76 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   rename _lhsIoptions
                                   {-# LINE 1047 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 84, column 3)
                              _prodsOnt =
                                  ({-# LINE 84 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   nt_
                                   {-# LINE 1053 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 94, column 3)
                              _prodsOparams =
                                  ({-# LINE 94 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   params_
                                   {-# LINE 1059 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 113, column 3)
                              _lhsOdatas =
                                  ({-# LINE 113 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1086 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 136, column 3)
                              _lhsOcode =
                                  ({-# LINE 136 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1112 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 161, column 3)
                              _lhsOmodules =
                                  ({-# LINE 161 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _moduleDecl
                                   {-# LINE 1118 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 163, column 3)
                              _hasWrapper =
                                  ({-# LINE 163 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   nt_ `Set.member` _lhsIwrappers
                                   {-# LINE 1124 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 214, column 3)
                              _t_params =
                                  ({-# LINE 214 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   ppTypeParams params_
                                   {-# LINE 1130 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 215, column 3)
                              _aliasPre =
                                  ({-# LINE 215 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   "and" >#< _t_params     >#< nt_ >#< "="
                                   {-# LINE 1136 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 216, column 3)
                              _aliasMod =
                                  ({-# LINE 216 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _aliasPre     >#< modName nt_ >|< ".t"
                                   {-# LINE 1142 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 217, column 3)
                              _datatypeNt =
                                  ({-# LINE 217 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1160 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 236, column 3)
                              _datatypeCon =
                                  ({-# LINE 236 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   case lookup nt_ _lhsItypeSyns of
                                     Just _  -> empty
                                     Nothing -> vlist _prodsIdatatype_con
                                   {-# LINE 1168 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 241, column 3)
                              _moduleDecl =
                                  ({-# LINE 241 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   let ppModule :: PP a => a -> PP_Doc
                                       ppModule expr = "module" >#< modName nt_ >#< "="
                                   in case lookup nt_ _lhsItypeSyns of
                                        Just (Map k _)  -> ppModule ("Map.Make" >#< pp_parens (ppTp k))
                                        Just (IntMap _) -> ppModule ("Map.Make ()")
                                        Just (OrdSet t) -> ppModule ("Set.Make" >#< pp_parens (ppTp t))
                                        Just IntSet     -> ppModule ("Set.Make (struct  type t = int  let compare = Pervasives.compare  end)")
                                        _               -> empty
                                   {-# LINE 1181 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 251, column 3)
                              _datatypeProds =
                                  ({-# LINE 251 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   vlist _prodsIdatatype
                                   {-# LINE 1187 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 311, column 3)
                              _fsemname =
                                  ({-# LINE 311 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   \x -> prefix _lhsIoptions ++ show x
                                   {-# LINE 1193 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 312, column 3)
                              _semname =
                                  ({-# LINE 312 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _fsemname     nt_
                                   {-# LINE 1199 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 313, column 3)
                              _frecarg =
                                  ({-# LINE 313 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   \t x -> case t of
                                     NT nt _ _ -> pp_parens (_fsemname nt >#< x)
                                     _         -> x
                                   {-# LINE 1207 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 317, column 3)
                              _sem_param_tp =
                                  ({-# LINE 317 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _t_params     >#< nt_
                                   {-# LINE 1213 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 318, column 3)
                              _sem_res_tp =
                                  ({-# LINE 318 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _t_params     >#< _t_type
                                   {-# LINE 1219 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 319, column 3)
                              _sem_tp =
                                  ({-# LINE 319 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _sem_param_tp     >#< "->" >#< _sem_res_tp
                                   {-# LINE 1225 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 321, column 3)
                              _o_sigs =
                                  ({-# LINE 321 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   typeSigs _lhsIoptions
                                   {-# LINE 1231 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 322, column 3)
                              _sem_nt_body =
                                  ({-# LINE 322 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   "match arg with" >-< (indent 2 $ _prodsIsem_nt)
                                   {-# LINE 1237 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 323, column 3)
                              _sem_nt =
                                  ({-# LINE 323 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1274 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 418, column 19)
                              (Just _prodsOinhmap) =
                                  ({-# LINE 418 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 1280 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 419, column 19)
                              (Just _prodsOsynmap) =
                                  ({-# LINE 419 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 1286 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 420, column 18)
                              _prodsOallInhmap =
                                  ({-# LINE 420 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 1292 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 421, column 18)
                              _prodsOallSynmap =
                                  ({-# LINE 421 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 1298 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 442, column 3)
                              _allstates =
                                  ({-# LINE 442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   orderStates initial_ _prodsIallvisits
                                   {-# LINE 1304 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 443, column 3)
                              _stvisits =
                                  ({-# LINE 443 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1310 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 444, column 3)
                              _t_type =
                                  ({-# LINE 444 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   type_nt_sem_top nt_
                                   {-# LINE 1316 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 445, column 3)
                              _t_c_params =
                                  ({-# LINE 445 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   ppTypeParams (cont_tvar : map pp params_)
                                   {-# LINE 1322 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 448, column 3)
                              _t_init =
                                  ({-# LINE 448 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   "and" >#< _t_params     >#< _t_type     >#< "=" >#< pp_braces ( nm_attach nt_ >#< ":" >#< "unit" >#< "->" >#< _t_params     >#< type_nt_sem nt_ initial_)
                                   {-# LINE 1328 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 451, column 3)
                              _t_states =
                                  ({-# LINE 451 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1343 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 504, column 3)
                              _c_states =
                                  ({-# LINE 504 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                   {-# LINE 1362 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 586, column 3)
                              _wr_inh =
                                  ({-# LINE 586 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _genwrap     "inh" _wr_inhs1
                                   {-# LINE 1368 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 587, column 3)
                              _wr_syn =
                                  ({-# LINE 587 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _genwrap     "syn" _wr_syns
                                   {-# LINE 1374 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 588, column 3)
                              _genwrap =
                                  ({-# LINE 588 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   \nm attrs ->
                                     "and" >#< _t_params     >#< nm >|< "_" >|< nt_ >#< "=" >#< ppRecordTp
                                       [ i >|< "_" >|< nm >|< "_" >|< nt_ >#< ":" >#< ppTp t | (i,t) <- attrs ]
                                   {-# LINE 1382 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 592, column 3)
                              _inhAttrs =
                                  ({-# LINE 592 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   fromJust $ Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 1388 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 593, column 3)
                              _wr_inhs =
                                  ({-# LINE 593 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.toList $ _wr_filter     $ _inhAttrs
                                   {-# LINE 1394 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 594, column 3)
                              _wr_inhs1 =
                                  ({-# LINE 594 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.toList _inhAttrs
                                   {-# LINE 1400 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 595, column 3)
                              _wr_filter =
                                  ({-# LINE 595 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                                   then Map.delete idLateBindingAttr
                                   else id
                                   {-# LINE 1408 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 598, column 3)
                              _wr_syns =
                                  ({-# LINE 598 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 1414 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 600, column 3)
                              _wrapname =
                                  ({-# LINE 600 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   text ("wrap_" ++ show nt_)
                                   {-# LINE 1420 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 601, column 3)
                              _inhname =
                                  ({-# LINE 601 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   text ("inh_" ++ show nt_)
                                   {-# LINE 1426 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 602, column 3)
                              _synname =
                                  ({-# LINE 602 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   text ("syn_" ++ show nt_)
                                   {-# LINE 1432 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 603, column 3)
                              _firstVisitInfo =
                                  ({-# LINE 603 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.findWithDefault ManyVis initial_ nextVisits_
                                   {-# LINE 1438 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 605, column 3)
                              _wrapArgSemTp =
                                  ({-# LINE 605 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _t_params     >#< _t_type
                                   {-# LINE 1444 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 606, column 3)
                              _wrapArgInhTp =
                                  ({-# LINE 606 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _t_params     >#< _inhname
                                   {-# LINE 1450 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 607, column 3)
                              _wrapArgPats =
                                  ({-# LINE 607 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   ppRecordVal [ i >|< "_inh_" >|< nt_ >#< "=" >#< lhsname True i | (i,_) <- _wr_inhs1     ]
                                   {-# LINE 1456 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 608, column 3)
                              _wrapResTp =
                                  ({-# LINE 608 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _t_params     >#< _synname
                                   {-# LINE 1462 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 609, column 3)
                              _wrapper =
                                  ({-# LINE 609 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   "and" >#< ppFunDecl _o_sigs     _wrapname     [(pp "act", _wrapArgSemTp    ), (_wrapArgPats    , _wrapArgInhTp    )] _wrapResTp     _wrapperPreamble
                                   {-# LINE 1468 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 610, column 3)
                              _wrapperPreamble =
                                  ({-# LINE 610 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   ( if lateHigherOrderBinding _lhsIoptions
                                     then "let" >#< lhsname True idLateBindingAttr >#< "=" >#< lateBindingFieldNm _lhsImainName >#< "in"
                                     else empty
                                   )
                                   >-< _wrapperBody
                                   {-# LINE 1478 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 616, column 3)
                              _wrapperBody =
                                  ({-# LINE 616 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   case initialv_ of
                                     Nothing -> text "{ }"
                                     Just initv ->
                                       let attach  = "let" >#< "sem" >#< "=" >#< "act." >|< nm_attach nt_ >#< "()" >#< "in"
                                           pat     = ppRecordVal [ nm_outarg i nt_ initv >#< "=" >#< lhsname False i | (i,_) <- _wr_syns     ]
                                           bld     = ppRecordVal [ i >|< "_syn_" >|< nt_ >#< "=" >#< lhsname False i | (i,_) <- _wr_syns     ]
                                           res     = "let res = function" >#< pat >#< "->" >#< bld >#< "in"
                                           inps    = "let" >#< "inps" >#< "=" >#< ppRecordVal [ nm_inarg i nt_ initv >#< "=" >#< lhsname True i | (i,_) <- _wr_inhs     ] >#< "in"
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
                                   {-# LINE 1501 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 646, column 3)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 646 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                                   {-# LINE 1507 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 647, column 3)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 647 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                                   {-# LINE 1513 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 648, column 3)
                              _semFunBndDef =
                                  ({-# LINE 648 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _semFunBndNm     >#< "=" >#< _semname
                                   {-# LINE 1519 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 649, column 3)
                              _semFunBndTp =
                                  ({-# LINE 649 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _semFunBndNm     >#< ":" >#< _sem_tp
                                   {-# LINE 1525 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 650, column 3)
                              _semFunBndNm =
                                  ({-# LINE 650 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   lateSemNtLabel nt_
                                   {-# LINE 1531 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 680, column 3)
                              _prodsOinitial =
                                  ({-# LINE 680 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   initial_
                                   {-# LINE 1537 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 681, column 3)
                              _prodsOallstates =
                                  ({-# LINE 681 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _allstates
                                   {-# LINE 1543 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 1387, column 3)
                              _prodsOnextVisits =
                                  ({-# LINE 1387 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   nextVisits_
                                   {-# LINE 1549 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 1388, column 3)
                              _prodsOprevVisits =
                                  ({-# LINE 1388 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   prevVisits_
                                   {-# LINE 1555 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 1432, column 3)
                              _prodsOlocalAttrTypes =
                                  ({-# LINE 1432 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                                   {-# LINE 1561 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 1459, column 3)
                              _lhsOinitStates =
                                  ({-# LINE 1459 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.singleton nt_ initial_
                                   {-# LINE 1567 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 1473, column 3)
                              _ntType =
                                  ({-# LINE 1473 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   NT nt_ (map show params_) False
                                   {-# LINE 1573 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIchildvisit
                                   {-# LINE 1579 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIerrors
                                   {-# LINE 1585 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIfromToStates
                                   {-# LINE 1591 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIvisitKinds
                                   {-# LINE 1597 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIvisitdefs
                                   {-# LINE 1603 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _prodsIvisituses
                                   {-# LINE 1609 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallFromToStates =
                                  ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 1615 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallInitStates =
                                  ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 1621 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallVisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 1627 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallchildvisit =
                                  ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 1633 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisitdefs =
                                  ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 1639 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisituses =
                                  ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 1645 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainFile =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 1651 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainName =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainName
                                   {-# LINE 1657 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (from local)
                              _prodsOntType =
                                  ({-# LINE 1471 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _ntType
                                   {-# LINE 1663 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOoptions =
                                  ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1669 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              ( _prodsIallvisits,_prodsIchildvisit,_prodsIcount,_prodsIdatatype,_prodsIdatatype_call,_prodsIdatatype_con,_prodsIerrors,_prodsIfromToStates,_prodsIsemFunBndDefs,_prodsIsemFunBndTps,_prodsIsem_nt,_prodsIsem_prod,_prodsIt_visits,_prodsIvisitKinds,_prodsIvisitdefs,_prodsIvisituses) =
                                  prods_ _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOprevVisits _prodsOrename _prodsOsynmap
                          in  ( _lhsOchildvisit,_lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOfromToStates,_lhsOinitStates,_lhsOmodules,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- ENonterminals -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainFile             : String
         mainName             : String
         options              : Options
         synmap               : Map NontermIdent Attributes
         typeSyns             : TypeSyns
         wrappers             : Set NontermIdent
      synthesized attributes:
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         code                 : PP_Doc
         datas                : PP_Doc
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         initStates           : Map NontermIdent Int
         modules              : PP_Doc
         semFunBndDefs        : Seq PP_Doc
         semFunBndTps         : Seq PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : ENonterminal 
         child tl             : ENonterminals 
      alternative Nil:
-}
-- cata
sem_ENonterminals :: ENonterminals ->
                     T_ENonterminals
sem_ENonterminals list =
    (Prelude.foldr sem_ENonterminals_Cons sem_ENonterminals_Nil (Prelude.map sem_ENonterminal list))
-- semantic domain
newtype T_ENonterminals = T_ENonterminals ((Map VisitIdentifier (Int,Int)) ->
                                           (Map NontermIdent Int) ->
                                           (Map VisitIdentifier VisitKind) ->
                                           (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                           (Map VisitIdentifier (Set Identifier)) ->
                                           (Map VisitIdentifier (Set Identifier)) ->
                                           (Map NontermIdent Attributes) ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                           String ->
                                           String ->
                                           Options ->
                                           (Map NontermIdent Attributes) ->
                                           TypeSyns ->
                                           (Set NontermIdent) ->
                                           ( (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),PP_Doc,PP_Doc,(Seq Error),(Map VisitIdentifier (Int,Int)),(Map NontermIdent Int),PP_Doc,(Seq PP_Doc),(Seq PP_Doc),(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_ENonterminals = Inh_ENonterminals {allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_ENonterminals :: (Map NontermIdent Int),allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_ENonterminals :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)),inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes),localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainFile_Inh_ENonterminals :: String,mainName_Inh_ENonterminals :: String,options_Inh_ENonterminals :: Options,synmap_Inh_ENonterminals :: (Map NontermIdent Attributes),typeSyns_Inh_ENonterminals :: TypeSyns,wrappers_Inh_ENonterminals :: (Set NontermIdent)}
data Syn_ENonterminals = Syn_ENonterminals {childvisit_Syn_ENonterminals :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),code_Syn_ENonterminals :: PP_Doc,datas_Syn_ENonterminals :: PP_Doc,errors_Syn_ENonterminals :: (Seq Error),fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)),initStates_Syn_ENonterminals :: (Map NontermIdent Int),modules_Syn_ENonterminals :: PP_Doc,semFunBndDefs_Syn_ENonterminals :: (Seq PP_Doc),semFunBndTps_Syn_ENonterminals :: (Seq PP_Doc),visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind),visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier))}
wrap_ENonterminals :: T_ENonterminals ->
                      Inh_ENonterminals ->
                      Syn_ENonterminals
wrap_ENonterminals (T_ENonterminals sem) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers) =
    (let ( _lhsOchildvisit,_lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOfromToStates,_lhsOinitStates,_lhsOmodules,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItypeSyns _lhsIwrappers
     in  (Syn_ENonterminals _lhsOchildvisit _lhsOcode _lhsOdatas _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOmodules _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_ENonterminals_Cons :: T_ENonterminal ->
                          T_ENonterminals ->
                          T_ENonterminals
sem_ENonterminals_Cons (T_ENonterminal hd_) (T_ENonterminals tl_) =
    (T_ENonterminals (\ _lhsIallFromToStates
                        _lhsIallInitStates
                        _lhsIallVisitKinds
                        _lhsIallchildvisit
                        _lhsIavisitdefs
                        _lhsIavisituses
                        _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainFile
                        _lhsImainName
                        _lhsIoptions
                        _lhsIsynmap
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _lhsOcode :: PP_Doc
                               _lhsOdatas :: PP_Doc
                               _lhsOerrors :: (Seq Error)
                               _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                               _lhsOinitStates :: (Map NontermIdent Int)
                               _lhsOmodules :: PP_Doc
                               _lhsOsemFunBndDefs :: (Seq PP_Doc)
                               _lhsOsemFunBndTps :: (Seq PP_Doc)
                               _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                               _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                               _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _hdOallInitStates :: (Map NontermIdent Int)
                               _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _hdOinhmap :: (Map NontermIdent Attributes)
                               _hdOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _hdOmainFile :: String
                               _hdOmainName :: String
                               _hdOoptions :: Options
                               _hdOsynmap :: (Map NontermIdent Attributes)
                               _hdOtypeSyns :: TypeSyns
                               _hdOwrappers :: (Set NontermIdent)
                               _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _tlOallInitStates :: (Map NontermIdent Int)
                               _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _tlOinhmap :: (Map NontermIdent Attributes)
                               _tlOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _tlOmainFile :: String
                               _tlOmainName :: String
                               _tlOoptions :: Options
                               _tlOsynmap :: (Map NontermIdent Attributes)
                               _tlOtypeSyns :: TypeSyns
                               _tlOwrappers :: (Set NontermIdent)
                               _hdIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _hdIcode :: PP_Doc
                               _hdIdatas :: PP_Doc
                               _hdIerrors :: (Seq Error)
                               _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _hdIinitStates :: (Map NontermIdent Int)
                               _hdImodules :: PP_Doc
                               _hdIsemFunBndDefs :: (Seq PP_Doc)
                               _hdIsemFunBndTps :: (Seq PP_Doc)
                               _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                               _tlIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _tlIcode :: PP_Doc
                               _tlIdatas :: PP_Doc
                               _tlIerrors :: (Seq Error)
                               _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _tlIinitStates :: (Map NontermIdent Int)
                               _tlImodules :: PP_Doc
                               _tlIsemFunBndDefs :: (Seq PP_Doc)
                               _tlIsemFunBndTps :: (Seq PP_Doc)
                               _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIchildvisit `Map.union` _tlIchildvisit
                                    {-# LINE 1826 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOcode =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIcode >-< _tlIcode
                                    {-# LINE 1832 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOdatas =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIdatas >-< _tlIdatas
                                    {-# LINE 1838 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIerrors Seq.>< _tlIerrors
                                    {-# LINE 1844 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                               _lhsOfromToStates =
                                   ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIfromToStates `mappend` _tlIfromToStates
                                    {-# LINE 1850 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1454, column 50)
                               _lhsOinitStates =
                                   ({-# LINE 1454 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIinitStates `mappend` _tlIinitStates
                                    {-# LINE 1856 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOmodules =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdImodules >-< _tlImodules
                                    {-# LINE 1862 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                               _lhsOsemFunBndDefs =
                                   ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
                                    {-# LINE 1868 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                               _lhsOsemFunBndTps =
                                   ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
                                    {-# LINE 1874 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                               _lhsOvisitKinds =
                                   ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIvisitKinds `mappend` _tlIvisitKinds
                                    {-# LINE 1880 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                    {-# LINE 1886 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _hdIvisituses `uwSetUnion` _tlIvisituses
                                    {-# LINE 1892 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallFromToStates =
                                   ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallFromToStates
                                    {-# LINE 1898 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallInitStates =
                                   ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallInitStates
                                    {-# LINE 1904 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallVisitKinds =
                                   ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallVisitKinds
                                    {-# LINE 1910 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallchildvisit =
                                   ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 1916 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisitdefs =
                                   ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 1922 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisituses =
                                   ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 1928 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOinhmap =
                                   ({-# LINE 405 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 1934 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOlocalAttrTypes =
                                   ({-# LINE 1427 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 1940 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainFile =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 1946 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainName =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainName
                                    {-# LINE 1952 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOoptions =
                                   ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 1958 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOsynmap =
                                   ({-# LINE 406 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 1964 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtypeSyns =
                                   ({-# LINE 168 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 1970 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOwrappers =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 1976 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallFromToStates =
                                   ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallFromToStates
                                    {-# LINE 1982 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallInitStates =
                                   ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallInitStates
                                    {-# LINE 1988 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallVisitKinds =
                                   ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallVisitKinds
                                    {-# LINE 1994 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallchildvisit =
                                   ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 2000 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisitdefs =
                                   ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 2006 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisituses =
                                   ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 2012 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOinhmap =
                                   ({-# LINE 405 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 2018 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOlocalAttrTypes =
                                   ({-# LINE 1427 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 2024 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainFile =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 2030 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainName =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainName
                                    {-# LINE 2036 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOoptions =
                                   ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2042 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOsynmap =
                                   ({-# LINE 406 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 2048 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtypeSyns =
                                   ({-# LINE 168 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 2054 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOwrappers =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 2060 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               ( _hdIchildvisit,_hdIcode,_hdIdatas,_hdIerrors,_hdIfromToStates,_hdIinitStates,_hdImodules,_hdIsemFunBndDefs,_hdIsemFunBndTps,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                                   hd_ _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOoptions _hdOsynmap _hdOtypeSyns _hdOwrappers
                               ( _tlIchildvisit,_tlIcode,_tlIdatas,_tlIerrors,_tlIfromToStates,_tlIinitStates,_tlImodules,_tlIsemFunBndDefs,_tlIsemFunBndTps,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                                   tl_ _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOoptions _tlOsynmap _tlOtypeSyns _tlOwrappers
                           in  ( _lhsOchildvisit,_lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOfromToStates,_lhsOinitStates,_lhsOmodules,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
sem_ENonterminals_Nil :: T_ENonterminals
sem_ENonterminals_Nil =
    (T_ENonterminals (\ _lhsIallFromToStates
                        _lhsIallInitStates
                        _lhsIallVisitKinds
                        _lhsIallchildvisit
                        _lhsIavisitdefs
                        _lhsIavisituses
                        _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainFile
                        _lhsImainName
                        _lhsIoptions
                        _lhsIsynmap
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _lhsOcode :: PP_Doc
                               _lhsOdatas :: PP_Doc
                               _lhsOerrors :: (Seq Error)
                               _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                               _lhsOinitStates :: (Map NontermIdent Int)
                               _lhsOmodules :: PP_Doc
                               _lhsOsemFunBndDefs :: (Seq PP_Doc)
                               _lhsOsemFunBndTps :: (Seq PP_Doc)
                               _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                               _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Map.empty
                                    {-# LINE 2099 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOcode =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    empty
                                    {-# LINE 2105 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOdatas =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    empty
                                    {-# LINE 2111 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Seq.empty
                                    {-# LINE 2117 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                               _lhsOfromToStates =
                                   ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    mempty
                                    {-# LINE 2123 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1454, column 50)
                               _lhsOinitStates =
                                   ({-# LINE 1454 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    mempty
                                    {-# LINE 2129 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 107, column 58)
                               _lhsOmodules =
                                   ({-# LINE 107 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    empty
                                    {-# LINE 2135 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                               _lhsOsemFunBndDefs =
                                   ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Seq.empty
                                    {-# LINE 2141 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                               _lhsOsemFunBndTps =
                                   ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Seq.empty
                                    {-# LINE 2147 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                               _lhsOvisitKinds =
                                   ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    mempty
                                    {-# LINE 2153 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Map.empty
                                    {-# LINE 2159 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    Map.empty
                                    {-# LINE 2165 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                           in  ( _lhsOchildvisit,_lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOfromToStates,_lhsOinitStates,_lhsOmodules,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- EProduction -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         allstates            : [StateIdentifier]
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         inhmap               : Attributes
         initial              : StateIdentifier
         localAttrTypes       : Map ConstructorIdent (Map Identifier Type)
         mainFile             : String
         mainName             : String
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         ntType               : Type
         options              : Options
         params               : [Identifier]
         prevVisits           : Map StateIdentifier StateCtx
         rename               : Bool
         synmap               : Attributes
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         count                : Int
         datatype             : PP_Doc
         datatype_call        : PP_Doc
         datatype_con         : PP_Doc
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         semFunBndDefs        : Seq PP_Doc
         semFunBndTps         : Seq PP_Doc
         sem_nt               : PP_Doc
         sem_prod             : PP_Doc
         t_visits             : PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative EProduction:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child rules          : ERules 
         child children       : EChildren 
         child visits         : Visits 
         visit 0:
            local o_records   : _
            local t_params    : _
            local t_c_params  : _
            local conname     : _
            local recname     : _
            local semFunBndDef : _
            local semFunBndTp : _
            local semFunBndNm : _
            local o_sigs      : _
            local t_type      : _
            local semname     : _
            local sem_res_tp  : _
            local sem_tp      : _
            local initializer : _
            local sem_prod    : _
            local prod_body   : _
            local statefuns   : _
            local genstfn     : _
            local stargs      : _
            local stvisits    : _
            local stks        : _
            local stvs        : _
            local lazyIntras  : _
            local childTypes  : _
            local localAttrTypes : _
-}
-- cata
sem_EProduction :: EProduction ->
                   T_EProduction
sem_EProduction (EProduction _con _params _constraints _rules _children _visits) =
    (sem_EProduction_EProduction _con _params _constraints (sem_ERules _rules) (sem_EChildren _children) (sem_Visits _visits))
-- semantic domain
newtype T_EProduction = T_EProduction ((Map VisitIdentifier (Int,Int)) ->
                                       (Map NontermIdent Attributes) ->
                                       (Map NontermIdent Int) ->
                                       (Map NontermIdent Attributes) ->
                                       (Map VisitIdentifier VisitKind) ->
                                       (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                       ([StateIdentifier]) ->
                                       (Map VisitIdentifier (Set Identifier)) ->
                                       (Map VisitIdentifier (Set Identifier)) ->
                                       Attributes ->
                                       StateIdentifier ->
                                       (Map ConstructorIdent (Map Identifier Type)) ->
                                       String ->
                                       String ->
                                       (Map StateIdentifier StateCtx) ->
                                       NontermIdent ->
                                       Type ->
                                       Options ->
                                       ([Identifier]) ->
                                       (Map StateIdentifier StateCtx) ->
                                       Bool ->
                                       Attributes ->
                                       ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),Int,PP_Doc,PP_Doc,PP_Doc,(Seq Error),(Map VisitIdentifier (Int,Int)),(Seq PP_Doc),(Seq PP_Doc),PP_Doc,PP_Doc,PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_EProduction = Inh_EProduction {allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_EProduction :: (Map NontermIdent Attributes),allInitStates_Inh_EProduction :: (Map NontermIdent Int),allSynmap_Inh_EProduction :: (Map NontermIdent Attributes),allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_EProduction :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),allstates_Inh_EProduction :: ([StateIdentifier]),avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)),inhmap_Inh_EProduction :: Attributes,initial_Inh_EProduction :: StateIdentifier,localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)),mainFile_Inh_EProduction :: String,mainName_Inh_EProduction :: String,nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx),nt_Inh_EProduction :: NontermIdent,ntType_Inh_EProduction :: Type,options_Inh_EProduction :: Options,params_Inh_EProduction :: ([Identifier]),prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx),rename_Inh_EProduction :: Bool,synmap_Inh_EProduction :: Attributes}
data Syn_EProduction = Syn_EProduction {allvisits_Syn_EProduction :: ([VisitStateState]),childvisit_Syn_EProduction :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),count_Syn_EProduction :: Int,datatype_Syn_EProduction :: PP_Doc,datatype_call_Syn_EProduction :: PP_Doc,datatype_con_Syn_EProduction :: PP_Doc,errors_Syn_EProduction :: (Seq Error),fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)),semFunBndDefs_Syn_EProduction :: (Seq PP_Doc),semFunBndTps_Syn_EProduction :: (Seq PP_Doc),sem_nt_Syn_EProduction :: PP_Doc,sem_prod_Syn_EProduction :: PP_Doc,t_visits_Syn_EProduction :: PP_Doc,visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind),visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier))}
wrap_EProduction :: T_EProduction ->
                    Inh_EProduction ->
                    Syn_EProduction
wrap_EProduction (T_EProduction sem) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOdatatype_call,_lhsOdatatype_con,_lhsOerrors,_lhsOfromToStates,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap
     in  (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_EProduction_EProduction :: ConstructorIdent ->
                               ([Identifier]) ->
                               ([Type]) ->
                               T_ERules ->
                               T_EChildren ->
                               T_Visits ->
                               T_EProduction
sem_EProduction_EProduction con_ params_ constraints_ (T_ERules rules_) (T_EChildren children_) (T_Visits visits_) =
    (T_EProduction (\ _lhsIallFromToStates
                      _lhsIallInhmap
                      _lhsIallInitStates
                      _lhsIallSynmap
                      _lhsIallVisitKinds
                      _lhsIallchildvisit
                      _lhsIallstates
                      _lhsIavisitdefs
                      _lhsIavisituses
                      _lhsIinhmap
                      _lhsIinitial
                      _lhsIlocalAttrTypes
                      _lhsImainFile
                      _lhsImainName
                      _lhsInextVisits
                      _lhsInt
                      _lhsIntType
                      _lhsIoptions
                      _lhsIparams
                      _lhsIprevVisits
                      _lhsIrename
                      _lhsIsynmap ->
                        (let _childrenOcon :: ConstructorIdent
                             _rulesOcon :: ConstructorIdent
                             _visitsOcon :: ConstructorIdent
                             _lhsOdatatype :: PP_Doc
                             _lhsOdatatype_call :: PP_Doc
                             _lhsOdatatype_con :: PP_Doc
                             _lhsOcount :: Int
                             _lhsOsem_nt :: PP_Doc
                             _lhsOsemFunBndDefs :: (Seq PP_Doc)
                             _lhsOsemFunBndTps :: (Seq PP_Doc)
                             _visitsOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                             _visitsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                             _rulesOusageInfo :: (Map Identifier Int)
                             _rulesOruleKinds :: (Map Identifier (Set VisitKind))
                             _visitsOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                             _visitsOterminaldefs :: (Set String)
                             _visitsOruledefs :: (Map Identifier (Set String))
                             _visitsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                             _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                             _lhsOerrors :: (Seq Error)
                             _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                             _lhsOt_visits :: PP_Doc
                             _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                             _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                             _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                             _lhsOallvisits :: ([VisitStateState])
                             _lhsOsem_prod :: PP_Doc
                             _rulesOallInhmap :: (Map NontermIdent Attributes)
                             _rulesOallSynmap :: (Map NontermIdent Attributes)
                             _rulesOchildTypes :: (Map Identifier Type)
                             _rulesOinhmap :: Attributes
                             _rulesOlazyIntras :: (Set String)
                             _rulesOlocalAttrTypes :: (Map Identifier Type)
                             _rulesOmainFile :: String
                             _rulesOmainName :: String
                             _rulesOnt :: NontermIdent
                             _rulesOoptions :: Options
                             _rulesOsynmap :: Attributes
                             _childrenOallInitStates :: (Map NontermIdent Int)
                             _childrenOmainFile :: String
                             _childrenOmainName :: String
                             _childrenOnt :: NontermIdent
                             _childrenOoptions :: Options
                             _visitsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                             _visitsOallInhmap :: (Map NontermIdent Attributes)
                             _visitsOallInitStates :: (Map NontermIdent Int)
                             _visitsOallSynmap :: (Map NontermIdent Attributes)
                             _visitsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                             _visitsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                             _visitsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                             _visitsOavisituses :: (Map VisitIdentifier (Set Identifier))
                             _visitsOchildTypes :: (Map Identifier Type)
                             _visitsOinhmap :: Attributes
                             _visitsOnextVisits :: (Map StateIdentifier StateCtx)
                             _visitsOnt :: NontermIdent
                             _visitsOoptions :: Options
                             _visitsOparams :: ([Identifier])
                             _visitsOprevVisits :: (Map StateIdentifier StateCtx)
                             _visitsOsynmap :: Attributes
                             _rulesIerrors :: (Seq Error)
                             _rulesImrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                             _rulesIruledefs :: (Map Identifier (Set String))
                             _rulesIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                             _rulesIsem_rules :: PP_Doc
                             _childrenIargnamesw :: ([PP_Doc])
                             _childrenIchildTypes :: (Map Identifier Type)
                             _childrenIchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                             _childrenIsigs :: ([(PP_Doc,PP_Doc,PP_Doc,PP_Doc)])
                             _childrenIterminaldefs :: (Set String)
                             _visitsIallvisits :: ([VisitStateState])
                             _visitsIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                             _visitsIerrors :: (Seq Error)
                             _visitsIfromToStates :: (Map VisitIdentifier (Int,Int))
                             _visitsIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                             _visitsIlazyIntras :: (Set String)
                             _visitsIruleKinds :: (Map Identifier (Set VisitKind))
                             _visitsIruleUsage :: (Map Identifier Int)
                             _visitsIsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                             _visitsIt_visits :: PP_Doc
                             _visitsIvisitKinds :: (Map VisitIdentifier VisitKind)
                             _visitsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                             _visitsIvisituses :: (Map VisitIdentifier (Set Identifier))
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 88, column 31)
                             _childrenOcon =
                                 ({-# LINE 88 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  con_
                                  {-# LINE 2399 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 89, column 3)
                             _rulesOcon =
                                 ({-# LINE 89 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  con_
                                  {-# LINE 2405 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 90, column 3)
                             _visitsOcon =
                                 ({-# LINE 90 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  con_
                                  {-# LINE 2411 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 257, column 3)
                             _o_records =
                                 ({-# LINE 257 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  dataRecords _lhsIoptions
                                  {-# LINE 2417 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 258, column 3)
                             _t_params =
                                 ({-# LINE 258 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  ppTypeParams _lhsIparams
                                  {-# LINE 2423 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 259, column 3)
                             _t_c_params =
                                 ({-# LINE 259 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  ppTypeParams (cont_tvar : map pp params_)
                                  {-# LINE 2429 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 260, column 3)
                             _conname =
                                 ({-# LINE 260 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  conname _lhsIrename _lhsInt con_
                                  {-# LINE 2435 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 261, column 3)
                             _recname =
                                 ({-# LINE 261 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp "fields_" >|< _conname
                                  {-# LINE 2441 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 262, column 3)
                             _lhsOdatatype =
                                 ({-# LINE 262 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  "and" >#< _t_params     >#< _recname     >#< "="
                                  >#< ppFieldsType _o_records     False _childrenIsigs
                                  {-# LINE 2448 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 264, column 3)
                             _lhsOdatatype_call =
                                 ({-# LINE 264 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp "|" >#< _conname     >#< "of" >#< pp_parens (_t_params     >#< _recname    )
                                  {-# LINE 2454 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 265, column 3)
                             _lhsOdatatype_con =
                                 ({-# LINE 265 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  let funNm  = _lhsInt >|< "_" >|< con_
                                      decl   = "and" >#< ppFunDecl _o_sigs     funNm params (_t_params     >#< _lhsInt) body
                                      params = [ (x, t) | (_,x,_,t) <- _childrenIsigs ]
                                      body   = _conname     >#< ppFieldsVal _o_records     _childrenIsigs
                                  in decl
                                  {-# LINE 2464 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 382, column 32)
                             _lhsOcount =
                                 ({-# LINE 382 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  1
                                  {-# LINE 2470 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 387, column 3)
                             _lhsOsem_nt =
                                 ({-# LINE 387 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  "|" >#< conname _lhsIrename _lhsInt con_ >#< ppFieldsVal _o_records     _childrenIsigs >#< "->" >#<
                                    prefix _lhsIoptions >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                                  {-# LINE 2477 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 653, column 3)
                             _lhsOsemFunBndDefs =
                                 ({-# LINE 653 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  Seq.singleton _semFunBndDef
                                  {-# LINE 2483 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 654, column 3)
                             _lhsOsemFunBndTps =
                                 ({-# LINE 654 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  Seq.singleton _semFunBndTp
                                  {-# LINE 2489 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 655, column 3)
                             _semFunBndDef =
                                 ({-# LINE 655 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _semFunBndNm     >#< "=" >#< _semname
                                  {-# LINE 2495 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 656, column 3)
                             _semFunBndTp =
                                 ({-# LINE 656 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _semFunBndNm     >#< ":" >#< _sem_tp
                                  {-# LINE 2501 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 657, column 3)
                             _semFunBndNm =
                                 ({-# LINE 657 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  lateSemConLabel _lhsInt con_
                                  {-# LINE 2507 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 684, column 3)
                             _o_sigs =
                                 ({-# LINE 684 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  typeSigs _lhsIoptions
                                  {-# LINE 2513 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 685, column 3)
                             _t_type =
                                 ({-# LINE 685 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  type_nt_sem_top _lhsInt
                                  {-# LINE 2519 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 686, column 3)
                             _semname =
                                 ({-# LINE 686 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  prefix _lhsIoptions >|< _lhsInt >|< "_" >|< con_
                                  {-# LINE 2525 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 687, column 3)
                             _sem_res_tp =
                                 ({-# LINE 687 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _t_params     >#< _t_type
                                  {-# LINE 2531 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 688, column 3)
                             _sem_tp =
                                 ({-# LINE 688 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  pp_block "" "" "->" [ d | (_,_,d,_) <- _childrenIsigs ] >#< "->" >#< _sem_res_tp
                                  {-# LINE 2537 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 690, column 3)
                             _initializer =
                                 ({-# LINE 690 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  empty
                                  {-# LINE 2543 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 697, column 3)
                             _sem_prod =
                                 ({-# LINE 697 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  "and" >#< ppFunDecl _o_sigs     _semname     [ (x,d) | (_,x,d,_) <- _childrenIsigs ] _sem_res_tp     _prod_body
                                  {-# LINE 2549 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 698, column 3)
                             _prod_body =
                                 ({-# LINE 698 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                  {-# LINE 2564 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 715, column 3)
                             _statefuns =
                                 ({-# LINE 715 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  map _genstfn     _lhsIallstates
                                  {-# LINE 2570 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 716, column 3)
                             _genstfn =
                                 ({-# LINE 716 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                  {-# LINE 2586 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 736, column 3)
                             _stargs =
                                 ({-# LINE 736 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                  {-# LINE 2604 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 752, column 3)
                             _stvisits =
                                 ({-# LINE 752 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  \st -> filter (\(_,f,_) -> f == st) _visitsIallvisits
                                  {-# LINE 2610 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 753, column 3)
                             _stks =
                                 ({-# LINE 753 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                  {-# LINE 2632 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 775, column 3)
                             _stvs =
                                 ({-# LINE 775 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  \st -> [ppf | (f,ppf) <- _visitsIsem_visit, f == st]
                                  {-# LINE 2638 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 776, column 3)
                             _visitsOmrules =
                                 ({-# LINE 776 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _rulesImrules
                                  {-# LINE 2644 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 917, column 3)
                             _visitsOchildintros =
                                 ({-# LINE 917 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _childrenIchildintros
                                  {-# LINE 2650 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1222, column 32)
                             _rulesOusageInfo =
                                 ({-# LINE 1222 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIruleUsage
                                  {-# LINE 2656 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1237, column 3)
                             _rulesOruleKinds =
                                 ({-# LINE 1237 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIruleKinds
                                  {-# LINE 2662 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1266, column 3)
                             _visitsOallintramap =
                                 ({-# LINE 1266 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIintramap
                                  {-# LINE 2668 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1267, column 3)
                             _visitsOterminaldefs =
                                 ({-# LINE 1267 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _childrenIterminaldefs
                                  {-# LINE 2674 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1291, column 17)
                             _visitsOruledefs =
                                 ({-# LINE 1291 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _rulesIruledefs
                                  {-# LINE 2680 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1292, column 17)
                             _visitsOruleuses =
                                 ({-# LINE 1292 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _rulesIruleuses
                                  {-# LINE 2686 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1346, column 3)
                             _lazyIntras =
                                 ({-# LINE 1346 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIlazyIntras
                                  {-# LINE 2692 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1418, column 3)
                             _childTypes =
                                 ({-# LINE 1418 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                                  {-# LINE 2698 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- "./src-ag/ExecutionPlan2Caml.ag"(line 1435, column 3)
                             _localAttrTypes =
                                 ({-# LINE 1435 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                                  {-# LINE 2704 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                             _lhsOchildvisit =
                                 ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIchildvisit
                                  {-# LINE 2710 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                             _lhsOerrors =
                                 ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _rulesIerrors Seq.>< _visitsIerrors
                                  {-# LINE 2716 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                             _lhsOfromToStates =
                                 ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIfromToStates
                                  {-# LINE 2722 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 529, column 59)
                             _lhsOt_visits =
                                 ({-# LINE 529 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIt_visits
                                  {-# LINE 2728 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                             _lhsOvisitKinds =
                                 ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIvisitKinds
                                  {-# LINE 2734 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                             _lhsOvisitdefs =
                                 ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIvisitdefs
                                  {-# LINE 2740 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                             _lhsOvisituses =
                                 ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIvisituses
                                  {-# LINE 2746 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOallvisits =
                                 ({-# LINE 431 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _visitsIallvisits
                                  {-# LINE 2752 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (from local)
                             _lhsOsem_prod =
                                 ({-# LINE 674 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _sem_prod
                                  {-# LINE 2758 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOallInhmap =
                                 ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallInhmap
                                  {-# LINE 2764 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOallSynmap =
                                 ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallSynmap
                                  {-# LINE 2770 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOchildTypes =
                                 ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _childTypes
                                  {-# LINE 2776 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOinhmap =
                                 ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIinhmap
                                  {-# LINE 2782 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOlazyIntras =
                                 ({-# LINE 1332 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lazyIntras
                                  {-# LINE 2788 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOlocalAttrTypes =
                                 ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _localAttrTypes
                                  {-# LINE 2794 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOmainFile =
                                 ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsImainFile
                                  {-# LINE 2800 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOmainName =
                                 ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsImainName
                                  {-# LINE 2806 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOnt =
                                 ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsInt
                                  {-# LINE 2812 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOoptions =
                                 ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2818 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOsynmap =
                                 ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIsynmap
                                  {-# LINE 2824 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOallInitStates =
                                 ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallInitStates
                                  {-# LINE 2830 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOmainFile =
                                 ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsImainFile
                                  {-# LINE 2836 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOmainName =
                                 ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsImainName
                                  {-# LINE 2842 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOnt =
                                 ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsInt
                                  {-# LINE 2848 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOoptions =
                                 ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2854 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallFromToStates =
                                 ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallFromToStates
                                  {-# LINE 2860 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallInhmap =
                                 ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallInhmap
                                  {-# LINE 2866 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallInitStates =
                                 ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallInitStates
                                  {-# LINE 2872 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallSynmap =
                                 ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallSynmap
                                  {-# LINE 2878 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallVisitKinds =
                                 ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallVisitKinds
                                  {-# LINE 2884 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallchildvisit =
                                 ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIallchildvisit
                                  {-# LINE 2890 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisitdefs =
                                 ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIavisitdefs
                                  {-# LINE 2896 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisituses =
                                 ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIavisituses
                                  {-# LINE 2902 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (from local)
                             _visitsOchildTypes =
                                 ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _childTypes
                                  {-# LINE 2908 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOinhmap =
                                 ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIinhmap
                                  {-# LINE 2914 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnextVisits =
                                 ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsInextVisits
                                  {-# LINE 2920 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnt =
                                 ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsInt
                                  {-# LINE 2926 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOoptions =
                                 ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2932 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOparams =
                                 ({-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIparams
                                  {-# LINE 2938 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOprevVisits =
                                 ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIprevVisits
                                  {-# LINE 2944 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOsynmap =
                                 ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                  _lhsIsynmap
                                  {-# LINE 2950 "dist/build/ExecutionPlan2Caml.hs" #-}
                                  )
                             ( _rulesIerrors,_rulesImrules,_rulesIruledefs,_rulesIruleuses,_rulesIsem_rules) =
                                 rules_ _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOnt _rulesOoptions _rulesOruleKinds _rulesOsynmap _rulesOusageInfo
                             ( _childrenIargnamesw,_childrenIchildTypes,_childrenIchildintros,_childrenIsigs,_childrenIterminaldefs) =
                                 children_ _childrenOallInitStates _childrenOcon _childrenOmainFile _childrenOmainName _childrenOnt _childrenOoptions
                             ( _visitsIallvisits,_visitsIchildvisit,_visitsIerrors,_visitsIfromToStates,_visitsIintramap,_visitsIlazyIntras,_visitsIruleKinds,_visitsIruleUsage,_visitsIsem_visit,_visitsIt_visits,_visitsIvisitKinds,_visitsIvisitdefs,_visitsIvisituses) =
                                 visits_ _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOchildintros _visitsOcon _visitsOinhmap _visitsOmrules _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs
                         in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOdatatype_call,_lhsOdatatype_con,_lhsOerrors,_lhsOfromToStates,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- EProductions ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         allstates            : [StateIdentifier]
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         inhmap               : Attributes
         initial              : StateIdentifier
         localAttrTypes       : Map ConstructorIdent (Map Identifier Type)
         mainFile             : String
         mainName             : String
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         ntType               : Type
         options              : Options
         params               : [Identifier]
         prevVisits           : Map StateIdentifier StateCtx
         rename               : Bool
         synmap               : Attributes
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         count                : Int
         datatype             : [PP_Doc]
         datatype_call        : [PP_Doc]
         datatype_con         : [PP_Doc]
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         semFunBndDefs        : Seq PP_Doc
         semFunBndTps         : Seq PP_Doc
         sem_nt               : PP_Doc
         sem_prod             : PP_Doc
         t_visits             : PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : EProduction 
         child tl             : EProductions 
      alternative Nil:
-}
-- cata
sem_EProductions :: EProductions ->
                    T_EProductions
sem_EProductions list =
    (Prelude.foldr sem_EProductions_Cons sem_EProductions_Nil (Prelude.map sem_EProduction list))
-- semantic domain
newtype T_EProductions = T_EProductions ((Map VisitIdentifier (Int,Int)) ->
                                         (Map NontermIdent Attributes) ->
                                         (Map NontermIdent Int) ->
                                         (Map NontermIdent Attributes) ->
                                         (Map VisitIdentifier VisitKind) ->
                                         (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                         ([StateIdentifier]) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         Attributes ->
                                         StateIdentifier ->
                                         (Map ConstructorIdent (Map Identifier Type)) ->
                                         String ->
                                         String ->
                                         (Map StateIdentifier StateCtx) ->
                                         NontermIdent ->
                                         Type ->
                                         Options ->
                                         ([Identifier]) ->
                                         (Map StateIdentifier StateCtx) ->
                                         Bool ->
                                         Attributes ->
                                         ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),Int,([PP_Doc]),([PP_Doc]),([PP_Doc]),(Seq Error),(Map VisitIdentifier (Int,Int)),(Seq PP_Doc),(Seq PP_Doc),PP_Doc,PP_Doc,PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_EProductions = Inh_EProductions {allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_EProductions :: (Map NontermIdent Attributes),allInitStates_Inh_EProductions :: (Map NontermIdent Int),allSynmap_Inh_EProductions :: (Map NontermIdent Attributes),allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_EProductions :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),allstates_Inh_EProductions :: ([StateIdentifier]),avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)),inhmap_Inh_EProductions :: Attributes,initial_Inh_EProductions :: StateIdentifier,localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)),mainFile_Inh_EProductions :: String,mainName_Inh_EProductions :: String,nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx),nt_Inh_EProductions :: NontermIdent,ntType_Inh_EProductions :: Type,options_Inh_EProductions :: Options,params_Inh_EProductions :: ([Identifier]),prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx),rename_Inh_EProductions :: Bool,synmap_Inh_EProductions :: Attributes}
data Syn_EProductions = Syn_EProductions {allvisits_Syn_EProductions :: ([VisitStateState]),childvisit_Syn_EProductions :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),count_Syn_EProductions :: Int,datatype_Syn_EProductions :: ([PP_Doc]),datatype_call_Syn_EProductions :: ([PP_Doc]),datatype_con_Syn_EProductions :: ([PP_Doc]),errors_Syn_EProductions :: (Seq Error),fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)),semFunBndDefs_Syn_EProductions :: (Seq PP_Doc),semFunBndTps_Syn_EProductions :: (Seq PP_Doc),sem_nt_Syn_EProductions :: PP_Doc,sem_prod_Syn_EProductions :: PP_Doc,t_visits_Syn_EProductions :: PP_Doc,visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind),visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier))}
wrap_EProductions :: T_EProductions ->
                     Inh_EProductions ->
                     Syn_EProductions
wrap_EProductions (T_EProductions sem) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOdatatype_call,_lhsOdatatype_con,_lhsOerrors,_lhsOfromToStates,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap
     in  (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOdatatype_call _lhsOdatatype_con _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_EProductions_Cons :: T_EProduction ->
                         T_EProductions ->
                         T_EProductions
sem_EProductions_Cons (T_EProduction hd_) (T_EProductions tl_) =
    (T_EProductions (\ _lhsIallFromToStates
                       _lhsIallInhmap
                       _lhsIallInitStates
                       _lhsIallSynmap
                       _lhsIallVisitKinds
                       _lhsIallchildvisit
                       _lhsIallstates
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsInextVisits
                       _lhsInt
                       _lhsIntType
                       _lhsIoptions
                       _lhsIparams
                       _lhsIprevVisits
                       _lhsIrename
                       _lhsIsynmap ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOt_visits :: PP_Doc
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _lhsOcount :: Int
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOdatatype_call :: ([PP_Doc])
                              _lhsOdatatype_con :: ([PP_Doc])
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOsemFunBndDefs :: (Seq PP_Doc)
                              _lhsOsemFunBndTps :: (Seq PP_Doc)
                              _lhsOsem_nt :: PP_Doc
                              _lhsOsem_prod :: PP_Doc
                              _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                              _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                              _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                              _hdOallInhmap :: (Map NontermIdent Attributes)
                              _hdOallInitStates :: (Map NontermIdent Int)
                              _hdOallSynmap :: (Map NontermIdent Attributes)
                              _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                              _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _hdOallstates :: ([StateIdentifier])
                              _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _hdOinhmap :: Attributes
                              _hdOinitial :: StateIdentifier
                              _hdOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _hdOmainFile :: String
                              _hdOmainName :: String
                              _hdOnextVisits :: (Map StateIdentifier StateCtx)
                              _hdOnt :: NontermIdent
                              _hdOntType :: Type
                              _hdOoptions :: Options
                              _hdOparams :: ([Identifier])
                              _hdOprevVisits :: (Map StateIdentifier StateCtx)
                              _hdOrename :: Bool
                              _hdOsynmap :: Attributes
                              _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                              _tlOallInhmap :: (Map NontermIdent Attributes)
                              _tlOallInitStates :: (Map NontermIdent Int)
                              _tlOallSynmap :: (Map NontermIdent Attributes)
                              _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                              _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _tlOallstates :: ([StateIdentifier])
                              _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _tlOinhmap :: Attributes
                              _tlOinitial :: StateIdentifier
                              _tlOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _tlOmainFile :: String
                              _tlOmainName :: String
                              _tlOnextVisits :: (Map StateIdentifier StateCtx)
                              _tlOnt :: NontermIdent
                              _tlOntType :: Type
                              _tlOoptions :: Options
                              _tlOparams :: ([Identifier])
                              _tlOprevVisits :: (Map StateIdentifier StateCtx)
                              _tlOrename :: Bool
                              _tlOsynmap :: Attributes
                              _hdIallvisits :: ([VisitStateState])
                              _hdIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _hdIcount :: Int
                              _hdIdatatype :: PP_Doc
                              _hdIdatatype_call :: PP_Doc
                              _hdIdatatype_con :: PP_Doc
                              _hdIerrors :: (Seq Error)
                              _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _hdIsemFunBndDefs :: (Seq PP_Doc)
                              _hdIsemFunBndTps :: (Seq PP_Doc)
                              _hdIsem_nt :: PP_Doc
                              _hdIsem_prod :: PP_Doc
                              _hdIt_visits :: PP_Doc
                              _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                              _tlIallvisits :: ([VisitStateState])
                              _tlIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _tlIcount :: Int
                              _tlIdatatype :: ([PP_Doc])
                              _tlIdatatype_call :: ([PP_Doc])
                              _tlIdatatype_con :: ([PP_Doc])
                              _tlIerrors :: (Seq Error)
                              _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _tlIsemFunBndDefs :: (Seq PP_Doc)
                              _tlIsemFunBndTps :: (Seq PP_Doc)
                              _tlIsem_nt :: PP_Doc
                              _tlIsem_prod :: PP_Doc
                              _tlIt_visits :: PP_Doc
                              _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 437, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 437 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIallvisits
                                   {-# LINE 3167 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 532, column 3)
                              _lhsOt_visits =
                                  ({-# LINE 532 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIt_visits
                                   {-# LINE 3173 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIchildvisit `Map.union` _tlIchildvisit
                                   {-# LINE 3179 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 381, column 43)
                              _lhsOcount =
                                  ({-# LINE 381 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIcount + _tlIcount
                                   {-# LINE 3185 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIdatatype : _tlIdatatype
                                   {-# LINE 3191 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype_call =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIdatatype_call : _tlIdatatype_call
                                   {-# LINE 3197 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype_con =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIdatatype_con : _tlIdatatype_con
                                   {-# LINE 3203 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIerrors Seq.>< _tlIerrors
                                   {-# LINE 3209 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIfromToStates `mappend` _tlIfromToStates
                                   {-# LINE 3215 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
                                   {-# LINE 3221 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
                                   {-# LINE 3227 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 385, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 385 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIsem_nt >-< _tlIsem_nt
                                   {-# LINE 3233 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 675, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 675 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIsem_prod >-< _tlIsem_prod
                                   {-# LINE 3239 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIvisitKinds `mappend` _tlIvisitKinds
                                   {-# LINE 3245 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                   {-# LINE 3251 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _hdIvisituses `uwSetUnion` _tlIvisituses
                                   {-# LINE 3257 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallFromToStates =
                                  ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 3263 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallInhmap =
                                  ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallInhmap
                                   {-# LINE 3269 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallInitStates =
                                  ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 3275 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallSynmap =
                                  ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallSynmap
                                   {-# LINE 3281 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallVisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 3287 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallchildvisit =
                                  ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 3293 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallstates =
                                  ({-# LINE 678 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 3299 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisitdefs =
                                  ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 3305 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisituses =
                                  ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 3311 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhmap =
                                  ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 3317 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinitial =
                                  ({-# LINE 677 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 3323 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOlocalAttrTypes =
                                  ({-# LINE 1428 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIlocalAttrTypes
                                   {-# LINE 3329 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainFile =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 3335 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainName =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainName
                                   {-# LINE 3341 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnextVisits =
                                  ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsInextVisits
                                   {-# LINE 3347 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnt =
                                  ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsInt
                                   {-# LINE 3353 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOntType =
                                  ({-# LINE 1471 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIntType
                                   {-# LINE 3359 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 3365 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOparams =
                                  ({-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIparams
                                   {-# LINE 3371 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOprevVisits =
                                  ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIprevVisits
                                   {-# LINE 3377 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOrename =
                                  ({-# LINE 73 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIrename
                                   {-# LINE 3383 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynmap =
                                  ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 3389 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallFromToStates =
                                  ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 3395 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallInhmap =
                                  ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallInhmap
                                   {-# LINE 3401 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallInitStates =
                                  ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 3407 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallSynmap =
                                  ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallSynmap
                                   {-# LINE 3413 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallVisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 3419 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallchildvisit =
                                  ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 3425 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallstates =
                                  ({-# LINE 678 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 3431 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisitdefs =
                                  ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 3437 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisituses =
                                  ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 3443 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhmap =
                                  ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 3449 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinitial =
                                  ({-# LINE 677 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 3455 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOlocalAttrTypes =
                                  ({-# LINE 1428 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIlocalAttrTypes
                                   {-# LINE 3461 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainFile =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 3467 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainName =
                                  ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsImainName
                                   {-# LINE 3473 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnextVisits =
                                  ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsInextVisits
                                   {-# LINE 3479 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnt =
                                  ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsInt
                                   {-# LINE 3485 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOntType =
                                  ({-# LINE 1471 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIntType
                                   {-# LINE 3491 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 3497 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOparams =
                                  ({-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIparams
                                   {-# LINE 3503 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOprevVisits =
                                  ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIprevVisits
                                   {-# LINE 3509 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOrename =
                                  ({-# LINE 73 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIrename
                                   {-# LINE 3515 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynmap =
                                  ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 3521 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              ( _hdIallvisits,_hdIchildvisit,_hdIcount,_hdIdatatype,_hdIdatatype_call,_hdIdatatype_con,_hdIerrors,_hdIfromToStates,_hdIsemFunBndDefs,_hdIsemFunBndTps,_hdIsem_nt,_hdIsem_prod,_hdIt_visits,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                                  hd_ _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOprevVisits _hdOrename _hdOsynmap
                              ( _tlIallvisits,_tlIchildvisit,_tlIcount,_tlIdatatype,_tlIdatatype_call,_tlIdatatype_con,_tlIerrors,_tlIfromToStates,_tlIsemFunBndDefs,_tlIsemFunBndTps,_tlIsem_nt,_tlIsem_prod,_tlIt_visits,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                                  tl_ _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOprevVisits _tlOrename _tlOsynmap
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOdatatype_call,_lhsOdatatype_con,_lhsOerrors,_lhsOfromToStates,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
sem_EProductions_Nil :: T_EProductions
sem_EProductions_Nil =
    (T_EProductions (\ _lhsIallFromToStates
                       _lhsIallInhmap
                       _lhsIallInitStates
                       _lhsIallSynmap
                       _lhsIallVisitKinds
                       _lhsIallchildvisit
                       _lhsIallstates
                       _lhsIavisitdefs
                       _lhsIavisituses
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsInextVisits
                       _lhsInt
                       _lhsIntType
                       _lhsIoptions
                       _lhsIparams
                       _lhsIprevVisits
                       _lhsIrename
                       _lhsIsynmap ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                              _lhsOcount :: Int
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOdatatype_call :: ([PP_Doc])
                              _lhsOdatatype_con :: ([PP_Doc])
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOsemFunBndDefs :: (Seq PP_Doc)
                              _lhsOsemFunBndTps :: (Seq PP_Doc)
                              _lhsOsem_nt :: PP_Doc
                              _lhsOsem_prod :: PP_Doc
                              _lhsOt_visits :: PP_Doc
                              _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                              _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "./src-ag/ExecutionPlan2Caml.ag"(line 438, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 438 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   error "Every nonterminal should have at least 1 production"
                                   {-# LINE 3572 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.empty
                                   {-# LINE 3578 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 381, column 43)
                              _lhsOcount =
                                  ({-# LINE 381 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   0
                                   {-# LINE 3584 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   []
                                   {-# LINE 3590 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype_call =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   []
                                   {-# LINE 3596 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 254, column 63)
                              _lhsOdatatype_con =
                                  ({-# LINE 254 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   []
                                   {-# LINE 3602 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Seq.empty
                                   {-# LINE 3608 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   mempty
                                   {-# LINE 3614 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Seq.empty
                                   {-# LINE 3620 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 644, column 92)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 644 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Seq.empty
                                   {-# LINE 3626 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 385, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 385 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   empty
                                   {-# LINE 3632 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 675, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 675 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   empty
                                   {-# LINE 3638 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 529, column 59)
                              _lhsOt_visits =
                                  ({-# LINE 529 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   empty
                                   {-# LINE 3644 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   mempty
                                   {-# LINE 3650 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.empty
                                   {-# LINE 3656 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                              -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                   Map.empty
                                   {-# LINE 3662 "dist/build/ExecutionPlan2Caml.hs" #-}
                                   )
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOdatatype_call,_lhsOdatatype_con,_lhsOerrors,_lhsOfromToStates,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- ERule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         childTypes           : Map Identifier Type
         con                  : ConstructorIdent
         inhmap               : Attributes
         lazyIntras           : Set String
         localAttrTypes       : Map Identifier Type
         mainFile             : String
         mainName             : String
         nt                   : NontermIdent
         options              : Options
         ruleKinds            : Map Identifier (Set VisitKind)
         synmap               : Attributes
         usageInfo            : Map Identifier Int
      synthesized attributes:
         errors               : Seq Error
         mrules               : Map Identifier (VisitKind -> Either Error PP_Doc)
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         sem_rules            : PP_Doc
   alternatives:
      alternative ERule:
         child name           : {Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         child pure           : {Bool}
         child mbError        : {Maybe Error}
         visit 0:
            local rulecode    : _
            local pragma      : _
            local endpragma   : _
            local genpragma   : _
            local haspos      : _
            local declHead    : _
            local argPats     : _
            local argExprs    : _
            local stepcode    : _
            local used        : _
            local kinds       : _
            local anyLazyKind : _
-}
-- cata
sem_ERule :: ERule ->
             T_ERule
sem_ERule (ERule _name _pattern _rhs _owrt _origin _explicit _pure _mbError) =
    (sem_ERule_ERule _name (sem_Pattern _pattern) (sem_Expression _rhs) _owrt _origin _explicit _pure _mbError)
-- semantic domain
newtype T_ERule = T_ERule ((Map NontermIdent Attributes) ->
                           (Map NontermIdent Attributes) ->
                           (Map Identifier Type) ->
                           ConstructorIdent ->
                           Attributes ->
                           (Set String) ->
                           (Map Identifier Type) ->
                           String ->
                           String ->
                           NontermIdent ->
                           Options ->
                           (Map Identifier (Set VisitKind)) ->
                           Attributes ->
                           (Map Identifier Int) ->
                           ( (Seq Error),(Map Identifier (VisitKind -> Either Error PP_Doc)),(Map Identifier (Set String)),(Map Identifier (Map String (Maybe NonLocalAttr))),PP_Doc))
data Inh_ERule = Inh_ERule {allInhmap_Inh_ERule :: (Map NontermIdent Attributes),allSynmap_Inh_ERule :: (Map NontermIdent Attributes),childTypes_Inh_ERule :: (Map Identifier Type),con_Inh_ERule :: ConstructorIdent,inhmap_Inh_ERule :: Attributes,lazyIntras_Inh_ERule :: (Set String),localAttrTypes_Inh_ERule :: (Map Identifier Type),mainFile_Inh_ERule :: String,mainName_Inh_ERule :: String,nt_Inh_ERule :: NontermIdent,options_Inh_ERule :: Options,ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)),synmap_Inh_ERule :: Attributes,usageInfo_Inh_ERule :: (Map Identifier Int)}
data Syn_ERule = Syn_ERule {errors_Syn_ERule :: (Seq Error),mrules_Syn_ERule :: (Map Identifier (VisitKind -> Either Error PP_Doc)),ruledefs_Syn_ERule :: (Map Identifier (Set String)),ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))),sem_rules_Syn_ERule :: PP_Doc}
wrap_ERule :: T_ERule ->
              Inh_ERule ->
              Syn_ERule
wrap_ERule (T_ERule sem) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) =
    (let ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo
     in  (Syn_ERule _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules))
sem_ERule_ERule :: Identifier ->
                   T_Pattern ->
                   T_Expression ->
                   Bool ->
                   String ->
                   Bool ->
                   Bool ->
                   (Maybe Error) ->
                   T_ERule
sem_ERule_ERule name_ (T_Pattern pattern_) (T_Expression rhs_) owrt_ origin_ explicit_ pure_ mbError_ =
    (T_ERule (\ _lhsIallInhmap
                _lhsIallSynmap
                _lhsIchildTypes
                _lhsIcon
                _lhsIinhmap
                _lhsIlazyIntras
                _lhsIlocalAttrTypes
                _lhsImainFile
                _lhsImainName
                _lhsInt
                _lhsIoptions
                _lhsIruleKinds
                _lhsIsynmap
                _lhsIusageInfo ->
                  (let _lhsOsem_rules :: PP_Doc
                       _lhsOmrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                       _lhsOruledefs :: (Map Identifier (Set String))
                       _lhsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                       _lhsOerrors :: (Seq Error)
                       _patternOallInhmap :: (Map NontermIdent Attributes)
                       _patternOallSynmap :: (Map NontermIdent Attributes)
                       _patternOanyLazyKind :: Bool
                       _patternOinhmap :: Attributes
                       _patternOlocalAttrTypes :: (Map Identifier Type)
                       _patternOoptions :: Options
                       _patternOsynmap :: Attributes
                       _patternIattrTypes :: PP_Doc
                       _patternIattrs :: (Set String)
                       _patternIcopy :: Pattern
                       _patternIextraDefs :: ([(PP_Doc,PP_Doc)])
                       _patternIisUnderscore :: Bool
                       _patternIsem_lhs :: ( PP_Doc )
                       _rhsIattrs :: (Map String (Maybe NonLocalAttr))
                       _rhsIpos :: Pos
                       _rhsIsemfunc :: PP_Doc
                       _rhsItks :: ([HsToken])
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 975, column 6)
                       _lhsOsem_rules =
                           ({-# LINE 975 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            if _used     == 0
                            then empty
                            else _rulecode
                            {-# LINE 3794 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 978, column 6)
                       _rulecode =
                           ({-# LINE 978 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                            {-# LINE 3810 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 991, column 7)
                       _pragma =
                           ({-# LINE 991 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppLinePragma _lhsIoptions (line _rhsIpos) (file _rhsIpos)
                            {-# LINE 3816 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 992, column 7)
                       _endpragma =
                           ({-# LINE 992 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppWithLineNr (\ln -> ppLinePragma _lhsIoptions (ln+1) _lhsImainFile)
                            {-# LINE 3822 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 993, column 7)
                       _genpragma =
                           ({-# LINE 993 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            genLinePragmas _lhsIoptions && explicit_ && _haspos
                            {-# LINE 3828 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 994, column 7)
                       _haspos =
                           ({-# LINE 994 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            line _rhsIpos > 0 && column _rhsIpos >= 0 && not (null (file _rhsIpos))
                            {-# LINE 3834 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 998, column 7)
                       _declHead =
                           ({-# LINE 998 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            "let" >#< name_ >#< _argPats     >#< dummyPat _lhsIoptions (Map.null _rhsIattrs) >#< "="
                            {-# LINE 3840 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 999, column 7)
                       _argPats =
                           ({-# LINE 999 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                            {-# LINE 3858 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1014, column 7)
                       _argExprs =
                           ({-# LINE 1014 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppSpaced $ Map.keys _rhsIattrs
                            {-# LINE 3864 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1015, column 7)
                       _stepcode =
                           ({-# LINE 1015 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            \kind ->
                              let mkBind (pat,expr) = "let" >#< pat >#< "=" >#< expr >#< "in"
                              in if kind `compatibleRule` pure_
                                 then Right $ mkBind (_patternIsem_lhs, name_ >#< _argExprs     >#< dummyArg _lhsIoptions (Map.null _rhsIattrs))
                                              >-< vlist (map mkBind _patternIextraDefs)
                                 else Left $ IncompatibleRuleKind name_ kind
                            {-# LINE 3875 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1022, column 7)
                       _lhsOmrules =
                           ({-# LINE 1022 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton name_ _stepcode
                            {-# LINE 3881 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1224, column 32)
                       _used =
                           ({-# LINE 1224 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.findWithDefault 0 name_ _lhsIusageInfo
                            {-# LINE 3887 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1240, column 3)
                       _kinds =
                           ({-# LINE 1240 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.findWithDefault Set.empty name_ _lhsIruleKinds
                            {-# LINE 3893 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1241, column 3)
                       _anyLazyKind =
                           ({-# LINE 1241 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Set.fold (\k r -> isLazyKind k || r) False _kinds
                            {-# LINE 3899 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1287, column 11)
                       _lhsOruledefs =
                           ({-# LINE 1287 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton name_ _patternIattrs
                            {-# LINE 3905 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1288, column 11)
                       _lhsOruleuses =
                           ({-# LINE 1288 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton name_ _rhsIattrs
                            {-# LINE 3911 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1482, column 3)
                       _lhsOerrors =
                           ({-# LINE 1482 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            case mbError_ of
                              Just e | _used     > 0 -> Seq.singleton e
                              _                      -> Seq.empty
                            {-# LINE 3919 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOallInhmap =
                           ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallInhmap
                            {-# LINE 3925 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOallSynmap =
                           ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallSynmap
                            {-# LINE 3931 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (from local)
                       _patternOanyLazyKind =
                           ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _anyLazyKind
                            {-# LINE 3937 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOinhmap =
                           ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIinhmap
                            {-# LINE 3943 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOlocalAttrTypes =
                           ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIlocalAttrTypes
                            {-# LINE 3949 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOoptions =
                           ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIoptions
                            {-# LINE 3955 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _patternOsynmap =
                           ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIsynmap
                            {-# LINE 3961 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       ( _patternIattrTypes,_patternIattrs,_patternIcopy,_patternIextraDefs,_patternIisUnderscore,_patternIsem_lhs) =
                           pattern_ _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap
                       ( _rhsIattrs,_rhsIpos,_rhsIsemfunc,_rhsItks) =
                           rhs_
                   in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
-- ERules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         childTypes           : Map Identifier Type
         con                  : ConstructorIdent
         inhmap               : Attributes
         lazyIntras           : Set String
         localAttrTypes       : Map Identifier Type
         mainFile             : String
         mainName             : String
         nt                   : NontermIdent
         options              : Options
         ruleKinds            : Map Identifier (Set VisitKind)
         synmap               : Attributes
         usageInfo            : Map Identifier Int
      synthesized attributes:
         errors               : Seq Error
         mrules               : Map Identifier (VisitKind -> Either Error PP_Doc)
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         sem_rules            : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : ERule 
         child tl             : ERules 
      alternative Nil:
-}
-- cata
sem_ERules :: ERules ->
              T_ERules
sem_ERules list =
    (Prelude.foldr sem_ERules_Cons sem_ERules_Nil (Prelude.map sem_ERule list))
-- semantic domain
newtype T_ERules = T_ERules ((Map NontermIdent Attributes) ->
                             (Map NontermIdent Attributes) ->
                             (Map Identifier Type) ->
                             ConstructorIdent ->
                             Attributes ->
                             (Set String) ->
                             (Map Identifier Type) ->
                             String ->
                             String ->
                             NontermIdent ->
                             Options ->
                             (Map Identifier (Set VisitKind)) ->
                             Attributes ->
                             (Map Identifier Int) ->
                             ( (Seq Error),(Map Identifier (VisitKind -> Either Error PP_Doc)),(Map Identifier (Set String)),(Map Identifier (Map String (Maybe NonLocalAttr))),PP_Doc))
data Inh_ERules = Inh_ERules {allInhmap_Inh_ERules :: (Map NontermIdent Attributes),allSynmap_Inh_ERules :: (Map NontermIdent Attributes),childTypes_Inh_ERules :: (Map Identifier Type),con_Inh_ERules :: ConstructorIdent,inhmap_Inh_ERules :: Attributes,lazyIntras_Inh_ERules :: (Set String),localAttrTypes_Inh_ERules :: (Map Identifier Type),mainFile_Inh_ERules :: String,mainName_Inh_ERules :: String,nt_Inh_ERules :: NontermIdent,options_Inh_ERules :: Options,ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)),synmap_Inh_ERules :: Attributes,usageInfo_Inh_ERules :: (Map Identifier Int)}
data Syn_ERules = Syn_ERules {errors_Syn_ERules :: (Seq Error),mrules_Syn_ERules :: (Map Identifier (VisitKind -> Either Error PP_Doc)),ruledefs_Syn_ERules :: (Map Identifier (Set String)),ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))),sem_rules_Syn_ERules :: PP_Doc}
wrap_ERules :: T_ERules ->
               Inh_ERules ->
               Syn_ERules
wrap_ERules (T_ERules sem) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo) =
    (let ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsIusageInfo
     in  (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules))
sem_ERules_Cons :: T_ERule ->
                   T_ERules ->
                   T_ERules
sem_ERules_Cons (T_ERule hd_) (T_ERules tl_) =
    (T_ERules (\ _lhsIallInhmap
                 _lhsIallSynmap
                 _lhsIchildTypes
                 _lhsIcon
                 _lhsIinhmap
                 _lhsIlazyIntras
                 _lhsIlocalAttrTypes
                 _lhsImainFile
                 _lhsImainName
                 _lhsInt
                 _lhsIoptions
                 _lhsIruleKinds
                 _lhsIsynmap
                 _lhsIusageInfo ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                        _lhsOruledefs :: (Map Identifier (Set String))
                        _lhsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _lhsOsem_rules :: PP_Doc
                        _hdOallInhmap :: (Map NontermIdent Attributes)
                        _hdOallSynmap :: (Map NontermIdent Attributes)
                        _hdOchildTypes :: (Map Identifier Type)
                        _hdOcon :: ConstructorIdent
                        _hdOinhmap :: Attributes
                        _hdOlazyIntras :: (Set String)
                        _hdOlocalAttrTypes :: (Map Identifier Type)
                        _hdOmainFile :: String
                        _hdOmainName :: String
                        _hdOnt :: NontermIdent
                        _hdOoptions :: Options
                        _hdOruleKinds :: (Map Identifier (Set VisitKind))
                        _hdOsynmap :: Attributes
                        _hdOusageInfo :: (Map Identifier Int)
                        _tlOallInhmap :: (Map NontermIdent Attributes)
                        _tlOallSynmap :: (Map NontermIdent Attributes)
                        _tlOchildTypes :: (Map Identifier Type)
                        _tlOcon :: ConstructorIdent
                        _tlOinhmap :: Attributes
                        _tlOlazyIntras :: (Set String)
                        _tlOlocalAttrTypes :: (Map Identifier Type)
                        _tlOmainFile :: String
                        _tlOmainName :: String
                        _tlOnt :: NontermIdent
                        _tlOoptions :: Options
                        _tlOruleKinds :: (Map Identifier (Set VisitKind))
                        _tlOsynmap :: Attributes
                        _tlOusageInfo :: (Map Identifier Int)
                        _hdIerrors :: (Seq Error)
                        _hdImrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                        _hdIruledefs :: (Map Identifier (Set String))
                        _hdIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _hdIsem_rules :: PP_Doc
                        _tlIerrors :: (Seq Error)
                        _tlImrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                        _tlIruledefs :: (Map Identifier (Set String))
                        _tlIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _tlIsem_rules :: PP_Doc
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                        _lhsOerrors =
                            ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIerrors Seq.>< _tlIerrors
                             {-# LINE 4092 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 972, column 32)
                        _lhsOmrules =
                            ({-# LINE 972 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdImrules `Map.union` _tlImrules
                             {-# LINE 4098 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1280, column 34)
                        _lhsOruledefs =
                            ({-# LINE 1280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIruledefs `uwSetUnion` _tlIruledefs
                             {-# LINE 4104 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1281, column 34)
                        _lhsOruleuses =
                            ({-# LINE 1281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIruleuses `uwMapUnion` _tlIruleuses
                             {-# LINE 4110 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 971, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 971 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIsem_rules >-< _tlIsem_rules
                             {-# LINE 4116 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInhmap =
                            ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 4122 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallSynmap =
                            ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 4128 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildTypes =
                            ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 4134 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOcon =
                            ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIcon
                             {-# LINE 4140 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOinhmap =
                            ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIinhmap
                             {-# LINE 4146 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOlazyIntras =
                            ({-# LINE 1332 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIlazyIntras
                             {-# LINE 4152 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOlocalAttrTypes =
                            ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIlocalAttrTypes
                             {-# LINE 4158 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmainFile =
                            ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImainFile
                             {-# LINE 4164 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmainName =
                            ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImainName
                             {-# LINE 4170 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnt =
                            ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInt
                             {-# LINE 4176 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIoptions
                             {-# LINE 4182 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruleKinds =
                            ({-# LINE 1235 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruleKinds
                             {-# LINE 4188 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOsynmap =
                            ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIsynmap
                             {-# LINE 4194 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOusageInfo =
                            ({-# LINE 1220 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIusageInfo
                             {-# LINE 4200 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInhmap =
                            ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 4206 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallSynmap =
                            ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 4212 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildTypes =
                            ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 4218 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOcon =
                            ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIcon
                             {-# LINE 4224 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOinhmap =
                            ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIinhmap
                             {-# LINE 4230 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOlazyIntras =
                            ({-# LINE 1332 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIlazyIntras
                             {-# LINE 4236 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOlocalAttrTypes =
                            ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIlocalAttrTypes
                             {-# LINE 4242 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmainFile =
                            ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImainFile
                             {-# LINE 4248 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmainName =
                            ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImainName
                             {-# LINE 4254 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnt =
                            ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInt
                             {-# LINE 4260 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIoptions
                             {-# LINE 4266 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruleKinds =
                            ({-# LINE 1235 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruleKinds
                             {-# LINE 4272 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOsynmap =
                            ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIsynmap
                             {-# LINE 4278 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOusageInfo =
                            ({-# LINE 1220 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIusageInfo
                             {-# LINE 4284 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        ( _hdIerrors,_hdImrules,_hdIruledefs,_hdIruleuses,_hdIsem_rules) =
                            hd_ _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnt _hdOoptions _hdOruleKinds _hdOsynmap _hdOusageInfo
                        ( _tlIerrors,_tlImrules,_tlIruledefs,_tlIruleuses,_tlIsem_rules) =
                            tl_ _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnt _tlOoptions _tlOruleKinds _tlOsynmap _tlOusageInfo
                    in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
sem_ERules_Nil :: T_ERules
sem_ERules_Nil =
    (T_ERules (\ _lhsIallInhmap
                 _lhsIallSynmap
                 _lhsIchildTypes
                 _lhsIcon
                 _lhsIinhmap
                 _lhsIlazyIntras
                 _lhsIlocalAttrTypes
                 _lhsImainFile
                 _lhsImainName
                 _lhsInt
                 _lhsIoptions
                 _lhsIruleKinds
                 _lhsIsynmap
                 _lhsIusageInfo ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmrules :: (Map Identifier (VisitKind -> Either Error PP_Doc))
                        _lhsOruledefs :: (Map Identifier (Set String))
                        _lhsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _lhsOsem_rules :: PP_Doc
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                        _lhsOerrors =
                            ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Seq.empty
                             {-# LINE 4316 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 972, column 32)
                        _lhsOmrules =
                            ({-# LINE 972 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 4322 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1280, column 34)
                        _lhsOruledefs =
                            ({-# LINE 1280 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 4328 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1281, column 34)
                        _lhsOruleuses =
                            ({-# LINE 1281 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 4334 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 971, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 971 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             empty
                             {-# LINE 4340 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                    in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
-- ExecutionPlan -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainFile             : String
         mainName             : String
         options              : Options
         synmap               : Map NontermIdent Attributes
      synthesized attributes:
         code                 : PP_Doc
         datas                : PP_Doc
         errors               : Seq Error
         modules              : PP_Doc
   alternatives:
      alternative ExecutionPlan:
         child nonts          : ENonterminals 
         child typeSyns       : {TypeSyns}
         child wrappers       : {Set NontermIdent}
         child derivings      : {Derivings}
         visit 0:
            local wrappersExtra : _
            local commonExtra : _
            local lateSemBndTp : _
            local lateSemBndDef : _
-}
-- cata
sem_ExecutionPlan :: ExecutionPlan ->
                     T_ExecutionPlan
sem_ExecutionPlan (ExecutionPlan _nonts _typeSyns _wrappers _derivings) =
    (sem_ExecutionPlan_ExecutionPlan (sem_ENonterminals _nonts) _typeSyns _wrappers _derivings)
-- semantic domain
newtype T_ExecutionPlan = T_ExecutionPlan ((Map NontermIdent Attributes) ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                           String ->
                                           String ->
                                           Options ->
                                           (Map NontermIdent Attributes) ->
                                           ( PP_Doc,PP_Doc,(Seq Error),PP_Doc))
data Inh_ExecutionPlan = Inh_ExecutionPlan {inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes),localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainFile_Inh_ExecutionPlan :: String,mainName_Inh_ExecutionPlan :: String,options_Inh_ExecutionPlan :: Options,synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes)}
data Syn_ExecutionPlan = Syn_ExecutionPlan {code_Syn_ExecutionPlan :: PP_Doc,datas_Syn_ExecutionPlan :: PP_Doc,errors_Syn_ExecutionPlan :: (Seq Error),modules_Syn_ExecutionPlan :: PP_Doc}
wrap_ExecutionPlan :: T_ExecutionPlan ->
                      Inh_ExecutionPlan ->
                      Syn_ExecutionPlan
wrap_ExecutionPlan (T_ExecutionPlan sem) (Inh_ExecutionPlan _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap) =
    (let ( _lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOmodules) = sem _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap
     in  (Syn_ExecutionPlan _lhsOcode _lhsOdatas _lhsOerrors _lhsOmodules))
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals ->
                                   TypeSyns ->
                                   (Set NontermIdent) ->
                                   Derivings ->
                                   T_ExecutionPlan
sem_ExecutionPlan_ExecutionPlan (T_ENonterminals nonts_) typeSyns_ wrappers_ derivings_ =
    (T_ExecutionPlan (\ _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainFile
                        _lhsImainName
                        _lhsIoptions
                        _lhsIsynmap ->
                          (let _lhsOcode :: PP_Doc
                               _lhsOdatas :: PP_Doc
                               _nontsOwrappers :: (Set NontermIdent)
                               _nontsOtypeSyns :: TypeSyns
                               _nontsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _nontsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _nontsOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _nontsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _nontsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _nontsOallInitStates :: (Map NontermIdent Int)
                               _lhsOerrors :: (Seq Error)
                               _lhsOmodules :: PP_Doc
                               _nontsOinhmap :: (Map NontermIdent Attributes)
                               _nontsOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _nontsOmainFile :: String
                               _nontsOmainName :: String
                               _nontsOoptions :: Options
                               _nontsOsynmap :: (Map NontermIdent Attributes)
                               _nontsIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                               _nontsIcode :: PP_Doc
                               _nontsIdatas :: PP_Doc
                               _nontsIerrors :: (Seq Error)
                               _nontsIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _nontsIinitStates :: (Map NontermIdent Int)
                               _nontsImodules :: PP_Doc
                               _nontsIsemFunBndDefs :: (Seq PP_Doc)
                               _nontsIsemFunBndTps :: (Seq PP_Doc)
                               _nontsIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _nontsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _nontsIvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 103, column 3)
                               _lhsOcode =
                                   ({-# LINE 103 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIcode  >-< _wrappersExtra
                                    {-# LINE 4437 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 104, column 3)
                               _lhsOdatas =
                                   ({-# LINE 104 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIdatas >-< _commonExtra
                                    {-# LINE 4443 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 110, column 3)
                               _nontsOwrappers =
                                   ({-# LINE 110 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    wrappers_
                                    {-# LINE 4449 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 171, column 3)
                               _nontsOtypeSyns =
                                   ({-# LINE 171 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    typeSyns_
                                    {-# LINE 4455 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 661, column 3)
                               _wrappersExtra =
                                   ({-# LINE 661 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    if lateHigherOrderBinding _lhsIoptions
                                    then _lateSemBndDef
                                    else empty
                                    {-# LINE 4463 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 664, column 3)
                               _commonExtra =
                                   ({-# LINE 664 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    if lateHigherOrderBinding _lhsIoptions
                                    then _lateSemBndTp
                                    else empty
                                    {-# LINE 4471 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 667, column 3)
                               _lateSemBndTp =
                                   ({-# LINE 667 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    "and" >#< lateBindingTypeNm _lhsImainName >#< "=" >#< ppRecordTp (toList _nontsIsemFunBndTps)
                                    {-# LINE 4477 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 668, column 3)
                               _lateSemBndDef =
                                   ({-# LINE 668 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    "and" >#< lateBindingFieldNm _lhsImainName >#< ":" >#< lateBindingTypeNm _lhsImainName >#< "="
                                    >-< (indent 2 $ ppRecordVal $ toList _nontsIsemFunBndDefs)
                                    {-# LINE 4484 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1154, column 19)
                               _nontsOallchildvisit =
                                   ({-# LINE 1154 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIchildvisit
                                    {-# LINE 4490 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1312, column 19)
                               _nontsOavisitdefs =
                                   ({-# LINE 1312 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIvisitdefs
                                    {-# LINE 4496 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1313, column 19)
                               _nontsOavisituses =
                                   ({-# LINE 1313 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIvisituses
                                    {-# LINE 4502 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1404, column 3)
                               _nontsOallFromToStates =
                                   ({-# LINE 1404 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIfromToStates
                                    {-# LINE 4508 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1448, column 3)
                               _nontsOallVisitKinds =
                                   ({-# LINE 1448 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIvisitKinds
                                    {-# LINE 4514 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- "./src-ag/ExecutionPlan2Caml.ag"(line 1462, column 3)
                               _nontsOallInitStates =
                                   ({-# LINE 1462 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIinitStates
                                    {-# LINE 4520 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsIerrors
                                    {-# LINE 4526 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (up)
                               _lhsOmodules =
                                   ({-# LINE 100 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _nontsImodules
                                    {-# LINE 4532 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOinhmap =
                                   ({-# LINE 405 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 4538 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOlocalAttrTypes =
                                   ({-# LINE 1427 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 4544 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainFile =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 4550 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainName =
                                   ({-# LINE 60 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsImainName
                                    {-# LINE 4556 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOoptions =
                                   ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 4562 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOsynmap =
                                   ({-# LINE 406 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 4568 "dist/build/ExecutionPlan2Caml.hs" #-}
                                    )
                               ( _nontsIchildvisit,_nontsIcode,_nontsIdatas,_nontsIerrors,_nontsIfromToStates,_nontsIinitStates,_nontsImodules,_nontsIsemFunBndDefs,_nontsIsemFunBndTps,_nontsIvisitKinds,_nontsIvisitdefs,_nontsIvisituses) =
                                   nonts_ _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOoptions _nontsOsynmap _nontsOtypeSyns _nontsOwrappers
                           in  ( _lhsOcode,_lhsOdatas,_lhsOerrors,_lhsOmodules))))
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         attrs                : Map String (Maybe NonLocalAttr)
         pos                  : Pos
         semfunc              : PP_Doc
         tks                  : [HsToken]
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (Expression _pos _tks) =
    (sem_Expression_Expression _pos _tks)
-- semantic domain
newtype T_Expression = T_Expression (( (Map String (Maybe NonLocalAttr)),Pos,PP_Doc,([HsToken])))
data Inh_Expression = Inh_Expression {}
data Syn_Expression = Syn_Expression {attrs_Syn_Expression :: (Map String (Maybe NonLocalAttr)),pos_Syn_Expression :: Pos,semfunc_Syn_Expression :: PP_Doc,tks_Syn_Expression :: ([HsToken])}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression (T_Expression sem) (Inh_Expression) =
    (let ( _lhsOattrs,_lhsOpos,_lhsOsemfunc,_lhsOtks) = sem
     in  (Syn_Expression _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression
sem_Expression_Expression pos_ tks_ =
    (T_Expression (let _lhsOtks :: ([HsToken])
                       _lhsOpos :: Pos
                       _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                       _lhsOsemfunc :: PP_Doc
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1026, column 16)
                       _lhsOtks =
                           ({-# LINE 1026 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            tks_
                            {-# LINE 4613 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1047, column 29)
                       _lhsOpos =
                           ({-# LINE 1047 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            pos_
                            {-# LINE 4619 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1139, column 16)
                       _lhsOattrs =
                           ({-# LINE 1139 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 4625 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1140, column 16)
                       _lhsOsemfunc =
                           ({-# LINE 1140 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 4631 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                   in  ( _lhsOattrs,_lhsOpos,_lhsOsemfunc,_lhsOtks)))
-- HsToken -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         attrs                : Map String (Maybe NonLocalAttr)
         tok                  : (Pos,String)
   alternatives:
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tok         : _
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local mbAttr      : _
            local addTrace    : _
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken ->
               T_HsToken
sem_HsToken (AGLocal _var _pos _rdesc) =
    (sem_HsToken_AGLocal _var _pos _rdesc)
sem_HsToken (AGField _field _attr _pos _rdesc) =
    (sem_HsToken_AGField _field _attr _pos _rdesc)
sem_HsToken (HsToken _value _pos) =
    (sem_HsToken_HsToken _value _pos)
sem_HsToken (CharToken _value _pos) =
    (sem_HsToken_CharToken _value _pos)
sem_HsToken (StrToken _value _pos) =
    (sem_HsToken_StrToken _value _pos)
sem_HsToken (Err _mesg _pos) =
    (sem_HsToken_Err _mesg _pos)
-- semantic domain
newtype T_HsToken = T_HsToken (( (Map String (Maybe NonLocalAttr)),((Pos,String))))
data Inh_HsToken = Inh_HsToken {}
data Syn_HsToken = Syn_HsToken {attrs_Syn_HsToken :: (Map String (Maybe NonLocalAttr)),tok_Syn_HsToken :: ((Pos,String))}
wrap_HsToken :: T_HsToken ->
                Inh_HsToken ->
                Syn_HsToken
wrap_HsToken (T_HsToken sem) (Inh_HsToken) =
    (let ( _lhsOattrs,_lhsOtok) = sem
     in  (Syn_HsToken _lhsOattrs _lhsOtok))
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGLocal var_ pos_ rdesc_ =
    (T_HsToken (let _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    _lhsOtok :: ((Pos,String))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1098, column 15)
                    _lhsOattrs =
                        ({-# LINE 1098 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.singleton (fieldname var_) Nothing
                         {-# LINE 4704 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1360, column 15)
                    _tok =
                        ({-# LINE 1360 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_,fieldname var_)
                         {-# LINE 4710 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- copy rule (from local)
                    _lhsOtok =
                        ({-# LINE 1362 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         _tok
                         {-# LINE 4716 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGField field_ attr_ pos_ rdesc_ =
    (T_HsToken (let _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    _lhsOtok :: ((Pos,String))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1099, column 15)
                    _mbAttr =
                        ({-# LINE 1099 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         if field_ == _INST || field_ == _FIELD || field_ == _INST'
                         then Nothing
                         else Just $ mkNonLocalAttr (field_ == _LHS) field_ attr_
                         {-# LINE 4733 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1102, column 15)
                    _lhsOattrs =
                        ({-# LINE 1102 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.singleton (attrname True field_ attr_) _mbAttr
                         {-# LINE 4739 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1364, column 8)
                    _addTrace =
                        ({-# LINE 1364 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         case rdesc_ of
                           Just d  -> \x -> "(prerr_endline " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ "; " ++ x ++ ")"
                           Nothing -> id
                         {-# LINE 4747 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1367, column 8)
                    _lhsOtok =
                        ({-# LINE 1367 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, _addTrace     $ attrname True field_ attr_)
                         {-# LINE 4753 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken
sem_HsToken_HsToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1369, column 14)
                    _lhsOtok =
                        ({-# LINE 1369 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, value_)
                         {-# LINE 4766 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1096, column 37)
                    _lhsOattrs =
                        ({-# LINE 1096 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.empty
                         {-# LINE 4772 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken
sem_HsToken_CharToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1371, column 16)
                    _lhsOtok =
                        ({-# LINE 1371 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, if null value_
                                   then ""
                                   else showCharShort (head value_)
                         )
                         {-# LINE 4788 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1096, column 37)
                    _lhsOattrs =
                        ({-# LINE 1096 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.empty
                         {-# LINE 4794 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken
sem_HsToken_StrToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1376, column 16)
                    _lhsOtok =
                        ({-# LINE 1376 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, showStrShort value_)
                         {-# LINE 4807 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1096, column 37)
                    _lhsOattrs =
                        ({-# LINE 1096 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.empty
                         {-# LINE 4813 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken
sem_HsToken_Err mesg_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "./src-ag/ExecutionPlan2Caml.ag"(line 1377, column 16)
                    _lhsOtok =
                        ({-# LINE 1377 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         (pos_, "")
                         {-# LINE 4826 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                    -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1096, column 37)
                    _lhsOattrs =
                        ({-# LINE 1096 "./src-ag/ExecutionPlan2Caml.ag" #-}
                         Map.empty
                         {-# LINE 4832 "dist/build/ExecutionPlan2Caml.hs" #-}
                         )
                in  ( _lhsOattrs,_lhsOtok)))
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tks                  : [(Pos,String)]
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
-- cata
sem_HsTokens :: HsTokens ->
                T_HsTokens
sem_HsTokens list =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list))
-- semantic domain
newtype T_HsTokens = T_HsTokens (( ([(Pos,String)])))
data Inh_HsTokens = Inh_HsTokens {}
data Syn_HsTokens = Syn_HsTokens {tks_Syn_HsTokens :: ([(Pos,String)])}
wrap_HsTokens :: T_HsTokens ->
                 Inh_HsTokens ->
                 Syn_HsTokens
wrap_HsTokens (T_HsTokens sem) (Inh_HsTokens) =
    (let ( _lhsOtks) = sem
     in  (Syn_HsTokens _lhsOtks))
sem_HsTokens_Cons :: T_HsToken ->
                     T_HsTokens ->
                     T_HsTokens
sem_HsTokens_Cons (T_HsToken hd_) (T_HsTokens tl_) =
    (T_HsTokens (let _lhsOtks :: ([(Pos,String)])
                     _hdIattrs :: (Map String (Maybe NonLocalAttr))
                     _hdItok :: ((Pos,String))
                     _tlItks :: ([(Pos,String)])
                     -- "./src-ag/ExecutionPlan2Caml.ag"(line 1356, column 10)
                     _lhsOtks =
                         ({-# LINE 1356 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          _hdItok : _tlItks
                          {-# LINE 4873 "dist/build/ExecutionPlan2Caml.hs" #-}
                          )
                     ( _hdIattrs,_hdItok) =
                         hd_
                     ( _tlItks) =
                         tl_
                 in  ( _lhsOtks)))
sem_HsTokens_Nil :: T_HsTokens
sem_HsTokens_Nil =
    (T_HsTokens (let _lhsOtks :: ([(Pos,String)])
                     -- "./src-ag/ExecutionPlan2Caml.ag"(line 1357, column 10)
                     _lhsOtks =
                         ({-# LINE 1357 "./src-ag/ExecutionPlan2Caml.ag" #-}
                          []
                          {-# LINE 4887 "dist/build/ExecutionPlan2Caml.hs" #-}
                          )
                 in  ( _lhsOtks)))
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot ->
                    T_HsTokensRoot
sem_HsTokensRoot (HsTokensRoot _tokens) =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens))
-- semantic domain
newtype T_HsTokensRoot = T_HsTokensRoot (( ))
data Inh_HsTokensRoot = Inh_HsTokensRoot {}
data Syn_HsTokensRoot = Syn_HsTokensRoot {}
wrap_HsTokensRoot :: T_HsTokensRoot ->
                     Inh_HsTokensRoot ->
                     Syn_HsTokensRoot
wrap_HsTokensRoot (T_HsTokensRoot sem) (Inh_HsTokensRoot) =
    (let ( ) = sem
     in  (Syn_HsTokensRoot))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens ->
                                 T_HsTokensRoot
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_) =
    (T_HsTokensRoot (let _tokensItks :: ([(Pos,String)])
                         ( _tokensItks) =
                             tokens_
                     in  ( )))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         anyLazyKind          : Bool
         inhmap               : Attributes
         localAttrTypes       : Map Identifier Type
         options              : Options
         synmap               : Attributes
      synthesized attributes:
         attrTypes            : PP_Doc
         attrs                : Set String
         copy                 : Pattern 
         extraDefs            : [(PP_Doc,PP_Doc)]
         isUnderscore         : Bool
         sem_lhs              :  PP_Doc 
   alternatives:
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local var         : _
            local hasTp       : _
            local o_sigs      : _
            local mbTp        : _
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern ((Map NontermIdent Attributes) ->
                               (Map NontermIdent Attributes) ->
                               Bool ->
                               Attributes ->
                               (Map Identifier Type) ->
                               Options ->
                               Attributes ->
                               ( PP_Doc,(Set String),Pattern,([(PP_Doc,PP_Doc)]),Bool,( PP_Doc )))
data Inh_Pattern = Inh_Pattern {allInhmap_Inh_Pattern :: (Map NontermIdent Attributes),allSynmap_Inh_Pattern :: (Map NontermIdent Attributes),anyLazyKind_Inh_Pattern :: Bool,inhmap_Inh_Pattern :: Attributes,localAttrTypes_Inh_Pattern :: (Map Identifier Type),options_Inh_Pattern :: Options,synmap_Inh_Pattern :: Attributes}
data Syn_Pattern = Syn_Pattern {attrTypes_Syn_Pattern :: PP_Doc,attrs_Syn_Pattern :: (Set String),copy_Syn_Pattern :: Pattern,extraDefs_Syn_Pattern :: ([(PP_Doc,PP_Doc)]),isUnderscore_Syn_Pattern :: Bool,sem_lhs_Syn_Pattern :: ( PP_Doc )}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
    (let ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs) = sem _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
     in  (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOisUnderscore _lhsOsem_lhs))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (\ _lhsIallInhmap
                  _lhsIallSynmap
                  _lhsIanyLazyKind
                  _lhsIinhmap
                  _lhsIlocalAttrTypes
                  _lhsIoptions
                  _lhsIsynmap ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOisUnderscore :: Bool
                         _lhsOattrTypes :: PP_Doc
                         _lhsOattrs :: (Set String)
                         _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                         _lhsOcopy :: Pattern
                         _patsOallInhmap :: (Map NontermIdent Attributes)
                         _patsOallSynmap :: (Map NontermIdent Attributes)
                         _patsOanyLazyKind :: Bool
                         _patsOinhmap :: Attributes
                         _patsOlocalAttrTypes :: (Map Identifier Type)
                         _patsOoptions :: Options
                         _patsOsynmap :: Attributes
                         _patsIattrTypes :: PP_Doc
                         _patsIattrs :: (Set String)
                         _patsIcopy :: Patterns
                         _patsIextraDefs :: ([(PP_Doc,PP_Doc)])
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1064, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1064 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              pp_parens $ name_ >#< pp_block "(" ")" "," _patsIsem_lhs
                              {-# LINE 5029 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1073, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1073 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              False
                              {-# LINE 5035 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIattrTypes
                              {-# LINE 5041 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                         _lhsOattrs =
                             ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIattrs
                              {-# LINE 5047 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                         _lhsOextraDefs =
                             ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIextraDefs
                              {-# LINE 5053 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 5059 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 5065 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallInhmap =
                             ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 5071 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallSynmap =
                             ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 5077 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOanyLazyKind =
                             ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 5083 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinhmap =
                             ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIinhmap
                              {-# LINE 5089 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOlocalAttrTypes =
                             ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 5095 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOoptions =
                             ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIoptions
                              {-# LINE 5101 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsynmap =
                             ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIsynmap
                              {-# LINE 5107 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         ( _patsIattrTypes,_patsIattrs,_patsIcopy,_patsIextraDefs,_patsIsem_lhs) =
                             pats_ _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs))))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (\ _lhsIallInhmap
                  _lhsIallSynmap
                  _lhsIanyLazyKind
                  _lhsIinhmap
                  _lhsIlocalAttrTypes
                  _lhsIoptions
                  _lhsIsynmap ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOisUnderscore :: Bool
                         _lhsOattrTypes :: PP_Doc
                         _lhsOattrs :: (Set String)
                         _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                         _lhsOcopy :: Pattern
                         _patsOallInhmap :: (Map NontermIdent Attributes)
                         _patsOallSynmap :: (Map NontermIdent Attributes)
                         _patsOanyLazyKind :: Bool
                         _patsOinhmap :: Attributes
                         _patsOlocalAttrTypes :: (Map Identifier Type)
                         _patsOoptions :: Options
                         _patsOsynmap :: Attributes
                         _patsIattrTypes :: PP_Doc
                         _patsIattrs :: (Set String)
                         _patsIcopy :: Patterns
                         _patsIextraDefs :: ([(PP_Doc,PP_Doc)])
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1063, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1063 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              pp_block "(" ")" "," _patsIsem_lhs
                              {-# LINE 5145 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1074, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1074 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              False
                              {-# LINE 5151 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIattrTypes
                              {-# LINE 5157 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                         _lhsOattrs =
                             ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIattrs
                              {-# LINE 5163 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                         _lhsOextraDefs =
                             ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patsIextraDefs
                              {-# LINE 5169 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 5175 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 5181 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallInhmap =
                             ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 5187 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallSynmap =
                             ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 5193 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOanyLazyKind =
                             ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 5199 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinhmap =
                             ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIinhmap
                              {-# LINE 5205 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOlocalAttrTypes =
                             ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 5211 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOoptions =
                             ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIoptions
                              {-# LINE 5217 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsynmap =
                             ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIsynmap
                              {-# LINE 5223 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         ( _patsIattrTypes,_patsIattrs,_patsIcopy,_patsIextraDefs,_patsIsem_lhs) =
                             pats_ _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs))))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (\ _lhsIallInhmap
                  _lhsIallSynmap
                  _lhsIanyLazyKind
                  _lhsIinhmap
                  _lhsIlocalAttrTypes
                  _lhsIoptions
                  _lhsIsynmap ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                         _lhsOisUnderscore :: Bool
                         _lhsOattrs :: (Set String)
                         _lhsOattrTypes :: PP_Doc
                         _lhsOcopy :: Pattern
                         _patOallInhmap :: (Map NontermIdent Attributes)
                         _patOallSynmap :: (Map NontermIdent Attributes)
                         _patOanyLazyKind :: Bool
                         _patOinhmap :: Attributes
                         _patOlocalAttrTypes :: (Map Identifier Type)
                         _patOoptions :: Options
                         _patOsynmap :: Attributes
                         _patIattrTypes :: PP_Doc
                         _patIattrs :: (Set String)
                         _patIcopy :: Pattern
                         _patIextraDefs :: ([(PP_Doc,PP_Doc)])
                         _patIisUnderscore :: Bool
                         _patIsem_lhs :: ( PP_Doc )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1055, column 17)
                         _var =
                             ({-# LINE 1055 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              text $ attrname False field_ attr_
                              {-# LINE 5263 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1056, column 17)
                         _hasTp =
                             ({-# LINE 1056 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              isJust _mbTp
                              {-# LINE 5269 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1057, column 17)
                         _o_sigs =
                             ({-# LINE 1057 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              typeSigs _lhsIoptions
                              {-# LINE 5275 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1059, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1059 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              ppArg (_hasTp     && _o_sigs    ) _var     (maybe (text "?no type?") ppTp _mbTp    )
                              {-# LINE 5281 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1060, column 17)
                         _lhsOextraDefs =
                             ({-# LINE 1060 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              if _patIisUnderscore
                              then []
                              else [ (_patIsem_lhs, _var    ) ]
                              {-# LINE 5289 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1075, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1075 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              False
                              {-# LINE 5295 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1081, column 3)
                         _lhsOattrs =
                             ({-# LINE 1081 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              Set.insert (attrname False field_ attr_) _patIattrs
                              {-# LINE 5301 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1087, column 3)
                         _mbTp =
                             ({-# LINE 1087 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              if field_ == _LHS
                              then Map.lookup attr_ _lhsIsynmap
                              else if field_ == _LOC
                                   then Map.lookup attr_ _lhsIlocalAttrTypes
                                   else Nothing
                              {-# LINE 5311 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1092, column 3)
                         _lhsOattrTypes =
                             ({-# LINE 1092 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              maybe empty (\tp -> (attrname False field_ attr_) >#< "::" >#< ppTp tp) _mbTp
                              >-< _patIattrTypes
                              {-# LINE 5318 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy
                              {-# LINE 5324 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 5330 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallInhmap =
                             ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 5336 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallSynmap =
                             ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 5342 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOanyLazyKind =
                             ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 5348 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinhmap =
                             ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIinhmap
                              {-# LINE 5354 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOlocalAttrTypes =
                             ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 5360 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOoptions =
                             ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIoptions
                              {-# LINE 5366 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsynmap =
                             ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIsynmap
                              {-# LINE 5372 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         ( _patIattrTypes,_patIattrs,_patIcopy,_patIextraDefs,_patIisUnderscore,_patIsem_lhs) =
                             pat_ _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs))))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (\ _lhsIallInhmap
                  _lhsIallSynmap
                  _lhsIanyLazyKind
                  _lhsIinhmap
                  _lhsIlocalAttrTypes
                  _lhsIoptions
                  _lhsIsynmap ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOattrTypes :: PP_Doc
                         _lhsOattrs :: (Set String)
                         _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                         _lhsOcopy :: Pattern
                         _lhsOisUnderscore :: Bool
                         _patOallInhmap :: (Map NontermIdent Attributes)
                         _patOallSynmap :: (Map NontermIdent Attributes)
                         _patOanyLazyKind :: Bool
                         _patOinhmap :: Attributes
                         _patOlocalAttrTypes :: (Map Identifier Type)
                         _patOoptions :: Options
                         _patOsynmap :: Attributes
                         _patIattrTypes :: PP_Doc
                         _patIattrs :: (Set String)
                         _patIcopy :: Pattern
                         _patIextraDefs :: ([(PP_Doc,PP_Doc)])
                         _patIisUnderscore :: Bool
                         _patIsem_lhs :: ( PP_Doc )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1066, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1066 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              pp_parens (text "lazy" >#< _patIsem_lhs)
                              {-# LINE 5410 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patIattrTypes
                              {-# LINE 5416 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                         _lhsOattrs =
                             ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patIattrs
                              {-# LINE 5422 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                         _lhsOextraDefs =
                             ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patIextraDefs
                              {-# LINE 5428 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 5434 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 5440 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (up)
                         _lhsOisUnderscore =
                             ({-# LINE 1071 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _patIisUnderscore
                              {-# LINE 5446 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallInhmap =
                             ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 5452 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOallSynmap =
                             ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 5458 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOanyLazyKind =
                             ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 5464 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOinhmap =
                             ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIinhmap
                              {-# LINE 5470 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOlocalAttrTypes =
                             ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 5476 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOoptions =
                             ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIoptions
                              {-# LINE 5482 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- copy rule (down)
                         _patOsynmap =
                             ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              _lhsIsynmap
                              {-# LINE 5488 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         ( _patIattrTypes,_patIattrs,_patIcopy,_patIextraDefs,_patIisUnderscore,_patIsem_lhs) =
                             pat_ _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs))))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (\ _lhsIallInhmap
                  _lhsIallSynmap
                  _lhsIanyLazyKind
                  _lhsIinhmap
                  _lhsIlocalAttrTypes
                  _lhsIoptions
                  _lhsIsynmap ->
                    (let _lhsOsem_lhs :: ( PP_Doc )
                         _lhsOisUnderscore :: Bool
                         _lhsOattrTypes :: PP_Doc
                         _lhsOattrs :: (Set String)
                         _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                         _lhsOcopy :: Pattern
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1065, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1065 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              text "_"
                              {-# LINE 5513 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- "./src-ag/ExecutionPlan2Caml.ag"(line 1076, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1076 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              True
                              {-# LINE 5519 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              empty
                              {-# LINE 5525 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                         _lhsOattrs =
                             ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              Set.empty
                              {-# LINE 5531 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                         _lhsOextraDefs =
                             ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                              []
                              {-# LINE 5537 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 5543 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 5549 "dist/build/ExecutionPlan2Caml.hs" #-}
                              )
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOisUnderscore,_lhsOsem_lhs))))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         anyLazyKind          : Bool
         inhmap               : Attributes
         localAttrTypes       : Map Identifier Type
         options              : Options
         synmap               : Attributes
      synthesized attributes:
         attrTypes            : PP_Doc
         attrs                : Set String
         copy                 : Patterns 
         extraDefs            : [(PP_Doc,PP_Doc)]
         sem_lhs              : [PP_Doc]
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
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns ((Map NontermIdent Attributes) ->
                                 (Map NontermIdent Attributes) ->
                                 Bool ->
                                 Attributes ->
                                 (Map Identifier Type) ->
                                 Options ->
                                 Attributes ->
                                 ( PP_Doc,(Set String),Patterns,([(PP_Doc,PP_Doc)]),([PP_Doc])))
data Inh_Patterns = Inh_Patterns {allInhmap_Inh_Patterns :: (Map NontermIdent Attributes),allSynmap_Inh_Patterns :: (Map NontermIdent Attributes),anyLazyKind_Inh_Patterns :: Bool,inhmap_Inh_Patterns :: Attributes,localAttrTypes_Inh_Patterns :: (Map Identifier Type),options_Inh_Patterns :: Options,synmap_Inh_Patterns :: Attributes}
data Syn_Patterns = Syn_Patterns {attrTypes_Syn_Patterns :: PP_Doc,attrs_Syn_Patterns :: (Set String),copy_Syn_Patterns :: Patterns,extraDefs_Syn_Patterns :: ([(PP_Doc,PP_Doc)]),sem_lhs_Syn_Patterns :: ([PP_Doc])}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
    (let ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOsem_lhs) = sem _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
     in  (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOextraDefs _lhsOsem_lhs))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (\ _lhsIallInhmap
                   _lhsIallSynmap
                   _lhsIanyLazyKind
                   _lhsIinhmap
                   _lhsIlocalAttrTypes
                   _lhsIoptions
                   _lhsIsynmap ->
                     (let _lhsOattrTypes :: PP_Doc
                          _lhsOattrs :: (Set String)
                          _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                          _lhsOsem_lhs :: ([PP_Doc])
                          _lhsOcopy :: Patterns
                          _hdOallInhmap :: (Map NontermIdent Attributes)
                          _hdOallSynmap :: (Map NontermIdent Attributes)
                          _hdOanyLazyKind :: Bool
                          _hdOinhmap :: Attributes
                          _hdOlocalAttrTypes :: (Map Identifier Type)
                          _hdOoptions :: Options
                          _hdOsynmap :: Attributes
                          _tlOallInhmap :: (Map NontermIdent Attributes)
                          _tlOallSynmap :: (Map NontermIdent Attributes)
                          _tlOanyLazyKind :: Bool
                          _tlOinhmap :: Attributes
                          _tlOlocalAttrTypes :: (Map Identifier Type)
                          _tlOoptions :: Options
                          _tlOsynmap :: Attributes
                          _hdIattrTypes :: PP_Doc
                          _hdIattrs :: (Set String)
                          _hdIcopy :: Pattern
                          _hdIextraDefs :: ([(PP_Doc,PP_Doc)])
                          _hdIisUnderscore :: Bool
                          _hdIsem_lhs :: ( PP_Doc )
                          _tlIattrTypes :: PP_Doc
                          _tlIattrs :: (Set String)
                          _tlIcopy :: Patterns
                          _tlIextraDefs :: ([(PP_Doc,PP_Doc)])
                          _tlIsem_lhs :: ([PP_Doc])
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                          _lhsOattrTypes =
                              ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _hdIattrTypes >-< _tlIattrTypes
                               {-# LINE 5646 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                          _lhsOattrs =
                              ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _hdIattrs `Set.union` _tlIattrs
                               {-# LINE 5652 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                          _lhsOextraDefs =
                              ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _hdIextraDefs ++ _tlIextraDefs
                               {-# LINE 5658 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1051, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 1051 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _hdIsem_lhs : _tlIsem_lhs
                               {-# LINE 5664 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 5670 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 5676 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallInhmap =
                              ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIallInhmap
                               {-# LINE 5682 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallSynmap =
                              ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIallSynmap
                               {-# LINE 5688 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOanyLazyKind =
                              ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIanyLazyKind
                               {-# LINE 5694 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhmap =
                              ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIinhmap
                               {-# LINE 5700 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOlocalAttrTypes =
                              ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIlocalAttrTypes
                               {-# LINE 5706 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOoptions =
                              ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIoptions
                               {-# LINE 5712 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynmap =
                              ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIsynmap
                               {-# LINE 5718 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallInhmap =
                              ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIallInhmap
                               {-# LINE 5724 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallSynmap =
                              ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIallSynmap
                               {-# LINE 5730 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOanyLazyKind =
                              ({-# LINE 1243 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIanyLazyKind
                               {-# LINE 5736 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhmap =
                              ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIinhmap
                               {-# LINE 5742 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOlocalAttrTypes =
                              ({-# LINE 1429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIlocalAttrTypes
                               {-# LINE 5748 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOoptions =
                              ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIoptions
                               {-# LINE 5754 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynmap =
                              ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               _lhsIsynmap
                               {-# LINE 5760 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          ( _hdIattrTypes,_hdIattrs,_hdIcopy,_hdIextraDefs,_hdIisUnderscore,_hdIsem_lhs) =
                              hd_ _hdOallInhmap _hdOallSynmap _hdOanyLazyKind _hdOinhmap _hdOlocalAttrTypes _hdOoptions _hdOsynmap
                          ( _tlIattrTypes,_tlIattrs,_tlIcopy,_tlIextraDefs,_tlIsem_lhs) =
                              tl_ _tlOallInhmap _tlOallSynmap _tlOanyLazyKind _tlOinhmap _tlOlocalAttrTypes _tlOoptions _tlOsynmap
                      in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOsem_lhs))))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (\ _lhsIallInhmap
                   _lhsIallSynmap
                   _lhsIanyLazyKind
                   _lhsIinhmap
                   _lhsIlocalAttrTypes
                   _lhsIoptions
                   _lhsIsynmap ->
                     (let _lhsOattrTypes :: PP_Doc
                          _lhsOattrs :: (Set String)
                          _lhsOextraDefs :: ([(PP_Doc,PP_Doc)])
                          _lhsOsem_lhs :: ([PP_Doc])
                          _lhsOcopy :: Patterns
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1085, column 40)
                          _lhsOattrTypes =
                              ({-# LINE 1085 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               empty
                               {-# LINE 5785 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1079, column 36)
                          _lhsOattrs =
                              ({-# LINE 1079 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               Set.empty
                               {-# LINE 5791 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1052, column 39)
                          _lhsOextraDefs =
                              ({-# LINE 1052 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               []
                               {-# LINE 5797 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1051, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 1051 "./src-ag/ExecutionPlan2Caml.ag" #-}
                               []
                               {-# LINE 5803 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 5809 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 5815 "dist/build/ExecutionPlan2Caml.hs" #-}
                               )
                      in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOextraDefs,_lhsOsem_lhs))))
-- Visit -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         allintramap          : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         con                  : ConstructorIdent
         inhmap               : Attributes
         mrules               : Map Identifier (VisitKind ->  Either Error PP_Doc)
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         options              : Options
         params               : [Identifier]
         prevVisits           : Map StateIdentifier StateCtx
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         synmap               : Attributes
         terminaldefs         : Set String
      synthesized attributes:
         allvisits            :  VisitStateState 
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         intramap             : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_visit            :   (StateIdentifier,PP_Doc)  
         t_visits             : PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative Visit:
         child ident          : {VisitIdentifier}
         child from           : {StateIdentifier}
         child to             : {StateIdentifier}
         child inh            : {Set Identifier}
         child syn            : {Set Identifier}
         child steps          : VisitSteps 
         child kind           : {VisitKind}
         visit 0:
            local nameTIn_visit : _
            local nameTOut_visit : _
            local nameNextState : _
            local nameCaller_visit : _
            local nextVisitInfo : _
            local t_params    : _
            local t_c_params  : _
            local contpart    : _
            local inhpart     : _
            local synpart     : _
            local ppTypeList  : _
            local o_sigs      : _
            local nextArgsMp  : _
            local nextArgs    : _
            local nextStExp   : _
            local resultval   : _
            local nextStBuild : _
            local nextStRefExp : _
            local prevVisitInfo : _
            local invokecode  : _
            local thisintra   : _
            local nextintra   : _
            local uses        : _
            local inhVarNms   : _
            local defs        : _
            local defsAsMap   : _
            local lazyIntrasInh : _
-}
-- cata
sem_Visit :: Visit ->
             T_Visit
sem_Visit (Visit _ident _from _to _inh _syn _steps _kind) =
    (sem_Visit_Visit _ident _from _to _inh _syn (sem_VisitSteps _steps) _kind)
-- semantic domain
newtype T_Visit = T_Visit ((Map VisitIdentifier (Int,Int)) ->
                           (Map NontermIdent Attributes) ->
                           (Map NontermIdent Int) ->
                           (Map NontermIdent Attributes) ->
                           (Map VisitIdentifier VisitKind) ->
                           (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                           (Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                           (Map VisitIdentifier (Set Identifier)) ->
                           (Map VisitIdentifier (Set Identifier)) ->
                           (Map Identifier Type) ->
                           (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                           ConstructorIdent ->
                           Attributes ->
                           (Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
                           (Map StateIdentifier StateCtx) ->
                           NontermIdent ->
                           Options ->
                           ([Identifier]) ->
                           (Map StateIdentifier StateCtx) ->
                           (Map Identifier (Set String)) ->
                           (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                           Attributes ->
                           (Set String) ->
                           ( ( VisitStateState ),(Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),(Seq Error),(Map VisitIdentifier (Int,Int)),(Map StateIdentifier (Map String (Maybe NonLocalAttr))),(Set String),(Map Identifier (Set VisitKind)),(Map Identifier Int),(  (StateIdentifier,PP_Doc)  ),PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_Visit = Inh_Visit {allFromToStates_Inh_Visit :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_Visit :: (Map NontermIdent Attributes),allInitStates_Inh_Visit :: (Map NontermIdent Int),allSynmap_Inh_Visit :: (Map NontermIdent Attributes),allVisitKinds_Inh_Visit :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_Visit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),allintramap_Inh_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),avisitdefs_Inh_Visit :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_Visit :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_Visit :: (Map Identifier Type),childintros_Inh_Visit :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),con_Inh_Visit :: ConstructorIdent,inhmap_Inh_Visit :: Attributes,mrules_Inh_Visit :: (Map Identifier (VisitKind ->  Either Error PP_Doc)),nextVisits_Inh_Visit :: (Map StateIdentifier StateCtx),nt_Inh_Visit :: NontermIdent,options_Inh_Visit :: Options,params_Inh_Visit :: ([Identifier]),prevVisits_Inh_Visit :: (Map StateIdentifier StateCtx),ruledefs_Inh_Visit :: (Map Identifier (Set String)),ruleuses_Inh_Visit :: (Map Identifier (Map String (Maybe NonLocalAttr))),synmap_Inh_Visit :: Attributes,terminaldefs_Inh_Visit :: (Set String)}
data Syn_Visit = Syn_Visit {allvisits_Syn_Visit :: ( VisitStateState ),childvisit_Syn_Visit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),errors_Syn_Visit :: (Seq Error),fromToStates_Syn_Visit :: (Map VisitIdentifier (Int,Int)),intramap_Syn_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),lazyIntras_Syn_Visit :: (Set String),ruleKinds_Syn_Visit :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_Visit :: (Map Identifier Int),sem_visit_Syn_Visit :: (  (StateIdentifier,PP_Doc)  ),t_visits_Syn_Visit :: PP_Doc,visitKinds_Syn_Visit :: (Map VisitIdentifier VisitKind),visitdefs_Syn_Visit :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_Visit :: (Map VisitIdentifier (Set Identifier))}
wrap_Visit :: T_Visit ->
              Inh_Visit ->
              Syn_Visit
wrap_Visit (T_Visit sem) (Inh_Visit _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
     in  (Syn_Visit _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_Visit_Visit :: VisitIdentifier ->
                   StateIdentifier ->
                   StateIdentifier ->
                   (Set Identifier) ->
                   (Set Identifier) ->
                   T_VisitSteps ->
                   VisitKind ->
                   T_Visit
sem_Visit_Visit ident_ from_ to_ inh_ syn_ (T_VisitSteps steps_) kind_ =
    (T_Visit (\ _lhsIallFromToStates
                _lhsIallInhmap
                _lhsIallInitStates
                _lhsIallSynmap
                _lhsIallVisitKinds
                _lhsIallchildvisit
                _lhsIallintramap
                _lhsIavisitdefs
                _lhsIavisituses
                _lhsIchildTypes
                _lhsIchildintros
                _lhsIcon
                _lhsIinhmap
                _lhsImrules
                _lhsInextVisits
                _lhsInt
                _lhsIoptions
                _lhsIparams
                _lhsIprevVisits
                _lhsIruledefs
                _lhsIruleuses
                _lhsIsynmap
                _lhsIterminaldefs ->
                  (let _lhsOallvisits :: ( VisitStateState )
                       _lhsOt_visits :: PP_Doc
                       _lhsOsem_visit :: (  (StateIdentifier,PP_Doc)  )
                       _stepsOfollow :: PP_Doc
                       _stepsOkind :: VisitKind
                       _stepsOindex :: Int
                       _stepsOprevMaxSimRefs :: Int
                       _stepsOuseParallel :: Bool
                       _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                       _lhsOintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                       _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                       _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                       _lhsOlazyIntras :: (Set String)
                       _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                       _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                       _lhsOerrors :: (Seq Error)
                       _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                       _lhsOruleUsage :: (Map Identifier Int)
                       _stepsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                       _stepsOallInitStates :: (Map NontermIdent Int)
                       _stepsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                       _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                       _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                       _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                       _stepsOchildTypes :: (Map Identifier Type)
                       _stepsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                       _stepsOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                       _stepsOoptions :: Options
                       _stepsOruledefs :: (Map Identifier (Set String))
                       _stepsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                       _stepsIdefs :: (Set String)
                       _stepsIerrors :: (Seq Error)
                       _stepsIindex :: Int
                       _stepsIisLast :: Bool
                       _stepsIlazyIntras :: (Set String)
                       _stepsIprevMaxSimRefs :: Int
                       _stepsIruleKinds :: (Map Identifier (Set VisitKind))
                       _stepsIruleUsage :: (Map Identifier Int)
                       _stepsIsem_steps :: PP_Doc
                       _stepsIsize :: Int
                       _stepsIuses :: (Map String (Maybe NonLocalAttr))
                       _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 434, column 11)
                       _lhsOallvisits =
                           ({-# LINE 434 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 6013 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 537, column 3)
                       _nameTIn_visit =
                           ({-# LINE 537 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            conNmTVisitIn _lhsInt ident_
                            {-# LINE 6019 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 538, column 3)
                       _nameTOut_visit =
                           ({-# LINE 538 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            conNmTVisitOut _lhsInt ident_
                            {-# LINE 6025 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 539, column 3)
                       _nameNextState =
                           ({-# LINE 539 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            type_nt_sem _lhsInt to_
                            {-# LINE 6031 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 540, column 3)
                       _nameCaller_visit =
                           ({-# LINE 540 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            type_caller_visit _lhsInt ident_
                            {-# LINE 6037 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 542, column 3)
                       _nextVisitInfo =
                           ({-# LINE 542 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.findWithDefault ManyVis to_ _lhsInextVisits
                            {-# LINE 6043 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 544, column 3)
                       _t_params =
                           ({-# LINE 544 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppTypeParams _lhsIparams
                            {-# LINE 6049 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 545, column 3)
                       _t_c_params =
                           ({-# LINE 545 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppTypeParams (cont_tvar : map pp _lhsIparams)
                            {-# LINE 6055 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 549, column 3)
                       _lhsOt_visits =
                           ({-# LINE 549 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            "and" >#< _t_c_params     >#< _nameCaller_visit     >#< "=" >#< ppRecordTp
                              [ nm_inh _lhsInt ident_ >#< ":" >#< _t_params     >#< conNmTVisitIn _lhsInt ident_
                              , nm_cont _lhsInt ident_ >#< ":" >#< _t_params     >#< conNmTVisitOut _lhsInt ident_ >#< "->" >#< cont_tvar
                              ]
                            >-< "and" >#< _t_params     >#< _nameTIn_visit      >#< "=" >#< ppRecordTp _inhpart
                            >-< "and" >#< _t_params     >#< _nameTOut_visit     >#< "=" >#< ppRecordTp (_synpart     ++ _contpart    )
                            {-# LINE 6066 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 556, column 3)
                       _contpart =
                           ({-# LINE 556 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            case _nextVisitInfo     of
                              NoneVis -> []
                              _       -> [ nm_outarg_cont _lhsInt ident_ >#< ":" >#< _t_params     >#< _nameNextState     ]
                            {-# LINE 6074 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 560, column 3)
                       _inhpart =
                           ({-# LINE 560 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _ppTypeList     nm_inarg inh_ _lhsIinhmap
                            {-# LINE 6080 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 561, column 3)
                       _synpart =
                           ({-# LINE 561 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _ppTypeList     nm_outarg syn_ _lhsIsynmap
                            {-# LINE 6086 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 562, column 3)
                       _ppTypeList =
                           ({-# LINE 562 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            \f s m -> map (\i -> case Map.lookup i m of
                                                   Just tp -> f i _lhsInt ident_ >#< ":" >#< ppTp tp ) $ Set.toList s
                            {-# LINE 6093 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 796, column 3)
                       _o_sigs =
                           ({-# LINE 796 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            typeSigs _lhsIoptions
                            {-# LINE 6099 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 797, column 3)
                       _lhsOsem_visit =
                           ({-# LINE 797 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ( from_
                            , let resTp = _t_params     >#< _nameTOut_visit
                                  argTp = _t_params     >#< _nameTIn_visit
                                  argMatch = ppRecordVal [ nm_inarg i _lhsInt ident_ >#< "=" >#< lhsname True i | i <- Set.toList inh_ ]
                              in ppFunDecl _o_sigs     (nm_visit ident_) [(argMatch, argTp)] resTp _stepsIsem_steps
                            )
                            {-# LINE 6110 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 804, column 3)
                       _stepsOfollow =
                           ({-# LINE 804 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _nextStBuild     >-< _resultval
                            {-# LINE 6116 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 806, column 3)
                       _nextArgsMp =
                           ({-# LINE 806 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 6122 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 807, column 3)
                       _nextArgs =
                           ({-# LINE 807 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppSpaced $ Map.keys $ _nextArgsMp
                            {-# LINE 6128 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 808, column 3)
                       _nextStExp =
                           ({-# LINE 808 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            nm_st to_ >#< _nextArgs     >#< dummyArg _lhsIoptions (Map.null _nextArgsMp    )
                            {-# LINE 6134 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 810, column 3)
                       _resultval =
                           ({-# LINE 810 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            ppRecordVal
                              (  [ nm_outarg i _lhsInt ident_ >#< "=" >#< lhsname False i | i <- Set.toList syn_ ]
                              ++ [ _nextStRefExp     ])
                            {-# LINE 6142 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 814, column 3)
                       (_nextStBuild,_nextStRefExp) =
                           ({-# LINE 814 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            case _nextVisitInfo     of
                              NoneVis  -> (empty, empty)
                              _        -> ( "let" >#< nextStName >#< "=" >#< _nextStExp     >#< "in"
                                          , nm_outarg_cont _lhsInt ident_ >#< "=" >#< nextStName)
                            {-# LINE 6151 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 830, column 20)
                       _stepsOkind =
                           ({-# LINE 830 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            kind_
                            {-# LINE 6157 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 882, column 22)
                       _stepsOindex =
                           ({-# LINE 882 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            0
                            {-# LINE 6163 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 889, column 22)
                       _stepsOprevMaxSimRefs =
                           ({-# LINE 889 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            0
                            {-# LINE 6169 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 906, column 22)
                       _stepsOuseParallel =
                           ({-# LINE 906 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            False
                            {-# LINE 6175 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1162, column 3)
                       _prevVisitInfo =
                           ({-# LINE 1162 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.findWithDefault ManyVis from_ _lhsInextVisits
                            {-# LINE 6181 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1163, column 3)
                       _lhsOchildvisit =
                           ({-# LINE 1163 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ _invokecode
                            {-# LINE 6187 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1164, column 3)
                       _invokecode =
                           ({-# LINE 1164 "./src-ag/ExecutionPlan2Caml.ag" #-}
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
                                  cont_in = [ nm_outarg i _lhsInt ident_ >#< "=" >#< attrname True chld i | i <- Set.toList syn_ ]
                                            ++ case _nextVisitInfo     of
                                                 NoneVis -> []
                                                 _       -> [ nm_outarg_cont _lhsInt ident_ >#< "=" >#< childNmTo ]
                                  cont_in_tp = ppTypeParams childTpArgs >#< _nameTOut_visit
                                  inps = "let" >#< inpsNm >#< "=" >#< ppRecordVal
                                           [ nm_inh _lhsInt ident_ >#< "=" >#< ppRecordVal inps_in
                                           , nm_cont _lhsInt ident_ >#< "=" >#< contNm
                                           ] >#< "in"
                                  inps_in = [ nm_inarg i _lhsInt ident_ >#< "=" >#< attrname False chld i | i <- Set.toList inh_ ]
                                  call = childNmFrom >|< "." >|< nm_invoke _lhsInt from_ >#< arg
                                  arg = case _prevVisitInfo     of
                                          NoneVis  -> error "error: invocation of a visit from a state that has no next visits"
                                          OneVis _ -> pp inpsNm
                                          ManyVis  -> pp_parens (con_visit _lhsInt ident_ >#< inpsNm)
                              in if kind `compatibleKind` kind_
                                 then Right code
                                 else Left $ IncompatibleVisitKind chld ident_ kind kind_
                            {-# LINE 6220 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1270, column 11)
                       _thisintra =
                           ({-# LINE 1270 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            (_uses     `Map.union` _nextintra    ) `Map.difference` _defsAsMap
                            {-# LINE 6226 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1271, column 11)
                       _lhsOintramap =
                           ({-# LINE 1271 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton from_ _thisintra
                            {-# LINE 6232 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1272, column 11)
                       _nextintra =
                           ({-# LINE 1272 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 6238 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1273, column 11)
                       _uses =
                           ({-# LINE 1273 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            let mp1 = _stepsIuses
                                mp2 = Map.fromList [ (lhsname False i, Just (AttrSyn _LHS i)) | i <- Set.elems syn_ ]
                            in mp1 `Map.union` mp2
                            {-# LINE 6246 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1276, column 11)
                       _inhVarNms =
                           ({-# LINE 1276 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Set.map (lhsname True) inh_
                            {-# LINE 6252 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1277, column 11)
                       _defs =
                           ({-# LINE 1277 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _stepsIdefs `Set.union` _inhVarNms     `Set.union` _lhsIterminaldefs
                            {-# LINE 6258 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1278, column 11)
                       _defsAsMap =
                           ({-# LINE 1278 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.fromList [ (a, Nothing) | a <- Set.elems _defs     ]
                            {-# LINE 6264 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1302, column 11)
                       _lhsOvisitdefs =
                           ({-# LINE 1302 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 6270 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1303, column 11)
                       _lhsOvisituses =
                           ({-# LINE 1303 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 6276 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1335, column 3)
                       _lazyIntrasInh =
                           ({-# LINE 1335 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            case kind_ of
                              VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                              _               -> Set.empty
                            {-# LINE 6284 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1338, column 3)
                       _lhsOlazyIntras =
                           ({-# LINE 1338 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                            {-# LINE 6290 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1401, column 3)
                       _lhsOfromToStates =
                           ({-# LINE 1401 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ (from_, to_)
                            {-# LINE 6296 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- "./src-ag/ExecutionPlan2Caml.ag"(line 1445, column 3)
                       _lhsOvisitKinds =
                           ({-# LINE 1445 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            Map.singleton ident_ kind_
                            {-# LINE 6302 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                       _lhsOerrors =
                           ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _stepsIerrors
                            {-# LINE 6308 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                       _lhsOruleKinds =
                           ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _stepsIruleKinds
                            {-# LINE 6314 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                       _lhsOruleUsage =
                           ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _stepsIruleUsage
                            {-# LINE 6320 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallFromToStates =
                           ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallFromToStates
                            {-# LINE 6326 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallInitStates =
                           ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallInitStates
                            {-# LINE 6332 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallVisitKinds =
                           ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallVisitKinds
                            {-# LINE 6338 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallchildvisit =
                           ({-# LINE 1151 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIallchildvisit
                            {-# LINE 6344 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisitdefs =
                           ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIavisitdefs
                            {-# LINE 6350 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisituses =
                           ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIavisituses
                            {-# LINE 6356 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOchildTypes =
                           ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIchildTypes
                            {-# LINE 6362 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOchildintros =
                           ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIchildintros
                            {-# LINE 6368 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOmrules =
                           ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsImrules
                            {-# LINE 6374 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOoptions =
                           ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIoptions
                            {-# LINE 6380 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruledefs =
                           ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIruledefs
                            {-# LINE 6386 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruleuses =
                           ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                            _lhsIruleuses
                            {-# LINE 6392 "dist/build/ExecutionPlan2Caml.hs" #-}
                            )
                       ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIuses,_stepsIvisitKinds) =
                           steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                   in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- VisitStep ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         follow               : PP_Doc
         kind                 : VisitKind
         mrules               : Map Identifier (VisitKind ->  Either Error PP_Doc)
         options              : Options
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         useParallel          : Bool
      chained attributes:
         index                : Int
         isLast               : Bool
         prevMaxSimRefs       : Int
      synthesized attributes:
         defs                 : Set String
         errors               : Seq Error
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_steps            : PP_Doc
         uses                 : Map String (Maybe NonLocalAttr)
         visitKinds           : Map VisitIdentifier VisitKind
   alternatives:
      alternative Sem:
         child name           : {Identifier}
         visit 0:
            local ruleItf     : _
            local sem_steps   : _
      alternative ChildVisit:
         child child          : {Identifier}
         child nonterm        : {NontermIdent}
         child visit          : {VisitIdentifier}
         visit 0:
            local visitItf    : _
            local childType   : _
            local from        : _
            local to          : _
      alternative PureGroup:
         child steps          : VisitSteps 
         child ordered        : {Bool}
      alternative Sim:
         child steps          : VisitSteps 
         visit 0:
            local useParallel : _
      alternative ChildIntro:
         child child          : {Identifier}
         visit 0:
            local attachItf   : _
            local sem_steps   : _
-}
-- cata
sem_VisitStep :: VisitStep ->
                 T_VisitStep
sem_VisitStep (Sem _name) =
    (sem_VisitStep_Sem _name)
sem_VisitStep (ChildVisit _child _nonterm _visit) =
    (sem_VisitStep_ChildVisit _child _nonterm _visit)
sem_VisitStep (PureGroup _steps _ordered) =
    (sem_VisitStep_PureGroup (sem_VisitSteps _steps) _ordered)
sem_VisitStep (Sim _steps) =
    (sem_VisitStep_Sim (sem_VisitSteps _steps))
sem_VisitStep (ChildIntro _child) =
    (sem_VisitStep_ChildIntro _child)
-- semantic domain
newtype T_VisitStep = T_VisitStep ((Map VisitIdentifier (Int,Int)) ->
                                   (Map NontermIdent Int) ->
                                   (Map VisitIdentifier VisitKind) ->
                                   (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                   (Map VisitIdentifier (Set Identifier)) ->
                                   (Map VisitIdentifier (Set Identifier)) ->
                                   (Map Identifier Type) ->
                                   (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                   PP_Doc ->
                                   Int ->
                                   Bool ->
                                   VisitKind ->
                                   (Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
                                   Options ->
                                   Int ->
                                   (Map Identifier (Set String)) ->
                                   (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                   Bool ->
                                   ( (Set String),(Seq Error),Int,Bool,(Set String),Int,(Map Identifier (Set VisitKind)),(Map Identifier Int),PP_Doc,(Map String (Maybe NonLocalAttr)),(Map VisitIdentifier VisitKind)))
data Inh_VisitStep = Inh_VisitStep {allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_VisitStep :: (Map NontermIdent Int),allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_VisitStep :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_VisitStep :: (Map Identifier Type),childintros_Inh_VisitStep :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),follow_Inh_VisitStep :: PP_Doc,index_Inh_VisitStep :: Int,isLast_Inh_VisitStep :: Bool,kind_Inh_VisitStep :: VisitKind,mrules_Inh_VisitStep :: (Map Identifier (VisitKind ->  Either Error PP_Doc)),options_Inh_VisitStep :: Options,prevMaxSimRefs_Inh_VisitStep :: Int,ruledefs_Inh_VisitStep :: (Map Identifier (Set String)),ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))),useParallel_Inh_VisitStep :: Bool}
data Syn_VisitStep = Syn_VisitStep {defs_Syn_VisitStep :: (Set String),errors_Syn_VisitStep :: (Seq Error),index_Syn_VisitStep :: Int,isLast_Syn_VisitStep :: Bool,lazyIntras_Syn_VisitStep :: (Set String),prevMaxSimRefs_Syn_VisitStep :: Int,ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_VisitStep :: (Map Identifier Int),sem_steps_Syn_VisitStep :: PP_Doc,uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)),visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind)}
wrap_VisitStep :: T_VisitStep ->
                  Inh_VisitStep ->
                  Syn_VisitStep
wrap_VisitStep (T_VisitStep sem) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
    (let ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
     in  (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOuses _lhsOvisitKinds))
sem_VisitStep_Sem :: Identifier ->
                     T_VisitStep
sem_VisitStep_Sem name_ =
    (T_VisitStep (\ _lhsIallFromToStates
                    _lhsIallInitStates
                    _lhsIallVisitKinds
                    _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildTypes
                    _lhsIchildintros
                    _lhsIfollow
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _lhsOerrors :: (Seq Error)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOdefs :: (Set String)
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOlazyIntras :: (Set String)
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 847, column 16)
                           _ruleItf =
                               ({-# LINE 847 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error $ "Rule "  ++ show name_  ++ " not found") name_ _lhsImrules
                                {-# LINE 6534 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 848, column 16)
                           (_lhsOerrors,_sem_steps) =
                               ({-# LINE 848 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                case _ruleItf     _lhsIkind of
                                  Left e     -> (Seq.singleton e, empty)
                                  Right stmt -> (Seq.empty, stmt)
                                {-# LINE 6542 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 851, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 851 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _sem_steps     >-< _lhsIfollow
                                {-# LINE 6548 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1223, column 32)
                           _lhsOruleUsage =
                               ({-# LINE 1223 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.singleton name_ 1
                                {-# LINE 6554 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1233, column 3)
                           _lhsOruleKinds =
                               ({-# LINE 1233 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.singleton name_ (Set.singleton _lhsIkind)
                                {-# LINE 6560 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1318, column 16)
                           _lhsOdefs =
                               ({-# LINE 1318 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                                {-# LINE 6566 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1319, column 16)
                           _lhsOuses =
                               ({-# LINE 1319 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                                {-# LINE 6572 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Set.empty
                                {-# LINE 6578 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                mempty
                                {-# LINE 6584 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIindex
                                {-# LINE 6590 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 896 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIisLast
                                {-# LINE 6596 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 6602 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds))))
sem_VisitStep_ChildVisit :: Identifier ->
                            NontermIdent ->
                            VisitIdentifier ->
                            T_VisitStep
sem_VisitStep_ChildVisit child_ nonterm_ visit_ =
    (T_VisitStep (\ _lhsIallFromToStates
                    _lhsIallInitStates
                    _lhsIallVisitKinds
                    _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildTypes
                    _lhsIchildintros
                    _lhsIfollow
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _lhsOerrors :: (Seq Error)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOdefs :: (Set String)
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOlazyIntras :: (Set String)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 858, column 16)
                           _visitItf =
                               ({-# LINE 858 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error $ "Visit " ++ show visit_ ++ " not found") visit_ _lhsIallchildvisit
                                {-# LINE 6643 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 859, column 16)
                           _childType =
                               ({-# LINE 859 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error ("type of child " ++ show child_ ++ " is not in the childTypes map! " ++ show _lhsIchildTypes)) child_ _lhsIchildTypes
                                {-# LINE 6649 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 860, column 16)
                           (_lhsOerrors,_lhsOsem_steps) =
                               ({-# LINE 860 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                case _visitItf     child_ _childType     _lhsIkind _lhsIfollow of
                                  Left e      -> (Seq.singleton e, empty)
                                  Right steps -> (Seq.empty, steps)
                                {-# LINE 6657 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1320, column 16)
                           _lhsOdefs =
                               ({-# LINE 1320 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Set.insert (stname child_ _to) $ maybe (error "Visit not found") (Set.map $ attrname True child_) $ Map.lookup visit_ _lhsIavisitdefs
                                {-# LINE 6663 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1321, column 16)
                           _lhsOuses =
                               ({-# LINE 1321 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                let convert attrs = Map.fromList [ (attrname False child_ attr, Just $ mkNonLocalAttr True child_ attr) | attr <- Set.elems attrs ]
                                in Map.insert (stname child_ _from) Nothing $ convert $
                                     maybe (error "Visit not found") id $ Map.lookup visit_ _lhsIavisituses
                                {-# LINE 6671 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1407, column 3)
                           (_from,_to) =
                               ({-# LINE 1407 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                                {-# LINE 6677 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Set.empty
                                {-# LINE 6683 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.empty
                                {-# LINE 6689 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.empty
                                {-# LINE 6695 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                mempty
                                {-# LINE 6701 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIindex
                                {-# LINE 6707 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 896 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIisLast
                                {-# LINE 6713 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 6719 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds))))
sem_VisitStep_PureGroup :: T_VisitSteps ->
                           Bool ->
                           T_VisitStep
sem_VisitStep_PureGroup (T_VisitSteps steps_) ordered_ =
    (T_VisitStep (\ _lhsIallFromToStates
                    _lhsIallInitStates
                    _lhsIallVisitKinds
                    _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildTypes
                    _lhsIchildintros
                    _lhsIfollow
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _stepsOkind :: VisitKind
                           _lhsOlazyIntras :: (Set String)
                           _lhsOdefs :: (Set String)
                           _lhsOerrors :: (Seq Error)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           _stepsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                           _stepsOallInitStates :: (Map NontermIdent Int)
                           _stepsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                           _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                           _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                           _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                           _stepsOchildTypes :: (Map Identifier Type)
                           _stepsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _stepsOfollow :: PP_Doc
                           _stepsOindex :: Int
                           _stepsOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                           _stepsOoptions :: Options
                           _stepsOprevMaxSimRefs :: Int
                           _stepsOruledefs :: (Map Identifier (Set String))
                           _stepsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                           _stepsOuseParallel :: Bool
                           _stepsIdefs :: (Set String)
                           _stepsIerrors :: (Seq Error)
                           _stepsIindex :: Int
                           _stepsIisLast :: Bool
                           _stepsIlazyIntras :: (Set String)
                           _stepsIprevMaxSimRefs :: Int
                           _stepsIruleKinds :: (Map Identifier (Set VisitKind))
                           _stepsIruleUsage :: (Map Identifier Int)
                           _stepsIsem_steps :: PP_Doc
                           _stepsIsize :: Int
                           _stepsIuses :: (Map String (Maybe NonLocalAttr))
                           _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 834, column 3)
                           _stepsOkind =
                               ({-# LINE 834 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                VisitPure ordered_
                                {-# LINE 6788 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 1341, column 3)
                           _lhsOlazyIntras =
                               ({-# LINE 1341 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                if ordered_
                                then _stepsIlazyIntras
                                else _stepsIdefs
                                {-# LINE 6796 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1315, column 38)
                           _lhsOdefs =
                               ({-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIdefs
                                {-# LINE 6802 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                           _lhsOerrors =
                               ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIerrors
                                {-# LINE 6808 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIruleKinds
                                {-# LINE 6814 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIruleUsage
                                {-# LINE 6820 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 838, column 59)
                           _lhsOsem_steps =
                               ({-# LINE 838 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIsem_steps
                                {-# LINE 6826 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1316, column 38)
                           _lhsOuses =
                               ({-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIuses
                                {-# LINE 6832 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIvisitKinds
                                {-# LINE 6838 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOindex =
                               ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIindex
                                {-# LINE 6844 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOisLast =
                               ({-# LINE 896 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIisLast
                                {-# LINE 6850 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIprevMaxSimRefs
                                {-# LINE 6856 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallFromToStates =
                               ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallFromToStates
                                {-# LINE 6862 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallInitStates =
                               ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 6868 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallVisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallVisitKinds
                                {-# LINE 6874 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallchildvisit =
                               ({-# LINE 1151 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallchildvisit
                                {-# LINE 6880 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisitdefs =
                               ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIavisitdefs
                                {-# LINE 6886 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisituses =
                               ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIavisituses
                                {-# LINE 6892 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildTypes =
                               ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIchildTypes
                                {-# LINE 6898 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildintros =
                               ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIchildintros
                                {-# LINE 6904 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOfollow =
                               ({-# LINE 838 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIfollow
                                {-# LINE 6910 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOindex =
                               ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIindex
                                {-# LINE 6916 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOmrules =
                               ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImrules
                                {-# LINE 6922 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOoptions =
                               ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIoptions
                                {-# LINE 6928 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 6934 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruledefs =
                               ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIruledefs
                                {-# LINE 6940 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruleuses =
                               ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIruleuses
                                {-# LINE 6946 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOuseParallel =
                               ({-# LINE 905 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIuseParallel
                                {-# LINE 6952 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIuses,_stepsIvisitKinds) =
                               steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds))))
sem_VisitStep_Sim :: T_VisitSteps ->
                     T_VisitStep
sem_VisitStep_Sim (T_VisitSteps steps_) =
    (T_VisitStep (\ _lhsIallFromToStates
                    _lhsIallInitStates
                    _lhsIallVisitKinds
                    _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildTypes
                    _lhsIchildintros
                    _lhsIfollow
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _stepsOindex :: Int
                           _lhsOindex :: Int
                           _lhsOprevMaxSimRefs :: Int
                           _lhsOdefs :: (Set String)
                           _lhsOerrors :: (Seq Error)
                           _lhsOlazyIntras :: (Set String)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOisLast :: Bool
                           _stepsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                           _stepsOallInitStates :: (Map NontermIdent Int)
                           _stepsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                           _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                           _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                           _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                           _stepsOchildTypes :: (Map Identifier Type)
                           _stepsOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _stepsOfollow :: PP_Doc
                           _stepsOkind :: VisitKind
                           _stepsOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                           _stepsOoptions :: Options
                           _stepsOprevMaxSimRefs :: Int
                           _stepsOruledefs :: (Map Identifier (Set String))
                           _stepsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                           _stepsOuseParallel :: Bool
                           _stepsIdefs :: (Set String)
                           _stepsIerrors :: (Seq Error)
                           _stepsIindex :: Int
                           _stepsIisLast :: Bool
                           _stepsIlazyIntras :: (Set String)
                           _stepsIprevMaxSimRefs :: Int
                           _stepsIruleKinds :: (Map Identifier (Set VisitKind))
                           _stepsIruleUsage :: (Map Identifier Int)
                           _stepsIsem_steps :: PP_Doc
                           _stepsIsize :: Int
                           _stepsIuses :: (Map String (Maybe NonLocalAttr))
                           _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 883, column 22)
                           _stepsOindex =
                               ({-# LINE 883 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                0
                                {-# LINE 7022 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 884, column 22)
                           _lhsOindex =
                               ({-# LINE 884 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIindex
                                {-# LINE 7028 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 891, column 3)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 891 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                if _useParallel
                                then _lhsIprevMaxSimRefs `max` (_stepsIindex - 1)
                                else _lhsIprevMaxSimRefs
                                {-# LINE 7036 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 907, column 22)
                           _useParallel =
                               ({-# LINE 907 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                parallelInvoke _lhsIoptions && _stepsIsize > 1
                                {-# LINE 7042 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1315, column 38)
                           _lhsOdefs =
                               ({-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIdefs
                                {-# LINE 7048 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                           _lhsOerrors =
                               ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIerrors
                                {-# LINE 7054 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIlazyIntras
                                {-# LINE 7060 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIruleKinds
                                {-# LINE 7066 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIruleUsage
                                {-# LINE 7072 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 838, column 59)
                           _lhsOsem_steps =
                               ({-# LINE 838 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIsem_steps
                                {-# LINE 7078 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1316, column 38)
                           _lhsOuses =
                               ({-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIuses
                                {-# LINE 7084 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIvisitKinds
                                {-# LINE 7090 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOisLast =
                               ({-# LINE 896 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _stepsIisLast
                                {-# LINE 7096 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallFromToStates =
                               ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallFromToStates
                                {-# LINE 7102 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallInitStates =
                               ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 7108 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallVisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallVisitKinds
                                {-# LINE 7114 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallchildvisit =
                               ({-# LINE 1151 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIallchildvisit
                                {-# LINE 7120 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisitdefs =
                               ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIavisitdefs
                                {-# LINE 7126 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisituses =
                               ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIavisituses
                                {-# LINE 7132 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildTypes =
                               ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIchildTypes
                                {-# LINE 7138 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildintros =
                               ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIchildintros
                                {-# LINE 7144 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOfollow =
                               ({-# LINE 838 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIfollow
                                {-# LINE 7150 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOkind =
                               ({-# LINE 829 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIkind
                                {-# LINE 7156 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOmrules =
                               ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsImrules
                                {-# LINE 7162 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOoptions =
                               ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIoptions
                                {-# LINE 7168 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 7174 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruledefs =
                               ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIruledefs
                                {-# LINE 7180 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruleuses =
                               ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIruleuses
                                {-# LINE 7186 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (from local)
                           _stepsOuseParallel =
                               ({-# LINE 905 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _useParallel
                                {-# LINE 7192 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIuses,_stepsIvisitKinds) =
                               steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfollow _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds))))
sem_VisitStep_ChildIntro :: Identifier ->
                            T_VisitStep
sem_VisitStep_ChildIntro child_ =
    (T_VisitStep (\ _lhsIallFromToStates
                    _lhsIallInitStates
                    _lhsIallVisitKinds
                    _lhsIallchildvisit
                    _lhsIavisitdefs
                    _lhsIavisituses
                    _lhsIchildTypes
                    _lhsIchildintros
                    _lhsIfollow
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _lhsOerrors :: (Seq Error)
                           _lhsOdefs :: (Set String)
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOsem_steps :: PP_Doc
                           _lhsOlazyIntras :: (Set String)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 852, column 16)
                           _attachItf =
                               ({-# LINE 852 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.findWithDefault (error $ "Child " ++ show child_ ++ " not found") child_ _lhsIchildintros
                                {-# LINE 7233 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 853, column 16)
                           (_lhsOerrors,_sem_steps,_lhsOdefs,_lhsOuses) =
                               ({-# LINE 853 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                case _attachItf     _lhsIkind of
                                  Left e                   -> (Seq.singleton e, empty, Set.empty, Map.empty)
                                  Right (code, defs, uses) -> (Seq.empty, code, defs, uses)
                                {-# LINE 7241 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- "./src-ag/ExecutionPlan2Caml.ag"(line 857, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 857 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _sem_steps     >-< _lhsIfollow
                                {-# LINE 7247 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Set.empty
                                {-# LINE 7253 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.empty
                                {-# LINE 7259 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                Map.empty
                                {-# LINE 7265 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                mempty
                                {-# LINE 7271 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIindex
                                {-# LINE 7277 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 896 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIisLast
                                {-# LINE 7283 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 7289 "dist/build/ExecutionPlan2Caml.hs" #-}
                                )
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOuses,_lhsOvisitKinds))))
-- VisitSteps --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         follow               : PP_Doc
         kind                 : VisitKind
         mrules               : Map Identifier (VisitKind ->  Either Error PP_Doc)
         options              : Options
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         useParallel          : Bool
      chained attributes:
         index                : Int
         prevMaxSimRefs       : Int
      synthesized attributes:
         defs                 : Set String
         errors               : Seq Error
         isLast               : Bool
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_steps            : PP_Doc
         size                 : Int
         uses                 : Map String (Maybe NonLocalAttr)
         visitKinds           : Map VisitIdentifier VisitKind
   alternatives:
      alternative Cons:
         child hd             : VisitStep 
         child tl             : VisitSteps 
      alternative Nil:
-}
-- cata
sem_VisitSteps :: VisitSteps ->
                  T_VisitSteps
sem_VisitSteps list =
    (Prelude.foldr sem_VisitSteps_Cons sem_VisitSteps_Nil (Prelude.map sem_VisitStep list))
-- semantic domain
newtype T_VisitSteps = T_VisitSteps ((Map VisitIdentifier (Int,Int)) ->
                                     (Map NontermIdent Int) ->
                                     (Map VisitIdentifier VisitKind) ->
                                     (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                                     (Map VisitIdentifier (Set Identifier)) ->
                                     (Map VisitIdentifier (Set Identifier)) ->
                                     (Map Identifier Type) ->
                                     (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                     PP_Doc ->
                                     Int ->
                                     VisitKind ->
                                     (Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
                                     Options ->
                                     Int ->
                                     (Map Identifier (Set String)) ->
                                     (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                     Bool ->
                                     ( (Set String),(Seq Error),Int,Bool,(Set String),Int,(Map Identifier (Set VisitKind)),(Map Identifier Int),PP_Doc,Int,(Map String (Maybe NonLocalAttr)),(Map VisitIdentifier VisitKind)))
data Inh_VisitSteps = Inh_VisitSteps {allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_VisitSteps :: (Map NontermIdent Int),allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_VisitSteps :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_VisitSteps :: (Map Identifier Type),childintros_Inh_VisitSteps :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),follow_Inh_VisitSteps :: PP_Doc,index_Inh_VisitSteps :: Int,kind_Inh_VisitSteps :: VisitKind,mrules_Inh_VisitSteps :: (Map Identifier (VisitKind ->  Either Error PP_Doc)),options_Inh_VisitSteps :: Options,prevMaxSimRefs_Inh_VisitSteps :: Int,ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)),ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))),useParallel_Inh_VisitSteps :: Bool}
data Syn_VisitSteps = Syn_VisitSteps {defs_Syn_VisitSteps :: (Set String),errors_Syn_VisitSteps :: (Seq Error),index_Syn_VisitSteps :: Int,isLast_Syn_VisitSteps :: Bool,lazyIntras_Syn_VisitSteps :: (Set String),prevMaxSimRefs_Syn_VisitSteps :: Int,ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_VisitSteps :: (Map Identifier Int),sem_steps_Syn_VisitSteps :: PP_Doc,size_Syn_VisitSteps :: Int,uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)),visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind)}
wrap_VisitSteps :: T_VisitSteps ->
                   Inh_VisitSteps ->
                   Syn_VisitSteps
wrap_VisitSteps (T_VisitSteps sem) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
    (let ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOuses,_lhsOvisitKinds) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfollow _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
     in  (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOuses _lhsOvisitKinds))
sem_VisitSteps_Cons :: T_VisitStep ->
                       T_VisitSteps ->
                       T_VisitSteps
sem_VisitSteps_Cons (T_VisitStep hd_) (T_VisitSteps tl_) =
    (T_VisitSteps (\ _lhsIallFromToStates
                     _lhsIallInitStates
                     _lhsIallVisitKinds
                     _lhsIallchildvisit
                     _lhsIavisitdefs
                     _lhsIavisituses
                     _lhsIchildTypes
                     _lhsIchildintros
                     _lhsIfollow
                     _lhsIindex
                     _lhsIkind
                     _lhsImrules
                     _lhsIoptions
                     _lhsIprevMaxSimRefs
                     _lhsIruledefs
                     _lhsIruleuses
                     _lhsIuseParallel ->
                       (let _hdOfollow :: PP_Doc
                            _lhsOsem_steps :: PP_Doc
                            _lhsOsize :: Int
                            _hdOindex :: Int
                            _tlOindex :: Int
                            _lhsOindex :: Int
                            _lhsOisLast :: Bool
                            _hdOisLast :: Bool
                            _lhsOdefs :: (Set String)
                            _lhsOerrors :: (Seq Error)
                            _lhsOlazyIntras :: (Set String)
                            _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                            _lhsOruleUsage :: (Map Identifier Int)
                            _lhsOuses :: (Map String (Maybe NonLocalAttr))
                            _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                            _lhsOprevMaxSimRefs :: Int
                            _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                            _hdOallInitStates :: (Map NontermIdent Int)
                            _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                            _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                            _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                            _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                            _hdOchildTypes :: (Map Identifier Type)
                            _hdOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                            _hdOkind :: VisitKind
                            _hdOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                            _hdOoptions :: Options
                            _hdOprevMaxSimRefs :: Int
                            _hdOruledefs :: (Map Identifier (Set String))
                            _hdOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                            _hdOuseParallel :: Bool
                            _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                            _tlOallInitStates :: (Map NontermIdent Int)
                            _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                            _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                            _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                            _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                            _tlOchildTypes :: (Map Identifier Type)
                            _tlOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                            _tlOfollow :: PP_Doc
                            _tlOkind :: VisitKind
                            _tlOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                            _tlOoptions :: Options
                            _tlOprevMaxSimRefs :: Int
                            _tlOruledefs :: (Map Identifier (Set String))
                            _tlOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                            _tlOuseParallel :: Bool
                            _hdIdefs :: (Set String)
                            _hdIerrors :: (Seq Error)
                            _hdIindex :: Int
                            _hdIisLast :: Bool
                            _hdIlazyIntras :: (Set String)
                            _hdIprevMaxSimRefs :: Int
                            _hdIruleKinds :: (Map Identifier (Set VisitKind))
                            _hdIruleUsage :: (Map Identifier Int)
                            _hdIsem_steps :: PP_Doc
                            _hdIuses :: (Map String (Maybe NonLocalAttr))
                            _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                            _tlIdefs :: (Set String)
                            _tlIerrors :: (Seq Error)
                            _tlIindex :: Int
                            _tlIisLast :: Bool
                            _tlIlazyIntras :: (Set String)
                            _tlIprevMaxSimRefs :: Int
                            _tlIruleKinds :: (Map Identifier (Set VisitKind))
                            _tlIruleUsage :: (Map Identifier Int)
                            _tlIsem_steps :: PP_Doc
                            _tlIsize :: Int
                            _tlIuses :: (Map String (Maybe NonLocalAttr))
                            _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 842, column 11)
                            _hdOfollow =
                                ({-# LINE 842 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _tlIsem_steps
                                 {-# LINE 7458 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 843, column 11)
                            _lhsOsem_steps =
                                ({-# LINE 843 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIsem_steps
                                 {-# LINE 7464 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 874, column 10)
                            _lhsOsize =
                                ({-# LINE 874 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 1 + _tlIsize
                                 {-# LINE 7470 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 879, column 3)
                            _hdOindex =
                                ({-# LINE 879 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIindex
                                 {-# LINE 7476 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 880, column 3)
                            _tlOindex =
                                ({-# LINE 880 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 1 + _lhsIindex
                                 {-# LINE 7482 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 881, column 3)
                            _lhsOindex =
                                ({-# LINE 881 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _tlIindex
                                 {-# LINE 7488 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 900, column 11)
                            _lhsOisLast =
                                ({-# LINE 900 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 False
                                 {-# LINE 7494 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 901, column 11)
                            _hdOisLast =
                                ({-# LINE 901 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _tlIisLast
                                 {-# LINE 7500 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1315, column 38)
                            _lhsOdefs =
                                ({-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIdefs `Set.union` _tlIdefs
                                 {-# LINE 7506 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                            _lhsOerrors =
                                ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIerrors Seq.>< _tlIerrors
                                 {-# LINE 7512 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                            _lhsOlazyIntras =
                                ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIlazyIntras `Set.union` _tlIlazyIntras
                                 {-# LINE 7518 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                            _lhsOruleKinds =
                                ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIruleKinds `unionWithMappend` _tlIruleKinds
                                 {-# LINE 7524 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                            _lhsOruleUsage =
                                ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIruleUsage `unionWithSum` _tlIruleUsage
                                 {-# LINE 7530 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1316, column 38)
                            _lhsOuses =
                                ({-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIuses `Map.union` _tlIuses
                                 {-# LINE 7536 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                            _lhsOvisitKinds =
                                ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIvisitKinds `mappend` _tlIvisitKinds
                                 {-# LINE 7542 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (up)
                            _lhsOprevMaxSimRefs =
                                ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _tlIprevMaxSimRefs
                                 {-# LINE 7548 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallFromToStates =
                                ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallFromToStates
                                 {-# LINE 7554 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallInitStates =
                                ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallInitStates
                                 {-# LINE 7560 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallVisitKinds =
                                ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallVisitKinds
                                 {-# LINE 7566 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallchildvisit =
                                ({-# LINE 1151 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 7572 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisitdefs =
                                ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 7578 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisituses =
                                ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 7584 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOchildTypes =
                                ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIchildTypes
                                 {-# LINE 7590 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOchildintros =
                                ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 7596 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOkind =
                                ({-# LINE 829 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIkind
                                 {-# LINE 7602 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOmrules =
                                ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsImrules
                                 {-# LINE 7608 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOoptions =
                                ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 7614 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOprevMaxSimRefs =
                                ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIprevMaxSimRefs
                                 {-# LINE 7620 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruledefs =
                                ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 7626 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruleuses =
                                ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 7632 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOuseParallel =
                                ({-# LINE 905 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIuseParallel
                                 {-# LINE 7638 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallFromToStates =
                                ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallFromToStates
                                 {-# LINE 7644 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallInitStates =
                                ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallInitStates
                                 {-# LINE 7650 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallVisitKinds =
                                ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallVisitKinds
                                 {-# LINE 7656 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallchildvisit =
                                ({-# LINE 1151 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 7662 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisitdefs =
                                ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 7668 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisituses =
                                ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 7674 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOchildTypes =
                                ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIchildTypes
                                 {-# LINE 7680 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOchildintros =
                                ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 7686 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOfollow =
                                ({-# LINE 838 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIfollow
                                 {-# LINE 7692 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOkind =
                                ({-# LINE 829 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIkind
                                 {-# LINE 7698 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOmrules =
                                ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsImrules
                                 {-# LINE 7704 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOoptions =
                                ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 7710 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (chain)
                            _tlOprevMaxSimRefs =
                                ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _hdIprevMaxSimRefs
                                 {-# LINE 7716 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruledefs =
                                ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 7722 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruleuses =
                                ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 7728 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOuseParallel =
                                ({-# LINE 905 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIuseParallel
                                 {-# LINE 7734 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            ( _hdIdefs,_hdIerrors,_hdIindex,_hdIisLast,_hdIlazyIntras,_hdIprevMaxSimRefs,_hdIruleKinds,_hdIruleUsage,_hdIsem_steps,_hdIuses,_hdIvisitKinds) =
                                hd_ _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOfollow _hdOindex _hdOisLast _hdOkind _hdOmrules _hdOoptions _hdOprevMaxSimRefs _hdOruledefs _hdOruleuses _hdOuseParallel
                            ( _tlIdefs,_tlIerrors,_tlIindex,_tlIisLast,_tlIlazyIntras,_tlIprevMaxSimRefs,_tlIruleKinds,_tlIruleUsage,_tlIsem_steps,_tlIsize,_tlIuses,_tlIvisitKinds) =
                                tl_ _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOfollow _tlOindex _tlOkind _tlOmrules _tlOoptions _tlOprevMaxSimRefs _tlOruledefs _tlOruleuses _tlOuseParallel
                        in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOuses,_lhsOvisitKinds))))
sem_VisitSteps_Nil :: T_VisitSteps
sem_VisitSteps_Nil =
    (T_VisitSteps (\ _lhsIallFromToStates
                     _lhsIallInitStates
                     _lhsIallVisitKinds
                     _lhsIallchildvisit
                     _lhsIavisitdefs
                     _lhsIavisituses
                     _lhsIchildTypes
                     _lhsIchildintros
                     _lhsIfollow
                     _lhsIindex
                     _lhsIkind
                     _lhsImrules
                     _lhsIoptions
                     _lhsIprevMaxSimRefs
                     _lhsIruledefs
                     _lhsIruleuses
                     _lhsIuseParallel ->
                       (let _lhsOsem_steps :: PP_Doc
                            _lhsOsize :: Int
                            _lhsOisLast :: Bool
                            _lhsOdefs :: (Set String)
                            _lhsOerrors :: (Seq Error)
                            _lhsOlazyIntras :: (Set String)
                            _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                            _lhsOruleUsage :: (Map Identifier Int)
                            _lhsOuses :: (Map String (Maybe NonLocalAttr))
                            _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                            _lhsOindex :: Int
                            _lhsOprevMaxSimRefs :: Int
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 844, column 11)
                            _lhsOsem_steps =
                                ({-# LINE 844 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIfollow
                                 {-# LINE 7776 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 873, column 10)
                            _lhsOsize =
                                ({-# LINE 873 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 0
                                 {-# LINE 7782 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- "./src-ag/ExecutionPlan2Caml.ag"(line 899, column 11)
                            _lhsOisLast =
                                ({-# LINE 899 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 True
                                 {-# LINE 7788 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1315, column 38)
                            _lhsOdefs =
                                ({-# LINE 1315 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Set.empty
                                 {-# LINE 7794 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                            _lhsOerrors =
                                ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Seq.empty
                                 {-# LINE 7800 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                            _lhsOlazyIntras =
                                ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Set.empty
                                 {-# LINE 7806 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                            _lhsOruleKinds =
                                ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Map.empty
                                 {-# LINE 7812 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                            _lhsOruleUsage =
                                ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Map.empty
                                 {-# LINE 7818 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1316, column 38)
                            _lhsOuses =
                                ({-# LINE 1316 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 Map.empty
                                 {-# LINE 7824 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                            _lhsOvisitKinds =
                                ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 mempty
                                 {-# LINE 7830 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (chain)
                            _lhsOindex =
                                ({-# LINE 877 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIindex
                                 {-# LINE 7836 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                            -- copy rule (chain)
                            _lhsOprevMaxSimRefs =
                                ({-# LINE 888 "./src-ag/ExecutionPlan2Caml.ag" #-}
                                 _lhsIprevMaxSimRefs
                                 {-# LINE 7842 "dist/build/ExecutionPlan2Caml.hs" #-}
                                 )
                        in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOuses,_lhsOvisitKinds))))
-- Visits ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         allintramap          : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         con                  : ConstructorIdent
         inhmap               : Attributes
         mrules               : Map Identifier (VisitKind ->  Either Error PP_Doc)
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         options              : Options
         params               : [Identifier]
         prevVisits           : Map StateIdentifier StateCtx
         ruledefs             : Map Identifier (Set String)
         ruleuses             : Map Identifier (Map String (Maybe NonLocalAttr))
         synmap               : Attributes
         terminaldefs         : Set String
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         intramap             : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_visit            :  [(StateIdentifier,PP_Doc)] 
         t_visits             : PP_Doc
         visitKinds           : Map VisitIdentifier VisitKind
         visitdefs            : Map VisitIdentifier (Set Identifier)
         visituses            : Map VisitIdentifier (Set Identifier)
   alternatives:
      alternative Cons:
         child hd             : Visit 
         child tl             : Visits 
      alternative Nil:
-}
-- cata
sem_Visits :: Visits ->
              T_Visits
sem_Visits list =
    (Prelude.foldr sem_Visits_Cons sem_Visits_Nil (Prelude.map sem_Visit list))
-- semantic domain
newtype T_Visits = T_Visits ((Map VisitIdentifier (Int,Int)) ->
                             (Map NontermIdent Attributes) ->
                             (Map NontermIdent Int) ->
                             (Map NontermIdent Attributes) ->
                             (Map VisitIdentifier VisitKind) ->
                             (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)) ->
                             (Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                             (Map VisitIdentifier (Set Identifier)) ->
                             (Map VisitIdentifier (Set Identifier)) ->
                             (Map Identifier Type) ->
                             (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                             ConstructorIdent ->
                             Attributes ->
                             (Map Identifier (VisitKind ->  Either Error PP_Doc)) ->
                             (Map StateIdentifier StateCtx) ->
                             NontermIdent ->
                             Options ->
                             ([Identifier]) ->
                             (Map StateIdentifier StateCtx) ->
                             (Map Identifier (Set String)) ->
                             (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                             Attributes ->
                             (Set String) ->
                             ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),(Seq Error),(Map VisitIdentifier (Int,Int)),(Map StateIdentifier (Map String (Maybe NonLocalAttr))),(Set String),(Map Identifier (Set VisitKind)),(Map Identifier Int),( [(StateIdentifier,PP_Doc)] ),PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_Visits = Inh_Visits {allFromToStates_Inh_Visits :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_Visits :: (Map NontermIdent Attributes),allInitStates_Inh_Visits :: (Map NontermIdent Int),allSynmap_Inh_Visits :: (Map NontermIdent Attributes),allVisitKinds_Inh_Visits :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_Visits :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),allintramap_Inh_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),avisitdefs_Inh_Visits :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_Visits :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_Visits :: (Map Identifier Type),childintros_Inh_Visits :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),con_Inh_Visits :: ConstructorIdent,inhmap_Inh_Visits :: Attributes,mrules_Inh_Visits :: (Map Identifier (VisitKind ->  Either Error PP_Doc)),nextVisits_Inh_Visits :: (Map StateIdentifier StateCtx),nt_Inh_Visits :: NontermIdent,options_Inh_Visits :: Options,params_Inh_Visits :: ([Identifier]),prevVisits_Inh_Visits :: (Map StateIdentifier StateCtx),ruledefs_Inh_Visits :: (Map Identifier (Set String)),ruleuses_Inh_Visits :: (Map Identifier (Map String (Maybe NonLocalAttr))),synmap_Inh_Visits :: Attributes,terminaldefs_Inh_Visits :: (Set String)}
data Syn_Visits = Syn_Visits {allvisits_Syn_Visits :: ([VisitStateState]),childvisit_Syn_Visits :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc)),errors_Syn_Visits :: (Seq Error),fromToStates_Syn_Visits :: (Map VisitIdentifier (Int,Int)),intramap_Syn_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),lazyIntras_Syn_Visits :: (Set String),ruleKinds_Syn_Visits :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_Visits :: (Map Identifier Int),sem_visit_Syn_Visits :: ( [(StateIdentifier,PP_Doc)] ),t_visits_Syn_Visits :: PP_Doc,visitKinds_Syn_Visits :: (Map VisitIdentifier VisitKind),visitdefs_Syn_Visits :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_Visits :: (Map VisitIdentifier (Set Identifier))}
wrap_Visits :: T_Visits ->
               Inh_Visits ->
               Syn_Visits
wrap_Visits (T_Visits sem) (Inh_Visits _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
     in  (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
sem_Visits_Cons :: T_Visit ->
                   T_Visits ->
                   T_Visits
sem_Visits_Cons (T_Visit hd_) (T_Visits tl_) =
    (T_Visits (\ _lhsIallFromToStates
                 _lhsIallInhmap
                 _lhsIallInitStates
                 _lhsIallSynmap
                 _lhsIallVisitKinds
                 _lhsIallchildvisit
                 _lhsIallintramap
                 _lhsIavisitdefs
                 _lhsIavisituses
                 _lhsIchildTypes
                 _lhsIchildintros
                 _lhsIcon
                 _lhsIinhmap
                 _lhsImrules
                 _lhsInextVisits
                 _lhsInt
                 _lhsIoptions
                 _lhsIparams
                 _lhsIprevVisits
                 _lhsIruledefs
                 _lhsIruleuses
                 _lhsIsynmap
                 _lhsIterminaldefs ->
                   (let _lhsOallvisits :: ([VisitStateState])
                        _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _lhsOerrors :: (Seq Error)
                        _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                        _lhsOintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _lhsOlazyIntras :: (Set String)
                        _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                        _lhsOruleUsage :: (Map Identifier Int)
                        _lhsOsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                        _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                        _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                        _hdOallInhmap :: (Map NontermIdent Attributes)
                        _hdOallInitStates :: (Map NontermIdent Int)
                        _hdOallSynmap :: (Map NontermIdent Attributes)
                        _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                        _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _hdOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                        _hdOchildTypes :: (Map Identifier Type)
                        _hdOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _hdOcon :: ConstructorIdent
                        _hdOinhmap :: Attributes
                        _hdOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                        _hdOnextVisits :: (Map StateIdentifier StateCtx)
                        _hdOnt :: NontermIdent
                        _hdOoptions :: Options
                        _hdOparams :: ([Identifier])
                        _hdOprevVisits :: (Map StateIdentifier StateCtx)
                        _hdOruledefs :: (Map Identifier (Set String))
                        _hdOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _hdOsynmap :: Attributes
                        _hdOterminaldefs :: (Set String)
                        _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                        _tlOallInhmap :: (Map NontermIdent Attributes)
                        _tlOallInitStates :: (Map NontermIdent Int)
                        _tlOallSynmap :: (Map NontermIdent Attributes)
                        _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                        _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _tlOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                        _tlOchildTypes :: (Map Identifier Type)
                        _tlOchildintros :: (Map Identifier (VisitKind -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _tlOcon :: ConstructorIdent
                        _tlOinhmap :: Attributes
                        _tlOmrules :: (Map Identifier (VisitKind ->  Either Error PP_Doc))
                        _tlOnextVisits :: (Map StateIdentifier StateCtx)
                        _tlOnt :: NontermIdent
                        _tlOoptions :: Options
                        _tlOparams :: ([Identifier])
                        _tlOprevVisits :: (Map StateIdentifier StateCtx)
                        _tlOruledefs :: (Map Identifier (Set String))
                        _tlOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _tlOsynmap :: Attributes
                        _tlOterminaldefs :: (Set String)
                        _hdIallvisits :: ( VisitStateState )
                        _hdIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _hdIerrors :: (Seq Error)
                        _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                        _hdIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _hdIlazyIntras :: (Set String)
                        _hdIruleKinds :: (Map Identifier (Set VisitKind))
                        _hdIruleUsage :: (Map Identifier Int)
                        _hdIsem_visit :: (  (StateIdentifier,PP_Doc)  )
                        _hdIt_visits :: PP_Doc
                        _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                        _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                        _tlIallvisits :: ([VisitStateState])
                        _tlIchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _tlIerrors :: (Seq Error)
                        _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                        _tlIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _tlIlazyIntras :: (Set String)
                        _tlIruleKinds :: (Map Identifier (Set VisitKind))
                        _tlIruleUsage :: (Map Identifier Int)
                        _tlIsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _tlIt_visits :: PP_Doc
                        _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                        _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 429, column 29)
                        _lhsOallvisits =
                            ({-# LINE 429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIallvisits : _tlIallvisits
                             {-# LINE 8046 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIchildvisit `Map.union` _tlIchildvisit
                             {-# LINE 8052 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                        _lhsOerrors =
                            ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIerrors Seq.>< _tlIerrors
                             {-# LINE 8058 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                        _lhsOfromToStates =
                            ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIfromToStates `mappend` _tlIfromToStates
                             {-# LINE 8064 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1257, column 34)
                        _lhsOintramap =
                            ({-# LINE 1257 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIintramap `uwMapUnion` _tlIintramap
                             {-# LINE 8070 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                        _lhsOlazyIntras =
                            ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIlazyIntras `Set.union` _tlIlazyIntras
                             {-# LINE 8076 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                        _lhsOruleKinds =
                            ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIruleKinds `unionWithMappend` _tlIruleKinds
                             {-# LINE 8082 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                        _lhsOruleUsage =
                            ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIruleUsage `unionWithSum` _tlIruleUsage
                             {-# LINE 8088 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 793, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 793 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIsem_visit : _tlIsem_visit
                             {-# LINE 8094 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 529, column 59)
                        _lhsOt_visits =
                            ({-# LINE 529 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIt_visits >-< _tlIt_visits
                             {-# LINE 8100 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                        _lhsOvisitKinds =
                            ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIvisitKinds `mappend` _tlIvisitKinds
                             {-# LINE 8106 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                             {-# LINE 8112 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                        _lhsOvisituses =
                            ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _hdIvisituses `uwSetUnion` _tlIvisituses
                             {-# LINE 8118 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallFromToStates =
                            ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallFromToStates
                             {-# LINE 8124 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInhmap =
                            ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 8130 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInitStates =
                            ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInitStates
                             {-# LINE 8136 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallSynmap =
                            ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 8142 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallVisitKinds =
                            ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallVisitKinds
                             {-# LINE 8148 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallchildvisit =
                            ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 8154 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallintramap =
                            ({-# LINE 1256 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallintramap
                             {-# LINE 8160 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisitdefs =
                            ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 8166 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisituses =
                            ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIavisituses
                             {-# LINE 8172 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildTypes =
                            ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 8178 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildintros =
                            ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildintros
                             {-# LINE 8184 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOcon =
                            ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIcon
                             {-# LINE 8190 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOinhmap =
                            ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIinhmap
                             {-# LINE 8196 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmrules =
                            ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImrules
                             {-# LINE 8202 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnextVisits =
                            ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInextVisits
                             {-# LINE 8208 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnt =
                            ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInt
                             {-# LINE 8214 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIoptions
                             {-# LINE 8220 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOparams =
                            ({-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIparams
                             {-# LINE 8226 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOprevVisits =
                            ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIprevVisits
                             {-# LINE 8232 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruledefs =
                            ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruledefs
                             {-# LINE 8238 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruleuses =
                            ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruleuses
                             {-# LINE 8244 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOsynmap =
                            ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIsynmap
                             {-# LINE 8250 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOterminaldefs =
                            ({-# LINE 1259 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 8256 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallFromToStates =
                            ({-# LINE 1398 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallFromToStates
                             {-# LINE 8262 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInhmap =
                            ({-# LINE 414 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 8268 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInitStates =
                            ({-# LINE 1456 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallInitStates
                             {-# LINE 8274 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallSynmap =
                            ({-# LINE 415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 8280 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallVisitKinds =
                            ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallVisitKinds
                             {-# LINE 8286 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallchildvisit =
                            ({-# LINE 1148 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 8292 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallintramap =
                            ({-# LINE 1256 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIallintramap
                             {-# LINE 8298 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisitdefs =
                            ({-# LINE 1308 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 8304 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisituses =
                            ({-# LINE 1309 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIavisituses
                             {-# LINE 8310 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildTypes =
                            ({-# LINE 1415 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 8316 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildintros =
                            ({-# LINE 914 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIchildintros
                             {-# LINE 8322 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOcon =
                            ({-# LINE 86 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIcon
                             {-# LINE 8328 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOinhmap =
                            ({-# LINE 412 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIinhmap
                             {-# LINE 8334 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmrules =
                            ({-# LINE 837 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsImrules
                             {-# LINE 8340 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnextVisits =
                            ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInextVisits
                             {-# LINE 8346 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnt =
                            ({-# LINE 82 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsInt
                             {-# LINE 8352 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 72 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIoptions
                             {-# LINE 8358 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOparams =
                            ({-# LINE 92 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIparams
                             {-# LINE 8364 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOprevVisits =
                            ({-# LINE 1384 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIprevVisits
                             {-# LINE 8370 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruledefs =
                            ({-# LINE 1283 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruledefs
                             {-# LINE 8376 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruleuses =
                            ({-# LINE 1284 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIruleuses
                             {-# LINE 8382 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOsynmap =
                            ({-# LINE 413 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIsynmap
                             {-# LINE 8388 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOterminaldefs =
                            ({-# LINE 1259 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 8394 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        ( _hdIallvisits,_hdIchildvisit,_hdIerrors,_hdIfromToStates,_hdIintramap,_hdIlazyIntras,_hdIruleKinds,_hdIruleUsage,_hdIsem_visit,_hdIt_visits,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                            hd_ _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOcon _hdOinhmap _hdOmrules _hdOnextVisits _hdOnt _hdOoptions _hdOparams _hdOprevVisits _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs
                        ( _tlIallvisits,_tlIchildvisit,_tlIerrors,_tlIfromToStates,_tlIintramap,_tlIlazyIntras,_tlIruleKinds,_tlIruleUsage,_tlIsem_visit,_tlIt_visits,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                            tl_ _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOcon _tlOinhmap _tlOmrules _tlOnextVisits _tlOnt _tlOoptions _tlOparams _tlOprevVisits _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs
                    in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
sem_Visits_Nil :: T_Visits
sem_Visits_Nil =
    (T_Visits (\ _lhsIallFromToStates
                 _lhsIallInhmap
                 _lhsIallInitStates
                 _lhsIallSynmap
                 _lhsIallVisitKinds
                 _lhsIallchildvisit
                 _lhsIallintramap
                 _lhsIavisitdefs
                 _lhsIavisituses
                 _lhsIchildTypes
                 _lhsIchildintros
                 _lhsIcon
                 _lhsIinhmap
                 _lhsImrules
                 _lhsInextVisits
                 _lhsInt
                 _lhsIoptions
                 _lhsIparams
                 _lhsIprevVisits
                 _lhsIruledefs
                 _lhsIruleuses
                 _lhsIsynmap
                 _lhsIterminaldefs ->
                   (let _lhsOallvisits :: ([VisitStateState])
                        _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> Type -> VisitKind -> PP_Doc -> Either Error PP_Doc))
                        _lhsOerrors :: (Seq Error)
                        _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                        _lhsOintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _lhsOlazyIntras :: (Set String)
                        _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                        _lhsOruleUsage :: (Map Identifier Int)
                        _lhsOsem_visit :: ( [(StateIdentifier,PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                        _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 429, column 29)
                        _lhsOallvisits =
                            ({-# LINE 429 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             []
                             {-# LINE 8443 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1149, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 1149 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8449 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1480, column 132)
                        _lhsOerrors =
                            ({-# LINE 1480 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Seq.empty
                             {-# LINE 8455 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1395, column 22)
                        _lhsOfromToStates =
                            ({-# LINE 1395 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             mempty
                             {-# LINE 8461 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1257, column 34)
                        _lhsOintramap =
                            ({-# LINE 1257 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8467 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1331, column 57)
                        _lhsOlazyIntras =
                            ({-# LINE 1331 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Set.empty
                             {-# LINE 8473 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1231, column 56)
                        _lhsOruleKinds =
                            ({-# LINE 1231 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8479 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1219, column 56)
                        _lhsOruleUsage =
                            ({-# LINE 1219 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8485 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 793, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 793 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             []
                             {-# LINE 8491 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 529, column 59)
                        _lhsOt_visits =
                            ({-# LINE 529 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             empty
                             {-# LINE 8497 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1442, column 68)
                        _lhsOvisitKinds =
                            ({-# LINE 1442 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             mempty
                             {-# LINE 8503 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1298, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 1298 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8509 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                        -- use rule "./src-ag/ExecutionPlan2Caml.ag"(line 1299, column 36)
                        _lhsOvisituses =
                            ({-# LINE 1299 "./src-ag/ExecutionPlan2Caml.ag" #-}
                             Map.empty
                             {-# LINE 8515 "dist/build/ExecutionPlan2Caml.hs" #-}
                             )
                    in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))