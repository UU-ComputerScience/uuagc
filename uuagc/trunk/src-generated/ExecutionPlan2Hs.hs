{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionPlan2Hs where
{-# LINE 7 "./src-ag/ExecutionPlan2Hs.ag" #-}

import ExecutionPlan
import Pretty
import PPUtil
import Options
import Data.Monoid(mappend,mempty)
import Data.Maybe
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
{-# LINE 31 "dist/build/ExecutionPlan2Hs.hs" #-}

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
{-# LINE 45 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 52 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 58 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 64 "dist/build/ExecutionPlan2Hs.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 161 "./src-ag/ExecutionPlan2Hs.ag" #-}

classCtxsToDocs :: ClassContext -> [PP_Doc]
classCtxsToDocs = map toDoc where
  toDoc (ident,args) = (ident >#< ppSpaced (map pp_parens args))

classConstrsToDocs :: [Type] -> [PP_Doc]
classConstrsToDocs = map ppTp

ppClasses :: [PP_Doc] -> PP_Doc
ppClasses [] = empty
ppClasses xs = pp_block "(" ")" "," xs >#< "=>"

ppQuants :: [Identifier] -> PP_Doc
ppQuants [] = empty
ppQuants ps = "forall" >#< ppSpaced ps >#< "."
{-# LINE 83 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 190 "./src-ag/ExecutionPlan2Hs.ag" #-}

-- first parameter indicates: generate a record or not
ppConFields :: Bool -> [PP_Doc] -> PP_Doc
ppConFields True  flds = ppListSep "{" "}" ", " flds
ppConFields False flds = ppSpaced flds
{-# LINE 91 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 216 "./src-ag/ExecutionPlan2Hs.ag" #-}

ppTp :: Type -> PP_Doc
ppTp = text . typeToHaskellString Nothing []
{-# LINE 97 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 330 "./src-ag/ExecutionPlan2Hs.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 101 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 426 "./src-ag/ExecutionPlan2Hs.ag" #-}

conNmTVisit nt vId      = "T_" >|< nt >|< "_v"    >|< vId
conNmTVisitIn nt vId    = "T_" >|< nt >|< "_vIn"  >|< vId
conNmTVisitOut nt vId   = "T_" >|< nt >|< "_vOut" >|< vId
conNmTNextVisit nt stId = "T_" >|< nt >|< "_s"    >|< stId

ppMonadType :: Options -> PP_Doc
ppMonadType opts
  | parallelInvoke opts = text "IO"
  | otherwise           = text "Identity"
{-# LINE 114 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 576 "./src-ag/ExecutionPlan2Hs.ag" #-}

ppDefor :: Type -> PP_Doc
ppDefor (NT nt args _) = "T_" >|< nt >#< ppSpaced (map pp_parens args)
ppDefor (Haskell s)    = text s
{-# LINE 121 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 700 "./src-ag/ExecutionPlan2Hs.ag" #-}

mklet :: (PP a, PP b, PP c) => a -> b -> c -> PP_Doc
mklet prefix defs body =
  prefix >#< "let"
  >-< indent 3 defs
  >-< indent 2 "in" >#< body
{-# LINE 130 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 766 "./src-ag/ExecutionPlan2Hs.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 139 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 837 "./src-ag/ExecutionPlan2Hs.ag" #-}

parResultName :: String
parResultName = "__outcome_"

fmtDecl :: PP a => Bool -> FormatMode -> a -> PP_Doc
fmtDecl declPure fmt decl = case fmt of
  FormatLetDecl -> pp decl
  FormatLetLine -> "let" >#< decl >#< "in"
  FormatDo | declPure  -> "let" >#< decl
           | otherwise -> pp decl
{-# LINE 152 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 963 "./src-ag/ExecutionPlan2Hs.ag" #-}

stname :: Identifier -> Int -> String
stname child st = "_" ++ getName child ++ "X" ++ show st

-- should actually return some conversion info
compatibleAttach :: VisitKind -> NontermIdent -> Options -> Bool
compatibleAttach _ _ _ = True

unMon :: Options -> PP_Doc
unMon options
  | parallelInvoke options = text "System.IO.Unsafe.unsafePerformIO"    -- IO monad
  | otherwise              = text "Control.Monad.Identity.runIdentity"  -- identity monad
{-# LINE 167 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1071 "./src-ag/ExecutionPlan2Hs.ag" #-}

dummyPat :: Options -> Bool -> PP_Doc
dummyPat opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty  -- no unnecessary tuples
  | tupleAsDummyToken opts = if strictDummyToken opts
                             then text "()"
                             else text "(_ :: ())"
  | otherwise              = let match | strictDummyToken opts = "!_"
                                       | otherwise             = "_"
                             in pp_parens (match >#< "::" >#< dummyType opts noArgs)
  where match | strictDummyToken opts = "(!_)"
              | otherwise             = "_"

dummyArg :: Options -> Bool -> PP_Doc
dummyArg opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty    -- no unnecessary tuples
  | tupleAsDummyToken opts = text "()"
  | otherwise              = text "GHC.Prim.realWorld#"

dummyType :: Options -> Bool -> PP_Doc
dummyType opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty     -- no unnecessary tuples
  | tupleAsDummyToken opts = text "()"
  | otherwise              = text "(GHC.Prim.State# GHC.Prim.RealWorld)"
{-# LINE 194 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1097 "./src-ag/ExecutionPlan2Hs.ag" #-}

-- rules are "deinlined" to prevent needless code duplication.
-- if there is only a bit of duplication, we allow ghc to decide if it is worth it.
-- if the duplication crosses this threshold, however, we tell ghc definitely not to inline it.
ruleInlineThresholdSoft :: Int
ruleInlineThresholdSoft = 3

ruleInlineThresholdHard :: Int
ruleInlineThresholdHard = 5

reallyOftenUsedThreshold :: Int
reallyOftenUsedThreshold = 12
{-# LINE 209 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1163 "./src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 242 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1249 "./src-ag/ExecutionPlan2Hs.ag" #-}

-- a `compatibleKind` b  means: can kind b be invoked from a
compatibleKind :: VisitKind -> VisitKind -> Bool
compatibleKind _              _             = True

compatibleRule :: VisitKind -> Bool -> Bool
compatibleRule (VisitPure _) False = False
compatibleRule _             _     = True
{-# LINE 253 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1271 "./src-ag/ExecutionPlan2Hs.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 258 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1294 "./src-ag/ExecutionPlan2Hs.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 267 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1511 "./src-ag/ExecutionPlan2Hs.ag" #-}

renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""

writeModule :: FilePath -> [PP_Doc] -> IO ()
writeModule path docs
  = do bExists <- doesFileExist path
       if bExists
        then do input <- readFile path
                seq (length input) (return ())
                if input /= output
                 then dumpIt
                 else return ()
        else dumpIt
  where
    output = renderDocs docs
    dumpIt = writeFile path output
{-# LINE 287 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1658 "./src-ag/ExecutionPlan2Hs.ag" #-}

ppNoInline :: PP a => a -> PP_Doc
ppNoInline = ppPragmaBinding "NOINLINE"

ppInline :: PP a => a -> PP_Doc
ppInline = ppPragmaBinding "INLINE"

ppInlinable :: PP a => a -> PP_Doc
ppInlinable = ppPragmaBinding "INLINABLE"

ppPragmaBinding :: (PP a, PP b) => a -> b -> PP_Doc
ppPragmaBinding pragma nm = "{-#" >#< pragma >#< nm >#< "#-}"

ppCostCentre :: PP a => a -> PP_Doc
ppCostCentre nm = "{-#" >#< "SCC" >#< "\"" >|< nm >|< "\"" >#< "#-}"

warrenFlagsPP :: Options -> PP_Doc
warrenFlagsPP options = vlist
  [ pp "{-# LANGUAGE Rank2Types, GADTs #-}"
  , if bangpats options
    then pp "{-# LANGUAGE BangPatterns #-}"
    else empty
  , if noPerRuleTypeSigs options && noPerStateTypeSigs options
    then empty
    else pp "{-# LANGUAGE ScopedTypeVariables #-}"
  , if tupleAsDummyToken options
    then empty
    else pp "{-# LANGUAGE ScopedTypeVariables, MagicHash #-}"
  , -- not that the meaning of "unbox" is here that strict fields in data types may be
    -- unboxed if possible. This may affect user-defined data types declared in the module.
    -- Unfortunately, we cannot turn it on for only the AG generated data types without
    -- causing a zillion of warnings.
    if unbox options && bangpats options
        then pp $ "{-# OPTIONS_GHC -funbox-strict-fields -fstrictness #-}"
        else empty
  , if parallelInvoke options && not (noEagerBlackholing options)
    then pp $ "{-# OPTIONS_GHC -feager-blackholing #-}"
    else empty
  ]
{-# LINE 329 "dist/build/ExecutionPlan2Hs.hs" #-}
-- EChild ------------------------------------------------------
-- wrapper
data Inh_EChild  = Inh_EChild { allInitStates_Inh_EChild :: (Map NontermIdent Int), con_Inh_EChild :: (ConstructorIdent), importBlocks_Inh_EChild :: (PP_Doc), mainFile_Inh_EChild :: (String), mainName_Inh_EChild :: (String), moduleHeader_Inh_EChild :: (String -> String -> String -> Bool -> String), nt_Inh_EChild :: (NontermIdent), options_Inh_EChild :: (Options), pragmaBlocks_Inh_EChild :: (String), textBlocks_Inh_EChild :: (PP_Doc) }
data Syn_EChild  = Syn_EChild { argnamesw_Syn_EChild :: ( PP_Doc ), argpats_Syn_EChild :: (  PP_Doc  ), argtps_Syn_EChild :: (  PP_Doc  ), childTypes_Syn_EChild :: (Map Identifier Type), childintros_Syn_EChild :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), datatype_Syn_EChild :: (PP_Doc), terminaldefs_Syn_EChild :: (Set String), usedArgs_Syn_EChild :: (Set String) }
{-# INLINABLE wrap_EChild #-}
wrap_EChild :: T_EChild  -> Inh_EChild  -> (Syn_EChild )
wrap_EChild (T_EChild act) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
        (T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChild_s2 sem arg)
        return (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs)
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
data T_EChild_vIn1  = T_EChild_vIn1 (Map NontermIdent Int) (ConstructorIdent) (PP_Doc) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (PP_Doc)
data T_EChild_vOut1  = T_EChild_vOut1 ( PP_Doc ) (  PP_Doc  ) (  PP_Doc  ) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (PP_Doc) (Set String) (Set String)
{-# NOINLINE sem_EChild_EChild #-}
sem_EChild_EChild :: (Identifier) -> (Type) -> (ChildKind) -> (Bool) -> (Maybe [Identifier]) -> (Bool) -> T_EChild 
sem_EChild_EChild arg_name_ arg_tp_ arg_kind_ arg_hasAround_ _ _ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule0 _usedArgs_augmented_f1 _usedArgs_augmented_syn
         _usedArgs_augmented_f1 = rule1 arg_kind_ arg_name_
         _tpDoc = rule2 _addStrict arg_tp_
         _strNm = rule3 _lhsIcon _lhsInt arg_name_
         _field = rule4 _lhsIoptions _strNm _tpDoc
         _addStrict = rule5 _lhsIoptions
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule6 _field arg_kind_
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule7 _nt arg_kind_ arg_name_
         _lhsOargtps ::   PP_Doc  
         _lhsOargtps = rule8 arg_kind_ arg_tp_
         _argpats = rule9 arg_kind_ arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule10 _introcode arg_name_
         _isDefor = rule11 arg_tp_
         _valcode = rule12 _isDefor _lhsIoptions _nt arg_kind_ arg_name_
         _aroundcode = rule13 arg_hasAround_ arg_name_
         _introcode = rule14 _addbang _aroundcode _initSt _isDefor _lhsIoptions _nt _valcode arg_hasAround_ arg_kind_ arg_name_
         _nt = rule15 arg_tp_
         _addbang = rule16 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule17 arg_name_ arg_tp_
         _initSt = rule18 _lhsIallInitStates _nt
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule19  ()
         _usedArgs_augmented_syn = rule20  ()
         _lhsOargpats ::   PP_Doc  
         _lhsOargpats = rule21 _argpats
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule0 #-}
   rule0 = \ _usedArgs_augmented_f1 _usedArgs_augmented_syn ->
     foldr ($) _usedArgs_augmented_syn [_usedArgs_augmented_f1]
   {-# INLINE rule1 #-}
   rule1 = \ kind_ name_ ->
                         \s -> case kind_ of
                               ChildSyntax -> Set.insert ("arg_" ++ show name_ ++ "_") s
                               _           -> s
   {-# INLINE rule2 #-}
   {-# LINE 204 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule2 = \ _addStrict tp_ ->
                     {-# LINE 204 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                     {-# LINE 414 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 205 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule3 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 205 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 420 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 206 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule4 = \ ((_lhsIoptions) :: Options) _strNm _tpDoc ->
                     {-# LINE 206 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     if dataRecords _lhsIoptions
                     then _strNm     >#< "::" >#< _tpDoc
                     else _tpDoc
                     {-# LINE 428 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 209 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule5 = \ ((_lhsIoptions) :: Options) ->
                        {-# LINE 209 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        \x -> if strictData _lhsIoptions then "!" >|< x else x
                        {-# LINE 434 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 210 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule6 = \ _field kind_ ->
                             {-# LINE 210 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildAttr -> empty
                               _         -> _field
                             {-# LINE 442 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 298 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule7 = \ _nt kind_ name_ ->
                             {-# LINE 298 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildSyntax     -> "(" >#< "sem_" >|< _nt     >#< name_ >|< "_" >#< ")"
                               ChildAttr       -> empty
                               ChildReplace tp -> "(" >#< "sem_" >|< extractNonterminal tp >#< name_ >|< "_" >#< ")"
                             {-# LINE 451 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 565 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule8 = \ kind_ tp_ ->
                            {-# LINE 565 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            case kind_ of
                              ChildSyntax     -> ppDefor tp_ >#< "->"
                              ChildReplace tp -> ppDefor tp >#< "->"
                              _               -> empty
                            {-# LINE 460 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 569 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule9 = \ kind_ name_ ->
                           {-# LINE 569 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           case kind_ of
                             ChildSyntax    -> name_ >|< "_"
                             ChildReplace _ -> name_ >|< "_"
                             _              -> empty
                           {-# LINE 469 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 917 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule10 = \ _introcode name_ ->
                               {-# LINE 917 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               Map.singleton name_ _introcode
                               {-# LINE 475 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 918 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule11 = \ tp_ ->
                               {-# LINE 918 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               case tp_ of
                                 NT _ _ defor -> defor
                                 _            -> False
                               {-# LINE 483 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 921 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule12 = \ _isDefor ((_lhsIoptions) :: Options) _nt kind_ name_ ->
                               {-# LINE 921 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               case kind_ of
                                 ChildSyntax -> "arg_" >|< name_ >|< "_"
                                 ChildAttr   ->
                                                let prefix | not _isDefor     = if lateHigherOrderBinding _lhsIoptions
                                                                                then lateSemNtLabel _nt     >#< lhsname True idLateBindingAttr
                                                                                else "sem_" >|< _nt
                                                           | otherwise        = empty
                                                in pp_parens (prefix >#< instname name_)
                                 ChildReplace _ ->
                                                   pp_parens (instname name_ >#< name_ >|< "_")
                               {-# LINE 498 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 932 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule13 = \ hasAround_ name_ ->
                               {-# LINE 932 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               if hasAround_
                               then locname name_ >|< "_around"
                               else empty
                               {-# LINE 506 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 935 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule14 = \ _addbang _aroundcode _initSt _isDefor ((_lhsIoptions) :: Options) _nt _valcode hasAround_ kind_ name_ ->
                               {-# LINE 935 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               \kind fmtMode ->
                                        let pat       = text $ stname name_ _initSt
                                            patStrict = _addbang     pat
                                            attach    = "attach_T_" >|< _nt     >#< pp_parens (_aroundcode     >#< _valcode    )
                                            runAttach = unMon _lhsIoptions >#< pp_parens attach
                                            decl      = case kind of
                                                          VisitPure False -> pat >#< "=" >#< runAttach
                                                          VisitPure True  -> patStrict >#< "=" >#< runAttach
                                                          VisitMonadic    -> patStrict >#< "<-" >#< attach
                                        in if compatibleAttach kind _nt     _lhsIoptions
                                           then Right ( fmtDecl False fmtMode decl
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
                               {-# LINE 537 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 961 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule15 = \ tp_ ->
                            {-# LINE 961 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            extractNonterminal tp_
                            {-# LINE 543 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 1539 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule16 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1539 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 549 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 1591 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule17 = \ name_ tp_ ->
                     {-# LINE 1591 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 555 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 1635 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule18 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) _nt ->
                 {-# LINE 1635 "./src-ag/ExecutionPlan2Hs.ag" #-}
                 Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                 {-# LINE 561 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule19 #-}
   rule19 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule20 #-}
   rule20 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule21 #-}
   rule21 = \ _argpats ->
     _argpats
{-# NOINLINE sem_EChild_ETerm #-}
sem_EChild_ETerm :: (Identifier) -> (Type) -> T_EChild 
sem_EChild_ETerm arg_name_ arg_tp_ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _tpDoc = rule22 _addStrict arg_tp_
         _strNm = rule23 _lhsIcon _lhsInt arg_name_
         _field = rule24 _lhsIoptions _strNm _tpDoc
         _addStrict = rule25 _lhsIoptions
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule26 _field
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule27 arg_name_
         _lhsOargtps ::   PP_Doc  
         _lhsOargtps = rule28 arg_tp_
         _argpats = rule29 _addbang arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule30 arg_name_
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule31 arg_name_
         _addbang = rule32 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule33 arg_name_ arg_tp_
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule34  ()
         _lhsOargpats ::   PP_Doc  
         _lhsOargpats = rule35 _argpats
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule22 #-}
   {-# LINE 204 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule22 = \ _addStrict tp_ ->
                     {-# LINE 204 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                     {-# LINE 608 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 205 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule23 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 205 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 614 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 206 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule24 = \ ((_lhsIoptions) :: Options) _strNm _tpDoc ->
                     {-# LINE 206 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     if dataRecords _lhsIoptions
                     then _strNm     >#< "::" >#< _tpDoc
                     else _tpDoc
                     {-# LINE 622 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 209 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule25 = \ ((_lhsIoptions) :: Options) ->
                        {-# LINE 209 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        \x -> if strictData _lhsIoptions then "!" >|< x else x
                        {-# LINE 628 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 214 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule26 = \ _field ->
                             {-# LINE 214 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             _field
                             {-# LINE 634 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 302 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule27 = \ name_ ->
                             {-# LINE 302 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             text $ fieldname name_
                             {-# LINE 640 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 573 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule28 = \ tp_ ->
                           {-# LINE 573 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           (pp_parens $ show tp_) >#< "->"
                           {-# LINE 646 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 574 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule29 = \ _addbang name_ ->
                           {-# LINE 574 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           _addbang     $ text $ fieldname name_
                           {-# LINE 652 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 916 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule30 = \ name_ ->
                               {-# LINE 916 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               Map.singleton name_ (\_ _ -> Right (empty, Set.empty, Map.empty))
                               {-# LINE 658 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule31 #-}
   {-# LINE 1308 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule31 = \ name_ ->
                       {-# LINE 1308 "./src-ag/ExecutionPlan2Hs.ag" #-}
                       Set.singleton $ fieldname name_
                       {-# LINE 664 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule32 #-}
   {-# LINE 1540 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule32 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1540 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 670 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule33 #-}
   {-# LINE 1591 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule33 = \ name_ tp_ ->
                     {-# LINE 1591 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 676 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule34 #-}
   rule34 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule35 #-}
   rule35 = \ _argpats ->
     _argpats

-- EChildren ---------------------------------------------------
-- wrapper
data Inh_EChildren  = Inh_EChildren { allInitStates_Inh_EChildren :: (Map NontermIdent Int), con_Inh_EChildren :: (ConstructorIdent), importBlocks_Inh_EChildren :: (PP_Doc), mainFile_Inh_EChildren :: (String), mainName_Inh_EChildren :: (String), moduleHeader_Inh_EChildren :: (String -> String -> String -> Bool -> String), nt_Inh_EChildren :: (NontermIdent), options_Inh_EChildren :: (Options), pragmaBlocks_Inh_EChildren :: (String), textBlocks_Inh_EChildren :: (PP_Doc) }
data Syn_EChildren  = Syn_EChildren { argnamesw_Syn_EChildren :: ([PP_Doc]), argpats_Syn_EChildren :: ( [PP_Doc] ), argtps_Syn_EChildren :: ( [PP_Doc] ), childTypes_Syn_EChildren :: (Map Identifier Type), childintros_Syn_EChildren :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), datatype_Syn_EChildren :: ([PP_Doc]), terminaldefs_Syn_EChildren :: (Set String), usedArgs_Syn_EChildren :: (Set String) }
{-# INLINABLE wrap_EChildren #-}
wrap_EChildren :: T_EChildren  -> Inh_EChildren  -> (Syn_EChildren )
wrap_EChildren (T_EChildren act) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
        (T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChildren_s5 sem arg)
        return (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs)
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
data T_EChildren_vIn4  = T_EChildren_vIn4 (Map NontermIdent Int) (ConstructorIdent) (PP_Doc) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (PP_Doc)
data T_EChildren_vOut4  = T_EChildren_vOut4 ([PP_Doc]) ( [PP_Doc] ) ( [PP_Doc] ) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ([PP_Doc]) (Set String) (Set String)
{-# NOINLINE sem_EChildren_Cons #-}
sem_EChildren_Cons :: T_EChild  -> T_EChildren  -> T_EChildren 
sem_EChildren_Cons arg_hd_ arg_tl_ = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_EChild (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_tl_))
         (T_EChild_vOut1 _hdIargnamesw _hdIargpats _hdIargtps _hdIchildTypes _hdIchildintros _hdIdatatype _hdIterminaldefs _hdIusedArgs) = inv_EChild_s2 _hdX2 (T_EChild_vIn1 _hdOallInitStates _hdOcon _hdOimportBlocks _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOtextBlocks)
         (T_EChildren_vOut4 _tlIargnamesw _tlIargpats _tlIargtps _tlIchildTypes _tlIchildintros _tlIdatatype _tlIterminaldefs _tlIusedArgs) = inv_EChildren_s5 _tlX5 (T_EChildren_vIn4 _tlOallInitStates _tlOcon _tlOimportBlocks _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOtextBlocks)
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule36 _hdIargnamesw _tlIargnamesw
         _lhsOargpats ::  [PP_Doc] 
         _lhsOargpats = rule37 _hdIargpats _tlIargpats
         _lhsOargtps ::  [PP_Doc] 
         _lhsOargtps = rule38 _hdIargtps _tlIargtps
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule39 _hdIchildTypes _tlIchildTypes
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule40 _hdIchildintros _tlIchildintros
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule41 _hdIdatatype _tlIdatatype
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule42 _hdIterminaldefs _tlIterminaldefs
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule43 _hdIusedArgs _tlIusedArgs
         _hdOallInitStates = rule44 _lhsIallInitStates
         _hdOcon = rule45 _lhsIcon
         _hdOimportBlocks = rule46 _lhsIimportBlocks
         _hdOmainFile = rule47 _lhsImainFile
         _hdOmainName = rule48 _lhsImainName
         _hdOmoduleHeader = rule49 _lhsImoduleHeader
         _hdOnt = rule50 _lhsInt
         _hdOoptions = rule51 _lhsIoptions
         _hdOpragmaBlocks = rule52 _lhsIpragmaBlocks
         _hdOtextBlocks = rule53 _lhsItextBlocks
         _tlOallInitStates = rule54 _lhsIallInitStates
         _tlOcon = rule55 _lhsIcon
         _tlOimportBlocks = rule56 _lhsIimportBlocks
         _tlOmainFile = rule57 _lhsImainFile
         _tlOmainName = rule58 _lhsImainName
         _tlOmoduleHeader = rule59 _lhsImoduleHeader
         _tlOnt = rule60 _lhsInt
         _tlOoptions = rule61 _lhsIoptions
         _tlOpragmaBlocks = rule62 _lhsIpragmaBlocks
         _tlOtextBlocks = rule63 _lhsItextBlocks
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule36 #-}
   rule36 = \ ((_hdIargnamesw) ::  PP_Doc ) ((_tlIargnamesw) :: [PP_Doc]) ->
     _hdIargnamesw : _tlIargnamesw
   {-# INLINE rule37 #-}
   rule37 = \ ((_hdIargpats) ::   PP_Doc  ) ((_tlIargpats) ::  [PP_Doc] ) ->
     _hdIargpats : _tlIargpats
   {-# INLINE rule38 #-}
   rule38 = \ ((_hdIargtps) ::   PP_Doc  ) ((_tlIargtps) ::  [PP_Doc] ) ->
     _hdIargtps : _tlIargtps
   {-# INLINE rule39 #-}
   rule39 = \ ((_hdIchildTypes) :: Map Identifier Type) ((_tlIchildTypes) :: Map Identifier Type) ->
     _hdIchildTypes `mappend` _tlIchildTypes
   {-# INLINE rule40 #-}
   rule40 = \ ((_hdIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ((_tlIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _hdIchildintros `Map.union` _tlIchildintros
   {-# INLINE rule41 #-}
   rule41 = \ ((_hdIdatatype) :: PP_Doc) ((_tlIdatatype) :: [PP_Doc]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule42 #-}
   rule42 = \ ((_hdIterminaldefs) :: Set String) ((_tlIterminaldefs) :: Set String) ->
     _hdIterminaldefs `Set.union` _tlIterminaldefs
   {-# INLINE rule43 #-}
   rule43 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule63 #-}
   rule63 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
{-# NOINLINE sem_EChildren_Nil #-}
sem_EChildren_Nil ::  T_EChildren 
sem_EChildren_Nil  = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule64  ()
         _lhsOargpats ::  [PP_Doc] 
         _lhsOargpats = rule65  ()
         _lhsOargtps ::  [PP_Doc] 
         _lhsOargtps = rule66  ()
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule67  ()
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule68  ()
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule69  ()
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule70  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule71  ()
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule64 #-}
   rule64 = \  (_ :: ()) ->
     []
   {-# INLINE rule65 #-}
   rule65 = \  (_ :: ()) ->
     []
   {-# INLINE rule66 #-}
   rule66 = \  (_ :: ()) ->
     []
   {-# INLINE rule67 #-}
   rule67 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule68 #-}
   rule68 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule69 #-}
   rule69 = \  (_ :: ()) ->
     []
   {-# INLINE rule70 #-}
   rule70 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule71 #-}
   rule71 = \  (_ :: ()) ->
     Set.empty

-- ENonterminal ------------------------------------------------
-- wrapper
data Inh_ENonterminal  = Inh_ENonterminal { allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminal :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), derivings_Inh_ENonterminal :: (Derivings), importBlocks_Inh_ENonterminal :: (PP_Doc), inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminal :: (String), mainName_Inh_ENonterminal :: (String), moduleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String), options_Inh_ENonterminal :: (Options), pragmaBlocks_Inh_ENonterminal :: (String), synmap_Inh_ENonterminal :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminal :: (PP_Doc), typeSyns_Inh_ENonterminal :: (TypeSyns), wrappers_Inh_ENonterminal :: (Set NontermIdent) }
data Syn_ENonterminal  = Syn_ENonterminal { appendCommon_Syn_ENonterminal :: ( PP_Doc ), appendMain_Syn_ENonterminal :: ( PP_Doc ), childvisit_Syn_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_ENonterminal :: (Seq Error), fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_ENonterminal :: (IO ()), imports_Syn_ENonterminal :: ([PP_Doc]), initStates_Syn_ENonterminal :: (Map NontermIdent Int), output_Syn_ENonterminal :: (PP_Doc), semFunBndDefs_Syn_ENonterminal :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminal :: (Seq PP_Doc), visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminal #-}
wrap_ENonterminal :: T_ENonterminal  -> Inh_ENonterminal  -> (Syn_ENonterminal )
wrap_ENonterminal (T_ENonterminal act) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminal_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminal_s8 sem arg)
        return (Syn_ENonterminal _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_ENonterminal_vIn7  = T_ENonterminal_vIn7 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Derivings) (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) (PP_Doc) (TypeSyns) (Set NontermIdent)
data T_ENonterminal_vOut7  = T_ENonterminal_vOut7 ( PP_Doc ) ( PP_Doc ) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Map NontermIdent Int) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminal_ENonterminal #-}
sem_ENonterminal_ENonterminal :: (NontermIdent) -> ([Identifier]) -> (ClassContext) -> (StateIdentifier) -> (Maybe VisitIdentifier) -> (Map StateIdentifier StateCtx) -> (Map StateIdentifier StateCtx) -> T_EProductions  -> (Bool) -> (HigherOrderInfo) -> T_ENonterminal 
sem_ENonterminal_ENonterminal arg_nt_ arg_params_ arg_classCtxs_ arg_initial_ arg_initialv_ arg_nextVisits_ arg_prevVisits_ arg_prods_ arg_recursive_ _ = T_ENonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_ENonterminal_v7 
      v7 = \ (T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_prods_))
         (T_EProductions_vOut16 _prodsIallvisits _prodsIchildvisit _prodsIcount _prodsIdatatype _prodsIerrors _prodsIfromToStates _prodsIgenProdIO _prodsIimports _prodsIsemFunBndDefs _prodsIsemFunBndTps _prodsIsem_nt _prodsIsem_prod _prodsIt_visits _prodsIvisitKinds _prodsIvisitdefs _prodsIvisituses) = inv_EProductions_s17 _prodsX17 (T_EProductions_vIn16 _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOclassCtxs _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOmoduleHeader _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOpragmaBlocks _prodsOprevVisits _prodsOrename _prodsOsynmap _prodsOtextBlocks)
         _prodsOrename = rule72 _lhsIoptions
         _prodsOnt = rule73 arg_nt_
         _prodsOparams = rule74 arg_params_
         _prodsOclassCtxs = rule75 arg_classCtxs_
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule76 _datatype _hasWrapper _k_states _lhsIoptions _prodsIsem_prod _prodsIt_visits _sem_nt _t_init _t_states _wr_inh _wr_syn _wrapper arg_nt_
         _hasWrapper = rule77 _lhsIwrappers arg_nt_
         _classPP = rule78 arg_classCtxs_
         _aliasPre = rule79 _classPP _t_params arg_nt_
         _datatype = rule80 _aliasPre _classPP _derivings _lhsItypeSyns _prodsIdatatype _t_params arg_nt_
         _derivings = rule81 _lhsIderivings arg_nt_
         _fsemname = rule82  ()
         _semname = rule83 _fsemname arg_nt_
         _frecarg = rule84 _fsemname
         _sem_tp = rule85 _classPP _quantPP _t_params _t_type arg_nt_
         _quantPP = rule86 arg_params_
         _sem_nt = rule87 _frecarg _fsemname _lhsItypeSyns _prodsIsem_nt _semPragma _sem_tp _semname arg_nt_
         _inlineNt = rule88 _hasWrapper _lhsIoptions _prodsIcount arg_recursive_
         _semPragma = rule89 _inlineNt _lhsIoptions _semname
         (Just _prodsOinhmap) = rule90 _lhsIinhmap arg_nt_
         (Just _prodsOsynmap) = rule91 _lhsIsynmap arg_nt_
         _prodsOallInhmap = rule92 _lhsIinhmap
         _prodsOallSynmap = rule93 _lhsIsynmap
         _outedges = rule94 _prodsIallvisits
         _inedges = rule95 _prodsIallvisits
         _allstates = rule96 _inedges _outedges arg_initial_
         _stvisits = rule97 _prodsIallvisits
         _t_type = rule98 arg_nt_
         _t_params = rule99 arg_params_
         _t_init = rule100 _lhsIoptions _t_params _t_type arg_initial_
         _t_states = rule101 _allstates _t_params arg_nextVisits_ arg_nt_
         _k_type = rule102 arg_nt_
         _k_states = rule103 _allstates _k_type _prodsIallvisits _t_params _t_type arg_nextVisits_ arg_nt_
         _wr_inh = rule104 _genwrap _wr_inhs
         _wr_syn = rule105 _genwrap _wr_syns
         _genwrap = rule106 _addbang _t_params arg_nt_
         _synAttrs = rule107 _lhsIinhmap arg_nt_
         _wr_inhs = rule108 _synAttrs _wr_filter
         _wr_inhs1 = rule109 _synAttrs
         _wr_filter = rule110 _lhsIoptions
         _wr_syns = rule111 _lhsIsynmap arg_nt_
         _inhlist = rule112 _wr_inhs
         _inhlist1 = rule113 _wr_inhs1
         _synlist = rule114 _wr_syns
         _wrapname = rule115 arg_nt_
         _inhname = rule116 arg_nt_
         _synname = rule117 arg_nt_
         _firstVisitInfo = rule118 arg_initial_ arg_nextVisits_
         _wrapper = rule119 _addbang _addbangWrap _classPP _firstVisitInfo _inhlist _inhlist1 _inhname _k_type _lhsIallVisitKinds _lhsImainName _lhsIoptions _quantPP _synlist _synname _t_params _t_type _wrapPragma _wrapname arg_initial_ arg_initialv_ arg_nt_
         _wrapPragma = rule120 _lhsIoptions _wrapname
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule121 _prodsIsemFunBndDefs _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule122 _prodsIsemFunBndTps _semFunBndTp
         _semFunBndDef = rule123 _semFunBndNm _semname
         _semFunBndTp = rule124 _semFunBndNm _sem_tp
         _semFunBndNm = rule125 arg_nt_
         _prodsOinitial = rule126 arg_initial_
         _prodsOallstates = rule127 _allstates
         _lhsOappendMain ::  PP_Doc 
         _lhsOappendMain = rule128 _lhsIwrappers _sem_nt _wr_inh _wr_syn _wrapper arg_nt_
         _lhsOappendCommon ::  PP_Doc 
         _lhsOappendCommon = rule129 _datatype _k_states _lhsIoptions _prodsIt_visits _t_init _t_states
         _addbang = rule130 _lhsIoptions
         _addbangWrap = rule131  ()
         _prodsOnextVisits = rule132 arg_nextVisits_
         _prodsOprevVisits = rule133 arg_prevVisits_
         _prodsOlocalAttrTypes = rule134 _lhsIlocalAttrTypes arg_nt_
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule135 arg_initial_ arg_nt_
         _ntType = rule136 arg_nt_ arg_params_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule137 _prodsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule138 _prodsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule139 _prodsIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule140 _prodsIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule141 _prodsIimports
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule142 _prodsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule143 _prodsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule144 _prodsIvisituses
         _prodsOallFromToStates = rule145 _lhsIallFromToStates
         _prodsOallInitStates = rule146 _lhsIallInitStates
         _prodsOallVisitKinds = rule147 _lhsIallVisitKinds
         _prodsOallchildvisit = rule148 _lhsIallchildvisit
         _prodsOavisitdefs = rule149 _lhsIavisitdefs
         _prodsOavisituses = rule150 _lhsIavisituses
         _prodsOimportBlocks = rule151 _lhsIimportBlocks
         _prodsOmainFile = rule152 _lhsImainFile
         _prodsOmainName = rule153 _lhsImainName
         _prodsOmoduleHeader = rule154 _lhsImoduleHeader
         _prodsOntType = rule155 _ntType
         _prodsOoptions = rule156 _lhsIoptions
         _prodsOpragmaBlocks = rule157 _lhsIpragmaBlocks
         _prodsOtextBlocks = rule158 _lhsItextBlocks
         __result_ = T_ENonterminal_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminal_s8 v7
   {-# INLINE rule72 #-}
   {-# LINE 55 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule72 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 55 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  rename _lhsIoptions
                                  {-# LINE 1047 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 63 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule73 = \ nt_ ->
                              {-# LINE 63 "./src-ag/ExecutionPlan2Hs.ag" #-}
                              nt_
                              {-# LINE 1053 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 75 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule74 = \ params_ ->
                   {-# LINE 75 "./src-ag/ExecutionPlan2Hs.ag" #-}
                   params_
                   {-# LINE 1059 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 79 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule75 = \ classCtxs_ ->
                      {-# LINE 79 "./src-ag/ExecutionPlan2Hs.ag" #-}
                      classCtxs_
                      {-# LINE 1065 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 96 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule76 = \ _datatype _hasWrapper _k_states ((_lhsIoptions) :: Options) ((_prodsIsem_prod) :: PP_Doc) ((_prodsIt_visits) :: PP_Doc) _sem_nt _t_init _t_states _wr_inh _wr_syn _wrapper nt_ ->
                                {-# LINE 96 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                ("-- " ++ getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-')
                                >-< (if dataTypes _lhsIoptions
                                     then "-- data"
                                          >-< _datatype
                                          >-< ""
                                     else empty)
                                >-< (if _hasWrapper
                                     then "-- wrapper"
                                          >-< _wr_inh
                                          >-< _wr_syn
                                          >-< _wrapper
                                          >-< ""
                                     else empty)
                                >-< (if   folds _lhsIoptions
                                     then "-- cata"
                                          >-< _sem_nt
                                          >-< ""
                                     else empty)
                                >-< (if   semfuns _lhsIoptions
                                     then "-- semantic domain"
                                          >-< _t_init
                                          >-< _t_states
                                          >-< _k_states
                                          >-< _prodsIt_visits
                                          >-< _prodsIsem_prod
                                          >-< ""
                                     else empty)
                                {-# LINE 1097 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 123 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule77 = \ ((_lhsIwrappers) :: Set NontermIdent) nt_ ->
                                    {-# LINE 123 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    nt_ `Set.member` _lhsIwrappers
                                    {-# LINE 1103 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 136 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule78 = \ classCtxs_ ->
                                  {-# LINE 136 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppClasses $ classCtxsToDocs classCtxs_
                                  {-# LINE 1109 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 137 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule79 = \ _classPP _t_params nt_ ->
                                  {-# LINE 137 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  "type" >#< _classPP     >#< nt_ >#< _t_params     >#< "="
                                  {-# LINE 1115 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 138 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule80 = \ _aliasPre _classPP _derivings ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype) :: [PP_Doc]) _t_params nt_ ->
                                  {-# LINE 138 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  case lookup nt_ _lhsItypeSyns of
                                     Nothing -> "data" >#< _classPP     >#< nt_ >#< _t_params
                                                >-< ( if null _prodsIdatatype
                                                      then empty
                                                      else indent 2 $ vlist $ ( ("=" >#< head _prodsIdatatype)
                                                                              : (map ("|" >#<) $ tail _prodsIdatatype))
                                                    )
                                                >-< indent 2 _derivings
                                     Just (List t)     -> _aliasPre     >#< "[" >#< show t >#< "]"
                                     Just (Maybe t)    -> _aliasPre     >#< "Maybe" >#< show t
                                     Just (Tuple ts)   -> _aliasPre     >#< pp_parens (ppCommas $ map (show . snd) ts)
                                     Just (Either l r) -> _aliasPre     >#< "Either" >#< show l >#< show r
                                     Just (Map k v)    -> _aliasPre     >#< "Data.Map" >#< pp_parens (show k) >#< show v
                                     Just (IntMap t)   -> _aliasPre     >#< "Data.IntMap.IntMap" >#< show t
                                     Just (OrdSet t)   -> _aliasPre     >#< "Data.Set.Set" >#< show t
                                     Just IntSet       -> _aliasPre     >#< "Data.IntSet.IntSet"
                                  {-# LINE 1136 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 155 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule81 = \ ((_lhsIderivings) :: Derivings) nt_ ->
                                   {-# LINE 155 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Nothing -> empty
                                      Just s  -> if   Set.null s
                                                 then empty
                                                 else "deriving" >#< (pp_parens $ ppCommas $ map pp $ Set.toList s)
                                   {-# LINE 1146 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 225 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule82 = \  (_ :: ()) ->
                                  {-# LINE 225 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  \x -> "sem_" ++ show x
                                  {-# LINE 1152 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 226 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule83 = \ _fsemname nt_ ->
                                 {-# LINE 226 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _fsemname     nt_
                                 {-# LINE 1158 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 227 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule84 = \ _fsemname ->
                                 {-# LINE 227 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \t x -> case t of
                                            NT nt _ _ -> pp_parens (_fsemname nt >#< x)
                                            _         -> pp x
                                 {-# LINE 1166 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 233 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule85 = \ _classPP _quantPP _t_params _t_type nt_ ->
                                 {-# LINE 233 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _quantPP     >#< _classPP     >#< nt_ >#< _t_params     >#< "->" >#< _t_type     >#< _t_params
                                 {-# LINE 1172 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 234 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule86 = \ params_ ->
                                 {-# LINE 234 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppQuants params_
                                 {-# LINE 1178 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 235 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule87 = \ _frecarg _fsemname ((_lhsItypeSyns) :: TypeSyns) ((_prodsIsem_nt) :: PP_Doc) _semPragma _sem_tp _semname nt_ ->
                                 {-# LINE 235 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _semPragma
                                 >-< _semname     >#< "::" >#< _sem_tp
                                 >-< case lookup nt_ _lhsItypeSyns of
                                        Nothing -> _prodsIsem_nt
                                        Just (List t) -> _semname     >#< "list" >#< "=" >#< "Prelude.foldr" >#< _semname     >|< "_Cons"
                                                         >#< _semname     >|< "_Nil"
                                                         >#< case t of
                                                                NT nt _ _ -> pp_parens ("Prelude.map" >#< _fsemname nt >#< "list")
                                                                _         -> pp "list"
                                        Just (Maybe t) -> _semname     >#< "Prelude.Nothing" >#< "=" >#< _semname     >|< "_Nothing"
                                                          >-< _semname     >#< pp_parens ("Prelude.Just just") >#< "="
                                                          >#< _semname     >|< "_Just" >#< _frecarg t "just"
                                        Just (Tuple ts) -> _semname     >#< pp_parens (ppCommas $ map fst ts) >#< "="
                                                           >#< _semname     >|< "_Tuple" >#< ppSpaced (map (\t -> _frecarg (snd t) (show $ fst t)) ts)
                                        Just (Either l r) -> _semname     >#< "(Prelude.Left left)" >#< "=" >#< _semname     >|< "_Left" >#< _frecarg l "left"
                                                             >-< _semname     >#< "(Prelude.Right right)" >#< "=" >#< _semname     >|< "_Right" >#< _frecarg r "right"
                                        Just (Map k v) -> _semname     >#< "m" >#< "=" >#< "Data.Map.foldrWithKey"
                                                          >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                          >#< case v of
                                                                 NT nt _ _ -> pp_parens ("Data.Map.map" >#< _fsemname nt >#< "m")
                                                                 _         -> pp "m"
                                        Just (IntMap v) -> _semname     >#< "m" >#< "=" >#< "Data.IntMap.foldWithKey"
                                                           >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                           >#< case v of
                                                                  NT nt _ _ -> pp_parens ("Data.IntMap.map" >#< _fsemname nt >#< "m")
                                                                  _         -> pp "m"
                                        Just (OrdSet t) -> _semname     >#< "s" >#< "=" >#< "Prelude.foldr" >#< _semname     >|< "_Entry"
                                                           >#< _semname     >|< "_Nil"
                                                           >#< pp_parens (
                                                                 ( case t of
                                                                     NT nt _ _ -> pp_parens ("Prelude.map" >#< _fsemname nt)
                                                                     _         -> empty
                                                                 ) >#< pp_parens ("Data.IntSet.elems" >#< "s")
                                                               )
                                        Just IntSet     -> _semname     >#< "s" >#< "=" >#< "Prelude.foldr" >#< _semname     >|< "_Entry"
                                                           >#< _semname     >|< "_Nil"
                                                           >#< pp_parens ("Data.IntSet.elems" >#< "s")
                                 {-# LINE 1220 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 275 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule88 = \ _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIcount) :: Int) recursive_ ->
                                  {-# LINE 275 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  not (lateHigherOrderBinding _lhsIoptions) && not recursive_ && (_prodsIcount == 1 || (aggressiveInlinePragmas _lhsIoptions && not _hasWrapper    ))
                                  {-# LINE 1226 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 276 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule89 = \ _inlineNt ((_lhsIoptions) :: Options) _semname ->
                                  {-# LINE 276 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  if noInlinePragmas _lhsIoptions
                                  then empty
                                  else if _inlineNt
                                       then ppInline _semname
                                       else if helpInlining _lhsIoptions && not (lateHigherOrderBinding _lhsIoptions)
                                            then ppInlinable _semname
                                            else ppNoInline _semname
                                  {-# LINE 1238 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 322 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule90 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 322 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                         Map.lookup nt_ _lhsIinhmap
                                         {-# LINE 1244 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 323 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule91 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 323 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                         Map.lookup nt_ _lhsIsynmap
                                         {-# LINE 1250 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 324 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule92 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 324 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     _lhsIinhmap
                                     {-# LINE 1256 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 325 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule93 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 325 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     _lhsIsynmap
                                     {-# LINE 1262 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 346 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule94 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 346 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 1268 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 347 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule95 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 347 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 1274 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 348 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule96 = \ _inedges _outedges initial_ ->
                                   {-# LINE 348 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 1280 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 349 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule97 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 349 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1286 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 350 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule98 = \ nt_ ->
                                   {-# LINE 350 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   "T_" >|< nt_
                                   {-# LINE 1292 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 351 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule99 = \ params_ ->
                                   {-# LINE 351 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   ppSpaced params_
                                   {-# LINE 1298 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 352 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule100 = \ ((_lhsIoptions) :: Options) _t_params _t_type initial_ ->
                                   {-# LINE 352 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   "newtype" >#< _t_type     >#< _t_params     >#< "=" >#< _t_type     >#<
                                     pp_braces (
                                       "attach_">|< _t_type     >#< "::"
                                         >#< ppMonadType _lhsIoptions >#< pp_parens (_t_type     >|< "_s" >|< initial_ >#< _t_params    ))
                                   {-# LINE 1307 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 356 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule101 = \ _allstates _t_params nextVisits_ nt_ ->
                                   {-# LINE 356 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   vlist $ map (\st ->
                                      let nt_st = nt_ >|< "_s" >|< st
                                          t_st  = "T_" >|< nt_st
                                          k_st  = "K_" >|< nt_st
                                          c_st  = "C_" >|< nt_st
                                          inv_st  = "inv_" >|< nt_st
                                          nextVisit = Map.findWithDefault ManyVis st nextVisits_
                                      in  case nextVisit of
                                            NoneVis    -> "data" >#< t_st >#< _t_params     >#< "=" >#< c_st
                                            OneVis vId -> "newtype" >#< t_st >#< _t_params     >#< "=" >#< c_st
                                                           >#< (pp_braces $ inv_st >#< "::" >#< pp_parens (conNmTVisit nt_ vId >#< _t_params    ))
                                            ManyVis    -> "data" >#< t_st >#< _t_params     >#< "where" >#< c_st >#< "::"
                                                            >#< (pp_braces $ inv_st >#< "::" >#< "!" >|< pp_parens ("forall t." >#< k_st >#< _t_params     >#< "t" >#< "->" >#< "t"))
                                                            >#< "->" >#< t_st >#< _t_params
                                          ) $ Set.toList _allstates
                                   {-# LINE 1327 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule102 #-}
   {-# LINE 374 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule102 = \ nt_ ->
                                  {-# LINE 374 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  "K_" ++ show nt_
                                  {-# LINE 1333 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 375 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule103 = \ _allstates _k_type ((_prodsIallvisits) :: [VisitStateState]) _t_params _t_type nextVisits_ nt_ ->
                                  {-# LINE 375 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  vlist $ map (\st ->
                                     let nt_st = nt_ >|< "_s" >|< st
                                         k_st  = "K_" >|< nt_st
                                         outg  = filter (\(v,f,t) -> f == st) _prodsIallvisits
                                         visitlist = vlist $ map (\(v,f,t) ->
                                             _k_type     >|< "_v" >|< v >#< "::" >#< k_st >#< _t_params     >#< pp_parens (_t_type     >|< "_v" >|< v >#< _t_params    )
                                              ) outg
                                         nextVisit = Map.findWithDefault ManyVis st nextVisits_
                                         decl = "data" >#< k_st >#< "k" >#< _t_params     >#< "where" >-< indent 3 visitlist
                                     in  case nextVisit of
                                           NoneVis  -> empty
                                           OneVis _ -> empty
                                           ManyVis  -> decl
                                     ) $ Set.toList _allstates
                                  {-# LINE 1352 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 442 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule104 = \ _genwrap _wr_inhs ->
                                  {-# LINE 442 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _genwrap     "Inh" _wr_inhs
                                  {-# LINE 1358 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 443 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule105 = \ _genwrap _wr_syns ->
                                  {-# LINE 443 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _genwrap     "Syn" _wr_syns
                                  {-# LINE 1364 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule106 #-}
   {-# LINE 444 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule106 = \ _addbang _t_params nt_ ->
                                  {-# LINE 444 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  \nm attr -> "data" >#< nm >|< "_" >|< nt_ >#< _t_params     >#< "=" >#< nm >|< "_" >|< nt_ >#< "{"
                                              >#< (ppCommas $ map (\(i,t) -> i >|< "_" >|< nm >|< "_" >|< nt_ >#< "::"
                                              >#< (_addbang     $ pp_parens $ typeToHaskellString (Just nt_) [] t)) attr) >#< "}"
                                  {-# LINE 1372 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 447 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule107 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 447 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  fromJust $ Map.lookup nt_ _lhsIinhmap
                                  {-# LINE 1378 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule108 #-}
   {-# LINE 448 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule108 = \ _synAttrs _wr_filter ->
                                  {-# LINE 448 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.toList $ _wr_filter     $ _synAttrs
                                  {-# LINE 1384 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule109 #-}
   {-# LINE 449 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule109 = \ _synAttrs ->
                                  {-# LINE 449 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.toList _synAttrs
                                  {-# LINE 1390 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule110 #-}
   {-# LINE 450 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule110 = \ ((_lhsIoptions) :: Options) ->
                                   {-# LINE 450 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   if lateHigherOrderBinding _lhsIoptions
                                   then Map.delete idLateBindingAttr
                                   else id
                                   {-# LINE 1398 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule111 #-}
   {-# LINE 453 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule111 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 453 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                  {-# LINE 1404 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule112 #-}
   {-# LINE 454 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule112 = \ _wr_inhs ->
                                  {-# LINE 454 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  map (lhsname True . fst) _wr_inhs
                                  {-# LINE 1410 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule113 #-}
   {-# LINE 455 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule113 = \ _wr_inhs1 ->
                                  {-# LINE 455 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  map (lhsname True . fst) _wr_inhs1
                                  {-# LINE 1416 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule114 #-}
   {-# LINE 456 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule114 = \ _wr_syns ->
                                  {-# LINE 456 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  map (lhsname False . fst) _wr_syns
                                  {-# LINE 1422 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule115 #-}
   {-# LINE 457 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule115 = \ nt_ ->
                                  {-# LINE 457 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  "wrap_" ++ show nt_
                                  {-# LINE 1428 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule116 #-}
   {-# LINE 458 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule116 = \ nt_ ->
                                  {-# LINE 458 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  "Inh_" ++ show nt_
                                  {-# LINE 1434 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule117 #-}
   {-# LINE 459 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule117 = \ nt_ ->
                                  {-# LINE 459 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  "Syn_" ++ show nt_
                                  {-# LINE 1440 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule118 #-}
   {-# LINE 460 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule118 = \ initial_ nextVisits_ ->
                                        {-# LINE 460 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        Map.findWithDefault ManyVis initial_ nextVisits_
                                        {-# LINE 1446 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule119 #-}
   {-# LINE 461 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule119 = \ _addbang _addbangWrap _classPP _firstVisitInfo _inhlist _inhlist1 _inhname _k_type ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) _quantPP _synlist _synname _t_params _t_type _wrapPragma _wrapname initial_ initialv_ nt_ ->
                                  {-# LINE 461 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _wrapPragma
                                  >-< (_wrapname     >#< "::" >#< _quantPP     >#< _classPP     >#< _t_type     >#< _t_params     >#< "->"
                                        >#< _inhname     >#< _t_params     >#< "->" >#< ( if monadicWrappers _lhsIoptions then ppMonadType _lhsIoptions else empty) >#< pp_parens (_synname     >#< _t_params    ))
                                  >-< (_wrapname     >#< (_addbang     $ pp_parens (_t_type     >#< pp "act"))
                                      >#< (_addbang     $ pp_parens (_inhname
                                             >#< (ppSpaced $ map (_addbangWrap     . pp) _inhlist    )) >#< "="))
                                  >-<
                                  indent 3 (case initialv_ of
                                    Nothing -> _synname     >#< " { }"
                                    Just initv ->
                                      let inCon  = conNmTVisitIn nt_ initv
                                          outCon = conNmTVisitOut nt_ initv
                                          pat    = _addbang     $ pp_parens $ pat0
                                          pat0   = outCon >#< ppSpaced _synlist
                                          arg    = inCon >#< ppSpaced _inhlist1
                                          ind    = case _firstVisitInfo     of
                                                     NoneVis  -> error "wrapper: initial state should have a next visit but it has none"
                                                     OneVis _ -> empty
                                                     ManyVis  -> _k_type     >|< "_v" >|< initv
                                          extra  = if dummyTokenVisit _lhsIoptions
                                                   then pp $ dummyArg _lhsIoptions True
                                                   else empty
                                          convert = case Map.lookup initv _lhsIallVisitKinds of
                                                      Just kind -> case kind of
                                                                     VisitPure _  -> text "return"
                                                                     VisitMonadic -> empty
                                          unMonad | monadicWrappers _lhsIoptions = empty
                                                  | otherwise                    = unMon _lhsIoptions
                                      in unMonad >#< "("
                                         >-< indent 2 (
                                                "do" >#< ( _addbang     (pp "sem") >#< "<-" >#< "act"
                                                         >-< "let" >#< _addbangWrap     (pp "arg") >#< "=" >#< arg
                                                         >-< pat >#< "<-" >#< convert >#< pp_parens ("inv_" >|< nt_ >|< "_s" >|< initial_ >#< "sem" >#< ind >#< "arg" >#< extra)
                                                         >-< "return" >#< pp_parens (_synname     >#< ppSpaced _synlist    )
                                                         )
                                              )
                                         >-< ")" )
                                  >-< if lateHigherOrderBinding _lhsIoptions
                                      then indent 2 ("where" >#< lhsname True idLateBindingAttr >#< "=" >#< lateBindingFieldNm _lhsImainName)
                                      else empty
                                  {-# LINE 1491 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule120 #-}
   {-# LINE 502 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule120 = \ ((_lhsIoptions) :: Options) _wrapname ->
                                    {-# LINE 502 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    if parallelInvoke _lhsIoptions && not (monadicWrappers _lhsIoptions)
                                    then ppNoInline _wrapname
                                    else if noInlinePragmas _lhsIoptions
                                         then empty
                                         else ppInlinable _wrapname
                                    {-# LINE 1501 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 514 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule121 = \ ((_prodsIsemFunBndDefs) :: Seq PP_Doc) _semFunBndDef ->
                        {-# LINE 514 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                        {-# LINE 1507 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule122 #-}
   {-# LINE 515 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule122 = \ ((_prodsIsemFunBndTps) :: Seq PP_Doc) _semFunBndTp ->
                        {-# LINE 515 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                        {-# LINE 1513 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule123 #-}
   {-# LINE 516 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule123 = \ _semFunBndNm _semname ->
                        {-# LINE 516 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 1519 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule124 #-}
   {-# LINE 517 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule124 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 517 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndNm     >#< "::" >#< _sem_tp
                        {-# LINE 1525 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule125 #-}
   {-# LINE 518 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule125 = \ nt_ ->
                        {-# LINE 518 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        lateSemNtLabel nt_
                        {-# LINE 1531 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule126 #-}
   {-# LINE 556 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule126 = \ initial_ ->
                                     {-# LINE 556 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     initial_
                                     {-# LINE 1537 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule127 #-}
   {-# LINE 557 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule127 = \ _allstates ->
                                     {-# LINE 557 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     _allstates
                                     {-# LINE 1543 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule128 #-}
   {-# LINE 1471 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule128 = \ ((_lhsIwrappers) :: Set NontermIdent) _sem_nt _wr_inh _wr_syn _wrapper nt_ ->
                                      {-# LINE 1471 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                      (if nt_ `Set.member` _lhsIwrappers
                                       then     _wr_inh
                                            >-< _wr_syn
                                            >-< _wrapper
                                       else empty)
                                      >-< _sem_nt
                                      {-# LINE 1554 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule129 #-}
   {-# LINE 1477 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule129 = \ _datatype _k_states ((_lhsIoptions) :: Options) ((_prodsIt_visits) :: PP_Doc) _t_init _t_states ->
                                      {-# LINE 1477 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                      (if dataTypes _lhsIoptions then _datatype     else empty)
                                      >-< _t_init
                                      >-< _t_states
                                      >-< _k_states
                                      >-< _prodsIt_visits
                                      {-# LINE 1564 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule130 #-}
   {-# LINE 1537 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule130 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1537 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 1570 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule131 #-}
   {-# LINE 1545 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule131 = \  (_ :: ()) ->
                                                        {-# LINE 1545 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                        id
                                                        {-# LINE 1576 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule132 #-}
   {-# LINE 1557 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule132 = \ nextVisits_ ->
                       {-# LINE 1557 "./src-ag/ExecutionPlan2Hs.ag" #-}
                       nextVisits_
                       {-# LINE 1582 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule133 #-}
   {-# LINE 1558 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule133 = \ prevVisits_ ->
                       {-# LINE 1558 "./src-ag/ExecutionPlan2Hs.ag" #-}
                       prevVisits_
                       {-# LINE 1588 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule134 #-}
   {-# LINE 1602 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule134 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) nt_ ->
                           {-# LINE 1602 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                           {-# LINE 1594 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule135 #-}
   {-# LINE 1629 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule135 = \ initial_ nt_ ->
                     {-# LINE 1629 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     Map.singleton nt_ initial_
                     {-# LINE 1600 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule136 #-}
   {-# LINE 1643 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule136 = \ nt_ params_ ->
                 {-# LINE 1643 "./src-ag/ExecutionPlan2Hs.ag" #-}
                 NT nt_ (map show params_) False
                 {-# LINE 1606 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule137 #-}
   rule137 = \ ((_prodsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _prodsIchildvisit
   {-# INLINE rule138 #-}
   rule138 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule139 #-}
   rule139 = \ ((_prodsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _prodsIfromToStates
   {-# INLINE rule140 #-}
   rule140 = \ ((_prodsIgenProdIO) :: IO ()) ->
     _prodsIgenProdIO
   {-# INLINE rule141 #-}
   rule141 = \ ((_prodsIimports) :: [PP_Doc]) ->
     _prodsIimports
   {-# INLINE rule142 #-}
   rule142 = \ ((_prodsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _prodsIvisitKinds
   {-# INLINE rule143 #-}
   rule143 = \ ((_prodsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisitdefs
   {-# INLINE rule144 #-}
   rule144 = \ ((_prodsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisituses
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule155 #-}
   rule155 = \ _ntType ->
     _ntType
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

-- ENonterminals -----------------------------------------------
-- wrapper
data Inh_ENonterminals  = Inh_ENonterminals { allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminals :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), derivings_Inh_ENonterminals :: (Derivings), importBlocks_Inh_ENonterminals :: (PP_Doc), inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminals :: (String), mainName_Inh_ENonterminals :: (String), moduleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String), options_Inh_ENonterminals :: (Options), pragmaBlocks_Inh_ENonterminals :: (String), synmap_Inh_ENonterminals :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminals :: (PP_Doc), typeSyns_Inh_ENonterminals :: (TypeSyns), wrappers_Inh_ENonterminals :: (Set NontermIdent) }
data Syn_ENonterminals  = Syn_ENonterminals { appendCommon_Syn_ENonterminals :: ([PP_Doc]), appendMain_Syn_ENonterminals :: ([PP_Doc]), childvisit_Syn_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_ENonterminals :: (Seq Error), fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_ENonterminals :: (IO ()), imports_Syn_ENonterminals :: ([PP_Doc]), initStates_Syn_ENonterminals :: (Map NontermIdent Int), output_Syn_ENonterminals :: (PP_Doc), semFunBndDefs_Syn_ENonterminals :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminals :: (Seq PP_Doc), visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminals #-}
wrap_ENonterminals :: T_ENonterminals  -> Inh_ENonterminals  -> (Syn_ENonterminals )
wrap_ENonterminals (T_ENonterminals act) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminals_s11 sem arg)
        return (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_ENonterminals_vIn10  = T_ENonterminals_vIn10 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Derivings) (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) (PP_Doc) (TypeSyns) (Set NontermIdent)
data T_ENonterminals_vOut10  = T_ENonterminals_vOut10 ([PP_Doc]) ([PP_Doc]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Map NontermIdent Int) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminals_Cons #-}
sem_ENonterminals_Cons :: T_ENonterminal  -> T_ENonterminals  -> T_ENonterminals 
sem_ENonterminals_Cons arg_hd_ arg_tl_ = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_ENonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_tl_))
         (T_ENonterminal_vOut7 _hdIappendCommon _hdIappendMain _hdIchildvisit _hdIerrors _hdIfromToStates _hdIgenProdIO _hdIimports _hdIinitStates _hdIoutput _hdIsemFunBndDefs _hdIsemFunBndTps _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_ENonterminal_s8 _hdX8 (T_ENonterminal_vIn7 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOderivings _hdOimportBlocks _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOoptions _hdOpragmaBlocks _hdOsynmap _hdOtextBlocks _hdOtypeSyns _hdOwrappers)
         (T_ENonterminals_vOut10 _tlIappendCommon _tlIappendMain _tlIchildvisit _tlIerrors _tlIfromToStates _tlIgenProdIO _tlIimports _tlIinitStates _tlIoutput _tlIsemFunBndDefs _tlIsemFunBndTps _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_ENonterminals_s11 _tlX11 (T_ENonterminals_vIn10 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOderivings _tlOimportBlocks _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOoptions _tlOpragmaBlocks _tlOsynmap _tlOtextBlocks _tlOtypeSyns _tlOwrappers)
         _lhsOappendCommon :: [PP_Doc]
         _lhsOappendCommon = rule159 _hdIappendCommon _tlIappendCommon
         _lhsOappendMain :: [PP_Doc]
         _lhsOappendMain = rule160 _hdIappendMain _tlIappendMain
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule161 _hdIchildvisit _tlIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule162 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule163 _hdIfromToStates _tlIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule164 _hdIgenProdIO _tlIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule165 _hdIimports _tlIimports
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule166 _hdIinitStates _tlIinitStates
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule167 _hdIoutput _tlIoutput
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule168 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule169 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule170 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule171 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule172 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule173 _lhsIallFromToStates
         _hdOallInitStates = rule174 _lhsIallInitStates
         _hdOallVisitKinds = rule175 _lhsIallVisitKinds
         _hdOallchildvisit = rule176 _lhsIallchildvisit
         _hdOavisitdefs = rule177 _lhsIavisitdefs
         _hdOavisituses = rule178 _lhsIavisituses
         _hdOderivings = rule179 _lhsIderivings
         _hdOimportBlocks = rule180 _lhsIimportBlocks
         _hdOinhmap = rule181 _lhsIinhmap
         _hdOlocalAttrTypes = rule182 _lhsIlocalAttrTypes
         _hdOmainFile = rule183 _lhsImainFile
         _hdOmainName = rule184 _lhsImainName
         _hdOmoduleHeader = rule185 _lhsImoduleHeader
         _hdOoptions = rule186 _lhsIoptions
         _hdOpragmaBlocks = rule187 _lhsIpragmaBlocks
         _hdOsynmap = rule188 _lhsIsynmap
         _hdOtextBlocks = rule189 _lhsItextBlocks
         _hdOtypeSyns = rule190 _lhsItypeSyns
         _hdOwrappers = rule191 _lhsIwrappers
         _tlOallFromToStates = rule192 _lhsIallFromToStates
         _tlOallInitStates = rule193 _lhsIallInitStates
         _tlOallVisitKinds = rule194 _lhsIallVisitKinds
         _tlOallchildvisit = rule195 _lhsIallchildvisit
         _tlOavisitdefs = rule196 _lhsIavisitdefs
         _tlOavisituses = rule197 _lhsIavisituses
         _tlOderivings = rule198 _lhsIderivings
         _tlOimportBlocks = rule199 _lhsIimportBlocks
         _tlOinhmap = rule200 _lhsIinhmap
         _tlOlocalAttrTypes = rule201 _lhsIlocalAttrTypes
         _tlOmainFile = rule202 _lhsImainFile
         _tlOmainName = rule203 _lhsImainName
         _tlOmoduleHeader = rule204 _lhsImoduleHeader
         _tlOoptions = rule205 _lhsIoptions
         _tlOpragmaBlocks = rule206 _lhsIpragmaBlocks
         _tlOsynmap = rule207 _lhsIsynmap
         _tlOtextBlocks = rule208 _lhsItextBlocks
         _tlOtypeSyns = rule209 _lhsItypeSyns
         _tlOwrappers = rule210 _lhsIwrappers
         __result_ = T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule159 #-}
   rule159 = \ ((_hdIappendCommon) ::  PP_Doc ) ((_tlIappendCommon) :: [PP_Doc]) ->
     _hdIappendCommon : _tlIappendCommon
   {-# INLINE rule160 #-}
   rule160 = \ ((_hdIappendMain) ::  PP_Doc ) ((_tlIappendMain) :: [PP_Doc]) ->
     _hdIappendMain : _tlIappendMain
   {-# INLINE rule161 #-}
   rule161 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule162 #-}
   rule162 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule163 #-}
   rule163 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule164 #-}
   rule164 = \ ((_hdIgenProdIO) :: IO ()) ((_tlIgenProdIO) :: IO ()) ->
     _hdIgenProdIO >> _tlIgenProdIO
   {-# INLINE rule165 #-}
   rule165 = \ ((_hdIimports) :: [PP_Doc]) ((_tlIimports) :: [PP_Doc]) ->
     _hdIimports ++ _tlIimports
   {-# INLINE rule166 #-}
   rule166 = \ ((_hdIinitStates) :: Map NontermIdent Int) ((_tlIinitStates) :: Map NontermIdent Int) ->
     _hdIinitStates `mappend` _tlIinitStates
   {-# INLINE rule167 #-}
   rule167 = \ ((_hdIoutput) :: PP_Doc) ((_tlIoutput) :: PP_Doc) ->
     _hdIoutput >-< _tlIoutput
   {-# INLINE rule168 #-}
   rule168 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule169 #-}
   rule169 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule170 #-}
   rule170 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule171 #-}
   rule171 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule172 #-}
   rule172 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_ENonterminals_Nil #-}
sem_ENonterminals_Nil ::  T_ENonterminals 
sem_ENonterminals_Nil  = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _lhsOappendCommon :: [PP_Doc]
         _lhsOappendCommon = rule211  ()
         _lhsOappendMain :: [PP_Doc]
         _lhsOappendMain = rule212  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule213  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule214  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule215  ()
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule216  ()
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule217  ()
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule218  ()
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule219  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule220  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule221  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule222  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule223  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule224  ()
         __result_ = T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule211 #-}
   rule211 = \  (_ :: ()) ->
     []
   {-# INLINE rule212 #-}
   rule212 = \  (_ :: ()) ->
     []
   {-# INLINE rule213 #-}
   rule213 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule214 #-}
   rule214 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule215 #-}
   rule215 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule216 #-}
   rule216 = \  (_ :: ()) ->
     return ()
   {-# INLINE rule217 #-}
   rule217 = \  (_ :: ()) ->
     []
   {-# INLINE rule218 #-}
   rule218 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule219 #-}
   rule219 = \  (_ :: ()) ->
     empty
   {-# INLINE rule220 #-}
   rule220 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule221 #-}
   rule221 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule222 #-}
   rule222 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule224 #-}
   rule224 = \  (_ :: ()) ->
     Map.empty

-- EProduction -------------------------------------------------
-- wrapper
data Inh_EProduction  = Inh_EProduction { allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProduction :: (Map NontermIdent Attributes), allInitStates_Inh_EProduction :: (Map NontermIdent Int), allSynmap_Inh_EProduction :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allstates_Inh_EProduction :: (Set StateIdentifier), avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProduction :: (ClassContext), importBlocks_Inh_EProduction :: (PP_Doc), inhmap_Inh_EProduction :: (Attributes), initial_Inh_EProduction :: (StateIdentifier), localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProduction :: (String), mainName_Inh_EProduction :: (String), moduleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), nt_Inh_EProduction :: (NontermIdent), ntType_Inh_EProduction :: (Type), options_Inh_EProduction :: (Options), params_Inh_EProduction :: ([Identifier]), pragmaBlocks_Inh_EProduction :: (String), prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), rename_Inh_EProduction :: (Bool), synmap_Inh_EProduction :: (Attributes), textBlocks_Inh_EProduction :: (PP_Doc) }
data Syn_EProduction  = Syn_EProduction { allvisits_Syn_EProduction :: ([VisitStateState]), childvisit_Syn_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), count_Syn_EProduction :: (Int), datatype_Syn_EProduction :: (PP_Doc), errors_Syn_EProduction :: (Seq Error), fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_EProduction :: (IO ()), imports_Syn_EProduction :: ([PP_Doc]), semFunBndDefs_Syn_EProduction :: (Seq PP_Doc), semFunBndTps_Syn_EProduction :: (Seq PP_Doc), sem_nt_Syn_EProduction :: (PP_Doc), sem_prod_Syn_EProduction :: (PP_Doc), t_visits_Syn_EProduction :: (PP_Doc), visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProduction #-}
wrap_EProduction :: T_EProduction  -> Inh_EProduction  -> (Syn_EProduction )
wrap_EProduction (T_EProduction act) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProduction_s14 sem arg)
        return (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_EProduction_vIn13  = T_EProduction_vIn13 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) (PP_Doc) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) (PP_Doc)
data T_EProduction_vOut13  = T_EProduction_vOut13 ([VisitStateState]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Int) (PP_Doc) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProduction_EProduction #-}
sem_EProduction_EProduction :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_ERules  -> T_EChildren  -> T_Visits  -> T_EProduction 
sem_EProduction_EProduction arg_con_ arg_params_ arg_constraints_ arg_rules_ arg_children_ arg_visits_ = T_EProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_EProduction_v13 
      v13 = \ (T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _rulesX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_rules_))
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_children_))
         _visitsX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_visits_))
         (T_ERules_vOut22 _rulesIerrors _rulesImrules _rulesIruledefs _rulesIruleuses _rulesIsem_rules _rulesIusedArgs) = inv_ERules_s23 _rulesX23 (T_ERules_vIn22 _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOimportBlocks _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOmoduleHeader _rulesOnt _rulesOoptions _rulesOpragmaBlocks _rulesOruleKinds _rulesOsynmap _rulesOtextBlocks _rulesOusageInfo)
         (T_EChildren_vOut4 _childrenIargnamesw _childrenIargpats _childrenIargtps _childrenIchildTypes _childrenIchildintros _childrenIdatatype _childrenIterminaldefs _childrenIusedArgs) = inv_EChildren_s5 _childrenX5 (T_EChildren_vIn4 _childrenOallInitStates _childrenOcon _childrenOimportBlocks _childrenOmainFile _childrenOmainName _childrenOmoduleHeader _childrenOnt _childrenOoptions _childrenOpragmaBlocks _childrenOtextBlocks)
         (T_Visits_vOut55 _visitsIallvisits _visitsIchildvisit _visitsIerrors _visitsIfromToStates _visitsIintramap _visitsIlazyIntras _visitsIruleKinds _visitsIruleUsage _visitsIsem_visit _visitsIt_visits _visitsIusedArgs _visitsIvisitKinds _visitsIvisitdefs _visitsIvisituses) = inv_Visits_s56 _visitsX56 (T_Visits_vIn55 _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOchildintros _visitsOcon _visitsOinhmap _visitsOmrules _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs)
         _childrenOcon = rule225 arg_con_
         _rulesOcon = rule226 arg_con_
         _visitsOcon = rule227 arg_con_
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule228 _childrenIdatatype _classPP1 _lhsInt _lhsIoptions _lhsIrename _quantPP1 arg_con_
         _classPP1 = rule229 arg_constraints_
         _quantPP1 = rule230 arg_params_
         _lhsOcount :: Int
         _lhsOcount = rule231  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule232 _childrenIargnamesw _childrenIargpats _lhsInt _lhsIrename arg_con_
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule233 _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule234 _semFunBndTp
         _semFunBndDef = rule235 _semFunBndNm _semname
         _semFunBndTp = rule236 _semFunBndNm _sem_tp
         _semFunBndNm = rule237 _lhsInt arg_con_
         _t_type = rule238 _lhsInt
         _t_params = rule239 _lhsIparams
         _usedArgs = rule240 _childrenIusedArgs _rulesIusedArgs _visitsIusedArgs
         _args = rule241 _childrenIargpats _usedArgs
         _semname = rule242 _lhsInt arg_con_
         _sem_tp = rule243 _childrenIargtps _classPP2 _quantPP2 _t_params _t_type
         _classPP2 = rule244 _lhsIclassCtxs arg_constraints_
         _quantPP2 = rule245 _lhsIparams arg_params_
         _sem_prod = rule246 _args _lhsIinitial _mbInitializer _mkSemBody _outerlet _scc _semInlinePragma _sem_tp _semname _t_type
         _mkSemBody = rule247  ()
         _mbInitializer = rule248 _lhsIoptions
         _scc = rule249 _lhsIoptions _semname
         _semInlinePragma = rule250 _lhsIoptions _semname
         _outerlet = rule251 _rulesIsem_rules _statefns
         _statefns = rule252 _genstfn _lhsIallstates
         _genstfn = rule253 _addbang _lhsIinitial _lhsInextVisits _lhsInt _lhsIoptions _lhsIprevVisits _stargs _stks _stvs
         _stargs = rule254 _addbang _childTypes _lazyIntras _lhsIallInhmap _lhsIallSynmap _lhsIoptions _localAttrTypes _visitsIintramap
         _stks = rule255 _lhsInt _lhsIoptions _stvisits _t_params
         _stvisits = rule256 _visitsIallvisits
         _stvs = rule257 _visitsIsem_visit
         _visitsOmrules = rule258 _rulesImrules
         _visitsOchildintros = rule259 _childrenIchildintros
         _rulesOusageInfo = rule260 _visitsIruleUsage
         _rulesOruleKinds = rule261 _visitsIruleKinds
         _visitsOallintramap = rule262 _visitsIintramap
         _visitsOterminaldefs = rule263 _childrenIterminaldefs
         _visitsOruledefs = rule264 _rulesIruledefs
         _visitsOruleuses = rule265 _rulesIruleuses
         _lazyIntras = rule266 _visitsIlazyIntras
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule267 _moduleName
         _moduleName = rule268 _lhsImainName _suffix
         _suffix = rule269 _lhsInt arg_con_
         _outputfile = rule270 _lhsImainFile _suffix
         _ppMonadImports = rule271 _lhsIoptions
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule272 _lhsIimportBlocks _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _outputfile _ppMonadImports _sem_prod _semname _suffix
         _addbang = rule273 _lhsIoptions
         _childTypes = rule274 _childrenIchildTypes _lhsIntType
         _localAttrTypes = rule275 _lhsIlocalAttrTypes arg_con_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule276 _visitsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule277 _rulesIerrors _visitsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule278 _visitsIfromToStates
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule279 _visitsIt_visits
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule280 _visitsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule281 _visitsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule282 _visitsIvisituses
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule283 _visitsIallvisits
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule284 _sem_prod
         _rulesOallInhmap = rule285 _lhsIallInhmap
         _rulesOallSynmap = rule286 _lhsIallSynmap
         _rulesOchildTypes = rule287 _childTypes
         _rulesOimportBlocks = rule288 _lhsIimportBlocks
         _rulesOinhmap = rule289 _lhsIinhmap
         _rulesOlazyIntras = rule290 _lazyIntras
         _rulesOlocalAttrTypes = rule291 _localAttrTypes
         _rulesOmainFile = rule292 _lhsImainFile
         _rulesOmainName = rule293 _lhsImainName
         _rulesOmoduleHeader = rule294 _lhsImoduleHeader
         _rulesOnt = rule295 _lhsInt
         _rulesOoptions = rule296 _lhsIoptions
         _rulesOpragmaBlocks = rule297 _lhsIpragmaBlocks
         _rulesOsynmap = rule298 _lhsIsynmap
         _rulesOtextBlocks = rule299 _lhsItextBlocks
         _childrenOallInitStates = rule300 _lhsIallInitStates
         _childrenOimportBlocks = rule301 _lhsIimportBlocks
         _childrenOmainFile = rule302 _lhsImainFile
         _childrenOmainName = rule303 _lhsImainName
         _childrenOmoduleHeader = rule304 _lhsImoduleHeader
         _childrenOnt = rule305 _lhsInt
         _childrenOoptions = rule306 _lhsIoptions
         _childrenOpragmaBlocks = rule307 _lhsIpragmaBlocks
         _childrenOtextBlocks = rule308 _lhsItextBlocks
         _visitsOallFromToStates = rule309 _lhsIallFromToStates
         _visitsOallInhmap = rule310 _lhsIallInhmap
         _visitsOallInitStates = rule311 _lhsIallInitStates
         _visitsOallSynmap = rule312 _lhsIallSynmap
         _visitsOallVisitKinds = rule313 _lhsIallVisitKinds
         _visitsOallchildvisit = rule314 _lhsIallchildvisit
         _visitsOavisitdefs = rule315 _lhsIavisitdefs
         _visitsOavisituses = rule316 _lhsIavisituses
         _visitsOchildTypes = rule317 _childTypes
         _visitsOinhmap = rule318 _lhsIinhmap
         _visitsOnextVisits = rule319 _lhsInextVisits
         _visitsOnt = rule320 _lhsInt
         _visitsOoptions = rule321 _lhsIoptions
         _visitsOparams = rule322 _lhsIparams
         _visitsOprevVisits = rule323 _lhsIprevVisits
         _visitsOsynmap = rule324 _lhsIsynmap
         __result_ = T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProduction_s14 v13
   {-# INLINE rule225 #-}
   {-# LINE 69 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule225 = \ con_ ->
                                 {-# LINE 69 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 con_
                                 {-# LINE 2188 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule226 #-}
   {-# LINE 70 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule226 = \ con_ ->
                                 {-# LINE 70 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 con_
                                 {-# LINE 2194 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule227 #-}
   {-# LINE 71 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule227 = \ con_ ->
                                 {-# LINE 71 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 con_
                                 {-# LINE 2200 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule228 #-}
   {-# LINE 184 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule228 = \ ((_childrenIdatatype) :: [PP_Doc]) _classPP1 ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIrename) :: Bool) _quantPP1 con_ ->
                                 {-# LINE 184 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _quantPP1     >#< _classPP1
                                 >#< conname _lhsIrename _lhsInt con_
                                 >#< ppConFields (dataRecords _lhsIoptions) _childrenIdatatype
                                 {-# LINE 2208 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule229 #-}
   {-# LINE 187 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule229 = \ constraints_ ->
                                 {-# LINE 187 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppClasses (classConstrsToDocs constraints_)
                                 {-# LINE 2214 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule230 #-}
   {-# LINE 188 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule230 = \ params_ ->
                                 {-# LINE 188 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppQuants params_
                                 {-# LINE 2220 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule231 #-}
   {-# LINE 286 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule231 = \  (_ :: ()) ->
                                              {-# LINE 286 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                              1
                                              {-# LINE 2226 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule232 #-}
   {-# LINE 291 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule232 = \ ((_childrenIargnamesw) :: [PP_Doc]) ((_childrenIargpats) ::  [PP_Doc] ) ((_lhsInt) :: NontermIdent) ((_lhsIrename) :: Bool) con_ ->
                               {-# LINE 291 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               "sem_" >|< _lhsInt >#< "(" >#< conname _lhsIrename _lhsInt con_ >#< ppSpaced _childrenIargpats >#< ")"
                               >#< "=" >#< "sem_" >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                               {-# LINE 2233 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule233 #-}
   {-# LINE 521 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule233 = \ _semFunBndDef ->
                        {-# LINE 521 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        Seq.singleton _semFunBndDef
                        {-# LINE 2239 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule234 #-}
   {-# LINE 522 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule234 = \ _semFunBndTp ->
                        {-# LINE 522 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        Seq.singleton _semFunBndTp
                        {-# LINE 2245 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule235 #-}
   {-# LINE 523 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule235 = \ _semFunBndNm _semname ->
                        {-# LINE 523 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 2251 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule236 #-}
   {-# LINE 524 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule236 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 524 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        _semFunBndNm     >#< "::" >#< _sem_tp
                        {-# LINE 2257 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule237 #-}
   {-# LINE 525 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule237 = \ ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 525 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        lateSemConLabel _lhsInt con_
                        {-# LINE 2263 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule238 #-}
   {-# LINE 583 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule238 = \ ((_lhsInt) :: NontermIdent) ->
                                 {-# LINE 583 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 "T_" >|< _lhsInt
                                 {-# LINE 2269 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule239 #-}
   {-# LINE 584 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule239 = \ ((_lhsIparams) :: [Identifier]) ->
                                 {-# LINE 584 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppSpaced _lhsIparams
                                 {-# LINE 2275 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule240 #-}
   {-# LINE 585 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule240 = \ ((_childrenIusedArgs) :: Set String) ((_rulesIusedArgs) :: Set String) ((_visitsIusedArgs) :: Set String) ->
                                 {-# LINE 585 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _childrenIusedArgs `Set.union` _visitsIusedArgs `Set.union` _rulesIusedArgs
                                 {-# LINE 2281 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule241 #-}
   {-# LINE 588 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule241 = \ ((_childrenIargpats) ::  [PP_Doc] ) _usedArgs ->
                                 {-# LINE 588 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 map (\x -> let (name,arg) = case show x of
                                                         ""       -> ("", empty)
                                                         '!':name -> ("arg_" ++ name, "!arg_" >|< name)
                                                         name     -> ("arg_" ++ name, "arg_"  >|< name)
                                            in  if null name || name `Set.member` _usedArgs
                                                then arg
                                                else text "_") _childrenIargpats
                                 {-# LINE 2293 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule242 #-}
   {-# LINE 595 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule242 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                 {-# LINE 595 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 "sem_" ++ show _lhsInt ++ "_" ++ show con_
                                 {-# LINE 2299 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule243 #-}
   {-# LINE 596 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule243 = \ ((_childrenIargtps) ::  [PP_Doc] ) _classPP2 _quantPP2 _t_params _t_type ->
                                 {-# LINE 596 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _quantPP2     >#< _classPP2     >#< ppSpaced _childrenIargtps >#< _t_type     >#< _t_params
                                 {-# LINE 2305 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule244 #-}
   {-# LINE 597 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule244 = \ ((_lhsIclassCtxs) :: ClassContext) constraints_ ->
                                 {-# LINE 597 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppClasses (classCtxsToDocs _lhsIclassCtxs ++ classConstrsToDocs constraints_)
                                 {-# LINE 2311 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule245 #-}
   {-# LINE 598 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule245 = \ ((_lhsIparams) :: [Identifier]) params_ ->
                                 {-# LINE 598 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 ppQuants (_lhsIparams ++ params_)
                                 {-# LINE 2317 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule246 #-}
   {-# LINE 599 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule246 = \ _args ((_lhsIinitial) :: StateIdentifier) _mbInitializer _mkSemBody _outerlet _scc _semInlinePragma _sem_tp _semname _t_type ->
                                 {-# LINE 599 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _semInlinePragma
                                 >-< _semname     >#< "::" >#< _sem_tp
                                 >-< _mkSemBody     (_semname     >#< ppSpaced _args     >#< "=" >#< _scc     >#< _t_type    )
                                                    _mbInitializer     _outerlet     ("return" >#< "st" >|< _lhsIinitial)
                                 {-# LINE 2326 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule247 #-}
   {-# LINE 603 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule247 = \  (_ :: ()) ->
                                  {-# LINE 603 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  \prefix mbInit outerlet ret ->
                                    case mbInit of
                                      Nothing -> prefix >#< pp_parens ret >#< "where"
                                                 >-< indent 3 outerlet
                                      Just m  -> prefix >#< "(" >#< "do"
                                                 >-< indent 1 (
                                                       m
                                                       >-< "let"
                                                       >-< indent 2 outerlet
                                                       >-< ret )
                                                 >-< indent 1 ")"
                                  {-# LINE 2342 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule248 #-}
   {-# LINE 615 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule248 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 615 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        if parallelInvoke _lhsIoptions
                                        then (Nothing :: Maybe PP_Doc)
                                        else Nothing
                                        {-# LINE 2350 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule249 #-}
   {-# LINE 621 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule249 = \ ((_lhsIoptions) :: Options) _semname ->
                                        {-# LINE 621 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        if genCostCentres _lhsIoptions
                                        then ppCostCentre _semname
                                        else empty
                                        {-# LINE 2358 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule250 #-}
   {-# LINE 624 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule250 = \ ((_lhsIoptions) :: Options) _semname ->
                                        {-# LINE 624 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        if noInlinePragmas _lhsIoptions
                                        then empty
                                        else ppNoInline _semname
                                        {-# LINE 2366 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule251 #-}
   {-# LINE 627 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule251 = \ ((_rulesIsem_rules) :: PP_Doc) _statefns ->
                                 {-# LINE 627 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 vlist _statefns     >-< _rulesIsem_rules
                                 {-# LINE 2372 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule252 #-}
   {-# LINE 628 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule252 = \ _genstfn ((_lhsIallstates) :: Set StateIdentifier) ->
                                 {-# LINE 628 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 map _genstfn     $ Set.toList _lhsIallstates
                                 {-# LINE 2378 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule253 #-}
   {-# LINE 629 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule253 = \ _addbang ((_lhsIinitial) :: StateIdentifier) ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) _stargs _stks _stvs ->
                                 {-# LINE 629 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \st -> let nextVisitInfo = Map.findWithDefault ManyVis st _lhsInextVisits
                                            prevVisitInfo = Map.findWithDefault ManyVis st _lhsIprevVisits
                                            stNm = "st" >|< st
                                            lhs  = pragma >-< bang stNm >#< "=" >#<
                                                   (
                                                     if st == _lhsIinitial
                                                     then empty
                                                     else "\\" >#< _stargs     st >#< "->"
                                                   )
                                            pragma = if noInlinePragmas _lhsIoptions
                                                     then empty
                                                     else if helpInlining _lhsIoptions
                                                          then case prevVisitInfo of
                                                                 ManyVis  -> ppNoInline stNm
                                                                 OneVis _ -> if aggressiveInlinePragmas _lhsIoptions
                                                                             then ppInline stNm
                                                                             else ppInlinable stNm
                                                                 NoneVis  -> if st /= _lhsIinitial
                                                                             then error ("State " ++ show st ++ " is not reachable from the initial state.")
                                                                             else if aggressiveInlinePragmas _lhsIoptions
                                                                                  then ppInline stNm
                                                                                  else ppInlinable stNm
                                                          else ppNoInline stNm
                                            cCon = "C_" >|< _lhsInt >|< "_s" >|< st
                                            bang | st == _lhsIinitial = _addbang
                                                 | otherwise          = id
                                        in case nextVisitInfo of
                                             NoneVis    ->
                                                           if st == _lhsIinitial
                                                           then lhs >#< cCon
                                                           else empty
                                             OneVis vId -> mklet lhs (_stvs     st False) (cCon >#< "v" >|< vId)
                                             ManyVis    -> mklet lhs (_stks     st >-< _stvs     st True) (cCon >#< "k" >|< st)
                                 {-# LINE 2416 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule254 #-}
   {-# LINE 671 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule254 = \ _addbang _childTypes _lazyIntras ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIoptions) :: Options) _localAttrTypes ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                                 {-# LINE 671 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \st -> let attrs = maybe Map.empty id $ Map.lookup st _visitsIintramap
                                        in ppSpaced [ let match | str `Set.member` _lazyIntras     = pp str
                                                                | otherwise                        = _addbang     (pp str)
                                                      in case mbAttr of
                                                           Just (AttrSyn child nm) | child == _LOC && not (noPerStateTypeSigs _lhsIoptions) ->
                                                             case Map.lookup nm _localAttrTypes     of
                                                               Just tp -> pp_parens (pp_parens match >#< "::" >#< ppTp tp)
                                                               Nothing -> match
                                                           Just attr | not (noPerStateTypeSigs _lhsIoptions) ->
                                                             case lookupAttrType attr _lhsIallInhmap _lhsIallSynmap _childTypes     of
                                                               Just tpDoc -> pp_parens (pp_parens match >#< "::" >#< tpDoc)
                                                               Nothing    -> match
                                                           _ -> match
                                                   | (str,mbAttr) <- Map.assocs attrs
                                                   ] >#< dummyPat _lhsIoptions (Map.null attrs)
                                 {-# LINE 2436 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule255 #-}
   {-# LINE 687 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule255 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _stvisits _t_params ->
                                 {-# LINE 687 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \st -> if null (_stvisits     st)
                                        then empty
                                        else ( if not (noInlinePragmas _lhsIoptions) && helpInlining _lhsIoptions
                                               then ppNoInline ("k" >|< st)
                                               else empty
                                             )
                                             >-< "k" >|< st >#< "::" >#< "K_" >|< _lhsInt >|< "_s" >|< st >#< _t_params     >#< "t" >#< "->" >#< "t"
                                             >-< vlist (map (\(v,f,t) -> "k" >|< st >#< "K_" >|< _lhsInt >|< "_v" >|< v >#< "="
                                                                    >#< "v" >|< v) $ _stvisits     st)
                                 {-# LINE 2450 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule256 #-}
   {-# LINE 696 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule256 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
                                 {-# LINE 696 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \st -> filter (\(v,f,t) -> f == st) _visitsIallvisits
                                 {-# LINE 2456 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule257 #-}
   {-# LINE 697 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule257 = \ ((_visitsIsem_visit) ::  [(StateIdentifier,Bool -> PP_Doc)] ) ->
                                 {-# LINE 697 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 \st inlinePragma -> vlist [ppf inlinePragma | (f,ppf) <- _visitsIsem_visit, f == st]
                                 {-# LINE 2462 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule258 #-}
   {-# LINE 698 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule258 = \ ((_rulesImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                                  {-# LINE 698 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesImrules
                                  {-# LINE 2468 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule259 #-}
   {-# LINE 913 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule259 = \ ((_childrenIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                       {-# LINE 913 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                       _childrenIchildintros
                                       {-# LINE 2474 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule260 #-}
   {-# LINE 1267 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule260 = \ ((_visitsIruleUsage) :: Map Identifier Int) ->
                                                   {-# LINE 1267 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                   _visitsIruleUsage
                                                   {-# LINE 2480 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule261 #-}
   {-# LINE 1282 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule261 = \ ((_visitsIruleKinds) :: Map Identifier (Set VisitKind)) ->
                      {-# LINE 1282 "./src-ag/ExecutionPlan2Hs.ag" #-}
                      _visitsIruleKinds
                      {-# LINE 2486 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule262 #-}
   {-# LINE 1311 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule262 = \ ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                          {-# LINE 1311 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          _visitsIintramap
                          {-# LINE 2492 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule263 #-}
   {-# LINE 1312 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule263 = \ ((_childrenIterminaldefs) :: Set String) ->
                          {-# LINE 1312 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          _childrenIterminaldefs
                          {-# LINE 2498 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule264 #-}
   {-# LINE 1336 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule264 = \ ((_rulesIruledefs) :: Map Identifier (Set String)) ->
                                    {-# LINE 1336 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    _rulesIruledefs
                                    {-# LINE 2504 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule265 #-}
   {-# LINE 1337 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule265 = \ ((_rulesIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                    {-# LINE 1337 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    _rulesIruleuses
                                    {-# LINE 2510 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule266 #-}
   {-# LINE 1391 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule266 = \ ((_visitsIlazyIntras) :: Set String) ->
                     {-# LINE 1391 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     _visitsIlazyIntras
                     {-# LINE 2516 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule267 #-}
   {-# LINE 1488 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule267 = \ _moduleName ->
                                   {-# LINE 1488 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   [pp $ "import " ++ _moduleName    ]
                                   {-# LINE 2522 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule268 #-}
   {-# LINE 1489 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule268 = \ ((_lhsImainName) :: String) _suffix ->
                                   {-# LINE 1489 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName ++ _suffix
                                   {-# LINE 2528 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule269 #-}
   {-# LINE 1490 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule269 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                   {-# LINE 1490 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   "_" ++ show _lhsInt ++ "_" ++ show con_
                                   {-# LINE 2534 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule270 #-}
   {-# LINE 1491 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule270 = \ ((_lhsImainFile) :: String) _suffix ->
                                   {-# LINE 1491 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ _suffix    )
                                   {-# LINE 2540 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule271 #-}
   {-# LINE 1492 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule271 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 1492 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        if parallelInvoke _lhsIoptions
                                        then pp "import qualified System.IO.Unsafe(unsafePerformIO)"
                                             >-< pp "import System.IO(IO)"
                                             >-< pp "import Control.Concurrent(newEmptyMVar,forkIO,putMVar,takeMVar)"
                                        else pp "import Control.Monad.Identity"
                                        {-# LINE 2550 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule272 #-}
   {-# LINE 1497 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule272 = \ ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptions) :: Options) ((_lhsIpragmaBlocks) :: String) _outputfile _ppMonadImports _sem_prod _semname _suffix ->
                                   {-# LINE 1497 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   writeModule _outputfile
                                       [ warrenFlagsPP _lhsIoptions
                                       , pp $ _lhsIpragmaBlocks
                                       , pp $ _lhsImoduleHeader _lhsImainName _suffix     _semname     True
                                       , _lhsIimportBlocks
                                       , _ppMonadImports
                                       , ( if tupleAsDummyToken _lhsIoptions
                                              then empty
                                              else pp "import GHC.Prim"
                                        )
                                       , pp $ "import " ++ _lhsImainName ++ "_common"
                                       , _sem_prod
                                       ]
                                   {-# LINE 2568 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule273 #-}
   {-# LINE 1538 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule273 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1538 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 2574 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule274 #-}
   {-# LINE 1588 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule274 = \ ((_childrenIchildTypes) :: Map Identifier Type) ((_lhsIntType) :: Type) ->
                     {-# LINE 1588 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                     {-# LINE 2580 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule275 #-}
   {-# LINE 1605 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule275 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) con_ ->
                           {-# LINE 1605 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                           {-# LINE 2586 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule276 #-}
   rule276 = \ ((_visitsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _visitsIchildvisit
   {-# INLINE rule277 #-}
   rule277 = \ ((_rulesIerrors) :: Seq Error) ((_visitsIerrors) :: Seq Error) ->
     _rulesIerrors Seq.>< _visitsIerrors
   {-# INLINE rule278 #-}
   rule278 = \ ((_visitsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _visitsIfromToStates
   {-# INLINE rule279 #-}
   rule279 = \ ((_visitsIt_visits) :: PP_Doc) ->
     _visitsIt_visits
   {-# INLINE rule280 #-}
   rule280 = \ ((_visitsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _visitsIvisitKinds
   {-# INLINE rule281 #-}
   rule281 = \ ((_visitsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisitdefs
   {-# INLINE rule282 #-}
   rule282 = \ ((_visitsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisituses
   {-# INLINE rule283 #-}
   rule283 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
     _visitsIallvisits
   {-# INLINE rule284 #-}
   rule284 = \ _sem_prod ->
     _sem_prod
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule287 #-}
   rule287 = \ _childTypes ->
     _childTypes
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule290 #-}
   rule290 = \ _lazyIntras ->
     _lazyIntras
   {-# INLINE rule291 #-}
   rule291 = \ _localAttrTypes ->
     _localAttrTypes
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule309 #-}
   rule309 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule317 #-}
   rule317 = \ _childTypes ->
     _childTypes
   {-# INLINE rule318 #-}
   rule318 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap

-- EProductions ------------------------------------------------
-- wrapper
data Inh_EProductions  = Inh_EProductions { allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProductions :: (Map NontermIdent Attributes), allInitStates_Inh_EProductions :: (Map NontermIdent Int), allSynmap_Inh_EProductions :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allstates_Inh_EProductions :: (Set StateIdentifier), avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProductions :: (ClassContext), importBlocks_Inh_EProductions :: (PP_Doc), inhmap_Inh_EProductions :: (Attributes), initial_Inh_EProductions :: (StateIdentifier), localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProductions :: (String), mainName_Inh_EProductions :: (String), moduleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), nt_Inh_EProductions :: (NontermIdent), ntType_Inh_EProductions :: (Type), options_Inh_EProductions :: (Options), params_Inh_EProductions :: ([Identifier]), pragmaBlocks_Inh_EProductions :: (String), prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), rename_Inh_EProductions :: (Bool), synmap_Inh_EProductions :: (Attributes), textBlocks_Inh_EProductions :: (PP_Doc) }
data Syn_EProductions  = Syn_EProductions { allvisits_Syn_EProductions :: ([VisitStateState]), childvisit_Syn_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), count_Syn_EProductions :: (Int), datatype_Syn_EProductions :: ([PP_Doc]), errors_Syn_EProductions :: (Seq Error), fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_EProductions :: (IO ()), imports_Syn_EProductions :: ([PP_Doc]), semFunBndDefs_Syn_EProductions :: (Seq PP_Doc), semFunBndTps_Syn_EProductions :: (Seq PP_Doc), sem_nt_Syn_EProductions :: (PP_Doc), sem_prod_Syn_EProductions :: (PP_Doc), t_visits_Syn_EProductions :: (PP_Doc), visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProductions #-}
wrap_EProductions :: T_EProductions  -> Inh_EProductions  -> (Syn_EProductions )
wrap_EProductions (T_EProductions act) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProductions_s17 sem arg)
        return (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_EProductions_vIn16  = T_EProductions_vIn16 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) (PP_Doc) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) (PP_Doc)
data T_EProductions_vOut16  = T_EProductions_vOut16 ([VisitStateState]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Int) ([PP_Doc]) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProductions_Cons #-}
sem_EProductions_Cons :: T_EProduction  -> T_EProductions  -> T_EProductions 
sem_EProductions_Cons arg_hd_ arg_tl_ = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_EProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_tl_))
         (T_EProduction_vOut13 _hdIallvisits _hdIchildvisit _hdIcount _hdIdatatype _hdIerrors _hdIfromToStates _hdIgenProdIO _hdIimports _hdIsemFunBndDefs _hdIsemFunBndTps _hdIsem_nt _hdIsem_prod _hdIt_visits _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_EProduction_s14 _hdX14 (T_EProduction_vIn13 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOclassCtxs _hdOimportBlocks _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOpragmaBlocks _hdOprevVisits _hdOrename _hdOsynmap _hdOtextBlocks)
         (T_EProductions_vOut16 _tlIallvisits _tlIchildvisit _tlIcount _tlIdatatype _tlIerrors _tlIfromToStates _tlIgenProdIO _tlIimports _tlIsemFunBndDefs _tlIsemFunBndTps _tlIsem_nt _tlIsem_prod _tlIt_visits _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_EProductions_s17 _tlX17 (T_EProductions_vIn16 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOclassCtxs _tlOimportBlocks _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOpragmaBlocks _tlOprevVisits _tlOrename _tlOsynmap _tlOtextBlocks)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule325 _hdIallvisits
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule326 _hdIt_visits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule327 _hdIchildvisit _tlIchildvisit
         _lhsOcount :: Int
         _lhsOcount = rule328 _hdIcount _tlIcount
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule329 _hdIdatatype _tlIdatatype
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule330 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule331 _hdIfromToStates _tlIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule332 _hdIgenProdIO _tlIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule333 _hdIimports _tlIimports
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule334 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule335 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule336 _hdIsem_nt _tlIsem_nt
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule337 _hdIsem_prod _tlIsem_prod
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule338 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule339 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule340 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule341 _lhsIallFromToStates
         _hdOallInhmap = rule342 _lhsIallInhmap
         _hdOallInitStates = rule343 _lhsIallInitStates
         _hdOallSynmap = rule344 _lhsIallSynmap
         _hdOallVisitKinds = rule345 _lhsIallVisitKinds
         _hdOallchildvisit = rule346 _lhsIallchildvisit
         _hdOallstates = rule347 _lhsIallstates
         _hdOavisitdefs = rule348 _lhsIavisitdefs
         _hdOavisituses = rule349 _lhsIavisituses
         _hdOclassCtxs = rule350 _lhsIclassCtxs
         _hdOimportBlocks = rule351 _lhsIimportBlocks
         _hdOinhmap = rule352 _lhsIinhmap
         _hdOinitial = rule353 _lhsIinitial
         _hdOlocalAttrTypes = rule354 _lhsIlocalAttrTypes
         _hdOmainFile = rule355 _lhsImainFile
         _hdOmainName = rule356 _lhsImainName
         _hdOmoduleHeader = rule357 _lhsImoduleHeader
         _hdOnextVisits = rule358 _lhsInextVisits
         _hdOnt = rule359 _lhsInt
         _hdOntType = rule360 _lhsIntType
         _hdOoptions = rule361 _lhsIoptions
         _hdOparams = rule362 _lhsIparams
         _hdOpragmaBlocks = rule363 _lhsIpragmaBlocks
         _hdOprevVisits = rule364 _lhsIprevVisits
         _hdOrename = rule365 _lhsIrename
         _hdOsynmap = rule366 _lhsIsynmap
         _hdOtextBlocks = rule367 _lhsItextBlocks
         _tlOallFromToStates = rule368 _lhsIallFromToStates
         _tlOallInhmap = rule369 _lhsIallInhmap
         _tlOallInitStates = rule370 _lhsIallInitStates
         _tlOallSynmap = rule371 _lhsIallSynmap
         _tlOallVisitKinds = rule372 _lhsIallVisitKinds
         _tlOallchildvisit = rule373 _lhsIallchildvisit
         _tlOallstates = rule374 _lhsIallstates
         _tlOavisitdefs = rule375 _lhsIavisitdefs
         _tlOavisituses = rule376 _lhsIavisituses
         _tlOclassCtxs = rule377 _lhsIclassCtxs
         _tlOimportBlocks = rule378 _lhsIimportBlocks
         _tlOinhmap = rule379 _lhsIinhmap
         _tlOinitial = rule380 _lhsIinitial
         _tlOlocalAttrTypes = rule381 _lhsIlocalAttrTypes
         _tlOmainFile = rule382 _lhsImainFile
         _tlOmainName = rule383 _lhsImainName
         _tlOmoduleHeader = rule384 _lhsImoduleHeader
         _tlOnextVisits = rule385 _lhsInextVisits
         _tlOnt = rule386 _lhsInt
         _tlOntType = rule387 _lhsIntType
         _tlOoptions = rule388 _lhsIoptions
         _tlOparams = rule389 _lhsIparams
         _tlOpragmaBlocks = rule390 _lhsIpragmaBlocks
         _tlOprevVisits = rule391 _lhsIprevVisits
         _tlOrename = rule392 _lhsIrename
         _tlOsynmap = rule393 _lhsIsynmap
         _tlOtextBlocks = rule394 _lhsItextBlocks
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule325 #-}
   {-# LINE 341 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule325 = \ ((_hdIallvisits) :: [VisitStateState]) ->
                           {-# LINE 341 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           _hdIallvisits
                           {-# LINE 2870 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 394 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule326 = \ ((_hdIt_visits) :: PP_Doc) ->
                          {-# LINE 394 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          _hdIt_visits
                          {-# LINE 2876 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule327 #-}
   rule327 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule328 #-}
   rule328 = \ ((_hdIcount) :: Int) ((_tlIcount) :: Int) ->
     _hdIcount + _tlIcount
   {-# INLINE rule329 #-}
   rule329 = \ ((_hdIdatatype) :: PP_Doc) ((_tlIdatatype) :: [PP_Doc]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule330 #-}
   rule330 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule331 #-}
   rule331 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule332 #-}
   rule332 = \ ((_hdIgenProdIO) :: IO ()) ((_tlIgenProdIO) :: IO ()) ->
     _hdIgenProdIO >> _tlIgenProdIO
   {-# INLINE rule333 #-}
   rule333 = \ ((_hdIimports) :: [PP_Doc]) ((_tlIimports) :: [PP_Doc]) ->
     _hdIimports ++ _tlIimports
   {-# INLINE rule334 #-}
   rule334 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule335 #-}
   rule335 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule336 #-}
   rule336 = \ ((_hdIsem_nt) :: PP_Doc) ((_tlIsem_nt) :: PP_Doc) ->
     _hdIsem_nt >-< _tlIsem_nt
   {-# INLINE rule337 #-}
   rule337 = \ ((_hdIsem_prod) :: PP_Doc) ((_tlIsem_prod) :: PP_Doc) ->
     _hdIsem_prod >-< _tlIsem_prod
   {-# INLINE rule338 #-}
   rule338 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule339 #-}
   rule339 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule340 #-}
   rule340 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule341 #-}
   rule341 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule342 #-}
   rule342 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule343 #-}
   rule343 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule344 #-}
   rule344 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule345 #-}
   rule345 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule346 #-}
   rule346 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule347 #-}
   rule347 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule348 #-}
   rule348 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule349 #-}
   rule349 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule350 #-}
   rule350 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule351 #-}
   rule351 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule352 #-}
   rule352 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule353 #-}
   rule353 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule357 #-}
   rule357 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule360 #-}
   rule360 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule366 #-}
   rule366 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule367 #-}
   rule367 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule368 #-}
   rule368 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
{-# NOINLINE sem_EProductions_Nil #-}
sem_EProductions_Nil ::  T_EProductions 
sem_EProductions_Nil  = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule395  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule396  ()
         _lhsOcount :: Int
         _lhsOcount = rule397  ()
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule398  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule399  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule400  ()
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule401  ()
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule402  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule403  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule404  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule405  ()
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule406  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule407  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule408  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule409  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule410  ()
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule395 #-}
   {-# LINE 342 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule395 = \  (_ :: ()) ->
                           {-# LINE 342 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           error "Every nonterminal should have at least 1 production"
                           {-# LINE 3128 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule396 #-}
   rule396 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule397 #-}
   rule397 = \  (_ :: ()) ->
     0
   {-# INLINE rule398 #-}
   rule398 = \  (_ :: ()) ->
     []
   {-# INLINE rule399 #-}
   rule399 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule400 #-}
   rule400 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule401 #-}
   rule401 = \  (_ :: ()) ->
     return ()
   {-# INLINE rule402 #-}
   rule402 = \  (_ :: ()) ->
     []
   {-# INLINE rule403 #-}
   rule403 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule404 #-}
   rule404 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule405 #-}
   rule405 = \  (_ :: ()) ->
     empty
   {-# INLINE rule406 #-}
   rule406 = \  (_ :: ()) ->
     empty
   {-# INLINE rule407 #-}
   rule407 = \  (_ :: ()) ->
     empty
   {-# INLINE rule408 #-}
   rule408 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule409 #-}
   rule409 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule410 #-}
   rule410 = \  (_ :: ()) ->
     Map.empty

-- ERule -------------------------------------------------------
-- wrapper
data Inh_ERule  = Inh_ERule { allInhmap_Inh_ERule :: (Map NontermIdent Attributes), allSynmap_Inh_ERule :: (Map NontermIdent Attributes), childTypes_Inh_ERule :: (Map Identifier Type), con_Inh_ERule :: (ConstructorIdent), importBlocks_Inh_ERule :: (PP_Doc), inhmap_Inh_ERule :: (Attributes), lazyIntras_Inh_ERule :: (Set String), localAttrTypes_Inh_ERule :: (Map Identifier Type), mainFile_Inh_ERule :: (String), mainName_Inh_ERule :: (String), moduleHeader_Inh_ERule :: (String -> String -> String -> Bool -> String), nt_Inh_ERule :: (NontermIdent), options_Inh_ERule :: (Options), pragmaBlocks_Inh_ERule :: (String), ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)), synmap_Inh_ERule :: (Attributes), textBlocks_Inh_ERule :: (PP_Doc), usageInfo_Inh_ERule :: (Map Identifier Int) }
data Syn_ERule  = Syn_ERule { errors_Syn_ERule :: (Seq Error), mrules_Syn_ERule :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), ruledefs_Syn_ERule :: (Map Identifier (Set String)), ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERule :: (PP_Doc), usedArgs_Syn_ERule :: (Set String) }
{-# INLINABLE wrap_ERule #-}
wrap_ERule :: T_ERule  -> Inh_ERule  -> (Syn_ERule )
wrap_ERule (T_ERule act) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
        (T_ERule_vOut19 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs) <- return (inv_ERule_s20 sem arg)
        return (Syn_ERule _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs)
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
data T_ERule_vIn19  = T_ERule_vIn19 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (PP_Doc) (Attributes) (Set String) (Map Identifier Type) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (Map Identifier (Set VisitKind)) (Attributes) (PP_Doc) (Map Identifier Int)
data T_ERule_vOut19  = T_ERule_vOut19 (Seq Error) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc) (Set String)
{-# NOINLINE sem_ERule_ERule #-}
sem_ERule_ERule :: (Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Maybe Error) -> T_ERule 
sem_ERule_ERule arg_name_ arg_pattern_ arg_rhs_ _ _ arg_explicit_ arg_pure_ arg_mbError_ = T_ERule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_ERule_v19 
      v19 = \ (T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX29 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut40 _patternIattrTypes _patternIattrs _patternIcopy _patternIisUnderscore _patternIsem_lhs) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap)
         (T_Expression_vOut28 _rhsIattrs _rhsIpos _rhsIsemfunc _rhsItks) = inv_Expression_s29 _rhsX29 (T_Expression_vIn28 )
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule411 _usedArgs_augmented_f1 _usedArgs_augmented_syn
         _usedArgs_augmented_f1 = rule412 _rhsIattrs
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule413 _rulePragma _rulecode _used
         _rulecode = rule414 _endpragma _genpragma _lambda _pragma _rhsIpos _rhsIsemfunc _scc
         _rulePragma = rule415 _lhsIoptions _used arg_explicit_ arg_name_
         _scc = rule416 _lhsIcon _lhsInt _lhsIoptions _rhsIpos arg_explicit_ arg_name_ arg_pure_
         _pragma = rule417 _rhsIpos
         _endpragma = rule418 _lhsImainFile
         _genpragma = rule419 _haspos _lhsIoptions arg_explicit_
         _haspos = rule420 _rhsIpos
         _lambda = rule421 _argPats _lhsIoptions _rhsIattrs arg_name_
         _argPats = rule422 _addbang1 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIlazyIntras _lhsIlocalAttrTypes _lhsIoptions _rhsIattrs
         _argExprs = rule423 _rhsIattrs
         _stepcode = rule424 _argExprs _lhsIoptions _patternIattrTypes _patternIsem_lhs _rhsIattrs arg_name_ arg_pure_
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule425 _stepcode arg_name_
         _used = rule426 _lhsIusageInfo arg_name_
         _kinds = rule427 _lhsIruleKinds arg_name_
         _anyLazyKind = rule428 _kinds
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule429 _patternIattrs arg_name_
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule430 _rhsIattrs arg_name_
         _addbang = rule431 _lhsIoptions
         _addbang1 = rule432 _addbang _anyLazyKind
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule433 _used arg_mbError_
         _usedArgs_augmented_syn = rule434  ()
         _patternOallInhmap = rule435 _lhsIallInhmap
         _patternOallSynmap = rule436 _lhsIallSynmap
         _patternOanyLazyKind = rule437 _anyLazyKind
         _patternOinhmap = rule438 _lhsIinhmap
         _patternOlocalAttrTypes = rule439 _lhsIlocalAttrTypes
         _patternOoptions = rule440 _lhsIoptions
         _patternOsynmap = rule441 _lhsIsynmap
         __result_ = T_ERule_vOut19 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERule_s20 v19
   {-# INLINE rule411 #-}
   rule411 = \ _usedArgs_augmented_f1 _usedArgs_augmented_syn ->
     foldr ($) _usedArgs_augmented_syn [_usedArgs_augmented_f1]
   {-# INLINE rule412 #-}
   rule412 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                        Set.union $ Map.keysSet $ Map.mapKeys (\a -> "arg_" ++ a) $ Map.filter isNothing _rhsIattrs
   {-# INLINE rule413 #-}
   {-# LINE 982 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule413 = \ _rulePragma _rulecode _used ->
                          {-# LINE 982 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          if _used     == 0
                          then empty
                          else _rulePragma     >-< _rulecode
                          {-# LINE 3269 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule414 #-}
   {-# LINE 985 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule414 = \ _endpragma _genpragma _lambda _pragma ((_rhsIpos) :: Pos) ((_rhsIsemfunc) :: PP_Doc) _scc ->
                          {-# LINE 985 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          ( if _genpragma
                            then _pragma
                            else empty
                          )
                          >-< _lambda     >#< _scc
                          >-< indent ((column _rhsIpos - 2) `max` 2)
                                ( if _genpragma
                                  then _pragma     >-< _rhsIsemfunc >-< _endpragma
                                  else _rhsIsemfunc
                                )
                          {-# LINE 3284 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule415 #-}
   {-# LINE 997 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule415 = \ ((_lhsIoptions) :: Options) _used explicit_ name_ ->
                           {-# LINE 997 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           ( let reallyInlineStr   = "INLINE"
                                 reallyNoInlineStr = "NOINLINE"
                             in  if noInlinePragmas _lhsIoptions
                                 then empty
                                 else if _used     == 1
                                      then ppPragmaBinding reallyInlineStr name_
                                      else if helpInlining _lhsIoptions
                                           then if not explicit_ && _used     <= reallyOftenUsedThreshold
                                                then ppPragmaBinding "INLINE[1]" name_
                                                else if _used     > ruleInlineThresholdSoft && explicit_
                                                     then if _used     > ruleInlineThresholdHard
                                                          then ppPragmaBinding reallyNoInlineStr name_
                                                          else if aggressiveInlinePragmas _lhsIoptions
                                                               then ppPragmaBinding "NOINLINE[2]" name_
                                                               else ppNoInline name_
                                                     else if aggressiveInlinePragmas _lhsIoptions
                                                          then ppPragmaBinding "NOINLINE[1]" name_
                                                          else ppNoInline name_
                                           else if not explicit_ || _used     <= ruleInlineThresholdSoft
                                                then ppPragmaBinding "NOINLINE[1]" name_
                                                else ppNoInline name_
                               )
                           {-# LINE 3311 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule416 #-}
   {-# LINE 1019 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule416 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_rhsIpos) :: Pos) explicit_ name_ pure_ ->
                           {-# LINE 1019 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           if genCostCentres _lhsIoptions && explicit_ && pure_ && not (noPerRuleCostCentres _lhsIoptions)
                           then ppCostCentre (name_ >|< "_" >|< line _rhsIpos >|< "_" >|< _lhsInt >|< "_" >|< _lhsIcon)
                           else empty
                           {-# LINE 3319 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule417 #-}
   {-# LINE 1022 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule417 = \ ((_rhsIpos) :: Pos) ->
                           {-# LINE 1022 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           "{-# LINE" >#< show (line _rhsIpos) >#< show (file _rhsIpos) >#< "#-}"
                           {-# LINE 3325 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule418 #-}
   {-# LINE 1023 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule418 = \ ((_lhsImainFile) :: String) ->
                           {-# LINE 1023 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           ppWithLineNr (\ln -> "{-# LINE " ++ show (ln+1) ++ " " ++ show _lhsImainFile ++ "#-}")
                           {-# LINE 3331 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule419 #-}
   {-# LINE 1024 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule419 = \ _haspos ((_lhsIoptions) :: Options) explicit_ ->
                           {-# LINE 1024 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           genLinePragmas _lhsIoptions && explicit_ && _haspos
                           {-# LINE 3337 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule420 #-}
   {-# LINE 1025 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule420 = \ ((_rhsIpos) :: Pos) ->
                           {-# LINE 1025 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           line _rhsIpos > 0 && column _rhsIpos >= 0 && not (null (file _rhsIpos))
                           {-# LINE 3343 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule421 #-}
   {-# LINE 1034 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule421 = \ _argPats ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                           {-# LINE 1034 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           name_ >#< "=" >#< "\\" >#< _argPats     >#< dummyPat _lhsIoptions (Map.null _rhsIattrs) >#< "->"
                           {-# LINE 3349 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule422 #-}
   {-# LINE 1036 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule422 = \ _addbang1 ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIchildTypes) :: Map Identifier Type) ((_lhsIlazyIntras) :: Set String) ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                           {-# LINE 1036 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           ppSpaced [ let match | str `Set.member` _lhsIlazyIntras = pp str
                                                | otherwise                        = _addbang1     (pp str)
                                      in case mbAttr of
                                           Just (AttrSyn child nm) | child == _LOC && not (noPerStateTypeSigs _lhsIoptions) ->
                                             case Map.lookup nm _lhsIlocalAttrTypes of
                                               Just tp -> pp_parens (pp_parens match >#< "::" >#< ppTp tp)
                                               Nothing -> match
                                           Just attr | not (noPerRuleTypeSigs _lhsIoptions) ->
                                             case lookupAttrType attr _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes of
                                               Just tpDoc -> pp_parens (pp_parens match >#< "::" >#< tpDoc)
                                               Nothing    -> match
                                           _ -> match
                                    | (str,mbAttr) <- Map.assocs _rhsIattrs
                                    ]
                           {-# LINE 3368 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule423 #-}
   {-# LINE 1050 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule423 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                           {-# LINE 1050 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           ppSpaced [ case mbAttr of
                                         Nothing -> "arg_" >|< str
                                         _       -> text str
                                    | (str,mbAttr) <- Map.assocs _rhsIattrs
                                    ]
                           {-# LINE 3378 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule424 #-}
   {-# LINE 1055 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule424 = \ _argExprs ((_lhsIoptions) :: Options) ((_patternIattrTypes) :: PP_Doc) ((_patternIsem_lhs) ::  PP_Doc ) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ pure_ ->
                           {-# LINE 1055 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           \kind fmtMode -> if kind `compatibleRule` pure_
                                            then Right $ let oper | pure_     = "="
                                                                  | otherwise = "<-"
                                                             decl = _patternIsem_lhs >#< oper >#< name_ >#< _argExprs     >#< dummyArg _lhsIoptions (Map.null _rhsIattrs)
                                                             tp   = if pure_ && not (noPerRuleTypeSigs _lhsIoptions)
                                                                    then _patternIattrTypes
                                                                    else empty
                                                         in fmtDecl pure_ fmtMode (tp >-< decl)
                                            else Left $ IncompatibleRuleKind name_ kind
                           {-# LINE 3392 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule425 #-}
   {-# LINE 1065 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule425 = \ _stepcode name_ ->
                           {-# LINE 1065 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.singleton name_ _stepcode
                           {-# LINE 3398 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule426 #-}
   {-# LINE 1269 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule426 = \ ((_lhsIusageInfo) :: Map Identifier Int) name_ ->
                                                 {-# LINE 1269 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                 Map.findWithDefault 0 name_ _lhsIusageInfo
                                                 {-# LINE 3404 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule427 #-}
   {-# LINE 1285 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule427 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) name_ ->
                {-# LINE 1285 "./src-ag/ExecutionPlan2Hs.ag" #-}
                Map.findWithDefault Set.empty name_ _lhsIruleKinds
                {-# LINE 3410 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule428 #-}
   {-# LINE 1286 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule428 = \ _kinds ->
                      {-# LINE 1286 "./src-ag/ExecutionPlan2Hs.ag" #-}
                      Set.fold (\k r -> isLazyKind k || r) False _kinds
                      {-# LINE 3416 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule429 #-}
   {-# LINE 1332 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule429 = \ ((_patternIattrs) :: Set String) name_ ->
                           {-# LINE 1332 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.singleton name_ _patternIattrs
                           {-# LINE 3422 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule430 #-}
   {-# LINE 1333 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule430 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                           {-# LINE 1333 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.singleton name_ _rhsIattrs
                           {-# LINE 3428 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule431 #-}
   {-# LINE 1535 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule431 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1535 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 3434 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule432 #-}
   {-# LINE 1546 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule432 = \ _addbang _anyLazyKind ->
                                                     {-# LINE 1546 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     if _anyLazyKind     then id else _addbang
                                                     {-# LINE 3440 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule433 #-}
   {-# LINE 1652 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule433 = \ _used mbError_ ->
                 {-# LINE 1652 "./src-ag/ExecutionPlan2Hs.ag" #-}
                 case mbError_ of
                   Just e | _used     > 0 -> Seq.singleton e
                   _                      -> Seq.empty
                 {-# LINE 3448 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule435 #-}
   rule435 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule436 #-}
   rule436 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule437 #-}
   rule437 = \ _anyLazyKind ->
     _anyLazyKind
   {-# INLINE rule438 #-}
   rule438 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule439 #-}
   rule439 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule440 #-}
   rule440 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule441 #-}
   rule441 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap

-- ERules ------------------------------------------------------
-- wrapper
data Inh_ERules  = Inh_ERules { allInhmap_Inh_ERules :: (Map NontermIdent Attributes), allSynmap_Inh_ERules :: (Map NontermIdent Attributes), childTypes_Inh_ERules :: (Map Identifier Type), con_Inh_ERules :: (ConstructorIdent), importBlocks_Inh_ERules :: (PP_Doc), inhmap_Inh_ERules :: (Attributes), lazyIntras_Inh_ERules :: (Set String), localAttrTypes_Inh_ERules :: (Map Identifier Type), mainFile_Inh_ERules :: (String), mainName_Inh_ERules :: (String), moduleHeader_Inh_ERules :: (String -> String -> String -> Bool -> String), nt_Inh_ERules :: (NontermIdent), options_Inh_ERules :: (Options), pragmaBlocks_Inh_ERules :: (String), ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)), synmap_Inh_ERules :: (Attributes), textBlocks_Inh_ERules :: (PP_Doc), usageInfo_Inh_ERules :: (Map Identifier Int) }
data Syn_ERules  = Syn_ERules { errors_Syn_ERules :: (Seq Error), mrules_Syn_ERules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), ruledefs_Syn_ERules :: (Map Identifier (Set String)), ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERules :: (PP_Doc), usedArgs_Syn_ERules :: (Set String) }
{-# INLINABLE wrap_ERules #-}
wrap_ERules :: T_ERules  -> Inh_ERules  -> (Syn_ERules )
wrap_ERules (T_ERules act) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
        (T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs) <- return (inv_ERules_s23 sem arg)
        return (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs)
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
data T_ERules_vIn22  = T_ERules_vIn22 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (PP_Doc) (Attributes) (Set String) (Map Identifier Type) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (Map Identifier (Set VisitKind)) (Attributes) (PP_Doc) (Map Identifier Int)
data T_ERules_vOut22  = T_ERules_vOut22 (Seq Error) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc) (Set String)
{-# NOINLINE sem_ERules_Cons #-}
sem_ERules_Cons :: T_ERule  -> T_ERules  -> T_ERules 
sem_ERules_Cons arg_hd_ arg_tl_ = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_ERule (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_tl_))
         (T_ERule_vOut19 _hdIerrors _hdImrules _hdIruledefs _hdIruleuses _hdIsem_rules _hdIusedArgs) = inv_ERule_s20 _hdX20 (T_ERule_vIn19 _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOimportBlocks _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOruleKinds _hdOsynmap _hdOtextBlocks _hdOusageInfo)
         (T_ERules_vOut22 _tlIerrors _tlImrules _tlIruledefs _tlIruleuses _tlIsem_rules _tlIusedArgs) = inv_ERules_s23 _tlX23 (T_ERules_vIn22 _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOimportBlocks _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOruleKinds _tlOsynmap _tlOtextBlocks _tlOusageInfo)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule442 _hdIerrors _tlIerrors
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule443 _hdImrules _tlImrules
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule444 _hdIruledefs _tlIruledefs
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule445 _hdIruleuses _tlIruleuses
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule446 _hdIsem_rules _tlIsem_rules
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule447 _hdIusedArgs _tlIusedArgs
         _hdOallInhmap = rule448 _lhsIallInhmap
         _hdOallSynmap = rule449 _lhsIallSynmap
         _hdOchildTypes = rule450 _lhsIchildTypes
         _hdOcon = rule451 _lhsIcon
         _hdOimportBlocks = rule452 _lhsIimportBlocks
         _hdOinhmap = rule453 _lhsIinhmap
         _hdOlazyIntras = rule454 _lhsIlazyIntras
         _hdOlocalAttrTypes = rule455 _lhsIlocalAttrTypes
         _hdOmainFile = rule456 _lhsImainFile
         _hdOmainName = rule457 _lhsImainName
         _hdOmoduleHeader = rule458 _lhsImoduleHeader
         _hdOnt = rule459 _lhsInt
         _hdOoptions = rule460 _lhsIoptions
         _hdOpragmaBlocks = rule461 _lhsIpragmaBlocks
         _hdOruleKinds = rule462 _lhsIruleKinds
         _hdOsynmap = rule463 _lhsIsynmap
         _hdOtextBlocks = rule464 _lhsItextBlocks
         _hdOusageInfo = rule465 _lhsIusageInfo
         _tlOallInhmap = rule466 _lhsIallInhmap
         _tlOallSynmap = rule467 _lhsIallSynmap
         _tlOchildTypes = rule468 _lhsIchildTypes
         _tlOcon = rule469 _lhsIcon
         _tlOimportBlocks = rule470 _lhsIimportBlocks
         _tlOinhmap = rule471 _lhsIinhmap
         _tlOlazyIntras = rule472 _lhsIlazyIntras
         _tlOlocalAttrTypes = rule473 _lhsIlocalAttrTypes
         _tlOmainFile = rule474 _lhsImainFile
         _tlOmainName = rule475 _lhsImainName
         _tlOmoduleHeader = rule476 _lhsImoduleHeader
         _tlOnt = rule477 _lhsInt
         _tlOoptions = rule478 _lhsIoptions
         _tlOpragmaBlocks = rule479 _lhsIpragmaBlocks
         _tlOruleKinds = rule480 _lhsIruleKinds
         _tlOsynmap = rule481 _lhsIsynmap
         _tlOtextBlocks = rule482 _lhsItextBlocks
         _tlOusageInfo = rule483 _lhsIusageInfo
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule442 #-}
   rule442 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule443 #-}
   rule443 = \ ((_hdImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ((_tlImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _hdImrules `Map.union` _tlImrules
   {-# INLINE rule444 #-}
   rule444 = \ ((_hdIruledefs) :: Map Identifier (Set String)) ((_tlIruledefs) :: Map Identifier (Set String)) ->
     _hdIruledefs `uwSetUnion` _tlIruledefs
   {-# INLINE rule445 #-}
   rule445 = \ ((_hdIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ((_tlIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _hdIruleuses `uwMapUnion` _tlIruleuses
   {-# INLINE rule446 #-}
   rule446 = \ ((_hdIsem_rules) :: PP_Doc) ((_tlIsem_rules) :: PP_Doc) ->
     _hdIsem_rules >-< _tlIsem_rules
   {-# INLINE rule447 #-}
   rule447 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule448 #-}
   rule448 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule449 #-}
   rule449 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule450 #-}
   rule450 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule451 #-}
   rule451 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule452 #-}
   rule452 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule453 #-}
   rule453 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule454 #-}
   rule454 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule457 #-}
   rule457 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule473 #-}
   rule473 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
{-# NOINLINE sem_ERules_Nil #-}
sem_ERules_Nil ::  T_ERules 
sem_ERules_Nil  = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule484  ()
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule485  ()
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule486  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule487  ()
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule488  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule489  ()
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule484 #-}
   rule484 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule485 #-}
   rule485 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule486 #-}
   rule486 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule487 #-}
   rule487 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule488 #-}
   rule488 = \  (_ :: ()) ->
     empty
   {-# INLINE rule489 #-}
   rule489 = \  (_ :: ()) ->
     Set.empty

-- ExecutionPlan -----------------------------------------------
-- wrapper
data Inh_ExecutionPlan  = Inh_ExecutionPlan { importBlocks_Inh_ExecutionPlan :: (PP_Doc), inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainBlocksDoc_Inh_ExecutionPlan :: (PP_Doc), mainFile_Inh_ExecutionPlan :: (String), mainName_Inh_ExecutionPlan :: (String), moduleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String), options_Inh_ExecutionPlan :: (Options), pragmaBlocks_Inh_ExecutionPlan :: (String), synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), textBlockMap_Inh_ExecutionPlan :: (Map BlockInfo PP_Doc), textBlocks_Inh_ExecutionPlan :: (PP_Doc) }
data Syn_ExecutionPlan  = Syn_ExecutionPlan { errors_Syn_ExecutionPlan :: (Seq Error), genIO_Syn_ExecutionPlan :: (IO ()), output_Syn_ExecutionPlan :: (PP_Doc) }
{-# INLINABLE wrap_ExecutionPlan #-}
wrap_ExecutionPlan :: T_ExecutionPlan  -> Inh_ExecutionPlan  -> (Syn_ExecutionPlan )
wrap_ExecutionPlan (T_ExecutionPlan act) (Inh_ExecutionPlan _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ExecutionPlan_vIn25 _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks
        (T_ExecutionPlan_vOut25 _lhsOerrors _lhsOgenIO _lhsOoutput) <- return (inv_ExecutionPlan_s26 sem arg)
        return (Syn_ExecutionPlan _lhsOerrors _lhsOgenIO _lhsOoutput)
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
data T_ExecutionPlan_vIn25  = T_ExecutionPlan_vIn25 (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (PP_Doc) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) (Map BlockInfo PP_Doc) (PP_Doc)
data T_ExecutionPlan_vOut25  = T_ExecutionPlan_vOut25 (Seq Error) (IO ()) (PP_Doc)
{-# NOINLINE sem_ExecutionPlan_ExecutionPlan #-}
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals  -> (TypeSyns) -> (Set NontermIdent) -> (Derivings) -> T_ExecutionPlan 
sem_ExecutionPlan_ExecutionPlan arg_nonts_ arg_typeSyns_ arg_wrappers_ arg_derivings_ = T_ExecutionPlan (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_ExecutionPlan_v25 
      v25 = \ (T_ExecutionPlan_vIn25 _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_nonts_))
         (T_ENonterminals_vOut10 _nontsIappendCommon _nontsIappendMain _nontsIchildvisit _nontsIerrors _nontsIfromToStates _nontsIgenProdIO _nontsIimports _nontsIinitStates _nontsIoutput _nontsIsemFunBndDefs _nontsIsemFunBndTps _nontsIvisitKinds _nontsIvisitdefs _nontsIvisituses) = inv_ENonterminals_s11 _nontsX11 (T_ENonterminals_vIn10 _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOderivings _nontsOimportBlocks _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOmoduleHeader _nontsOoptions _nontsOpragmaBlocks _nontsOsynmap _nontsOtextBlocks _nontsOtypeSyns _nontsOwrappers)
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule490 _commonExtra _nontsIoutput _wrappersExtra
         _nontsOwrappers = rule491 arg_wrappers_
         _nontsOtypeSyns = rule492 arg_typeSyns_
         _nontsOderivings = rule493 arg_derivings_
         _wrappersExtra = rule494 _lateSemBndDef _lhsIoptions
         _commonExtra = rule495 _lateSemBndTp _lhsIoptions
         _lateSemBndTp = rule496 _lhsImainName _nontsIsemFunBndTps
         _lateSemBndDef = rule497 _lhsImainName _lhsIoptions _nontsIsemFunBndDefs arg_wrappers_
         _nontsOallchildvisit = rule498 _nontsIchildvisit
         _nontsOavisitdefs = rule499 _nontsIvisitdefs
         _nontsOavisituses = rule500 _nontsIvisituses
         _lhsOgenIO :: IO ()
         _lhsOgenIO = rule501 _genCommonModule _genMainModule _nontsIgenProdIO
         _mainModuleFile = rule502 _lhsImainFile
         _ppMonadImports = rule503 _lhsIoptions
         _genMainModule = rule504 _lhsImainBlocksDoc _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _mainModuleFile _nontsIappendMain _nontsIimports _ppMonadImports _wrappersExtra
         _commonFile = rule505 _lhsImainFile
         _genCommonModule = rule506 _commonExtra _commonFile _lhsIimportBlocks _lhsImainName _lhsImoduleHeader _lhsIpragmaBlocks _lhsItextBlocks _nontsIappendCommon _ppMonadImports
         _nontsOallFromToStates = rule507 _nontsIfromToStates
         _nontsOallVisitKinds = rule508 _nontsIvisitKinds
         _nontsOallInitStates = rule509 _nontsIinitStates
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule510 _nontsIerrors
         _nontsOimportBlocks = rule511 _lhsIimportBlocks
         _nontsOinhmap = rule512 _lhsIinhmap
         _nontsOlocalAttrTypes = rule513 _lhsIlocalAttrTypes
         _nontsOmainFile = rule514 _lhsImainFile
         _nontsOmainName = rule515 _lhsImainName
         _nontsOmoduleHeader = rule516 _lhsImoduleHeader
         _nontsOoptions = rule517 _lhsIoptions
         _nontsOpragmaBlocks = rule518 _lhsIpragmaBlocks
         _nontsOsynmap = rule519 _lhsIsynmap
         _nontsOtextBlocks = rule520 _lhsItextBlocks
         __result_ = T_ExecutionPlan_vOut25 _lhsOerrors _lhsOgenIO _lhsOoutput
         in __result_ )
     in C_ExecutionPlan_s26 v25
   {-# INLINE rule490 #-}
   {-# LINE 87 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule490 = \ _commonExtra ((_nontsIoutput) :: PP_Doc) _wrappersExtra ->
                                 {-# LINE 87 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _nontsIoutput >-< _commonExtra     >-< _wrappersExtra
                                 {-# LINE 3814 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule491 #-}
   {-# LINE 93 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule491 = \ wrappers_ ->
                                     {-# LINE 93 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     wrappers_
                                     {-# LINE 3820 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule492 #-}
   {-# LINE 132 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule492 = \ typeSyns_ ->
                                     {-# LINE 132 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     typeSyns_
                                     {-# LINE 3826 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule493 #-}
   {-# LINE 133 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule493 = \ derivings_ ->
                                      {-# LINE 133 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                      derivings_
                                      {-# LINE 3832 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule494 #-}
   {-# LINE 529 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule494 = \ _lateSemBndDef ((_lhsIoptions) :: Options) ->
                        {-# LINE 529 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndDef
                        else empty
                        {-# LINE 3840 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule495 #-}
   {-# LINE 532 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule495 = \ _lateSemBndTp ((_lhsIoptions) :: Options) ->
                        {-# LINE 532 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndTp
                        else empty
                        {-# LINE 3848 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule496 #-}
   {-# LINE 535 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule496 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndTps) :: Seq PP_Doc) ->
                       {-# LINE 535 "./src-ag/ExecutionPlan2Hs.ag" #-}
                       "data" >#< lateBindingTypeNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                        >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndTps)
                       {-# LINE 3855 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule497 #-}
   {-# LINE 537 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule497 = \ ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) ((_nontsIsemFunBndDefs) :: Seq PP_Doc) wrappers_ ->
                        {-# LINE 537 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        ( if noInlinePragmas _lhsIoptions
                          then empty
                          else if helpInlining _lhsIoptions && Set.size wrappers_ == 1
                               then ppInline $ lateBindingFieldNm _lhsImainName
                               else ppNoInline $ lateBindingFieldNm _lhsImainName
                        )
                        >-< lateBindingFieldNm _lhsImainName >#< "::" >#< lateBindingTypeNm _lhsImainName
                        >-< lateBindingFieldNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                        >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndDefs )
                        {-# LINE 3869 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule498 #-}
   {-# LINE 1213 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule498 = \ ((_nontsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                          {-# LINE 1213 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          _nontsIchildvisit
                                          {-# LINE 3875 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule499 #-}
   {-# LINE 1357 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule499 = \ ((_nontsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1357 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                       _nontsIvisitdefs
                                       {-# LINE 3881 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule500 #-}
   {-# LINE 1358 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule500 = \ ((_nontsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1358 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                       _nontsIvisituses
                                       {-# LINE 3887 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule501 #-}
   {-# LINE 1429 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule501 = \ _genCommonModule _genMainModule ((_nontsIgenProdIO) :: IO ()) ->
                                          {-# LINE 1429 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          do _genMainModule
                                             _genCommonModule
                                             _nontsIgenProdIO
                                          {-# LINE 3895 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule502 #-}
   {-# LINE 1432 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule502 = \ ((_lhsImainFile) :: String) ->
                                          {-# LINE 1432 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          _lhsImainFile
                                          {-# LINE 3901 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule503 #-}
   {-# LINE 1433 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule503 = \ ((_lhsIoptions) :: Options) ->
                                          {-# LINE 1433 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          ( if tupleAsDummyToken _lhsIoptions
                                            then empty
                                            else pp "import GHC.Prim"
                                          )
                                          >-< if parallelInvoke _lhsIoptions
                                              then pp "import qualified System.IO.Unsafe(unsafePerformIO)"
                                                   >-< pp "import System.IO(IO)"
                                                   >-< pp "import Control.Concurrent(newEmptyMVar,forkIO,putMVar,takeMVar)"
                                              else pp "import Control.Monad.Identity"
                                          {-# LINE 3915 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule504 #-}
   {-# LINE 1442 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule504 = \ ((_lhsImainBlocksDoc) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIoptions) :: Options) ((_lhsIpragmaBlocks) :: String) _mainModuleFile ((_nontsIappendMain) :: [PP_Doc]) ((_nontsIimports) :: [PP_Doc]) _ppMonadImports _wrappersExtra ->
                                          {-# LINE 1442 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          writeModule _mainModuleFile
                                            ( [ warrenFlagsPP _lhsIoptions
                                              , pp $ _lhsIpragmaBlocks
                                              , pp $ _lhsImoduleHeader _lhsImainName "" "" False
                                              , _ppMonadImports
                                              , pp $ "import " ++ _lhsImainName ++ "_common"
                                              ]
                                              ++ _nontsIimports
                                              ++ [_lhsImainBlocksDoc]
                                              ++ [_wrappersExtra    ]
                                              ++ _nontsIappendMain
                                            )
                                          {-# LINE 3932 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule505 #-}
   {-# LINE 1454 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule505 = \ ((_lhsImainFile) :: String) ->
                                          {-# LINE 1454 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                                          {-# LINE 3938 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule506 #-}
   {-# LINE 1455 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule506 = \ _commonExtra _commonFile ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIpragmaBlocks) :: String) ((_lhsItextBlocks) :: PP_Doc) ((_nontsIappendCommon) :: [PP_Doc]) _ppMonadImports ->
                                          {-# LINE 1455 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                          writeModule _commonFile
                                            ( [ pp $ "{-# LANGUAGE Rank2Types, GADTs #-}"
                                              , pp $ _lhsIpragmaBlocks
                                              , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                                              , _ppMonadImports
                                              , _lhsIimportBlocks
                                              , _lhsItextBlocks
                                              , _commonExtra
                                              ]
                                              ++ _nontsIappendCommon
                                            )
                                          {-# LINE 3954 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule507 #-}
   {-# LINE 1574 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule507 = \ ((_nontsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
                            {-# LINE 1574 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            _nontsIfromToStates
                            {-# LINE 3960 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule508 #-}
   {-# LINE 1618 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule508 = \ ((_nontsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
                          {-# LINE 1618 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          _nontsIvisitKinds
                          {-# LINE 3966 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule509 #-}
   {-# LINE 1632 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule509 = \ ((_nontsIinitStates) :: Map NontermIdent Int) ->
                          {-# LINE 1632 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          _nontsIinitStates
                          {-# LINE 3972 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule510 #-}
   rule510 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule511 #-}
   rule511 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression {  }
data Syn_Expression  = Syn_Expression { attrs_Syn_Expression :: (Map String (Maybe NonLocalAttr)), pos_Syn_Expression :: (Pos), semfunc_Syn_Expression :: (PP_Doc), tks_Syn_Expression :: ([HsToken]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn28 
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
data T_Expression_vIn28  = T_Expression_vIn28 
data T_Expression_vOut28  = T_Expression_vOut28 (Map String (Maybe NonLocalAttr)) (Pos) (PP_Doc) ([HsToken])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Expression_v28 
      v28 = \ (T_Expression_vIn28 ) -> ( let
         _lhsOtks :: [HsToken]
         _lhsOtks = rule521 arg_tks_
         _lhsOpos :: Pos
         _lhsOpos = rule522 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule523 arg_tks_
         _lhsOsemfunc :: PP_Doc
         _lhsOsemfunc = rule524 arg_tks_
         __result_ = T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks
         in __result_ )
     in C_Expression_s29 v28
   {-# INLINE rule521 #-}
   {-# LINE 1069 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule521 = \ tks_ ->
                           {-# LINE 1069 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           tks_
                           {-# LINE 4060 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule522 #-}
   {-# LINE 1112 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule522 = \ pos_ ->
                                        {-# LINE 1112 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                        pos_
                                        {-# LINE 4066 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule523 #-}
   {-# LINE 1198 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule523 = \ tks_ ->
                               {-# LINE 1198 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               Map.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                               {-# LINE 4072 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule524 #-}
   {-# LINE 1199 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule524 = \ tks_ ->
                               {-# LINE 1199 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                               {-# LINE 4078 "dist/build/ExecutionPlan2Hs.hs"#-}

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken {  }
data Syn_HsToken  = Syn_HsToken { attrs_Syn_HsToken :: (Map String (Maybe NonLocalAttr)), tok_Syn_HsToken :: ((Pos,String)) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsToken_vIn31 
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
data T_HsToken_vIn31  = T_HsToken_vIn31 
data T_HsToken_vOut31  = T_HsToken_vOut31 (Map String (Maybe NonLocalAttr)) ((Pos,String))
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ arg_pos_ _ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule525 arg_var_
         _tok = rule526 arg_pos_ arg_var_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule527 _tok
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule525 #-}
   {-# LINE 1157 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule525 = \ var_ ->
                              {-# LINE 1157 "./src-ag/ExecutionPlan2Hs.ag" #-}
                              Map.singleton (fieldname var_) Nothing
                              {-# LINE 4135 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule526 #-}
   {-# LINE 1402 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule526 = \ pos_ var_ ->
                          {-# LINE 1402 "./src-ag/ExecutionPlan2Hs.ag" #-}
                          (pos_,fieldname var_)
                          {-# LINE 4141 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule527 #-}
   rule527 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _mbAttr = rule528 arg_attr_ arg_field_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule529 _mbAttr arg_attr_ arg_field_
         _addTrace = rule530 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule531 _addTrace arg_attr_ arg_field_ arg_pos_
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule528 #-}
   {-# LINE 1158 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule528 = \ attr_ field_ ->
                              {-# LINE 1158 "./src-ag/ExecutionPlan2Hs.ag" #-}
                              if field_ == _INST || field_ == _FIELD || field_ == _INST'
                              then Nothing
                              else Just $ mkNonLocalAttr (field_ == _LHS) field_ attr_
                              {-# LINE 4168 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule529 #-}
   {-# LINE 1161 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule529 = \ _mbAttr attr_ field_ ->
                              {-# LINE 1161 "./src-ag/ExecutionPlan2Hs.ag" #-}
                              Map.singleton (attrname True field_ attr_) _mbAttr
                              {-# LINE 4174 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule530 #-}
   {-# LINE 1406 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule530 = \ attr_ field_ rdesc_ ->
                        {-# LINE 1406 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                          Nothing -> id
                        {-# LINE 4182 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule531 #-}
   {-# LINE 1409 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule531 = \ _addTrace attr_ field_ pos_ ->
                   {-# LINE 1409 "./src-ag/ExecutionPlan2Hs.ag" #-}
                   (pos_, _addTrace     $ attrname True field_ attr_)
                   {-# LINE 4188 "dist/build/ExecutionPlan2Hs.hs"#-}
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule532 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule533  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule532 #-}
   {-# LINE 1411 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule532 = \ pos_ value_ ->
                         {-# LINE 1411 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, value_)
                         {-# LINE 4208 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule533 #-}
   rule533 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule534 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule535  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule534 #-}
   {-# LINE 1413 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule534 = \ pos_ value_ ->
                           {-# LINE 1413 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 4234 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule535 #-}
   rule535 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule536 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule537  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule536 #-}
   {-# LINE 1418 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule536 = \ pos_ value_ ->
                           {-# LINE 1418 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 4257 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule537 #-}
   rule537 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 ) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule538 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule539  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule538 #-}
   {-# LINE 1419 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule538 = \ pos_ ->
                           {-# LINE 1419 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           (pos_, "")
                           {-# LINE 4280 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule539 #-}
   rule539 = \  (_ :: ()) ->
     Map.empty

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens {  }
data Syn_HsTokens  = Syn_HsTokens { tks_Syn_HsTokens :: ([(Pos,String)]) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokens_vIn34 
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
data T_HsTokens_vIn34  = T_HsTokens_vIn34 
data T_HsTokens_vOut34  = T_HsTokens_vOut34 ([(Pos,String)])
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 ) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut31 _hdIattrs _hdItok) = inv_HsToken_s32 _hdX32 (T_HsToken_vIn31 )
         (T_HsTokens_vOut34 _tlItks) = inv_HsTokens_s35 _tlX35 (T_HsTokens_vIn34 )
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule540 _hdItok _tlItks
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule540 #-}
   {-# LINE 1398 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule540 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 1398 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 4336 "dist/build/ExecutionPlan2Hs.hs"#-}
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 ) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule541  ()
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule541 #-}
   {-# LINE 1399 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule541 = \  (_ :: ()) ->
                     {-# LINE 1399 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     []
                     {-# LINE 4354 "dist/build/ExecutionPlan2Hs.hs"#-}

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot {  }
data Syn_HsTokensRoot  = Syn_HsTokensRoot {  }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokensRoot_vIn37 
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
data T_HsTokensRoot_vIn37  = T_HsTokensRoot_vIn37 
data T_HsTokensRoot_vOut37  = T_HsTokensRoot_vOut37 
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_HsTokensRoot_v37 
      v37 = \ (T_HsTokensRoot_vIn37 ) -> ( let
         _tokensX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut34 _tokensItks) = inv_HsTokens_s35 _tokensX35 (T_HsTokens_vIn34 )
         __result_ = T_HsTokensRoot_vOut37 
         in __result_ )
     in C_HsTokensRoot_s38 v37

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { allInhmap_Inh_Pattern :: (Map NontermIdent Attributes), allSynmap_Inh_Pattern :: (Map NontermIdent Attributes), anyLazyKind_Inh_Pattern :: (Bool), inhmap_Inh_Pattern :: (Attributes), localAttrTypes_Inh_Pattern :: (Map Identifier Type), options_Inh_Pattern :: (Options), synmap_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { attrTypes_Syn_Pattern :: (PP_Doc), attrs_Syn_Pattern :: (Set String), copy_Syn_Pattern :: (Pattern), isUnderscore_Syn_Pattern :: (Bool), sem_lhs_Syn_Pattern :: ( PP_Doc ) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
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
data T_Pattern_vOut40  = T_Pattern_vOut40 (PP_Doc) (Set String) (Pattern) (Bool) ( PP_Doc )
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrTypes _patsIattrs _patsIcopy _patsIsem_lhs) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule542 _addbang1 _patsIsem_lhs arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule543  ()
         _addbang = rule544 _lhsIoptions
         _addbang1 = rule545 _addbang _lhsIanyLazyKind
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule546 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule547 _patsIattrs
         _copy = rule548 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule549 _copy
         _patsOallInhmap = rule550 _lhsIallInhmap
         _patsOallSynmap = rule551 _lhsIallSynmap
         _patsOanyLazyKind = rule552 _lhsIanyLazyKind
         _patsOinhmap = rule553 _lhsIinhmap
         _patsOlocalAttrTypes = rule554 _lhsIlocalAttrTypes
         _patsOoptions = rule555 _lhsIoptions
         _patsOsynmap = rule556 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule542 #-}
   {-# LINE 1126 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule542 = \ _addbang1 ((_patsIsem_lhs) :: [PP_Doc]) name_ ->
                                  {-# LINE 1126 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _addbang1     $ pp_parens $ name_ >#< hv_sp _patsIsem_lhs
                                  {-# LINE 4470 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule543 #-}
   {-# LINE 1133 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule543 = \  (_ :: ()) ->
                                    {-# LINE 1133 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    False
                                    {-# LINE 4476 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule544 #-}
   {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule544 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4482 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule545 #-}
   {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule545 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4488 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule546 #-}
   rule546 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule547 #-}
   rule547 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule548 #-}
   rule548 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule549 #-}
   rule549 = \ _copy ->
     _copy
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule552 #-}
   rule552 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule553 #-}
   rule553 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule554 #-}
   rule554 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule555 #-}
   rule555 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrTypes _patsIattrs _patsIcopy _patsIsem_lhs) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule557 _addbang1 _patsIsem_lhs
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule558  ()
         _addbang = rule559 _lhsIoptions
         _addbang1 = rule560 _addbang _lhsIanyLazyKind
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule561 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule562 _patsIattrs
         _copy = rule563 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule564 _copy
         _patsOallInhmap = rule565 _lhsIallInhmap
         _patsOallSynmap = rule566 _lhsIallSynmap
         _patsOanyLazyKind = rule567 _lhsIanyLazyKind
         _patsOinhmap = rule568 _lhsIinhmap
         _patsOlocalAttrTypes = rule569 _lhsIlocalAttrTypes
         _patsOoptions = rule570 _lhsIoptions
         _patsOsynmap = rule571 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule557 #-}
   {-# LINE 1125 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule557 = \ _addbang1 ((_patsIsem_lhs) :: [PP_Doc]) ->
                                  {-# LINE 1125 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _addbang1     $ pp_block "(" ")" "," _patsIsem_lhs
                                  {-# LINE 4559 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule558 #-}
   {-# LINE 1134 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule558 = \  (_ :: ()) ->
                                    {-# LINE 1134 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    False
                                    {-# LINE 4565 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule559 #-}
   {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule559 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4571 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule560 #-}
   {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule560 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4577 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule561 #-}
   rule561 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule562 #-}
   rule562 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule563 #-}
   rule563 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule564 #-}
   rule564 = \ _copy ->
     _copy
   {-# INLINE rule565 #-}
   rule565 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule566 #-}
   rule566 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule567 #-}
   rule567 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule568 #-}
   rule568 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule569 #-}
   rule569 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule570 #-}
   rule570 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule571 #-}
   rule571 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrTypes _patIattrs _patIcopy _patIisUnderscore _patIsem_lhs) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _varPat = rule572 arg_attr_ arg_field_
         _patExpr = rule573 _patIisUnderscore _patIsem_lhs _varPat
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule574 _addbang1 _patExpr
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule575  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule576 _patIattrs arg_attr_ arg_field_
         _mbTp = rule577 _lhsIlocalAttrTypes _lhsIsynmap arg_attr_ arg_field_
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule578 _mbTp _patIattrTypes arg_attr_ arg_field_
         _addbang = rule579 _lhsIoptions
         _addbang1 = rule580 _addbang _lhsIanyLazyKind
         _copy = rule581 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule582 _copy
         _patOallInhmap = rule583 _lhsIallInhmap
         _patOallSynmap = rule584 _lhsIallSynmap
         _patOanyLazyKind = rule585 _lhsIanyLazyKind
         _patOinhmap = rule586 _lhsIinhmap
         _patOlocalAttrTypes = rule587 _lhsIlocalAttrTypes
         _patOoptions = rule588 _lhsIoptions
         _patOsynmap = rule589 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule572 #-}
   {-# LINE 1120 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule572 = \ attr_ field_ ->
                                  {-# LINE 1120 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  text $ attrname False field_ attr_
                                  {-# LINE 4651 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule573 #-}
   {-# LINE 1121 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule573 = \ ((_patIisUnderscore) :: Bool) ((_patIsem_lhs) ::  PP_Doc ) _varPat ->
                                  {-# LINE 1121 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  if _patIisUnderscore
                                  then _varPat
                                  else _varPat     >|< "@" >|< _patIsem_lhs
                                  {-# LINE 4659 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule574 #-}
   {-# LINE 1124 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule574 = \ _addbang1 _patExpr ->
                                  {-# LINE 1124 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  _addbang1     _patExpr
                                  {-# LINE 4665 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule575 #-}
   {-# LINE 1135 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule575 = \  (_ :: ()) ->
                                    {-# LINE 1135 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    False
                                    {-# LINE 4671 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule576 #-}
   {-# LINE 1141 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule576 = \ ((_patIattrs) :: Set String) attr_ field_ ->
                    {-# LINE 1141 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    Set.insert (attrname False field_ attr_) _patIattrs
                    {-# LINE 4677 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule577 #-}
   {-# LINE 1146 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule577 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIsynmap) :: Attributes) attr_ field_ ->
                    {-# LINE 1146 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    if field_ == _LHS
                    then Map.lookup attr_ _lhsIsynmap
                    else if field_ == _LOC
                         then Map.lookup attr_ _lhsIlocalAttrTypes
                         else Nothing
                    {-# LINE 4687 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule578 #-}
   {-# LINE 1151 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule578 = \ _mbTp ((_patIattrTypes) :: PP_Doc) attr_ field_ ->
                    {-# LINE 1151 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    maybe empty (\tp -> (attrname False field_ attr_) >#< "::" >#< ppTp tp) _mbTp
                    >-< _patIattrTypes
                    {-# LINE 4694 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule579 #-}
   {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule579 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1542 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4700 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule580 #-}
   {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule580 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1547 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4706 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule581 #-}
   rule581 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule582 #-}
   rule582 = \ _copy ->
     _copy
   {-# INLINE rule583 #-}
   rule583 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule584 #-}
   rule584 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule585 #-}
   rule585 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule586 #-}
   rule586 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule587 #-}
   rule587 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule589 #-}
   rule589 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrTypes _patIattrs _patIcopy _patIisUnderscore _patIsem_lhs) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule590 _patIsem_lhs
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule591 _patIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule592 _patIattrs
         _copy = rule593 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule594 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule595 _patIisUnderscore
         _patOallInhmap = rule596 _lhsIallInhmap
         _patOallSynmap = rule597 _lhsIallSynmap
         _patOanyLazyKind = rule598 _lhsIanyLazyKind
         _patOinhmap = rule599 _lhsIinhmap
         _patOlocalAttrTypes = rule600 _lhsIlocalAttrTypes
         _patOoptions = rule601 _lhsIoptions
         _patOsynmap = rule602 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule590 #-}
   {-# LINE 1128 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule590 = \ ((_patIsem_lhs) ::  PP_Doc ) ->
                                  {-# LINE 1128 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  text "~" >|< pp_parens _patIsem_lhs
                                  {-# LINE 4769 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule591 #-}
   rule591 = \ ((_patIattrTypes) :: PP_Doc) ->
     _patIattrTypes
   {-# INLINE rule592 #-}
   rule592 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule593 #-}
   rule593 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule594 #-}
   rule594 = \ _copy ->
     _copy
   {-# INLINE rule595 #-}
   rule595 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule600 #-}
   rule600 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule603  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule604  ()
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule605  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule606  ()
         _copy = rule607 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule608 _copy
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule603 #-}
   {-# LINE 1127 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule603 = \  (_ :: ()) ->
                                  {-# LINE 1127 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  text "_"
                                  {-# LINE 4832 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule604 #-}
   {-# LINE 1136 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule604 = \  (_ :: ()) ->
                                    {-# LINE 1136 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                    True
                                    {-# LINE 4838 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule605 #-}
   rule605 = \  (_ :: ()) ->
     empty
   {-# INLINE rule606 #-}
   rule606 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule607 #-}
   rule607 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule608 #-}
   rule608 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { allInhmap_Inh_Patterns :: (Map NontermIdent Attributes), allSynmap_Inh_Patterns :: (Map NontermIdent Attributes), anyLazyKind_Inh_Patterns :: (Bool), inhmap_Inh_Patterns :: (Attributes), localAttrTypes_Inh_Patterns :: (Map Identifier Type), options_Inh_Patterns :: (Options), synmap_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { attrTypes_Syn_Patterns :: (PP_Doc), attrs_Syn_Patterns :: (Set String), copy_Syn_Patterns :: (Patterns), sem_lhs_Syn_Patterns :: ([PP_Doc]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs)
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
data T_Patterns_vOut43  = T_Patterns_vOut43 (PP_Doc) (Set String) (Patterns) ([PP_Doc])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIattrTypes _hdIattrs _hdIcopy _hdIisUnderscore _hdIsem_lhs) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdOallInhmap _hdOallSynmap _hdOanyLazyKind _hdOinhmap _hdOlocalAttrTypes _hdOoptions _hdOsynmap)
         (T_Patterns_vOut43 _tlIattrTypes _tlIattrs _tlIcopy _tlIsem_lhs) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlOallInhmap _tlOallSynmap _tlOanyLazyKind _tlOinhmap _tlOlocalAttrTypes _tlOoptions _tlOsynmap)
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule609 _hdIattrTypes _tlIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule610 _hdIattrs _tlIattrs
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule611 _hdIsem_lhs _tlIsem_lhs
         _copy = rule612 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule613 _copy
         _hdOallInhmap = rule614 _lhsIallInhmap
         _hdOallSynmap = rule615 _lhsIallSynmap
         _hdOanyLazyKind = rule616 _lhsIanyLazyKind
         _hdOinhmap = rule617 _lhsIinhmap
         _hdOlocalAttrTypes = rule618 _lhsIlocalAttrTypes
         _hdOoptions = rule619 _lhsIoptions
         _hdOsynmap = rule620 _lhsIsynmap
         _tlOallInhmap = rule621 _lhsIallInhmap
         _tlOallSynmap = rule622 _lhsIallSynmap
         _tlOanyLazyKind = rule623 _lhsIanyLazyKind
         _tlOinhmap = rule624 _lhsIinhmap
         _tlOlocalAttrTypes = rule625 _lhsIlocalAttrTypes
         _tlOoptions = rule626 _lhsIoptions
         _tlOsynmap = rule627 _lhsIsynmap
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule609 #-}
   rule609 = \ ((_hdIattrTypes) :: PP_Doc) ((_tlIattrTypes) :: PP_Doc) ->
     _hdIattrTypes >-< _tlIattrTypes
   {-# INLINE rule610 #-}
   rule610 = \ ((_hdIattrs) :: Set String) ((_tlIattrs) :: Set String) ->
     _hdIattrs `Set.union` _tlIattrs
   {-# INLINE rule611 #-}
   rule611 = \ ((_hdIsem_lhs) ::  PP_Doc ) ((_tlIsem_lhs) :: [PP_Doc]) ->
     _hdIsem_lhs : _tlIsem_lhs
   {-# INLINE rule612 #-}
   rule612 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule613 #-}
   rule613 = \ _copy ->
     _copy
   {-# INLINE rule614 #-}
   rule614 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule616 #-}
   rule616 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule617 #-}
   rule617 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule618 #-}
   rule618 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule619 #-}
   rule619 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule620 #-}
   rule620 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule621 #-}
   rule621 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule623 #-}
   rule623 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule624 #-}
   rule624 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule625 #-}
   rule625 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule626 #-}
   rule626 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule627 #-}
   rule627 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule628  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule629  ()
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule630  ()
         _copy = rule631  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule632 _copy
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule628 #-}
   rule628 = \  (_ :: ()) ->
     empty
   {-# INLINE rule629 #-}
   rule629 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule630 #-}
   rule630 = \  (_ :: ()) ->
     []
   {-# INLINE rule631 #-}
   rule631 = \  (_ :: ()) ->
     []
   {-# INLINE rule632 #-}
   rule632 = \ _copy ->
     _copy

-- Visit -------------------------------------------------------
-- wrapper
data Inh_Visit  = Inh_Visit { allFromToStates_Inh_Visit :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visit :: (Map NontermIdent Attributes), allInitStates_Inh_Visit :: (Map NontermIdent Int), allSynmap_Inh_Visit :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visit :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_Visit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allintramap_Inh_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visit :: (Map Identifier Type), childintros_Inh_Visit :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), con_Inh_Visit :: (ConstructorIdent), inhmap_Inh_Visit :: (Attributes), mrules_Inh_Visit :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), nextVisits_Inh_Visit :: (Map StateIdentifier StateCtx), nt_Inh_Visit :: (NontermIdent), options_Inh_Visit :: (Options), params_Inh_Visit :: ([Identifier]), prevVisits_Inh_Visit :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visit :: (Map Identifier (Set String)), ruleuses_Inh_Visit :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visit :: (Attributes), terminaldefs_Inh_Visit :: (Set String) }
data Syn_Visit  = Syn_Visit { allvisits_Syn_Visit :: ( VisitStateState ), childvisit_Syn_Visit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_Visit :: (Seq Error), fromToStates_Syn_Visit :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visit :: (Set String), ruleKinds_Syn_Visit :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visit :: (Map Identifier Int), sem_visit_Syn_Visit :: (  (StateIdentifier,Bool -> PP_Doc)  ), t_visits_Syn_Visit :: (PP_Doc), usedArgs_Syn_Visit :: (Set String), visitKinds_Syn_Visit :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visit :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visit :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visit #-}
wrap_Visit :: T_Visit  -> Inh_Visit  -> (Syn_Visit )
wrap_Visit (T_Visit act) (Inh_Visit _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visit_vOut46 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visit_s47 sem arg)
        return (Syn_Visit _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_Visit_vIn46  = T_Visit_vIn46 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (ConstructorIdent) (Attributes) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visit_vOut46  = T_Visit_vOut46 ( VisitStateState ) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (  (StateIdentifier,Bool -> PP_Doc)  ) (PP_Doc) (Set String) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visit_Visit #-}
sem_Visit_Visit :: (VisitIdentifier) -> (StateIdentifier) -> (StateIdentifier) -> (Set Identifier) -> (Set Identifier) -> T_VisitSteps  -> (VisitKind) -> T_Visit 
sem_Visit_Visit arg_ident_ arg_from_ arg_to_ arg_inh_ arg_syn_ arg_steps_ arg_kind_ = T_Visit (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Visit_v46 
      v46 = \ (T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIsync_steps _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _lhsOallvisits ::  VisitStateState 
         _lhsOallvisits = rule633 arg_from_ arg_ident_ arg_to_
         _nameT_visit = rule634 _lhsInt arg_ident_
         _nameTIn_visit = rule635 _lhsInt arg_ident_
         _nameTOut_visit = rule636 _lhsInt arg_ident_
         _nameTNext_visit = rule637 _lhsInt arg_to_
         _nextVisitInfo = rule638 _lhsInextVisits arg_to_
         _typecon = rule639 _lhsIoptions arg_kind_
         _t_params = rule640 _lhsIparams
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule641 _addbang1 _inhpart _lhsIoptions _nameTIn_visit _nameTNext_visit _nameTOut_visit _nameT_visit _nextVisitInfo _synpart _t_params _typecon
         _inhpart = rule642 _lhsIinhmap _ppTypeList arg_inh_
         _synpart = rule643 _lhsIsynmap _ppTypeList arg_syn_
         _ppTypeList = rule644 _addbang1
         _lhsOsem_visit ::   (StateIdentifier,Bool -> PP_Doc)  
         _lhsOsem_visit = rule645 _addbang _inhpats _lhsIcon _lhsInt _lhsIoptions _nameTIn_visit _nameT_visit _stepsClosing _stepsInitial _stepsIsem_steps _t_params _vname arg_from_ arg_ident_
         _stepsInitial = rule646 arg_kind_
         _stepsClosing = rule647 _addbang _nextStBuild _resultval arg_kind_
         _vname = rule648 arg_ident_
         _inhpats = rule649 arg_inh_
         _inhargs = rule650 arg_inh_
         _synargs = rule651 arg_syn_
         _nextargsMp = rule652 _lhsIallintramap arg_to_
         _nextargs = rule653 _nextargsMp
         _nextst = rule654 _lhsIoptions _nextargs _nextargsMp arg_to_
         _resultval = rule655 _nameTOut_visit _nextStRef _synargs
         (_nextStBuild,_nextStRef) = rule656 _addbang _nextVisitInfo _nextst
         _stepsOkind = rule657 arg_kind_
         _stepsOfmtMode = rule658 arg_kind_
         _stepsOindex = rule659  ()
         _stepsOprevMaxSimRefs = rule660  ()
         _stepsOuseParallel = rule661  ()
         _prevVisitInfo = rule662 _lhsInextVisits arg_from_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule663 _invokecode arg_ident_
         _invokecode = rule664 _addbang _inhargs _lhsInt _lhsIoptions _nameTIn_visit _nameTOut_visit _nextVisitInfo _prevVisitInfo arg_from_ arg_ident_ arg_kind_ arg_syn_ arg_to_
         _thisintra = rule665 _defsAsMap _nextintra _uses
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule666 _thisintra arg_from_
         _nextintra = rule667 _lhsIallintramap arg_to_
         _uses = rule668 _stepsIuses arg_syn_
         _inhVarNms = rule669 arg_inh_
         _defs = rule670 _inhVarNms _lhsIterminaldefs _stepsIdefs
         _defsAsMap = rule671 _defs
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule672 arg_ident_ arg_syn_
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule673 arg_ident_ arg_inh_
         _lazyIntrasInh = rule674 _inhVarNms _stepsIdefs arg_kind_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule675 _lazyIntrasInh _stepsIlazyIntras
         _addbang = rule676 _lhsIoptions
         _addbang1 = rule677 _addbang arg_kind_
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule678 arg_from_ arg_ident_ arg_to_
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule679 arg_ident_ arg_kind_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule680 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule681 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule682 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule683 _stepsIusedArgs
         _stepsOallFromToStates = rule684 _lhsIallFromToStates
         _stepsOallInitStates = rule685 _lhsIallInitStates
         _stepsOallVisitKinds = rule686 _lhsIallVisitKinds
         _stepsOallchildvisit = rule687 _lhsIallchildvisit
         _stepsOavisitdefs = rule688 _lhsIavisitdefs
         _stepsOavisituses = rule689 _lhsIavisituses
         _stepsOchildTypes = rule690 _lhsIchildTypes
         _stepsOchildintros = rule691 _lhsIchildintros
         _stepsOmrules = rule692 _lhsImrules
         _stepsOoptions = rule693 _lhsIoptions
         _stepsOruledefs = rule694 _lhsIruledefs
         _stepsOruleuses = rule695 _lhsIruleuses
         __result_ = T_Visit_vOut46 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visit_s47 v46
   {-# INLINE rule633 #-}
   {-# LINE 338 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule633 = \ from_ ident_ to_ ->
                            {-# LINE 338 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 5135 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule634 #-}
   {-# LINE 397 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule634 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 397 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  conNmTVisit _lhsInt ident_
                                  {-# LINE 5141 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule635 #-}
   {-# LINE 398 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule635 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 398 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  conNmTVisitIn _lhsInt ident_
                                  {-# LINE 5147 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule636 #-}
   {-# LINE 399 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule636 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 399 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  conNmTVisitOut _lhsInt ident_
                                  {-# LINE 5153 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule637 #-}
   {-# LINE 400 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule637 = \ ((_lhsInt) :: NontermIdent) to_ ->
                                  {-# LINE 400 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  conNmTNextVisit _lhsInt to_
                                  {-# LINE 5159 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule638 #-}
   {-# LINE 402 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule638 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) to_ ->
                                  {-# LINE 402 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.findWithDefault ManyVis to_ _lhsInextVisits
                                  {-# LINE 5165 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule639 #-}
   {-# LINE 403 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule639 = \ ((_lhsIoptions) :: Options) kind_ ->
                                  {-# LINE 403 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  case kind_ of
                                    VisitPure _  -> empty
                                    VisitMonadic -> ppMonadType _lhsIoptions
                                  {-# LINE 5173 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule640 #-}
   {-# LINE 407 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule640 = \ ((_lhsIparams) :: [Identifier]) ->
                            {-# LINE 407 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced _lhsIparams
                            {-# LINE 5179 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule641 #-}
   {-# LINE 408 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule641 = \ _addbang1 _inhpart ((_lhsIoptions) :: Options) _nameTIn_visit _nameTNext_visit _nameTOut_visit _nameT_visit _nextVisitInfo _synpart _t_params _typecon ->
                           {-# LINE 408 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           "type" >#< _nameT_visit     >#< _t_params     >#< "=" >#<
                             pp_parens (_nameTIn_visit     >#< _t_params    )
                               >#< ( if dummyTokenVisit _lhsIoptions
                                     then "->" >#< dummyType _lhsIoptions True
                                     else empty
                                   )
                               >#< "->" >#< _typecon     >#< pp_parens (_nameTOut_visit     >#< _t_params    )
                           >-< "data" >#< _nameTIn_visit     >#< _t_params     >#< "=" >#< _nameTIn_visit     >#<
                                _inhpart
                           >-< "data" >#< _nameTOut_visit     >#< _t_params     >#< "=" >#< _nameTOut_visit     >#<
                                _synpart     >#< case _nextVisitInfo     of
                                                   NoneVis    -> empty
                                                   _          -> _addbang1     $ pp_parens (_nameTNext_visit     >#< _t_params    )
                           {-# LINE 5197 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule642 #-}
   {-# LINE 421 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule642 = \ ((_lhsIinhmap) :: Attributes) _ppTypeList inh_ ->
                            {-# LINE 421 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            _ppTypeList     inh_ _lhsIinhmap
                            {-# LINE 5203 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule643 #-}
   {-# LINE 422 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule643 = \ ((_lhsIsynmap) :: Attributes) _ppTypeList syn_ ->
                            {-# LINE 422 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            _ppTypeList     syn_ _lhsIsynmap
                            {-# LINE 5209 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule644 #-}
   {-# LINE 423 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule644 = \ _addbang1 ->
                             {-# LINE 423 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             \s m -> ppSpaced $ map (\i -> _addbang1     $ pp_parens $ case Map.lookup i m of
                                                                                        Just tp -> ppTp tp ) $ Set.toList s
                             {-# LINE 5216 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule645 #-}
   {-# LINE 715 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule645 = \ _addbang _inhpats ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nameTIn_visit _nameT_visit _stepsClosing _stepsInitial ((_stepsIsem_steps) :: PP_Doc) _t_params _vname from_ ident_ ->
                            {-# LINE 715 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            ( from_
                            , \addInlinePragma ->
                                 ( if noInlinePragmas _lhsIoptions
                                   then empty
                                   else if addInlinePragma && aggressiveInlinePragmas _lhsIoptions
                                        then ppInline _vname
                                        else if helpInlining _lhsIoptions
                                             then ppNoInline _vname
                                             else empty
                                 )
                                 >-< "v" >|< ident_ >#< "::" >#< _nameT_visit     >#< _t_params
                                 >-< "v" >|< ident_ >#< "=" >#< "\\" >#< (_addbang     $ pp_parens (_nameTIn_visit     >#< _inhpats    ))
                                 >#< ( if dummyTokenVisit _lhsIoptions
                                       then pp $ dummyPat _lhsIoptions True
                                       else empty
                                     )
                                 >#< "->"
                                 >#< ( if genCostCentres _lhsIoptions
                                       then ppCostCentre (_vname     >|< "_" >|< _lhsInt >|< "_" >|< _lhsIcon)
                                       else empty
                                     ) >#< "(" >#< _stepsInitial
                                 >-< indent 3 (_stepsIsem_steps >-< _stepsClosing     >#< ")")
                            )
                            {-# LINE 5244 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule646 #-}
   {-# LINE 740 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule646 = \ kind_ ->
                               {-# LINE 740 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               case kind_ of
                                 VisitPure False -> text "let"
                                 VisitPure True  -> empty
                                 VisitMonadic    -> text "do"
                               {-# LINE 5253 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule647 #-}
   {-# LINE 744 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule647 = \ _addbang _nextStBuild _resultval kind_ ->
                                {-# LINE 744 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                let decls =  _nextStBuild
                                             >-<  _addbang     (pp resultValName) >#< "=" >#< _resultval
                                in case kind_ of
                                     VisitPure False -> decls
                                                        >-< "in" >#< resultValName
                                     VisitPure True  -> "let" >#< decls
                                                        >-< indent 1 ("in" >#< resultValName)
                                     VisitMonadic    -> "let" >#< decls
                                                        >-< "return" >#< resultValName
                                {-# LINE 5267 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule648 #-}
   {-# LINE 753 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule648 = \ ident_ ->
                            {-# LINE 753 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            "v" >|< ident_
                            {-# LINE 5273 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule649 #-}
   {-# LINE 754 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule649 = \ inh_ ->
                            {-# LINE 754 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ map (\arg ->                    pp $ attrname True _LHS arg) $ Set.toList inh_
                            {-# LINE 5279 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule650 #-}
   {-# LINE 755 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule650 = \ inh_ ->
                            {-# LINE 755 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            \chn -> ppSpaced $ map (attrname False chn) $ Set.toList inh_
                            {-# LINE 5285 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule651 #-}
   {-# LINE 756 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule651 = \ syn_ ->
                            {-# LINE 756 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ map (\arg -> attrname False _LHS arg) $ Set.toList syn_
                            {-# LINE 5291 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule652 #-}
   {-# LINE 757 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule652 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                             {-# LINE 757 "./src-ag/ExecutionPlan2Hs.ag" #-}
                             maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                             {-# LINE 5297 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule653 #-}
   {-# LINE 758 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule653 = \ _nextargsMp ->
                            {-# LINE 758 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ Map.keys $ _nextargsMp
                            {-# LINE 5303 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule654 #-}
   {-# LINE 759 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule654 = \ ((_lhsIoptions) :: Options) _nextargs _nextargsMp to_ ->
                            {-# LINE 759 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            "st" >|< to_ >#< _nextargs     >#< dummyArg _lhsIoptions (Map.null _nextargsMp    )
                            {-# LINE 5309 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule655 #-}
   {-# LINE 760 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule655 = \ _nameTOut_visit _nextStRef _synargs ->
                            {-# LINE 760 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            _nameTOut_visit     >#< _synargs     >#< _nextStRef
                            {-# LINE 5315 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule656 #-}
   {-# LINE 762 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule656 = \ _addbang _nextVisitInfo _nextst ->
                {-# LINE 762 "./src-ag/ExecutionPlan2Hs.ag" #-}
                case _nextVisitInfo     of
                  NoneVis  -> (empty, empty)
                  _        -> (_addbang     (pp nextStName) >#< "=" >#< _nextst    , pp nextStName)
                {-# LINE 5323 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule657 #-}
   {-# LINE 776 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule657 = \ kind_ ->
                                  {-# LINE 776 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                  kind_
                                  {-# LINE 5329 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule658 #-}
   {-# LINE 827 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule658 = \ kind_ ->
                    {-# LINE 827 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    case kind_ of
                      VisitPure False -> FormatLetDecl
                      VisitPure True  -> FormatLetLine
                      VisitMonadic    -> FormatDo
                    {-# LINE 5338 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule659 #-}
   {-# LINE 876 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule659 = \  (_ :: ()) ->
                                     {-# LINE 876 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     0
                                     {-# LINE 5344 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule660 #-}
   {-# LINE 883 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule660 = \  (_ :: ()) ->
                                              {-# LINE 883 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                              0
                                              {-# LINE 5350 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule661 #-}
   {-# LINE 899 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule661 = \  (_ :: ()) ->
                                           {-# LINE 899 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                           False
                                           {-# LINE 5356 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule662 #-}
   {-# LINE 1217 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule662 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) from_ ->
                           {-# LINE 1217 "./src-ag/ExecutionPlan2Hs.ag" #-}
                           Map.findWithDefault ManyVis from_ _lhsInextVisits
                           {-# LINE 5362 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule663 #-}
   {-# LINE 1218 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule663 = \ _invokecode ident_ ->
                        {-# LINE 1218 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        Map.singleton ident_ _invokecode
                        {-# LINE 5368 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule664 #-}
   {-# LINE 1219 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule664 = \ _addbang _inhargs ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nameTIn_visit _nameTOut_visit _nextVisitInfo _prevVisitInfo from_ ident_ kind_ syn_ to_ ->
                        {-# LINE 1219 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        \chn kind ->
                          if kind `compatibleKind` kind_
                          then Right $
                            let pat | isLazyKind kind_ = pat0
                                    | otherwise        = _addbang     pat0
                                pat0  = pp_parens pat1
                                pat1  = _nameTOut_visit     >#< (ppSpaced $ map (attrname True chn) $ Set.toList syn_)
                                                            >#< cont
                                cont  = case _nextVisitInfo     of
                                          NoneVis  -> empty
                                          _        -> ch1
                                ch0   = text $ stname chn from_
                                ch1   = text $ stname chn to_
                                expr  = case (kind, kind_) of
                                          (VisitPure _,  VisitPure _)  -> expr0
                                          (VisitPure _,  VisitMonadic) -> unMon _lhsIoptions >#< expr0
                                          (VisitMonadic, VisitPure _)  -> "return" >#< expr0
                                          (VisitMonadic, VisitMonadic) -> expr0
                                expr0 = case _prevVisitInfo     of
                                          NoneVis  -> error "error: invocation of a visit from a state that has no next visits"
                                          OneVis _ -> "inv_" >|< _lhsInt >|< "_s" >|< from_ >#< ch0 >#< args
                                          ManyVis  -> "inv_" >|< _lhsInt >|< "_s" >|< from_ >#< ch0
                                                      >#< "K_" >|< _lhsInt >|< "_v" >|< ident_ >#< args
                                args  = pp_parens args0 >#< args1
                                args0 = _nameTIn_visit     >#< _inhargs     chn
                                args1 | dummyTokenVisit _lhsIoptions = pp $ dummyArg _lhsIoptions True
                                      | otherwise                    = empty
                            in (pat, expr)
                          else Left $ IncompatibleVisitKind chn ident_ kind kind_
                        {-# LINE 5402 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule665 #-}
   {-# LINE 1315 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule665 = \ _defsAsMap _nextintra _uses ->
                            {-# LINE 1315 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            (_uses     `Map.union` _nextintra    ) `Map.difference` _defsAsMap
                            {-# LINE 5408 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule666 #-}
   {-# LINE 1316 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule666 = \ _thisintra from_ ->
                            {-# LINE 1316 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton from_ _thisintra
                            {-# LINE 5414 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule667 #-}
   {-# LINE 1317 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule667 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                            {-# LINE 1317 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 5420 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule668 #-}
   {-# LINE 1318 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule668 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) syn_ ->
                            {-# LINE 1318 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            let mp1 = _stepsIuses
                                mp2 = Map.fromList [ (lhsname False i, Just (AttrSyn _LHS i)) | i <- Set.elems syn_ ]
                            in mp1 `Map.union` mp2
                            {-# LINE 5428 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule669 #-}
   {-# LINE 1321 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule669 = \ inh_ ->
                            {-# LINE 1321 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Set.map (lhsname True) inh_
                            {-# LINE 5434 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule670 #-}
   {-# LINE 1322 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule670 = \ _inhVarNms ((_lhsIterminaldefs) :: Set String) ((_stepsIdefs) :: Set String) ->
                            {-# LINE 1322 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIdefs `Set.union` _inhVarNms     `Set.union` _lhsIterminaldefs
                            {-# LINE 5440 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule671 #-}
   {-# LINE 1323 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule671 = \ _defs ->
                            {-# LINE 1323 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.fromList [ (a, Nothing) | a <- Set.elems _defs     ]
                            {-# LINE 5446 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule672 #-}
   {-# LINE 1347 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule672 = \ ident_ syn_ ->
                            {-# LINE 1347 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 5452 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule673 #-}
   {-# LINE 1348 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule673 = \ ident_ inh_ ->
                            {-# LINE 1348 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 5458 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule674 #-}
   {-# LINE 1380 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule674 = \ _inhVarNms ((_stepsIdefs) :: Set String) kind_ ->
                        {-# LINE 1380 "./src-ag/ExecutionPlan2Hs.ag" #-}
                        case kind_ of
                          VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                          _               -> Set.empty
                        {-# LINE 5466 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule675 #-}
   {-# LINE 1383 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule675 = \ _lazyIntrasInh ((_stepsIlazyIntras) :: Set String) ->
                     {-# LINE 1383 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                     {-# LINE 5472 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule676 #-}
   {-# LINE 1536 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule676 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1536 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 5478 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule677 #-}
   {-# LINE 1544 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule677 = \ _addbang kind_ ->
                                                     {-# LINE 1544 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                     if isLazyKind kind_ then id else _addbang
                                                     {-# LINE 5484 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule678 #-}
   {-# LINE 1571 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule678 = \ from_ ident_ to_ ->
                       {-# LINE 1571 "./src-ag/ExecutionPlan2Hs.ag" #-}
                       Map.singleton ident_ (from_, to_)
                       {-# LINE 5490 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule679 #-}
   {-# LINE 1615 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule679 = \ ident_ kind_ ->
                     {-# LINE 1615 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     Map.singleton ident_ kind_
                     {-# LINE 5496 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule680 #-}
   rule680 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule681 #-}
   rule681 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule682 #-}
   rule682 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule683 #-}
   rule683 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule684 #-}
   rule684 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule685 #-}
   rule685 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule686 #-}
   rule686 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule687 #-}
   rule687 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule692 #-}
   rule692 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule693 #-}
   rule693 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule694 #-}
   rule694 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses

-- VisitStep ---------------------------------------------------
-- wrapper
data Inh_VisitStep  = Inh_VisitStep { allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitStep :: (Map NontermIdent Int), allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitStep :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitStep :: (Map Identifier Type), childintros_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), fmtMode_Inh_VisitStep :: (FormatMode), index_Inh_VisitStep :: (Int), isLast_Inh_VisitStep :: (Bool), kind_Inh_VisitStep :: (VisitKind), mrules_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), options_Inh_VisitStep :: (Options), prevMaxSimRefs_Inh_VisitStep :: (Int), ruledefs_Inh_VisitStep :: (Map Identifier (Set String)), ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))), useParallel_Inh_VisitStep :: (Bool) }
data Syn_VisitStep  = Syn_VisitStep { defs_Syn_VisitStep :: (Set String), errors_Syn_VisitStep :: (Seq Error), index_Syn_VisitStep :: (Int), isLast_Syn_VisitStep :: (Bool), lazyIntras_Syn_VisitStep :: (Set String), prevMaxSimRefs_Syn_VisitStep :: (Int), ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitStep :: (Map Identifier Int), sem_steps_Syn_VisitStep :: (PP_Doc), sync_steps_Syn_VisitStep :: (PP_Doc), usedArgs_Syn_VisitStep :: (Set String), uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitStep #-}
wrap_VisitStep :: T_VisitStep  -> Inh_VisitStep  -> (Syn_VisitStep )
wrap_VisitStep (T_VisitStep act) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
        (T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitStep_s50 sem arg)
        return (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
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
data T_VisitStep_vIn49  = T_VisitStep_vIn49 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (FormatMode) (Int) (Bool) (VisitKind) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Bool)
data T_VisitStep_vOut49  = T_VisitStep_vOut49 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (PP_Doc) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitStep_Sem #-}
sem_VisitStep_Sem :: (Identifier) -> T_VisitStep 
sem_VisitStep_Sem arg_name_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _ruleItf = rule696 _lhsImrules arg_name_
         _lhsOerrors :: Seq Error
         (_lhsOerrors,_sem_steps) = rule697 _lhsIfmtMode _lhsIkind _ruleItf
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule698 arg_name_
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule699 _lhsIkind arg_name_
         _lhsOdefs :: Set String
         _lhsOdefs = rule700 _lhsIruledefs arg_name_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule701 _lhsIruleuses arg_name_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule702  ()
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule703 _sem_steps
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule704  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule705  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule706  ()
         _lhsOindex :: Int
         _lhsOindex = rule707 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule708 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule709 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule696 #-}
   {-# LINE 785 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule696 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) name_ ->
                               {-# LINE 785 "./src-ag/ExecutionPlan2Hs.ag" #-}
                               Map.findWithDefault (error $ "Rule "  ++ show name_  ++ " not found") name_ _lhsImrules
                               {-# LINE 5622 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule697 #-}
   {-# LINE 786 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule697 = \ ((_lhsIfmtMode) :: FormatMode) ((_lhsIkind) :: VisitKind) _ruleItf ->
                                               {-# LINE 786 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                               case _ruleItf     _lhsIkind _lhsIfmtMode of
                                                 Left e     -> (Seq.singleton e, empty)
                                                 Right stmt -> (Seq.empty, stmt)
                                               {-# LINE 5630 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule698 #-}
   {-# LINE 1268 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule698 = \ name_ ->
                                                 {-# LINE 1268 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                 Map.singleton name_ 1
                                                 {-# LINE 5636 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule699 #-}
   {-# LINE 1278 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule699 = \ ((_lhsIkind) :: VisitKind) name_ ->
                    {-# LINE 1278 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    Map.singleton name_ (Set.singleton _lhsIkind)
                    {-# LINE 5642 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule700 #-}
   {-# LINE 1363 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule700 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) name_ ->
                            {-# LINE 1363 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                            {-# LINE 5648 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule701 #-}
   {-# LINE 1364 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule701 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) name_ ->
                            {-# LINE 1364 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                            {-# LINE 5654 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule702 #-}
   rule702 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule703 #-}
   rule703 = \ _sem_steps ->
     _sem_steps
   {-# INLINE rule704 #-}
   rule704 = \  (_ :: ()) ->
     empty
   {-# INLINE rule705 #-}
   rule705 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule706 #-}
   rule706 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule707 #-}
   rule707 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_ChildVisit #-}
sem_VisitStep_ChildVisit :: (Identifier) -> (NontermIdent) -> (VisitIdentifier) -> T_VisitStep 
sem_VisitStep_ChildVisit arg_child_ _ arg_visit_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _visitItf = rule710 _lhsIallchildvisit arg_visit_
         _lhsOerrors :: Seq Error
         (_lhsOerrors,_patPP,_exprPP) = rule711 _lhsIkind _visitItf arg_child_
         _useParallel = rule712 _lhsIisLast _lhsIuseParallel
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule713 _addbang _convToMonad _exprPP _lhsIfmtMode _lhsIindex _lhsIkind _patPP _useParallel
         _convToMonad = rule714 _callKind
         _callKind = rule715 _lhsIallVisitKinds arg_visit_
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule716 _lhsIindex _patPP _useParallel
         _lhsOdefs :: Set String
         _lhsOdefs = rule717 _lhsIavisitdefs _to arg_child_ arg_visit_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule718 _from _lhsIavisituses arg_child_ arg_visit_
         _addbang = rule719 _lhsIoptions
         (_from,_to) = rule720 _lhsIallFromToStates arg_visit_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule721  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule722  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule723  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule724  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule725  ()
         _lhsOindex :: Int
         _lhsOindex = rule726 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule727 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule728 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule710 #-}
   {-# LINE 794 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule710 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) visit_ ->
                                {-# LINE 794 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error $ "Visit " ++ show visit_ ++ " not found") visit_ _lhsIallchildvisit
                                {-# LINE 5726 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule711 #-}
   {-# LINE 795 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule711 = \ ((_lhsIkind) :: VisitKind) _visitItf child_ ->
                                                       {-# LINE 795 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                       case _visitItf     child_ _lhsIkind of
                                                         Left e           -> (Seq.singleton e, empty, empty)
                                                         Right (pat,expr) -> (Seq.empty, pat, expr)
                                                       {-# LINE 5734 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule712 #-}
   {-# LINE 799 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule712 = \ ((_lhsIisLast) :: Bool) ((_lhsIuseParallel) :: Bool) ->
                                   {-# LINE 799 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIuseParallel && not _lhsIisLast
                                   {-# LINE 5740 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule713 #-}
   {-# LINE 800 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule713 = \ _addbang _convToMonad _exprPP ((_lhsIfmtMode) :: FormatMode) ((_lhsIindex) :: Int) ((_lhsIkind) :: VisitKind) _patPP _useParallel ->
                                 {-# LINE 800 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 if _useParallel
                                 then _addbang     ("sync_" >|< _lhsIindex) >#< "<- newEmptyMVar"
                                      >-< "forkIO" >#< pp_parens (_convToMonad     >#< pp_parens _exprPP     >#< ">>= \\" >#< _addbang     (pp parResultName) >#< " -> putMVar sync_" >|< _lhsIindex >#< parResultName)
                                 else let decl = case _lhsIkind of
                                                   VisitPure _  -> _patPP     >#< "=" >#< _exprPP
                                                   VisitMonadic -> _patPP     >#< "<-" >#< _exprPP
                                      in fmtDecl False _lhsIfmtMode decl
                                 {-# LINE 5752 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule714 #-}
   {-# LINE 807 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule714 = \ _callKind ->
                                   {-# LINE 807 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                   case _callKind     of
                                     VisitPure _  -> text "return"
                                     VisitMonadic -> empty
                                   {-# LINE 5760 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule715 #-}
   {-# LINE 810 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule715 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) visit_ ->
                                 {-# LINE 810 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 Map.findWithDefault (error "visit kind should be in the map") visit_ _lhsIallVisitKinds
                                 {-# LINE 5766 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule716 #-}
   {-# LINE 818 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule716 = \ ((_lhsIindex) :: Int) _patPP _useParallel ->
                     {-# LINE 818 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     if _useParallel
                     then _patPP     >#< "<-" >#< "takeMVar sync_" >|< _lhsIindex
                     else empty
                     {-# LINE 5774 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule717 #-}
   {-# LINE 1365 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule717 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) _to child_ visit_ ->
                            {-# LINE 1365 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            Set.insert (stname child_ _to) $ maybe (error "Visit not found") (Set.map $ attrname True child_) $ Map.lookup visit_ _lhsIavisitdefs
                            {-# LINE 5780 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule718 #-}
   {-# LINE 1366 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule718 = \ _from ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) child_ visit_ ->
                            {-# LINE 1366 "./src-ag/ExecutionPlan2Hs.ag" #-}
                            let convert attrs = Map.fromList [ (attrname False child_ attr, Just $ mkNonLocalAttr True child_ attr) | attr <- Set.elems attrs ]
                            in Map.insert (stname child_ _from) Nothing $ convert $
                                 maybe (error "Visit not found") id $ Map.lookup visit_ _lhsIavisituses
                            {-# LINE 5788 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule719 #-}
   {-# LINE 1541 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule719 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1541 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 5794 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule720 #-}
   {-# LINE 1577 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule720 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) visit_ ->
                         {-# LINE 1577 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                         {-# LINE 5800 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule721 #-}
   rule721 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule722 #-}
   rule722 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule723 #-}
   rule723 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule724 #-}
   rule724 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule725 #-}
   rule725 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_PureGroup #-}
sem_VisitStep_PureGroup :: T_VisitSteps  -> (Bool) -> T_VisitStep 
sem_VisitStep_PureGroup arg_steps_ arg_ordered_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIsync_steps _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _stepsOkind = rule729 arg_ordered_
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule730 _lhsIfmtMode _stepsIsem_steps
         _stepsOfmtMode = rule731 _lhsIfmtMode
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule732 _stepsIdefs _stepsIlazyIntras arg_ordered_
         _lhsOdefs :: Set String
         _lhsOdefs = rule733 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule734 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule735 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule736 _stepsIruleUsage
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule737 _stepsIsync_steps
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule738 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule739 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule740 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule741 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule742 _stepsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule743 _stepsIprevMaxSimRefs
         _stepsOallFromToStates = rule744 _lhsIallFromToStates
         _stepsOallInitStates = rule745 _lhsIallInitStates
         _stepsOallVisitKinds = rule746 _lhsIallVisitKinds
         _stepsOallchildvisit = rule747 _lhsIallchildvisit
         _stepsOavisitdefs = rule748 _lhsIavisitdefs
         _stepsOavisituses = rule749 _lhsIavisituses
         _stepsOchildTypes = rule750 _lhsIchildTypes
         _stepsOchildintros = rule751 _lhsIchildintros
         _stepsOindex = rule752 _lhsIindex
         _stepsOmrules = rule753 _lhsImrules
         _stepsOoptions = rule754 _lhsIoptions
         _stepsOprevMaxSimRefs = rule755 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule756 _lhsIruledefs
         _stepsOruleuses = rule757 _lhsIruleuses
         _stepsOuseParallel = rule758 _lhsIuseParallel
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule729 #-}
   {-# LINE 780 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule729 = \ ordered_ ->
                 {-# LINE 780 "./src-ag/ExecutionPlan2Hs.ag" #-}
                 VisitPure ordered_
                 {-# LINE 5885 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule730 #-}
   {-# LINE 812 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule730 = \ ((_lhsIfmtMode) :: FormatMode) ((_stepsIsem_steps) :: PP_Doc) ->
                                 {-# LINE 812 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 case _lhsIfmtMode of
                                   FormatDo -> "let" >#< _stepsIsem_steps
                                   _        -> _stepsIsem_steps
                                 {-# LINE 5893 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule731 #-}
   {-# LINE 833 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule731 = \ ((_lhsIfmtMode) :: FormatMode) ->
                    {-# LINE 833 "./src-ag/ExecutionPlan2Hs.ag" #-}
                    case _lhsIfmtMode of
                      FormatDo      -> FormatLetDecl
                      mode          -> mode
                    {-# LINE 5901 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule732 #-}
   {-# LINE 1386 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule732 = \ ((_stepsIdefs) :: Set String) ((_stepsIlazyIntras) :: Set String) ordered_ ->
                     {-# LINE 1386 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     if ordered_
                     then _stepsIlazyIntras
                     else _stepsIdefs
                     {-# LINE 5909 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule733 #-}
   rule733 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule734 #-}
   rule734 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule735 #-}
   rule735 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule736 #-}
   rule736 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule737 #-}
   rule737 = \ ((_stepsIsync_steps) :: PP_Doc) ->
     _stepsIsync_steps
   {-# INLINE rule738 #-}
   rule738 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule739 #-}
   rule739 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule740 #-}
   rule740 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule741 #-}
   rule741 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule742 #-}
   rule742 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule743 #-}
   rule743 = \ ((_stepsIprevMaxSimRefs) :: Int) ->
     _stepsIprevMaxSimRefs
   {-# INLINE rule744 #-}
   rule744 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule745 #-}
   rule745 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule746 #-}
   rule746 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule747 #-}
   rule747 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule748 #-}
   rule748 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule749 #-}
   rule749 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule750 #-}
   rule750 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule751 #-}
   rule751 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule752 #-}
   rule752 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule753 #-}
   rule753 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule754 #-}
   rule754 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule755 #-}
   rule755 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule756 #-}
   rule756 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule757 #-}
   rule757 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule758 #-}
   rule758 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
{-# NOINLINE sem_VisitStep_Sim #-}
sem_VisitStep_Sim :: T_VisitSteps  -> T_VisitStep 
sem_VisitStep_Sim arg_steps_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIsync_steps _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel)
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule759 _stepsIsem_steps _stepsIsync_steps
         _stepsOindex = rule760  ()
         _lhsOindex :: Int
         _lhsOindex = rule761 _lhsIindex
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule762 _lhsIprevMaxSimRefs _stepsIindex _useParallel
         _useParallel = rule763 _isMonadic _lhsIoptions _stepsIsize
         _isMonadic = rule764 _lhsIkind
         _lhsOdefs :: Set String
         _lhsOdefs = rule765 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule766 _stepsIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule767 _stepsIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule768 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule769 _stepsIruleUsage
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule770 _stepsIsync_steps
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule771 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule772 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule773 _stepsIvisitKinds
         _lhsOisLast :: Bool
         _lhsOisLast = rule774 _stepsIisLast
         _stepsOallFromToStates = rule775 _lhsIallFromToStates
         _stepsOallInitStates = rule776 _lhsIallInitStates
         _stepsOallVisitKinds = rule777 _lhsIallVisitKinds
         _stepsOallchildvisit = rule778 _lhsIallchildvisit
         _stepsOavisitdefs = rule779 _lhsIavisitdefs
         _stepsOavisituses = rule780 _lhsIavisituses
         _stepsOchildTypes = rule781 _lhsIchildTypes
         _stepsOchildintros = rule782 _lhsIchildintros
         _stepsOfmtMode = rule783 _lhsIfmtMode
         _stepsOkind = rule784 _lhsIkind
         _stepsOmrules = rule785 _lhsImrules
         _stepsOoptions = rule786 _lhsIoptions
         _stepsOprevMaxSimRefs = rule787 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule788 _lhsIruledefs
         _stepsOruleuses = rule789 _lhsIruleuses
         _stepsOuseParallel = rule790 _useParallel
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule759 #-}
   {-# LINE 811 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule759 = \ ((_stepsIsem_steps) :: PP_Doc) ((_stepsIsync_steps) :: PP_Doc) ->
                                 {-# LINE 811 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 _stepsIsem_steps >-< _stepsIsync_steps
                                 {-# LINE 6050 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule760 #-}
   {-# LINE 877 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule760 = \  (_ :: ()) ->
                                     {-# LINE 877 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     0
                                     {-# LINE 6056 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule761 #-}
   {-# LINE 878 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule761 = \ ((_lhsIindex) :: Int) ->
                                     {-# LINE 878 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                     _lhsIindex
                                     {-# LINE 6062 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule762 #-}
   {-# LINE 885 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule762 = \ ((_lhsIprevMaxSimRefs) :: Int) ((_stepsIindex) :: Int) _useParallel ->
                         {-# LINE 885 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         if _useParallel
                         then _lhsIprevMaxSimRefs `max` (_stepsIindex - 1)
                         else _lhsIprevMaxSimRefs
                         {-# LINE 6070 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule763 #-}
   {-# LINE 900 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule763 = \ _isMonadic ((_lhsIoptions) :: Options) ((_stepsIsize) :: Int) ->
                                         {-# LINE 900 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                         parallelInvoke _lhsIoptions && _stepsIsize > 1 && _isMonadic
                                         {-# LINE 6076 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule764 #-}
   {-# LINE 901 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule764 = \ ((_lhsIkind) :: VisitKind) ->
                                         {-# LINE 901 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                         case _lhsIkind of
                                           VisitMonadic -> True
                                           _            -> False
                                         {-# LINE 6084 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule765 #-}
   rule765 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule766 #-}
   rule766 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule767 #-}
   rule767 = \ ((_stepsIlazyIntras) :: Set String) ->
     _stepsIlazyIntras
   {-# INLINE rule768 #-}
   rule768 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule769 #-}
   rule769 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule770 #-}
   rule770 = \ ((_stepsIsync_steps) :: PP_Doc) ->
     _stepsIsync_steps
   {-# INLINE rule771 #-}
   rule771 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule772 #-}
   rule772 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule773 #-}
   rule773 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule774 #-}
   rule774 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule775 #-}
   rule775 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule776 #-}
   rule776 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule777 #-}
   rule777 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule778 #-}
   rule778 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule779 #-}
   rule779 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule780 #-}
   rule780 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule781 #-}
   rule781 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule782 #-}
   rule782 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule783 #-}
   rule783 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule784 #-}
   rule784 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule785 #-}
   rule785 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule786 #-}
   rule786 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule787 #-}
   rule787 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule788 #-}
   rule788 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule789 #-}
   rule789 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule790 #-}
   rule790 = \ _useParallel ->
     _useParallel
{-# NOINLINE sem_VisitStep_ChildIntro #-}
sem_VisitStep_ChildIntro :: (Identifier) -> T_VisitStep 
sem_VisitStep_ChildIntro arg_child_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _attachItf = rule791 _lhsIchildintros arg_child_
         _lhsOerrors :: Seq Error
         _lhsOsem_steps :: PP_Doc
         _lhsOdefs :: Set String
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         (_lhsOerrors,_lhsOsem_steps,_lhsOdefs,_lhsOuses) = rule792 _attachItf _lhsIfmtMode _lhsIkind
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule793  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule794  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule795  ()
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule796  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule797  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule798  ()
         _lhsOindex :: Int
         _lhsOindex = rule799 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule800 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule801 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule791 #-}
   {-# LINE 789 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule791 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) child_ ->
                                 {-# LINE 789 "./src-ag/ExecutionPlan2Hs.ag" #-}
                                 Map.findWithDefault (error $ "Child " ++ show child_ ++ " not found") child_ _lhsIchildintros
                                 {-# LINE 6202 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule792 #-}
   {-# LINE 791 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule792 = \ _attachItf ((_lhsIfmtMode) :: FormatMode) ((_lhsIkind) :: VisitKind) ->
                     {-# LINE 791 "./src-ag/ExecutionPlan2Hs.ag" #-}
                     case _attachItf     _lhsIkind _lhsIfmtMode of
                       Left e                   -> (Seq.singleton e, empty, Set.empty, Map.empty)
                       Right (code, defs, uses) -> (Seq.empty, code, defs, uses)
                     {-# LINE 6210 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule793 #-}
   rule793 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule794 #-}
   rule794 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule795 #-}
   rule795 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule796 #-}
   rule796 = \  (_ :: ()) ->
     empty
   {-# INLINE rule797 #-}
   rule797 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule798 #-}
   rule798 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule799 #-}
   rule799 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule800 #-}
   rule800 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule801 #-}
   rule801 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs

-- VisitSteps --------------------------------------------------
-- wrapper
data Inh_VisitSteps  = Inh_VisitSteps { allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitSteps :: (Map NontermIdent Int), allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitSteps :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitSteps :: (Map Identifier Type), childintros_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), fmtMode_Inh_VisitSteps :: (FormatMode), index_Inh_VisitSteps :: (Int), kind_Inh_VisitSteps :: (VisitKind), mrules_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), options_Inh_VisitSteps :: (Options), prevMaxSimRefs_Inh_VisitSteps :: (Int), ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)), ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))), useParallel_Inh_VisitSteps :: (Bool) }
data Syn_VisitSteps  = Syn_VisitSteps { defs_Syn_VisitSteps :: (Set String), errors_Syn_VisitSteps :: (Seq Error), index_Syn_VisitSteps :: (Int), isLast_Syn_VisitSteps :: (Bool), lazyIntras_Syn_VisitSteps :: (Set String), prevMaxSimRefs_Syn_VisitSteps :: (Int), ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitSteps :: (Map Identifier Int), sem_steps_Syn_VisitSteps :: (PP_Doc), size_Syn_VisitSteps :: (Int), sync_steps_Syn_VisitSteps :: (PP_Doc), usedArgs_Syn_VisitSteps :: (Set String), uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitSteps #-}
wrap_VisitSteps :: T_VisitSteps  -> Inh_VisitSteps  -> (Syn_VisitSteps )
wrap_VisitSteps (T_VisitSteps act) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
        (T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitSteps_s53 sem arg)
        return (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
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
data T_VisitSteps_vIn52  = T_VisitSteps_vIn52 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (FormatMode) (Int) (VisitKind) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Bool)
data T_VisitSteps_vOut52  = T_VisitSteps_vOut52 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (Int) (PP_Doc) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitSteps_Cons #-}
sem_VisitSteps_Cons :: T_VisitStep  -> T_VisitSteps  -> T_VisitSteps 
sem_VisitSteps_Cons arg_hd_ arg_tl_ = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_VisitStep (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_tl_))
         (T_VisitStep_vOut49 _hdIdefs _hdIerrors _hdIindex _hdIisLast _hdIlazyIntras _hdIprevMaxSimRefs _hdIruleKinds _hdIruleUsage _hdIsem_steps _hdIsync_steps _hdIusedArgs _hdIuses _hdIvisitKinds) = inv_VisitStep_s50 _hdX50 (T_VisitStep_vIn49 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOfmtMode _hdOindex _hdOisLast _hdOkind _hdOmrules _hdOoptions _hdOprevMaxSimRefs _hdOruledefs _hdOruleuses _hdOuseParallel)
         (T_VisitSteps_vOut52 _tlIdefs _tlIerrors _tlIindex _tlIisLast _tlIlazyIntras _tlIprevMaxSimRefs _tlIruleKinds _tlIruleUsage _tlIsem_steps _tlIsize _tlIsync_steps _tlIusedArgs _tlIuses _tlIvisitKinds) = inv_VisitSteps_s53 _tlX53 (T_VisitSteps_vIn52 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOfmtMode _tlOindex _tlOkind _tlOmrules _tlOoptions _tlOprevMaxSimRefs _tlOruledefs _tlOruleuses _tlOuseParallel)
         _lhsOsize :: Int
         _lhsOsize = rule802 _tlIsize
         _hdOindex = rule803 _lhsIindex
         _tlOindex = rule804 _lhsIindex
         _lhsOindex :: Int
         _lhsOindex = rule805 _tlIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule806  ()
         _hdOisLast = rule807 _tlIisLast
         _lhsOdefs :: Set String
         _lhsOdefs = rule808 _hdIdefs _tlIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule809 _hdIerrors _tlIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule810 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule811 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule812 _hdIruleUsage _tlIruleUsage
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule813 _hdIsem_steps _tlIsem_steps
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule814 _hdIsync_steps _tlIsync_steps
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule815 _hdIusedArgs _tlIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule816 _hdIuses _tlIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule817 _hdIvisitKinds _tlIvisitKinds
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule818 _tlIprevMaxSimRefs
         _hdOallFromToStates = rule819 _lhsIallFromToStates
         _hdOallInitStates = rule820 _lhsIallInitStates
         _hdOallVisitKinds = rule821 _lhsIallVisitKinds
         _hdOallchildvisit = rule822 _lhsIallchildvisit
         _hdOavisitdefs = rule823 _lhsIavisitdefs
         _hdOavisituses = rule824 _lhsIavisituses
         _hdOchildTypes = rule825 _lhsIchildTypes
         _hdOchildintros = rule826 _lhsIchildintros
         _hdOfmtMode = rule827 _lhsIfmtMode
         _hdOkind = rule828 _lhsIkind
         _hdOmrules = rule829 _lhsImrules
         _hdOoptions = rule830 _lhsIoptions
         _hdOprevMaxSimRefs = rule831 _lhsIprevMaxSimRefs
         _hdOruledefs = rule832 _lhsIruledefs
         _hdOruleuses = rule833 _lhsIruleuses
         _hdOuseParallel = rule834 _lhsIuseParallel
         _tlOallFromToStates = rule835 _lhsIallFromToStates
         _tlOallInitStates = rule836 _lhsIallInitStates
         _tlOallVisitKinds = rule837 _lhsIallVisitKinds
         _tlOallchildvisit = rule838 _lhsIallchildvisit
         _tlOavisitdefs = rule839 _lhsIavisitdefs
         _tlOavisituses = rule840 _lhsIavisituses
         _tlOchildTypes = rule841 _lhsIchildTypes
         _tlOchildintros = rule842 _lhsIchildintros
         _tlOfmtMode = rule843 _lhsIfmtMode
         _tlOkind = rule844 _lhsIkind
         _tlOmrules = rule845 _lhsImrules
         _tlOoptions = rule846 _lhsIoptions
         _tlOprevMaxSimRefs = rule847 _hdIprevMaxSimRefs
         _tlOruledefs = rule848 _lhsIruledefs
         _tlOruleuses = rule849 _lhsIruleuses
         _tlOuseParallel = rule850 _lhsIuseParallel
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule802 #-}
   {-# LINE 868 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule802 = \ ((_tlIsize) :: Int) ->
                      {-# LINE 868 "./src-ag/ExecutionPlan2Hs.ag" #-}
                      1 + _tlIsize
                      {-# LINE 6351 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule803 #-}
   {-# LINE 873 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule803 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 873 "./src-ag/ExecutionPlan2Hs.ag" #-}
                _lhsIindex
                {-# LINE 6357 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule804 #-}
   {-# LINE 874 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule804 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 874 "./src-ag/ExecutionPlan2Hs.ag" #-}
                1 + _lhsIindex
                {-# LINE 6363 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule805 #-}
   {-# LINE 875 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule805 = \ ((_tlIindex) :: Int) ->
                {-# LINE 875 "./src-ag/ExecutionPlan2Hs.ag" #-}
                _tlIindex
                {-# LINE 6369 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule806 #-}
   {-# LINE 894 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule806 = \  (_ :: ()) ->
                         {-# LINE 894 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         False
                         {-# LINE 6375 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule807 #-}
   {-# LINE 895 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule807 = \ ((_tlIisLast) :: Bool) ->
                         {-# LINE 895 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         _tlIisLast
                         {-# LINE 6381 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule808 #-}
   rule808 = \ ((_hdIdefs) :: Set String) ((_tlIdefs) :: Set String) ->
     _hdIdefs `Set.union` _tlIdefs
   {-# INLINE rule809 #-}
   rule809 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule810 #-}
   rule810 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule811 #-}
   rule811 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule812 #-}
   rule812 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule813 #-}
   rule813 = \ ((_hdIsem_steps) :: PP_Doc) ((_tlIsem_steps) :: PP_Doc) ->
     _hdIsem_steps >-< _tlIsem_steps
   {-# INLINE rule814 #-}
   rule814 = \ ((_hdIsync_steps) :: PP_Doc) ((_tlIsync_steps) :: PP_Doc) ->
     _hdIsync_steps >-< _tlIsync_steps
   {-# INLINE rule815 #-}
   rule815 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule816 #-}
   rule816 = \ ((_hdIuses) :: Map String (Maybe NonLocalAttr)) ((_tlIuses) :: Map String (Maybe NonLocalAttr)) ->
     _hdIuses `Map.union` _tlIuses
   {-# INLINE rule817 #-}
   rule817 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule818 #-}
   rule818 = \ ((_tlIprevMaxSimRefs) :: Int) ->
     _tlIprevMaxSimRefs
   {-# INLINE rule819 #-}
   rule819 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule820 #-}
   rule820 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule821 #-}
   rule821 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule822 #-}
   rule822 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule823 #-}
   rule823 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule824 #-}
   rule824 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule825 #-}
   rule825 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule826 #-}
   rule826 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule827 #-}
   rule827 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule828 #-}
   rule828 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule829 #-}
   rule829 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule830 #-}
   rule830 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule831 #-}
   rule831 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule832 #-}
   rule832 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule833 #-}
   rule833 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule834 #-}
   rule834 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
   {-# INLINE rule835 #-}
   rule835 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule836 #-}
   rule836 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule837 #-}
   rule837 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule838 #-}
   rule838 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule839 #-}
   rule839 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule840 #-}
   rule840 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule841 #-}
   rule841 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule842 #-}
   rule842 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule843 #-}
   rule843 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule844 #-}
   rule844 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule845 #-}
   rule845 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule846 #-}
   rule846 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule847 #-}
   rule847 = \ ((_hdIprevMaxSimRefs) :: Int) ->
     _hdIprevMaxSimRefs
   {-# INLINE rule848 #-}
   rule848 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule849 #-}
   rule849 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule850 #-}
   rule850 = \ ((_lhsIuseParallel) :: Bool) ->
     _lhsIuseParallel
{-# NOINLINE sem_VisitSteps_Nil #-}
sem_VisitSteps_Nil ::  T_VisitSteps 
sem_VisitSteps_Nil  = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) -> ( let
         _lhsOsize :: Int
         _lhsOsize = rule851  ()
         _lhsOisLast :: Bool
         _lhsOisLast = rule852  ()
         _lhsOdefs :: Set String
         _lhsOdefs = rule853  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule854  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule855  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule856  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule857  ()
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule858  ()
         _lhsOsync_steps :: PP_Doc
         _lhsOsync_steps = rule859  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule860  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule861  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule862  ()
         _lhsOindex :: Int
         _lhsOindex = rule863 _lhsIindex
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule864 _lhsIprevMaxSimRefs
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule851 #-}
   {-# LINE 867 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule851 = \  (_ :: ()) ->
                      {-# LINE 867 "./src-ag/ExecutionPlan2Hs.ag" #-}
                      0
                      {-# LINE 6554 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule852 #-}
   {-# LINE 893 "./src-ag/ExecutionPlan2Hs.ag" #-}
   rule852 = \  (_ :: ()) ->
                         {-# LINE 893 "./src-ag/ExecutionPlan2Hs.ag" #-}
                         True
                         {-# LINE 6560 "dist/build/ExecutionPlan2Hs.hs"#-}
   {-# INLINE rule853 #-}
   rule853 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule854 #-}
   rule854 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule855 #-}
   rule855 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule856 #-}
   rule856 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule857 #-}
   rule857 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule858 #-}
   rule858 = \  (_ :: ()) ->
     empty
   {-# INLINE rule859 #-}
   rule859 = \  (_ :: ()) ->
     empty
   {-# INLINE rule860 #-}
   rule860 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule861 #-}
   rule861 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule862 #-}
   rule862 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule863 #-}
   rule863 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule864 #-}
   rule864 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs

-- Visits ------------------------------------------------------
-- wrapper
data Inh_Visits  = Inh_Visits { allFromToStates_Inh_Visits :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visits :: (Map NontermIdent Attributes), allInitStates_Inh_Visits :: (Map NontermIdent Int), allSynmap_Inh_Visits :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visits :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_Visits :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allintramap_Inh_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visits :: (Map Identifier Type), childintros_Inh_Visits :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), con_Inh_Visits :: (ConstructorIdent), inhmap_Inh_Visits :: (Attributes), mrules_Inh_Visits :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), nextVisits_Inh_Visits :: (Map StateIdentifier StateCtx), nt_Inh_Visits :: (NontermIdent), options_Inh_Visits :: (Options), params_Inh_Visits :: ([Identifier]), prevVisits_Inh_Visits :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visits :: (Map Identifier (Set String)), ruleuses_Inh_Visits :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visits :: (Attributes), terminaldefs_Inh_Visits :: (Set String) }
data Syn_Visits  = Syn_Visits { allvisits_Syn_Visits :: ([VisitStateState]), childvisit_Syn_Visits :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_Visits :: (Seq Error), fromToStates_Syn_Visits :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visits :: (Set String), ruleKinds_Syn_Visits :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visits :: (Map Identifier Int), sem_visit_Syn_Visits :: ( [(StateIdentifier,Bool -> PP_Doc)] ), t_visits_Syn_Visits :: (PP_Doc), usedArgs_Syn_Visits :: (Set String), visitKinds_Syn_Visits :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visits :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visits :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visits #-}
wrap_Visits :: T_Visits  -> Inh_Visits  -> (Syn_Visits )
wrap_Visits (T_Visits act) (Inh_Visits _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visits_s56 sem arg)
        return (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_Visits_vIn55  = T_Visits_vIn55 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (ConstructorIdent) (Attributes) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visits_vOut55  = T_Visits_vOut55 ([VisitStateState]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) ( [(StateIdentifier,Bool -> PP_Doc)] ) (PP_Doc) (Set String) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visits_Cons #-}
sem_Visits_Cons :: T_Visit  -> T_Visits  -> T_Visits 
sem_Visits_Cons arg_hd_ arg_tl_ = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_Visit (arg_hd_))
         _tlX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_tl_))
         (T_Visit_vOut46 _hdIallvisits _hdIchildvisit _hdIerrors _hdIfromToStates _hdIintramap _hdIlazyIntras _hdIruleKinds _hdIruleUsage _hdIsem_visit _hdIt_visits _hdIusedArgs _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_Visit_s47 _hdX47 (T_Visit_vIn46 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOcon _hdOinhmap _hdOmrules _hdOnextVisits _hdOnt _hdOoptions _hdOparams _hdOprevVisits _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs)
         (T_Visits_vOut55 _tlIallvisits _tlIchildvisit _tlIerrors _tlIfromToStates _tlIintramap _tlIlazyIntras _tlIruleKinds _tlIruleUsage _tlIsem_visit _tlIt_visits _tlIusedArgs _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_Visits_s56 _tlX56 (T_Visits_vIn55 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOcon _tlOinhmap _tlOmrules _tlOnextVisits _tlOnt _tlOoptions _tlOparams _tlOprevVisits _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule865 _hdIallvisits _tlIallvisits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule866 _hdIchildvisit _tlIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule867 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule868 _hdIfromToStates _tlIfromToStates
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule869 _hdIintramap _tlIintramap
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule870 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule871 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule872 _hdIruleUsage _tlIruleUsage
         _lhsOsem_visit ::  [(StateIdentifier,Bool -> PP_Doc)] 
         _lhsOsem_visit = rule873 _hdIsem_visit _tlIsem_visit
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule874 _hdIt_visits _tlIt_visits
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule875 _hdIusedArgs _tlIusedArgs
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule876 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule877 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule878 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule879 _lhsIallFromToStates
         _hdOallInhmap = rule880 _lhsIallInhmap
         _hdOallInitStates = rule881 _lhsIallInitStates
         _hdOallSynmap = rule882 _lhsIallSynmap
         _hdOallVisitKinds = rule883 _lhsIallVisitKinds
         _hdOallchildvisit = rule884 _lhsIallchildvisit
         _hdOallintramap = rule885 _lhsIallintramap
         _hdOavisitdefs = rule886 _lhsIavisitdefs
         _hdOavisituses = rule887 _lhsIavisituses
         _hdOchildTypes = rule888 _lhsIchildTypes
         _hdOchildintros = rule889 _lhsIchildintros
         _hdOcon = rule890 _lhsIcon
         _hdOinhmap = rule891 _lhsIinhmap
         _hdOmrules = rule892 _lhsImrules
         _hdOnextVisits = rule893 _lhsInextVisits
         _hdOnt = rule894 _lhsInt
         _hdOoptions = rule895 _lhsIoptions
         _hdOparams = rule896 _lhsIparams
         _hdOprevVisits = rule897 _lhsIprevVisits
         _hdOruledefs = rule898 _lhsIruledefs
         _hdOruleuses = rule899 _lhsIruleuses
         _hdOsynmap = rule900 _lhsIsynmap
         _hdOterminaldefs = rule901 _lhsIterminaldefs
         _tlOallFromToStates = rule902 _lhsIallFromToStates
         _tlOallInhmap = rule903 _lhsIallInhmap
         _tlOallInitStates = rule904 _lhsIallInitStates
         _tlOallSynmap = rule905 _lhsIallSynmap
         _tlOallVisitKinds = rule906 _lhsIallVisitKinds
         _tlOallchildvisit = rule907 _lhsIallchildvisit
         _tlOallintramap = rule908 _lhsIallintramap
         _tlOavisitdefs = rule909 _lhsIavisitdefs
         _tlOavisituses = rule910 _lhsIavisituses
         _tlOchildTypes = rule911 _lhsIchildTypes
         _tlOchildintros = rule912 _lhsIchildintros
         _tlOcon = rule913 _lhsIcon
         _tlOinhmap = rule914 _lhsIinhmap
         _tlOmrules = rule915 _lhsImrules
         _tlOnextVisits = rule916 _lhsInextVisits
         _tlOnt = rule917 _lhsInt
         _tlOoptions = rule918 _lhsIoptions
         _tlOparams = rule919 _lhsIparams
         _tlOprevVisits = rule920 _lhsIprevVisits
         _tlOruledefs = rule921 _lhsIruledefs
         _tlOruleuses = rule922 _lhsIruleuses
         _tlOsynmap = rule923 _lhsIsynmap
         _tlOterminaldefs = rule924 _lhsIterminaldefs
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule865 #-}
   rule865 = \ ((_hdIallvisits) ::  VisitStateState ) ((_tlIallvisits) :: [VisitStateState]) ->
     _hdIallvisits : _tlIallvisits
   {-# INLINE rule866 #-}
   rule866 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule867 #-}
   rule867 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule868 #-}
   rule868 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule869 #-}
   rule869 = \ ((_hdIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ((_tlIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _hdIintramap `uwMapUnion` _tlIintramap
   {-# INLINE rule870 #-}
   rule870 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule871 #-}
   rule871 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule872 #-}
   rule872 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule873 #-}
   rule873 = \ ((_hdIsem_visit) ::   (StateIdentifier,Bool -> PP_Doc)  ) ((_tlIsem_visit) ::  [(StateIdentifier,Bool -> PP_Doc)] ) ->
     _hdIsem_visit : _tlIsem_visit
   {-# INLINE rule874 #-}
   rule874 = \ ((_hdIt_visits) :: PP_Doc) ((_tlIt_visits) :: PP_Doc) ->
     _hdIt_visits >-< _tlIt_visits
   {-# INLINE rule875 #-}
   rule875 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule876 #-}
   rule876 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule877 #-}
   rule877 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule878 #-}
   rule878 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule879 #-}
   rule879 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule880 #-}
   rule880 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule881 #-}
   rule881 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule882 #-}
   rule882 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule883 #-}
   rule883 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule884 #-}
   rule884 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule885 #-}
   rule885 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule886 #-}
   rule886 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule887 #-}
   rule887 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule888 #-}
   rule888 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule889 #-}
   rule889 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule890 #-}
   rule890 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule891 #-}
   rule891 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule892 #-}
   rule892 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule893 #-}
   rule893 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule894 #-}
   rule894 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule895 #-}
   rule895 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule896 #-}
   rule896 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule897 #-}
   rule897 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule898 #-}
   rule898 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule899 #-}
   rule899 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule900 #-}
   rule900 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule901 #-}
   rule901 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
   {-# INLINE rule902 #-}
   rule902 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule903 #-}
   rule903 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule904 #-}
   rule904 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule905 #-}
   rule905 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule906 #-}
   rule906 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule907 #-}
   rule907 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule908 #-}
   rule908 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule909 #-}
   rule909 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule910 #-}
   rule910 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule911 #-}
   rule911 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule912 #-}
   rule912 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule913 #-}
   rule913 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule914 #-}
   rule914 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule915 #-}
   rule915 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule916 #-}
   rule916 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule917 #-}
   rule917 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule918 #-}
   rule918 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule919 #-}
   rule919 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule920 #-}
   rule920 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule921 #-}
   rule921 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule922 #-}
   rule922 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule923 #-}
   rule923 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule924 #-}
   rule924 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
{-# NOINLINE sem_Visits_Nil #-}
sem_Visits_Nil ::  T_Visits 
sem_Visits_Nil  = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule925  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule926  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule927  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule928  ()
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule929  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule930  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule931  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule932  ()
         _lhsOsem_visit ::  [(StateIdentifier,Bool -> PP_Doc)] 
         _lhsOsem_visit = rule933  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule934  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule935  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule936  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule937  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule938  ()
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule925 #-}
   rule925 = \  (_ :: ()) ->
     []
   {-# INLINE rule926 #-}
   rule926 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule927 #-}
   rule927 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule928 #-}
   rule928 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule929 #-}
   rule929 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule930 #-}
   rule930 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule931 #-}
   rule931 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule932 #-}
   rule932 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule933 #-}
   rule933 = \  (_ :: ()) ->
     []
   {-# INLINE rule934 #-}
   rule934 = \  (_ :: ()) ->
     empty
   {-# INLINE rule935 #-}
   rule935 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule936 #-}
   rule936 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule937 #-}
   rule937 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule938 #-}
   rule938 = \  (_ :: ()) ->
     Map.empty
