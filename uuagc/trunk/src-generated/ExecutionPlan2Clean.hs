{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionPlan2Clean where
{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 10 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 16 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 23 "dist/build/ExecutionPlan2Clean.hs" #-}

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
{-# LINE 37 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 7 "./src-ag/ExecutionPlan2Clean.ag" #-}

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
{-# LINE 64 "dist/build/ExecutionPlan2Clean.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 190 "./src-ag/ExecutionPlan2Clean.ag" #-}

classCtxsToDocs :: ClassContext -> [PP_Doc]
classCtxsToDocs = map toDoc where
  toDoc (ident,args) = (ident >#< ppSpaced (map pp_parens args))

classConstrsToDocs :: [Type] -> [PP_Doc]
classConstrsToDocs = map ppTp

ppClasses :: [PP_Doc] -> PP_Doc
ppClasses [] = empty
ppClasses xs = "|" >#< pp_block "" "" "&" xs

ppQuants :: [Identifier] -> PP_Doc
ppQuants [] = empty
ppQuants ps = "E." >#< ppSpaced ps >#< ":"
{-# LINE 83 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 227 "./src-ag/ExecutionPlan2Clean.ag" #-}

-- first parameter indicates: generate a record or not
ppConFields :: Bool -> [PP_Doc] -> PP_Doc
ppConFields True  flds = ppListSep "{" "}" ", " flds
ppConFields False flds = ppSpaced flds
{-# LINE 91 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 260 "./src-ag/ExecutionPlan2Clean.ag" #-}

ppTp :: Type -> PP_Doc
ppTp = text . typeToHaskellString Nothing []
{-# LINE 97 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 333 "./src-ag/ExecutionPlan2Clean.ag" #-}

isRecordConstructor :: NontermIdent -> Map NontermIdent ConstructorType -> Bool
isRecordConstructor nt ctm = Map.lookup nt ctm == Just RecordConstructor
{-# LINE 103 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 374 "./src-ag/ExecutionPlan2Clean.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 107 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 483 "./src-ag/ExecutionPlan2Clean.ag" #-}

conNmTVisit nt vId      = "T_" >|< nt >|< "_v"    >|< vId
conNmTVisitIn nt vId    = "T_" >|< nt >|< "_vIn"  >|< vId
conNmTVisitOut nt vId   = "T_" >|< nt >|< "_vOut" >|< vId
conNmTNextVisit nt stId = "T_" >|< nt >|< "_s"    >|< stId

ppMonadType :: Options -> PP_Doc
ppMonadType opts
  | parallelInvoke opts = text "IO"
  | otherwise           = text "Identity"
{-# LINE 120 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 652 "./src-ag/ExecutionPlan2Clean.ag" #-}

ppDefor :: Type -> PP_Doc
ppDefor (NT nt args _) = "T_" >|< nt >#< ppSpaced (map pp_parens args)
ppDefor (Haskell s)    = text s
{-# LINE 127 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 756 "./src-ag/ExecutionPlan2Clean.ag" #-}

mklet :: (PP a, PP b, PP c) => a -> b -> c -> PP_Doc
mklet prefix defs body =
  prefix
  >-< indent (length (show prefix))
    ("let"
     >-< indent 4 defs
     >-< "in" >#< body)
{-# LINE 138 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 817 "./src-ag/ExecutionPlan2Clean.ag" #-}

resultValName :: String
resultValName = "ag__result_"

nextStName :: String
nextStName = "ag__st_"
{-# LINE 147 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 878 "./src-ag/ExecutionPlan2Clean.ag" #-}

parResultName :: String
parResultName = "__outcome_"

fmtDecl :: PP a => Bool -> FormatMode -> a -> PP_Doc
fmtDecl declPure fmt decl = case fmt of
  FormatLetDecl -> pp decl
  FormatLetLine -> "let" >#< decl >#< "in"
  FormatDo | declPure  -> "let" >#< decl >#< "in"
           | otherwise -> pp decl
{-# LINE 160 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 992 "./src-ag/ExecutionPlan2Clean.ag" #-}

stname :: Identifier -> Int -> String
stname child st = "st_" ++ getName child ++ "X" ++ show st

-- should actually return some conversion info
compatibleAttach :: VisitKind -> NontermIdent -> Options -> Bool
compatibleAttach _ _ _ = True

unMon :: Options -> PP_Doc
unMon options
  | parallelInvoke options = text "'System.IO.Unsafe'.unsafePerformIO"    -- IO monad
  | otherwise              = text "'Control.Monad.Identity'.runIdentity"  -- identity monad
{-# LINE 175 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1079 "./src-ag/ExecutionPlan2Clean.ag" #-}

dummyPat :: Options -> Bool -> PP_Doc
dummyPat opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty  -- no unnecessary tuples
  | tupleAsDummyToken opts = if strictDummyToken opts
                             then text "Void"
                             else text "(_)"
  | otherwise              = let match | strictDummyToken opts = "!_"
                                       | otherwise             = "_"
                             in pp_parens (match >#< "::" >#< dummyType opts noArgs)
  where match | strictDummyToken opts = "(!_)"
              | otherwise             = "_"

dummyArg :: Options -> Bool -> PP_Doc
dummyArg opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty    -- no unnecessary tuples
  | tupleAsDummyToken opts = text "Void"
  | otherwise              = text "GHC.Prim.realWorld#"

dummyType :: Options -> Bool -> PP_Doc
dummyType opts noArgs
  | not noArgs && tupleAsDummyToken opts = empty     -- no unnecessary tuples
  | tupleAsDummyToken opts = text "Void"
  | otherwise              = text "(GHC.Prim.State# GHC.Prim.RealWorld)"
{-# LINE 202 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1105 "./src-ag/ExecutionPlan2Clean.ag" #-}

-- rules are "deinlined" to prevent needless code duplication.
-- if there is only a bit of duplication, we allow ghc to decide if it is worth it.
-- if the duplication crosses this threshold, however, we tell ghc definitely not to inline it.
ruleInlineThresholdSoft :: Int
ruleInlineThresholdSoft = 3

ruleInlineThresholdHard :: Int
ruleInlineThresholdHard = 5

reallyOftenUsedThreshold :: Int
reallyOftenUsedThreshold = 12
{-# LINE 217 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1172 "./src-ag/ExecutionPlan2Clean.ag" #-}

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
{-# LINE 250 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1259 "./src-ag/ExecutionPlan2Clean.ag" #-}

-- a `compatibleKind` b  means: can kind b be invoked from a
compatibleKind :: VisitKind -> VisitKind -> Bool
compatibleKind _              _             = True

compatibleRule :: VisitKind -> Bool -> Bool
compatibleRule (VisitPure _) False = False
compatibleRule _             _     = True
{-# LINE 261 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1281 "./src-ag/ExecutionPlan2Clean.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 266 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1304 "./src-ag/ExecutionPlan2Clean.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 275 "dist/build/ExecutionPlan2Clean.hs" #-}

{-# LINE 1499 "./src-ag/ExecutionPlan2Clean.ag" #-}

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

cleanIclModuleHeader :: Options -> String -> String
cleanIclModuleHeader flags input
 = case moduleName flags
   of Name nm -> genMod nm
      Default -> genMod (defaultModuleName input)
      NoName  -> ""
   where genMod x = "implementation module " ++ x 

cleanDclModuleHeader :: Options -> String -> Maybe String -> String
cleanDclModuleHeader flags input export
 = case moduleName flags
   of Name nm -> genMod nm
      Default -> genMod (defaultModuleName input)
      NoName  -> ""
   where genMod x = "definition module " ++ x ++ genExp export x
         genExp Nothing _ = ""
         genExp (Just e) x = "(module " ++ x ++ ", module " ++ e ++ ")"

defaultModuleName :: String -> String
defaultModuleName = dropExtension

mkIclModuleHeader :: Maybe (String,String,String) -> String -> String -> String -> Bool -> String
mkIclModuleHeader Nothing defaultName suffix _ _
  = "implementation module " ++ defaultName ++ suffix
mkIclModuleHeader (Just (name, exports, imports)) _ suffix addExports replaceExports
  = "implementation module " ++ name ++ suffix ++ "\n" ++ imports ++ "\n"

mkDclModuleHeader :: Maybe (String,String,String) -> String -> String -> String -> Bool -> String
mkDclModuleHeader Nothing defaultName suffix _ _
  = "definition module " ++ defaultName ++ suffix
mkDclModuleHeader (Just (name, exports, _)) _ suffix addExports replaceExports
  = "definition module " ++ name ++ suffix ++ ex ++ "\n"
  where
    ex  = if null exports || (replaceExports && null addExports)
          then ""
          else if null addExports
               then exports
               else if replaceExports
                    then addExports
                    else exports ++ "," ++ addExports
{-# LINE 336 "dist/build/ExecutionPlan2Clean.hs" #-}
-- EChild ------------------------------------------------------
-- wrapper
data Inh_EChild  = Inh_EChild { allInitStates_Inh_EChild :: (Map NontermIdent Int), con_Inh_EChild :: (ConstructorIdent), constructorTypeMap_Inh_EChild :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_EChild :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_EChild :: (String -> String -> String -> Bool -> String), importBlocks_Inh_EChild :: (PP_Doc), mainFile_Inh_EChild :: (String), mainName_Inh_EChild :: (String), nt_Inh_EChild :: (NontermIdent), options_Inh_EChild :: (Options), textBlocks_Inh_EChild :: (PP_Doc) }
data Syn_EChild  = Syn_EChild { argnamesw_Syn_EChild :: ( PP_Doc ), argpats_Syn_EChild :: (  PP_Doc  ), argtps_Syn_EChild :: (  PP_Doc  ), childTypes_Syn_EChild :: (Map Identifier Type), childintros_Syn_EChild :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), datatype_Syn_EChild :: (PP_Doc), recfields_Syn_EChild :: ( [Identifier] ), recordtype_Syn_EChild :: (PP_Doc), terminaldefs_Syn_EChild :: (Set String), usedArgs_Syn_EChild :: (Set String) }
{-# INLINABLE wrap_EChild #-}
wrap_EChild :: T_EChild  -> Inh_EChild  -> (Syn_EChild )
wrap_EChild (T_EChild act) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks
        (T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChild_s2 sem arg)
        return (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs)
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
data T_EChild_vIn1  = T_EChild_vIn1 (Map NontermIdent Int) (ConstructorIdent) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (String) (String) (NontermIdent) (Options) (PP_Doc)
data T_EChild_vOut1  = T_EChild_vOut1 ( PP_Doc ) (  PP_Doc  ) (  PP_Doc  ) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (PP_Doc) ( [Identifier] ) (PP_Doc) (Set String) (Set String)
{-# NOINLINE sem_EChild_EChild #-}
sem_EChild_EChild :: (Identifier) -> (Type) -> (ChildKind) -> (Bool) -> (Maybe [Identifier]) -> (Bool) -> T_EChild 
sem_EChild_EChild arg_name_ arg_tp_ arg_kind_ arg_hasAround_ _ _ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) -> ( let
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule0 _usedArgs_augmented_f1 _usedArgs_augmented_syn
         _usedArgs_augmented_f1 = rule1 arg_kind_ arg_name_
         _tpDoc = rule2 _addStrict arg_tp_
         _strNm = rule3 _lhsIcon _lhsInt arg_name_
         _field = rule4 _lhsIoptions _strNm _tpDoc
         _recordfield = rule5 _strNm _tpDoc
         _addStrict = rule6 _lhsIoptions
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule7 _field arg_kind_
         _lhsOrecordtype :: PP_Doc
         _lhsOrecordtype = rule8 _recordfield arg_kind_
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule9 _nt arg_kind_ arg_name_
         _lhsOargtps ::   PP_Doc  
         _lhsOargtps = rule10 arg_kind_ arg_tp_
         _argpats = rule11 arg_kind_ arg_name_
         _lhsOrecfields ::  [Identifier] 
         _lhsOrecfields = rule12 arg_kind_ arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule13 _introcode arg_name_
         _isDefor = rule14 arg_tp_
         _valcode = rule15 _isDefor _lhsIoptions _nt arg_kind_ arg_name_
         _aroundcode = rule16 _lhsIoptions arg_hasAround_ arg_name_
         _introcode = rule17 _addbang _aroundcode _initSt _isDefor _lhsIoptions _nt _valcode arg_hasAround_ arg_kind_ arg_name_
         _nt = rule18 arg_tp_
         _addbang = rule19 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule20 arg_name_ arg_tp_
         _initSt = rule21 _lhsIallInitStates _nt
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule22  ()
         _usedArgs_augmented_syn = rule23  ()
         _lhsOargpats ::   PP_Doc  
         _lhsOargpats = rule24 _argpats
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs
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
   {-# LINE 243 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule2 = \ _addStrict tp_ ->
                     {-# LINE 243 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                     {-# LINE 426 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 244 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule3 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 244 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 432 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 245 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule4 = \ ((_lhsIoptions) :: Options) _strNm _tpDoc ->
                     {-# LINE 245 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     if dataRecords _lhsIoptions
                     then _strNm     >#< "::" >#< _tpDoc
                     else _tpDoc
                     {-# LINE 440 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 248 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule5 = \ _strNm _tpDoc ->
                           {-# LINE 248 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           _strNm     >#< "::" >#< _tpDoc
                           {-# LINE 446 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 249 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule6 = \ ((_lhsIoptions) :: Options) ->
                        {-# LINE 249 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        \x -> if strictData _lhsIoptions then "!" >|< x else x
                        {-# LINE 452 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 250 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule7 = \ _field kind_ ->
                             {-# LINE 250 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             case kind_ of
                               ChildAttr -> empty
                               _         -> _field
                             {-# LINE 460 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 254 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule8 = \ _recordfield kind_ ->
                               {-# LINE 254 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               case kind_ of
                                 ChildAttr -> empty
                                 _         -> _recordfield
                               {-# LINE 468 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 342 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule9 = \ _nt kind_ name_ ->
                             {-# LINE 342 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             case kind_ of
                               ChildSyntax     -> "(" >#< "sem_" >|< _nt     >#< name_ >|< "_" >#< ")"
                               ChildAttr       -> empty
                               ChildReplace tp -> "(" >#< "sem_" >|< extractNonterminal tp >#< name_ >|< "_" >#< ")"
                             {-# LINE 477 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 633 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule10 = \ kind_ tp_ ->
                            {-# LINE 633 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            case kind_ of
                              ChildSyntax     -> pp_parens $ ppDefor tp_
                              ChildReplace tp -> pp_parens $ ppDefor tp
                              _               -> empty
                            {-# LINE 486 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 637 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule11 = \ kind_ name_ ->
                           {-# LINE 637 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           case kind_ of
                             ChildSyntax    -> name_ >|< "_"
                             ChildReplace _ -> name_ >|< "_"
                             _              -> empty
                           {-# LINE 495 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 642 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule12 = \ kind_ name_ ->
                             {-# LINE 642 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             case kind_ of
                               ChildSyntax    -> [name_]
                               ChildReplace _ -> [name_]
                               _              -> []
                             {-# LINE 504 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 946 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule13 = \ _introcode name_ ->
                               {-# LINE 946 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               Map.singleton name_ _introcode
                               {-# LINE 510 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 947 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule14 = \ tp_ ->
                               {-# LINE 947 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               case tp_ of
                                 NT _ _ defor -> defor
                                 _            -> False
                               {-# LINE 518 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 950 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule15 = \ _isDefor ((_lhsIoptions) :: Options) _nt kind_ name_ ->
                               {-# LINE 950 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               case kind_ of
                                 ChildSyntax -> "arg_" >|< name_ >|< "_"
                                 ChildAttr   ->
                                                let prefix | not _isDefor     = if lateHigherOrderBinding _lhsIoptions
                                                                                then lateSemNtLabel _nt     >#< lhsname _lhsIoptions True idLateBindingAttr
                                                                                else "sem_" >|< _nt
                                                           | otherwise        = empty
                                                in pp_parens (prefix >#< instname name_)
                                 ChildReplace _ ->
                                                   pp_parens (instname name_ >#< name_ >|< "_")
                               {-# LINE 533 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 961 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule16 = \ ((_lhsIoptions) :: Options) hasAround_ name_ ->
                               {-# LINE 961 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               if hasAround_
                               then locname _lhsIoptions name_ >|< "_around"
                               else empty
                               {-# LINE 541 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 964 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule17 = \ _addbang _aroundcode _initSt _isDefor ((_lhsIoptions) :: Options) _nt _valcode hasAround_ kind_ name_ ->
                               {-# LINE 964 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               \kind fmtMode ->
                                        let pat       = text $ stname name_ _initSt
                                            patStrict = _addbang     pat
                                            attach    = "attach_T_" >|< _nt     >#< pp_parens (_aroundcode     >#< _valcode    )
                                            runAttach = unMon _lhsIoptions >#< pp_parens attach
                                            decl      = case kind of
                                                          VisitPure False -> pat >#< "=" >#< runAttach
                                                          VisitPure True  -> patStrict >#< "=" >#< runAttach
                                                          VisitMonadic    -> attach >#< ">>= \\" >#< patStrict >#< "->"
                                        in if compatibleAttach kind _nt     _lhsIoptions
                                           then Right ( fmtDecl False fmtMode decl
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
                               {-# LINE 572 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 990 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule18 = \ tp_ ->
                            {-# LINE 990 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            extractNonterminal tp_
                            {-# LINE 578 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 1568 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule19 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1568 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 584 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 1620 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule20 = \ name_ tp_ ->
                     {-# LINE 1620 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 590 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 1664 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule21 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) _nt ->
                 {-# LINE 1664 "./src-ag/ExecutionPlan2Clean.ag" #-}
                 Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                 {-# LINE 596 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule22 #-}
   rule22 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule23 #-}
   rule23 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule24 #-}
   rule24 = \ _argpats ->
     _argpats
{-# NOINLINE sem_EChild_ETerm #-}
sem_EChild_ETerm :: (Identifier) -> (Type) -> T_EChild 
sem_EChild_ETerm arg_name_ arg_tp_ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) -> ( let
         _tpDoc = rule25 _addStrict arg_tp_
         _strNm = rule26 _lhsIcon _lhsInt arg_name_
         _field = rule27 _lhsIoptions _strNm _tpDoc
         _recordfield = rule28 _strNm _tpDoc
         _addStrict = rule29 _lhsIoptions
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule30 _field
         _lhsOrecordtype :: PP_Doc
         _lhsOrecordtype = rule31 _recordfield
         _lhsOargnamesw ::  PP_Doc 
         _lhsOargnamesw = rule32 arg_name_
         _lhsOargtps ::   PP_Doc  
         _lhsOargtps = rule33 arg_tp_
         _argpats = rule34 _addbang arg_name_
         _lhsOrecfields ::  [Identifier] 
         _lhsOrecfields = rule35 arg_name_
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule36 arg_name_
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule37 arg_name_
         _addbang = rule38 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule39 arg_name_ arg_tp_
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule40  ()
         _lhsOargpats ::   PP_Doc  
         _lhsOargpats = rule41 _argpats
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule25 #-}
   {-# LINE 243 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule25 = \ _addStrict tp_ ->
                     {-# LINE 243 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                     {-# LINE 648 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 244 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule26 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 244 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 654 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 245 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule27 = \ ((_lhsIoptions) :: Options) _strNm _tpDoc ->
                     {-# LINE 245 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     if dataRecords _lhsIoptions
                     then _strNm     >#< "::" >#< _tpDoc
                     else _tpDoc
                     {-# LINE 662 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 248 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule28 = \ _strNm _tpDoc ->
                           {-# LINE 248 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           _strNm     >#< "::" >#< _tpDoc
                           {-# LINE 668 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 249 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule29 = \ ((_lhsIoptions) :: Options) ->
                        {-# LINE 249 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        \x -> if strictData _lhsIoptions then "!" >|< x else x
                        {-# LINE 674 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 257 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule30 = \ _field ->
                                {-# LINE 257 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                _field
                                {-# LINE 680 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule31 #-}
   {-# LINE 258 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule31 = \ _recordfield ->
                                {-# LINE 258 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                _recordfield
                                {-# LINE 686 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule32 #-}
   {-# LINE 346 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule32 = \ name_ ->
                             {-# LINE 346 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             text $ fieldname name_
                             {-# LINE 692 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule33 #-}
   {-# LINE 648 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule33 = \ tp_ ->
                             {-# LINE 648 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             pp_parens $ show tp_
                             {-# LINE 698 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule34 #-}
   {-# LINE 649 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule34 = \ _addbang name_ ->
                             {-# LINE 649 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             _addbang     $ text $ fieldname name_
                             {-# LINE 704 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule35 #-}
   {-# LINE 650 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule35 = \ name_ ->
                             {-# LINE 650 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             [name_]
                             {-# LINE 710 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 945 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule36 = \ name_ ->
                               {-# LINE 945 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               Map.singleton name_ (\_ _ -> Right (empty, Set.empty, Map.empty))
                               {-# LINE 716 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 1318 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule37 = \ name_ ->
                       {-# LINE 1318 "./src-ag/ExecutionPlan2Clean.ag" #-}
                       Set.singleton $ fieldname name_
                       {-# LINE 722 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 1569 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule38 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1569 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 728 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule39 #-}
   {-# LINE 1620 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule39 = \ name_ tp_ ->
                     {-# LINE 1620 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 734 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule40 #-}
   rule40 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule41 #-}
   rule41 = \ _argpats ->
     _argpats

-- EChildren ---------------------------------------------------
-- wrapper
data Inh_EChildren  = Inh_EChildren { allInitStates_Inh_EChildren :: (Map NontermIdent Int), con_Inh_EChildren :: (ConstructorIdent), constructorTypeMap_Inh_EChildren :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_EChildren :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_EChildren :: (String -> String -> String -> Bool -> String), importBlocks_Inh_EChildren :: (PP_Doc), mainFile_Inh_EChildren :: (String), mainName_Inh_EChildren :: (String), nt_Inh_EChildren :: (NontermIdent), options_Inh_EChildren :: (Options), textBlocks_Inh_EChildren :: (PP_Doc) }
data Syn_EChildren  = Syn_EChildren { argnamesw_Syn_EChildren :: ([PP_Doc]), argpats_Syn_EChildren :: ( [PP_Doc] ), argtps_Syn_EChildren :: ( [PP_Doc] ), childTypes_Syn_EChildren :: (Map Identifier Type), childintros_Syn_EChildren :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), datatype_Syn_EChildren :: ([PP_Doc]), recfields_Syn_EChildren :: ( [Identifier] ), recordtype_Syn_EChildren :: ([PP_Doc]), terminaldefs_Syn_EChildren :: (Set String), usedArgs_Syn_EChildren :: (Set String) }
{-# INLINABLE wrap_EChildren #-}
wrap_EChildren :: T_EChildren  -> Inh_EChildren  -> (Syn_EChildren )
wrap_EChildren (T_EChildren act) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks
        (T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChildren_s5 sem arg)
        return (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs)
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
data T_EChildren_vIn4  = T_EChildren_vIn4 (Map NontermIdent Int) (ConstructorIdent) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (String) (String) (NontermIdent) (Options) (PP_Doc)
data T_EChildren_vOut4  = T_EChildren_vOut4 ([PP_Doc]) ( [PP_Doc] ) ( [PP_Doc] ) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ([PP_Doc]) ( [Identifier] ) ([PP_Doc]) (Set String) (Set String)
{-# NOINLINE sem_EChildren_Cons #-}
sem_EChildren_Cons :: T_EChild  -> T_EChildren  -> T_EChildren 
sem_EChildren_Cons arg_hd_ arg_tl_ = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_EChild (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_tl_))
         (T_EChild_vOut1 _hdIargnamesw _hdIargpats _hdIargtps _hdIchildTypes _hdIchildintros _hdIdatatype _hdIrecfields _hdIrecordtype _hdIterminaldefs _hdIusedArgs) = inv_EChild_s2 _hdX2 (T_EChild_vIn1 _hdOallInitStates _hdOcon _hdOconstructorTypeMap _hdOdclModuleHeader _hdOiclModuleHeader _hdOimportBlocks _hdOmainFile _hdOmainName _hdOnt _hdOoptions _hdOtextBlocks)
         (T_EChildren_vOut4 _tlIargnamesw _tlIargpats _tlIargtps _tlIchildTypes _tlIchildintros _tlIdatatype _tlIrecfields _tlIrecordtype _tlIterminaldefs _tlIusedArgs) = inv_EChildren_s5 _tlX5 (T_EChildren_vIn4 _tlOallInitStates _tlOcon _tlOconstructorTypeMap _tlOdclModuleHeader _tlOiclModuleHeader _tlOimportBlocks _tlOmainFile _tlOmainName _tlOnt _tlOoptions _tlOtextBlocks)
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule42 _hdIargnamesw _tlIargnamesw
         _lhsOargpats ::  [PP_Doc] 
         _lhsOargpats = rule43 _hdIargpats _tlIargpats
         _lhsOargtps ::  [PP_Doc] 
         _lhsOargtps = rule44 _hdIargtps _tlIargtps
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule45 _hdIchildTypes _tlIchildTypes
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule46 _hdIchildintros _tlIchildintros
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule47 _hdIdatatype _tlIdatatype
         _lhsOrecfields ::  [Identifier] 
         _lhsOrecfields = rule48 _hdIrecfields _tlIrecfields
         _lhsOrecordtype :: [PP_Doc]
         _lhsOrecordtype = rule49 _hdIrecordtype _tlIrecordtype
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule50 _hdIterminaldefs _tlIterminaldefs
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule51 _hdIusedArgs _tlIusedArgs
         _hdOallInitStates = rule52 _lhsIallInitStates
         _hdOcon = rule53 _lhsIcon
         _hdOconstructorTypeMap = rule54 _lhsIconstructorTypeMap
         _hdOdclModuleHeader = rule55 _lhsIdclModuleHeader
         _hdOiclModuleHeader = rule56 _lhsIiclModuleHeader
         _hdOimportBlocks = rule57 _lhsIimportBlocks
         _hdOmainFile = rule58 _lhsImainFile
         _hdOmainName = rule59 _lhsImainName
         _hdOnt = rule60 _lhsInt
         _hdOoptions = rule61 _lhsIoptions
         _hdOtextBlocks = rule62 _lhsItextBlocks
         _tlOallInitStates = rule63 _lhsIallInitStates
         _tlOcon = rule64 _lhsIcon
         _tlOconstructorTypeMap = rule65 _lhsIconstructorTypeMap
         _tlOdclModuleHeader = rule66 _lhsIdclModuleHeader
         _tlOiclModuleHeader = rule67 _lhsIiclModuleHeader
         _tlOimportBlocks = rule68 _lhsIimportBlocks
         _tlOmainFile = rule69 _lhsImainFile
         _tlOmainName = rule70 _lhsImainName
         _tlOnt = rule71 _lhsInt
         _tlOoptions = rule72 _lhsIoptions
         _tlOtextBlocks = rule73 _lhsItextBlocks
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule42 #-}
   rule42 = \ ((_hdIargnamesw) ::  PP_Doc ) ((_tlIargnamesw) :: [PP_Doc]) ->
     _hdIargnamesw : _tlIargnamesw
   {-# INLINE rule43 #-}
   rule43 = \ ((_hdIargpats) ::   PP_Doc  ) ((_tlIargpats) ::  [PP_Doc] ) ->
     _hdIargpats : _tlIargpats
   {-# INLINE rule44 #-}
   rule44 = \ ((_hdIargtps) ::   PP_Doc  ) ((_tlIargtps) ::  [PP_Doc] ) ->
     _hdIargtps : _tlIargtps
   {-# INLINE rule45 #-}
   rule45 = \ ((_hdIchildTypes) :: Map Identifier Type) ((_tlIchildTypes) :: Map Identifier Type) ->
     _hdIchildTypes `mappend` _tlIchildTypes
   {-# INLINE rule46 #-}
   rule46 = \ ((_hdIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ((_tlIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _hdIchildintros `Map.union` _tlIchildintros
   {-# INLINE rule47 #-}
   rule47 = \ ((_hdIdatatype) :: PP_Doc) ((_tlIdatatype) :: [PP_Doc]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule48 #-}
   rule48 = \ ((_hdIrecfields) ::  [Identifier] ) ((_tlIrecfields) ::  [Identifier] ) ->
     _hdIrecfields ++ _tlIrecfields
   {-# INLINE rule49 #-}
   rule49 = \ ((_hdIrecordtype) :: PP_Doc) ((_tlIrecordtype) :: [PP_Doc]) ->
     _hdIrecordtype : _tlIrecordtype
   {-# INLINE rule50 #-}
   rule50 = \ ((_hdIterminaldefs) :: Set String) ((_tlIterminaldefs) :: Set String) ->
     _hdIterminaldefs `Set.union` _tlIterminaldefs
   {-# INLINE rule51 #-}
   rule51 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule63 #-}
   rule63 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule64 #-}
   rule64 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule65 #-}
   rule65 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule67 #-}
   rule67 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule68 #-}
   rule68 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
{-# NOINLINE sem_EChildren_Nil #-}
sem_EChildren_Nil ::  T_EChildren 
sem_EChildren_Nil  = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsItextBlocks) -> ( let
         _lhsOargnamesw :: [PP_Doc]
         _lhsOargnamesw = rule74  ()
         _lhsOargpats ::  [PP_Doc] 
         _lhsOargpats = rule75  ()
         _lhsOargtps ::  [PP_Doc] 
         _lhsOargtps = rule76  ()
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule77  ()
         _lhsOchildintros :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         _lhsOchildintros = rule78  ()
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule79  ()
         _lhsOrecfields ::  [Identifier] 
         _lhsOrecfields = rule80  ()
         _lhsOrecordtype :: [PP_Doc]
         _lhsOrecordtype = rule81  ()
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule82  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule83  ()
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOrecfields _lhsOrecordtype _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule74 #-}
   rule74 = \  (_ :: ()) ->
     []
   {-# INLINE rule75 #-}
   rule75 = \  (_ :: ()) ->
     []
   {-# INLINE rule76 #-}
   rule76 = \  (_ :: ()) ->
     []
   {-# INLINE rule77 #-}
   rule77 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule78 #-}
   rule78 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     []
   {-# INLINE rule80 #-}
   rule80 = \  (_ :: ()) ->
     []
   {-# INLINE rule81 #-}
   rule81 = \  (_ :: ()) ->
     []
   {-# INLINE rule82 #-}
   rule82 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule83 #-}
   rule83 = \  (_ :: ()) ->
     Set.empty

-- ENonterminal ------------------------------------------------
-- wrapper
data Inh_ENonterminal  = Inh_ENonterminal { allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminal :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), constructorTypeMap_Inh_ENonterminal :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String), derivings_Inh_ENonterminal :: (Derivings), iclModuleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String), importBlocks_Inh_ENonterminal :: (PP_Doc), inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminal :: (String), mainName_Inh_ENonterminal :: (String), options_Inh_ENonterminal :: (Options), synmap_Inh_ENonterminal :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminal :: (PP_Doc), typeSyns_Inh_ENonterminal :: (TypeSyns), wrappers_Inh_ENonterminal :: (Set NontermIdent) }
data Syn_ENonterminal  = Syn_ENonterminal { appendCommon_Syn_ENonterminal :: ( PP_Doc ), appendMain_Syn_ENonterminal :: ( PP_Doc ), childvisit_Syn_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_ENonterminal :: (Seq Error), fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_ENonterminal :: (IO ()), imports_Syn_ENonterminal :: ([PP_Doc]), initStates_Syn_ENonterminal :: (Map NontermIdent Int), output_Syn_ENonterminal :: (PP_Doc), output_dcl_Syn_ENonterminal :: (PP_Doc), semFunBndDefs_Syn_ENonterminal :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminal :: (Seq PP_Doc), visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminal #-}
wrap_ENonterminal :: T_ENonterminal  -> Inh_ENonterminal  -> (Syn_ENonterminal )
wrap_ENonterminal (T_ENonterminal act) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminal_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminal_s8 sem arg)
        return (Syn_ENonterminal _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_ENonterminal_vIn7  = T_ENonterminal_vIn7 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (Derivings) (String -> String -> String -> Bool -> String) (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (Options) (Map NontermIdent Attributes) (PP_Doc) (TypeSyns) (Set NontermIdent)
data T_ENonterminal_vOut7  = T_ENonterminal_vOut7 ( PP_Doc ) ( PP_Doc ) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Map NontermIdent Int) (PP_Doc) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminal_ENonterminal #-}
sem_ENonterminal_ENonterminal :: (NontermIdent) -> ([Identifier]) -> (ClassContext) -> (StateIdentifier) -> (Maybe VisitIdentifier) -> (Map StateIdentifier StateCtx) -> (Map StateIdentifier StateCtx) -> T_EProductions  -> (Bool) -> (HigherOrderInfo) -> T_ENonterminal 
sem_ENonterminal_ENonterminal arg_nt_ arg_params_ arg_classCtxs_ arg_initial_ arg_initialv_ arg_nextVisits_ arg_prevVisits_ arg_prods_ _ _ = T_ENonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_ENonterminal_v7 
      v7 = \ (T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_prods_))
         (T_EProductions_vOut16 _prodsIallvisits _prodsIchildvisit _prodsIcount _prodsIdatatype _prodsIerrors _prodsIfromToStates _prodsIgenProdIO _prodsIimports _prodsIrecordtype _prodsIsemFunBndDefs _prodsIsemFunBndTps _prodsIsem_nt _prodsIsem_prod _prodsIsem_prod_tys _prodsIt_visits _prodsIvisitKinds _prodsIvisitdefs _prodsIvisituses) = inv_EProductions_s17 _prodsX17 (T_EProductions_vIn16 _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOclassCtxs _prodsOconstructorTypeMap _prodsOdclModuleHeader _prodsOiclModuleHeader _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOprevVisits _prodsOrename _prodsOsynmap _prodsOtextBlocks)
         _prodsOrename = rule84 _lhsIoptions
         _prodsOnt = rule85 arg_nt_
         _prodsOparams = rule86 arg_params_
         _prodsOclassCtxs = rule87 arg_classCtxs_
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule88 _hasWrapper _k_states _lhsIoptions _prodsIsem_prod _sem_nt _t_init_icl _t_states_icl _wr_inh_icl _wr_syn_icl _wrapper_icl arg_nt_
         _hasWrapper = rule89 _lhsIwrappers arg_nt_
         _lhsOoutput_dcl :: PP_Doc
         _lhsOoutput_dcl = rule90 _datatype _hasWrapper _lhsIoptions _prodsIsem_prod_tys _prodsIt_visits _sem_tp _semname _t_init_dcl _t_states_dcl _wr_inh_dcl _wr_syn_dcl _wrapper_dcl arg_nt_
         _classPP = rule91 arg_classCtxs_
         _aliasPre = rule92 _classPP _t_params arg_nt_
         _datatype = rule93 _aliasPre _classPP _derivings _lhsIconstructorTypeMap _lhsItypeSyns _prodsIdatatype _prodsIrecordtype _t_params arg_nt_
         _derivings = rule94 _lhsIderivings arg_nt_
         _fsemname = rule95  ()
         _semname = rule96 _fsemname arg_nt_
         _frecarg = rule97 _fsemname
         _sem_tp = rule98 _classPP _quantPP _t_params _t_type arg_nt_
         _quantPP = rule99 arg_params_
         _sem_nt = rule100 _frecarg _fsemname _lhsItypeSyns _prodsIsem_nt _sem_tp _semname arg_nt_
         (Just _prodsOinhmap) = rule101 _lhsIinhmap arg_nt_
         (Just _prodsOsynmap) = rule102 _lhsIsynmap arg_nt_
         _prodsOallInhmap = rule103 _lhsIinhmap
         _prodsOallSynmap = rule104 _lhsIsynmap
         _outedges = rule105 _prodsIallvisits
         _inedges = rule106 _prodsIallvisits
         _allstates = rule107 _inedges _outedges arg_initial_
         _stvisits = rule108 _prodsIallvisits
         _t_type = rule109 arg_nt_
         _lt_type = rule110 arg_nt_
         _t_params = rule111 arg_params_
         _t_init_icl = rule112 _lt_type _t_init_dcl _t_type
         _t_init_dcl = rule113 _lhsIoptions _t_params _t_type arg_initial_
         _t_states_icl = rule114 _allstates arg_nextVisits_ arg_nt_
         _t_states_dcl = rule115 _allstates _t_params arg_nextVisits_ arg_nt_
         _k_type = rule116 arg_nt_
         _k_states = rule117 _allstates _k_type _prodsIallvisits _t_params _t_type arg_nextVisits_ arg_nt_
         _wr_inh_icl = rule118 _genwrap_icl _wr_inhs
         _wr_syn_icl = rule119 _genwrap_icl _wr_syns
         _genwrap_icl = rule120 _addbang arg_nt_
         _wr_inh_dcl = rule121 _genwrap_dcl _wr_inhs
         _wr_syn_dcl = rule122 _genwrap_dcl _wr_syns
         _genwrap_dcl = rule123 _addbang _t_params arg_nt_
         _synAttrs = rule124 _lhsIinhmap arg_nt_
         _wr_inhs = rule125 _synAttrs _wr_filter
         _wr_inhs1 = rule126 _synAttrs
         _wr_filter = rule127 _lhsIoptions
         _wr_syns = rule128 _lhsIsynmap arg_nt_
         _inhlist = rule129 _lhsIoptions _wr_inhs
         _inhlist1 = rule130 _lhsIoptions _wr_inhs1
         _synlist = rule131 _lhsIoptions _wr_syns
         _wrapname = rule132 arg_nt_
         _inhname = rule133 arg_nt_
         _synname = rule134 arg_nt_
         _firstVisitInfo = rule135 arg_initial_ arg_nextVisits_
         _wrapper_icl = rule136 _addbang _addbangWrap _classPP _firstVisitInfo _inhlist _inhlist1 _inhname _k_type _lhsIallVisitKinds _lhsImainName _lhsIoptions _quantPP _synlist _synname _t_params _t_type _wrapname arg_initial_ arg_initialv_ arg_nt_
         _wrapper_dcl = rule137 _classPP _inhname _lhsIoptions _quantPP _synname _t_params _t_type _wrapname
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule138 _prodsIsemFunBndDefs _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule139 _prodsIsemFunBndTps _semFunBndTp
         _semFunBndDef = rule140 _semFunBndNm _semname
         _semFunBndTp = rule141 _semFunBndNm _sem_tp
         _semFunBndNm = rule142 arg_nt_
         _prodsOinitial = rule143 arg_initial_
         _prodsOallstates = rule144 _allstates
         _lhsOappendMain ::  PP_Doc 
         _lhsOappendMain = rule145 _lhsIwrappers _sem_nt _wr_inh_icl _wr_syn_icl _wrapper_icl arg_nt_
         _lhsOappendCommon ::  PP_Doc 
         _lhsOappendCommon = rule146 _datatype _k_states _lhsIoptions _prodsIt_visits _t_init_icl _t_states_icl
         _addbang = rule147 _lhsIoptions
         _addbangWrap = rule148  ()
         _prodsOnextVisits = rule149 arg_nextVisits_
         _prodsOprevVisits = rule150 arg_prevVisits_
         _prodsOlocalAttrTypes = rule151 _lhsIlocalAttrTypes arg_nt_
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule152 arg_initial_ arg_nt_
         _ntType = rule153 arg_nt_ arg_params_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule154 _prodsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule155 _prodsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule156 _prodsIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule157 _prodsIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule158 _prodsIimports
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule159 _prodsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule160 _prodsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule161 _prodsIvisituses
         _prodsOallFromToStates = rule162 _lhsIallFromToStates
         _prodsOallInitStates = rule163 _lhsIallInitStates
         _prodsOallVisitKinds = rule164 _lhsIallVisitKinds
         _prodsOallchildvisit = rule165 _lhsIallchildvisit
         _prodsOavisitdefs = rule166 _lhsIavisitdefs
         _prodsOavisituses = rule167 _lhsIavisituses
         _prodsOconstructorTypeMap = rule168 _lhsIconstructorTypeMap
         _prodsOdclModuleHeader = rule169 _lhsIdclModuleHeader
         _prodsOiclModuleHeader = rule170 _lhsIiclModuleHeader
         _prodsOimportBlocks = rule171 _lhsIimportBlocks
         _prodsOmainFile = rule172 _lhsImainFile
         _prodsOmainName = rule173 _lhsImainName
         _prodsOntType = rule174 _ntType
         _prodsOoptions = rule175 _lhsIoptions
         _prodsOtextBlocks = rule176 _lhsItextBlocks
         __result_ = T_ENonterminal_vOut7 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminal_s8 v7
   {-# INLINE rule84 #-}
   {-# LINE 58 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule84 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 58 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  rename _lhsIoptions
                                  {-# LINE 1140 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 66 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule85 = \ nt_ ->
                              {-# LINE 66 "./src-ag/ExecutionPlan2Clean.ag" #-}
                              nt_
                              {-# LINE 1146 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 78 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule86 = \ params_ ->
                   {-# LINE 78 "./src-ag/ExecutionPlan2Clean.ag" #-}
                   params_
                   {-# LINE 1152 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 82 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule87 = \ classCtxs_ ->
                      {-# LINE 82 "./src-ag/ExecutionPlan2Clean.ag" #-}
                      classCtxs_
                      {-# LINE 1158 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 102 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule88 = \ _hasWrapper _k_states ((_lhsIoptions) :: Options) ((_prodsIsem_prod) :: PP_Doc) _sem_nt _t_init_icl _t_states_icl _wr_inh_icl _wr_syn_icl _wrapper_icl nt_ ->
                                {-# LINE 102 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                ("// " ++ getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-')
                                >-< (if _hasWrapper
                                     then "// wrapper"
                                          >-< _wr_inh_icl
                                          >-< _wr_syn_icl
                                          >-< _wrapper_icl
                                          >-< ""
                                     else empty)
                                >-< (if   folds _lhsIoptions
                                     then "// cata"
                                          >-< _sem_nt
                                          >-< ""
                                     else empty)
                                >-< (if   semfuns _lhsIoptions
                                     then "// semantic domain"
                                          >-< _t_init_icl
                                          >-< _t_states_icl
                                          >-< _k_states
                                          >-< _prodsIsem_prod
                                          >-< ""
                                     else empty)
                                {-# LINE 1184 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 123 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule89 = \ ((_lhsIwrappers) :: Set NontermIdent) nt_ ->
                                    {-# LINE 123 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    nt_ `Set.member` _lhsIwrappers
                                    {-# LINE 1190 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 125 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule90 = \ _datatype _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIsem_prod_tys) :: PP_Doc) ((_prodsIt_visits) :: PP_Doc) _sem_tp _semname _t_init_dcl _t_states_dcl _wr_inh_dcl _wr_syn_dcl _wrapper_dcl nt_ ->
                                    {-# LINE 125 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    ("// " ++ getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-')
                                    >-< (if dataTypes _lhsIoptions
                                         then "// data"
                                              >-< _datatype
                                              >-< ""
                                         else empty)
                                    >-< (if _hasWrapper
                                         then "// wrapper"
                                              >-< _wr_inh_dcl
                                              >-< _wr_syn_dcl
                                              >-< _wrapper_dcl
                                              >-< ""
                                         else empty)
                                    >-< (if   folds _lhsIoptions
                                         then "// cata"
                                              >-< _semname     >#< "::" >#< _sem_tp
                                              >-< ""
                                         else empty)
                                    >-< (if   semfuns _lhsIoptions
                                         then "// semantic domain"
                                              >-< _t_init_dcl
                                              >-< _t_states_dcl
                                              >-< _prodsIt_visits
                                              >-< _prodsIsem_prod_tys
                                              >-< ""
                                         else empty)
                                    {-# LINE 1221 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 163 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule91 = \ classCtxs_ ->
                                  {-# LINE 163 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  ppClasses $ classCtxsToDocs classCtxs_
                                  {-# LINE 1227 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 164 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule92 = \ _classPP _t_params nt_ ->
                                  {-# LINE 164 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  "::" >#< _classPP     >#< nt_ >#< _t_params     >#< ":=="
                                  {-# LINE 1233 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 165 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule93 = \ _aliasPre _classPP _derivings ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype) :: [PP_Doc]) ((_prodsIrecordtype) :: PP_Doc) _t_params nt_ ->
                                  {-# LINE 165 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  case lookup nt_ _lhsItypeSyns of
                                     Nothing -> "::" >#< _classPP     >#< nt_ >#< _t_params
                                                >-< ( if null _prodsIdatatype
                                                      then empty
                                                      else if isRecordConstructor nt_ _lhsIconstructorTypeMap
                                                             then  indent 2 $ "=" >#< _prodsIrecordtype
                                                             else  indent 2 $ vlist $ ( ("=" >#< head _prodsIdatatype)
                                                                                    : (map ("|" >#<) $ tail _prodsIdatatype))
                                                    )
                                                >-< indent 2 _derivings
                                     Just (List t)     -> _aliasPre     >#< "[" >#< show t >#< "]"
                                     Just (Maybe t)    -> _aliasPre     >#< "Data.Maybe" >#< pp_parens (show t)
                                     Just (Tuple ts)   -> _aliasPre     >#< pp_parens (ppCommas $ map (show . snd) ts)
                                     Just (Either l r) -> _aliasPre     >#< "Data.Either" >#< pp_parens (show l) >#< pp_parens (show r)
                                     Just (Map k v)    -> _aliasPre     >#< "Data.Map" >#< pp_parens (show k) >#< pp_parens (show v)
                                     Just (IntMap t)   -> _aliasPre     >#< "Data.IntMap.IntMap" >#< pp_parens (show t)
                                     Just (OrdSet t)   -> _aliasPre     >#< "Data.Set.Set" >#< pp_parens (show t)
                                     Just IntSet       -> _aliasPre     >#< "Data.IntSet.IntSet"
                                  {-# LINE 1256 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 184 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule94 = \ ((_lhsIderivings) :: Derivings) nt_ ->
                                   {-# LINE 184 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Nothing -> empty
                                      Just s  -> if   Set.null s
                                                 then empty
                                                 else "deriving" >#< (pp_parens $ ppCommas $ map pp $ Set.toList s)
                                   {-# LINE 1266 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule95 #-}
   {-# LINE 269 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule95 = \  (_ :: ()) ->
                                  {-# LINE 269 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  \x -> "sem_" ++ show x
                                  {-# LINE 1272 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule96 #-}
   {-# LINE 270 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule96 = \ _fsemname nt_ ->
                                 {-# LINE 270 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _fsemname     nt_
                                 {-# LINE 1278 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule97 #-}
   {-# LINE 271 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule97 = \ _fsemname ->
                                 {-# LINE 271 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 \t x -> case t of
                                            NT nt _ _ -> pp_parens (_fsemname nt >#< x)
                                            _         -> pp x
                                 {-# LINE 1286 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule98 #-}
   {-# LINE 277 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule98 = \ _classPP _quantPP _t_params _t_type nt_ ->
                                 {-# LINE 277 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _quantPP     >#< _classPP     >#< nt_ >#< _t_params     >#< "->" >#< _t_type     >#< _t_params
                                 {-# LINE 1292 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule99 #-}
   {-# LINE 278 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule99 = \ params_ ->
                                 {-# LINE 278 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppQuants params_
                                 {-# LINE 1298 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule100 #-}
   {-# LINE 280 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule100 = \ _frecarg _fsemname ((_lhsItypeSyns) :: TypeSyns) ((_prodsIsem_nt) :: PP_Doc) _sem_tp _semname nt_ ->
                                 {-# LINE 280 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _semname     >#< "::" >#< _sem_tp
                                 >-< case lookup nt_ _lhsItypeSyns of
                                        Nothing -> _prodsIsem_nt
                                        Just (List t) -> _semname     >#< "list" >#< "=" >#< "foldr" >#< _semname     >|< "_Cons"
                                                         >#< _semname     >|< "_Nil"
                                                         >#< case t of
                                                                NT nt _ _ -> pp_parens ("map" >#< _fsemname nt >#< "list")
                                                                _         -> pp "list"
                                        Just (Maybe t) -> _semname     >#< "'Data.Maybe'.Nothing" >#< "=" >#< _semname     >|< "_Nothing"
                                                          >-< _semname     >#< pp_parens ("'Data.Maybe'.Just just") >#< "="
                                                          >#< _semname     >|< "_Just" >#< _frecarg t "just"
                                        Just (Tuple ts) -> _semname     >#< pp_parens (ppCommas $ map fst ts) >#< "="
                                                           >#< _semname     >|< "_Tuple" >#< ppSpaced (map (\t -> _frecarg (snd t) (show $ fst t)) ts)
                                        Just (Either l r) -> _semname     >#< "('Data.Either'.Left left)" >#< "=" >#< _semname     >|< "_Left" >#< _frecarg l "left"
                                                             >-< _semname     >#< "('Data.Either'.Right right)" >#< "=" >#< _semname     >|< "_Right" >#< _frecarg r "right"
                                        Just (Map k v) -> _semname     >#< "m" >#< "=" >#< "'Data.Map'.foldrWithKey"
                                                          >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                          >#< case v of
                                                                 NT nt _ _ -> pp_parens ("'Data.Map'.map" >#< _fsemname nt >#< "m")
                                                                 _         -> pp "m"
                                        Just (IntMap v) -> _semname     >#< "m" >#< "=" >#< "'Data.IntMap'.foldWithKey"
                                                           >#< _semname     >|< "_Entry" >#< _semname     >|< "_Nil"
                                                           >#< case v of
                                                                  NT nt _ _ -> pp_parens ("'Data.IntMap'.map" >#< _fsemname nt >#< "m")
                                                                  _         -> pp "m"
                                        Just (OrdSet t) -> _semname     >#< "s" >#< "=" >#< "foldr" >#< _semname     >|< "_Entry"
                                                           >#< _semname     >|< "_Nil"
                                                           >#< pp_parens (
                                                                 ( case t of
                                                                     NT nt _ _ -> pp_parens ("map" >#< _fsemname nt)
                                                                     _         -> empty
                                                                 ) >#< pp_parens ("'Data.IntSet'.elems" >#< "s")
                                                               )
                                        Just IntSet     -> _semname     >#< "s" >#< "=" >#< "foldr" >#< _semname     >|< "_Entry"
                                                           >#< _semname     >|< "_Nil"
                                                           >#< pp_parens ("'Data.IntSet'.elems" >#< "s")
                                 {-# LINE 1339 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule101 #-}
   {-# LINE 366 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule101 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 366 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                         Map.lookup nt_ _lhsIinhmap
                                         {-# LINE 1345 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule102 #-}
   {-# LINE 367 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule102 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 367 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                         Map.lookup nt_ _lhsIsynmap
                                         {-# LINE 1351 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule103 #-}
   {-# LINE 368 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule103 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 368 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _lhsIinhmap
                                     {-# LINE 1357 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule104 #-}
   {-# LINE 369 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule104 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 369 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _lhsIsynmap
                                     {-# LINE 1363 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule105 #-}
   {-# LINE 390 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule105 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 390 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 1369 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule106 #-}
   {-# LINE 391 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule106 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 391 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 1375 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 392 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule107 = \ _inedges _outedges initial_ ->
                                   {-# LINE 392 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 1381 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule108 #-}
   {-# LINE 393 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule108 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 393 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1387 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule109 #-}
   {-# LINE 394 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule109 = \ nt_ ->
                                   {-# LINE 394 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   "T_" >|< nt_
                                   {-# LINE 1393 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule110 #-}
   {-# LINE 395 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule110 = \ nt_ ->
                                   {-# LINE 395 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   "t_" >|< nt_
                                   {-# LINE 1399 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule111 #-}
   {-# LINE 396 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule111 = \ params_ ->
                                   {-# LINE 396 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   ppSpaced params_
                                   {-# LINE 1405 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule112 #-}
   {-# LINE 397 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule112 = \ _lt_type _t_init_dcl _t_type ->
                                       {-# LINE 397 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       _t_init_dcl     >-<
                                       "attach_" >|< _t_type     >#< pp_parens (_t_type     >#< _lt_type    ) >#< "=" >#< _lt_type
                                       {-# LINE 1412 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule113 #-}
   {-# LINE 399 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule113 = \ ((_lhsIoptions) :: Options) _t_params _t_type initial_ ->
                                       {-# LINE 399 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       "::" >#< _t_type     >#< _t_params     >#< "=" >#< _t_type
                                       >#<
                                       pp_parens (
                                       ppMonadType _lhsIoptions >#< pp_parens (_t_type     >|< "_s" >|< initial_ >#< _t_params    ))
                                       {-# LINE 1421 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule114 #-}
   {-# LINE 403 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule114 = \ _allstates nextVisits_ nt_ ->
                                       {-# LINE 403 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       vlist $ map (\st ->
                                         let nt_st = nt_ >|< "_s" >|< st
                                             c_st  = "C_" >|< nt_st
                                             inv_st  = "inv_" >|< nt_st
                                             nextVisit = Map.findWithDefault ManyVis st nextVisits_
                                         in  case nextVisit of
                                               NoneVis    -> empty
                                               OneVis vId -> inv_st >#< pp_parens (c_st >#< "x") >#< "=" >#< "x"
                                               ManyVis    -> empty
                                             ) $ Set.toList _allstates
                                       {-# LINE 1436 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule115 #-}
   {-# LINE 413 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule115 = \ _allstates _t_params nextVisits_ nt_ ->
                                       {-# LINE 413 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       vlist $ map (\st ->
                                         let nt_st   = nt_ >|< "_s" >|< st
                                             t_st    = "T_" >|< nt_st
                                             k_st    = "K_" >|< nt_st
                                             c_st    = "C_" >|< nt_st
                                             inv_st  = "inv_" >|< nt_st
                                             nextVisit = Map.findWithDefault ManyVis st nextVisits_
                                         in  case nextVisit of
                                               NoneVis    -> "::" >#< t_st >#< _t_params     >#< "=" >#< c_st
                                               OneVis vId -> "::" >#< t_st >#< _t_params     >#< "=" >#< c_st >#< (pp_parens (conNmTVisit nt_ vId >#< _t_params    ))
                                               ManyVis    -> "::" >#< t_st >#< _t_params     >#< "where" >#< c_st >#< "::"
                                                               >#< (pp_braces $ inv_st >#< "::" >#< "!" >|< pp_parens ("E.t:" >#< k_st >#< _t_params     >#< "t" >#< "->" >#< "t"))
                                                               >#< "->" >#< t_st >#< _t_params
                                             ) $ Set.toList _allstates
                                       {-# LINE 1455 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule116 #-}
   {-# LINE 430 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule116 = \ nt_ ->
                                  {-# LINE 430 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  "K_" ++ show nt_
                                  {-# LINE 1461 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule117 #-}
   {-# LINE 431 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule117 = \ _allstates _k_type ((_prodsIallvisits) :: [VisitStateState]) _t_params _t_type nextVisits_ nt_ ->
                                  {-# LINE 431 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  vlist $ map (\st ->
                                     let nt_st = nt_ >|< "_s" >|< st
                                         k_st  = "K_" >|< nt_st
                                         outg  = filter (\(v,f,t) -> f == st) _prodsIallvisits
                                         visitlist = vlist $ map (\(v,f,t) ->
                                             _k_type     >|< "_v" >|< v >#< "::" >#< k_st >#< _t_params     >#< pp_parens (_t_type     >|< "_v" >|< v >#< _t_params    )
                                              ) outg
                                         nextVisit = Map.findWithDefault ManyVis st nextVisits_
                                         decl = "::" >#< k_st >#< "k" >#< _t_params     >#< "where" >-< indent 3 visitlist
                                     in  case nextVisit of
                                           NoneVis  -> empty
                                           OneVis _ -> empty
                                           ManyVis  -> decl
                                     ) $ Set.toList _allstates
                                  {-# LINE 1480 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule118 #-}
   {-# LINE 499 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule118 = \ _genwrap_icl _wr_inhs ->
                                      {-# LINE 499 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      _genwrap_icl     "Inh" _wr_inhs
                                      {-# LINE 1486 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule119 #-}
   {-# LINE 500 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule119 = \ _genwrap_icl _wr_syns ->
                                      {-# LINE 500 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      _genwrap_icl     "Syn" _wr_syns
                                      {-# LINE 1492 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule120 #-}
   {-# LINE 501 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule120 = \ _addbang nt_ ->
                                      {-# LINE 501 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      \nm attr ->
                                        let tyConName = nm >|< "_" >|< nt_
                                        in  (let (d, _, _) = foldr (\(i, t) (d, c, n) -> (d >-<
                                                               i >|< "_" >|< tyConName >#< "::" >#< tyConName >#< "->" >#< (_addbang     $ pp_parens $ typeToHaskellString (Just nt_) [] t)
                                                               >-< i >|< "_" >|< tyConName >#< pp_parens (tyConName >#< unwords (replicate (n - c - 1) "_" ++ ["x"] ++ replicate c "_")) >#< "= x"
                                                               , c + 1, n)
                                                               ) (empty, 0, length attr) attr
                                            in  d)
                                      {-# LINE 1505 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule121 #-}
   {-# LINE 509 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule121 = \ _genwrap_dcl _wr_inhs ->
                                      {-# LINE 509 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      _genwrap_dcl     "Inh" _wr_inhs
                                      {-# LINE 1511 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule122 #-}
   {-# LINE 510 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule122 = \ _genwrap_dcl _wr_syns ->
                                      {-# LINE 510 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      _genwrap_dcl     "Syn" _wr_syns
                                      {-# LINE 1517 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule123 #-}
   {-# LINE 511 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule123 = \ _addbang _t_params nt_ ->
                                      {-# LINE 511 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      \nm attr ->
                                        let tyConName = nm >|< "_" >|< nt_
                                        in  "::" >#< tyConName >#< _t_params     >#< "=" >#< tyConName
                                            >#< (ppSpaced $ map (\(_,t) -> _addbang     $ pp_parens $ typeToHaskellString (Just nt_) [] t) attr)
                                            >-<
                                                (let (d, _, _) = foldr (\(i, t) (d, c, n) -> (d >-<
                                                                  i >|< "_" >|< tyConName >#< "::" >#< tyConName >#< "->" >#< (_addbang     $ pp_parens $ typeToHaskellString (Just nt_) [] t)
                                                                  , c + 1, n)
                                                                  ) (empty, 0, length attr) attr
                                                in  d)
                                      {-# LINE 1532 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule124 #-}
   {-# LINE 521 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule124 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 521 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  fromJust $ Map.lookup nt_ _lhsIinhmap
                                  {-# LINE 1538 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule125 #-}
   {-# LINE 522 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule125 = \ _synAttrs _wr_filter ->
                                  {-# LINE 522 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  Map.toList $ _wr_filter     $ _synAttrs
                                  {-# LINE 1544 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule126 #-}
   {-# LINE 523 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule126 = \ _synAttrs ->
                                  {-# LINE 523 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  Map.toList _synAttrs
                                  {-# LINE 1550 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule127 #-}
   {-# LINE 524 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule127 = \ ((_lhsIoptions) :: Options) ->
                                   {-# LINE 524 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   if lateHigherOrderBinding _lhsIoptions
                                   then Map.delete idLateBindingAttr
                                   else id
                                   {-# LINE 1558 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule128 #-}
   {-# LINE 527 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule128 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 527 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                  {-# LINE 1564 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule129 #-}
   {-# LINE 528 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule129 = \ ((_lhsIoptions) :: Options) _wr_inhs ->
                                  {-# LINE 528 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  map (lhsname _lhsIoptions True . fst) _wr_inhs
                                  {-# LINE 1570 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule130 #-}
   {-# LINE 529 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule130 = \ ((_lhsIoptions) :: Options) _wr_inhs1 ->
                                  {-# LINE 529 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  map (lhsname _lhsIoptions True . fst) _wr_inhs1
                                  {-# LINE 1576 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule131 #-}
   {-# LINE 530 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule131 = \ ((_lhsIoptions) :: Options) _wr_syns ->
                                  {-# LINE 530 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  map (lhsname _lhsIoptions False . fst) _wr_syns
                                  {-# LINE 1582 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule132 #-}
   {-# LINE 531 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule132 = \ nt_ ->
                                  {-# LINE 531 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  "wrap_" ++ show nt_
                                  {-# LINE 1588 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule133 #-}
   {-# LINE 532 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule133 = \ nt_ ->
                                  {-# LINE 532 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  "Inh_" ++ show nt_
                                  {-# LINE 1594 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule134 #-}
   {-# LINE 533 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule134 = \ nt_ ->
                                  {-# LINE 533 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  "Syn_" ++ show nt_
                                  {-# LINE 1600 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule135 #-}
   {-# LINE 534 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule135 = \ initial_ nextVisits_ ->
                                        {-# LINE 534 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                        Map.findWithDefault ManyVis initial_ nextVisits_
                                        {-# LINE 1606 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule136 #-}
   {-# LINE 535 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule136 = \ _addbang _addbangWrap _classPP _firstVisitInfo _inhlist _inhlist1 _inhname _k_type ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) _quantPP _synlist _synname _t_params _t_type _wrapname initial_ initialv_ nt_ ->
                                      {-# LINE 535 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      (_wrapname     >#< "::" >#< _quantPP     >#< _classPP     >#< _t_type     >#< _t_params
                                            >#< _inhname     >#< _t_params     >#< "->" >#< ( if monadicWrappers _lhsIoptions then ppMonadType _lhsIoptions else empty) >#< pp_parens (_synname     >#< _t_params    ))
                                      >-< (_wrapname     >#< (_addbang     $ pp_parens (_t_type     >#< pp "act"))
                                          >#< (_addbang     $ pp_parens (_inhname
                                                 >#< (ppSpaced $ map (_addbangWrap     . pp) _inhlist    )) >#< "="))
                                      >-<
                                      indent 3 (case initialv_ of
                                        Nothing -> text _synname
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
                                                                         VisitPure _  -> text "lift"
                                                                         VisitMonadic -> empty
                                                          _         -> empty
                                              unMonad | monadicWrappers _lhsIoptions = empty
                                                      | otherwise                    = unMon _lhsIoptions
                                          in unMonad >#< "("
                                             >-< indent 2
                                                   ("act >>= \\" >#< _addbang     (pp "sem") >#< "->"
                                                   >-< "lift" >#< pp_parens arg >#< ">>= \\" >#< _addbangWrap     (pp "arg") >#< "->"
                                                   >-< convert >#< pp_parens ("inv_" >|< nt_ >|< "_s" >|< initial_ >#< "sem" >#< ind >#< "arg" >#< extra) >#< ">>= \\" >#< pat >#< "->"
                                                   >-< "lift" >#< pp_parens (_synname     >#< ppSpaced _synlist    )
                                                   )
                                             >-< ")" )
                                      >-< if lateHigherOrderBinding _lhsIoptions
                                          then indent 2 ("where" >#< lhsname _lhsIoptions True idLateBindingAttr >#< "=" >#< lateBindingFieldNm _lhsImainName)
                                          else empty
                                      {-# LINE 1650 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule137 #-}
   {-# LINE 575 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule137 = \ _classPP _inhname ((_lhsIoptions) :: Options) _quantPP _synname _t_params _t_type _wrapname ->
                                      {-# LINE 575 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      (_wrapname     >#< "::" >#< _quantPP     >#< _classPP     >#< _t_type     >#< _t_params
                                            >#< _inhname     >#< _t_params     >#< "->" >#< ( if monadicWrappers _lhsIoptions then ppMonadType _lhsIoptions else empty) >#< pp_parens (_synname     >#< _t_params    ))
                                      {-# LINE 1657 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule138 #-}
   {-# LINE 584 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule138 = \ ((_prodsIsemFunBndDefs) :: Seq PP_Doc) _semFunBndDef ->
                        {-# LINE 584 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                        {-# LINE 1663 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule139 #-}
   {-# LINE 585 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule139 = \ ((_prodsIsemFunBndTps) :: Seq PP_Doc) _semFunBndTp ->
                        {-# LINE 585 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                        {-# LINE 1669 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule140 #-}
   {-# LINE 586 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule140 = \ _semFunBndNm _semname ->
                        {-# LINE 586 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 1675 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule141 #-}
   {-# LINE 587 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule141 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 587 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndNm     >#< "::" >#< _sem_tp
                        {-# LINE 1681 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule142 #-}
   {-# LINE 588 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule142 = \ nt_ ->
                        {-# LINE 588 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        lateSemNtLabel nt_
                        {-# LINE 1687 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule143 #-}
   {-# LINE 622 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule143 = \ initial_ ->
                                     {-# LINE 622 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     initial_
                                     {-# LINE 1693 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule144 #-}
   {-# LINE 623 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule144 = \ _allstates ->
                                     {-# LINE 623 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _allstates
                                     {-# LINE 1699 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule145 #-}
   {-# LINE 1469 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule145 = \ ((_lhsIwrappers) :: Set NontermIdent) _sem_nt _wr_inh_icl _wr_syn_icl _wrapper_icl nt_ ->
                                      {-# LINE 1469 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      (if nt_ `Set.member` _lhsIwrappers
                                       then     _wr_inh_icl
                                            >-< _wr_syn_icl
                                            >-< _wrapper_icl
                                       else empty)
                                      >-< _sem_nt
                                      {-# LINE 1710 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule146 #-}
   {-# LINE 1475 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule146 = \ _datatype _k_states ((_lhsIoptions) :: Options) ((_prodsIt_visits) :: PP_Doc) _t_init_icl _t_states_icl ->
                                      {-# LINE 1475 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      (if dataTypes _lhsIoptions then _datatype     else empty)
                                      >-< _t_init_icl
                                      >-< _t_states_icl
                                      >-< _k_states
                                      >-< _prodsIt_visits
                                      {-# LINE 1720 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule147 #-}
   {-# LINE 1566 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule147 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1566 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 1726 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule148 #-}
   {-# LINE 1574 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule148 = \  (_ :: ()) ->
                                                        {-# LINE 1574 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                        id
                                                        {-# LINE 1732 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule149 #-}
   {-# LINE 1586 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule149 = \ nextVisits_ ->
                       {-# LINE 1586 "./src-ag/ExecutionPlan2Clean.ag" #-}
                       nextVisits_
                       {-# LINE 1738 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule150 #-}
   {-# LINE 1587 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule150 = \ prevVisits_ ->
                       {-# LINE 1587 "./src-ag/ExecutionPlan2Clean.ag" #-}
                       prevVisits_
                       {-# LINE 1744 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule151 #-}
   {-# LINE 1631 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule151 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) nt_ ->
                           {-# LINE 1631 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                           {-# LINE 1750 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule152 #-}
   {-# LINE 1658 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule152 = \ initial_ nt_ ->
                     {-# LINE 1658 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     Map.singleton nt_ initial_
                     {-# LINE 1756 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule153 #-}
   {-# LINE 1672 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule153 = \ nt_ params_ ->
                 {-# LINE 1672 "./src-ag/ExecutionPlan2Clean.ag" #-}
                 NT nt_ (map show params_) False
                 {-# LINE 1762 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule154 #-}
   rule154 = \ ((_prodsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _prodsIchildvisit
   {-# INLINE rule155 #-}
   rule155 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule156 #-}
   rule156 = \ ((_prodsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _prodsIfromToStates
   {-# INLINE rule157 #-}
   rule157 = \ ((_prodsIgenProdIO) :: IO ()) ->
     _prodsIgenProdIO
   {-# INLINE rule158 #-}
   rule158 = \ ((_prodsIimports) :: [PP_Doc]) ->
     _prodsIimports
   {-# INLINE rule159 #-}
   rule159 = \ ((_prodsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _prodsIvisitKinds
   {-# INLINE rule160 #-}
   rule160 = \ ((_prodsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisitdefs
   {-# INLINE rule161 #-}
   rule161 = \ ((_prodsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisituses
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule174 #-}
   rule174 = \ _ntType ->
     _ntType
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

-- ENonterminals -----------------------------------------------
-- wrapper
data Inh_ENonterminals  = Inh_ENonterminals { allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminals :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), constructorTypeMap_Inh_ENonterminals :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String), derivings_Inh_ENonterminals :: (Derivings), iclModuleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String), importBlocks_Inh_ENonterminals :: (PP_Doc), inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminals :: (String), mainName_Inh_ENonterminals :: (String), options_Inh_ENonterminals :: (Options), synmap_Inh_ENonterminals :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminals :: (PP_Doc), typeSyns_Inh_ENonterminals :: (TypeSyns), wrappers_Inh_ENonterminals :: (Set NontermIdent) }
data Syn_ENonterminals  = Syn_ENonterminals { appendCommon_Syn_ENonterminals :: ([PP_Doc]), appendMain_Syn_ENonterminals :: ([PP_Doc]), childvisit_Syn_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), errors_Syn_ENonterminals :: (Seq Error), fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_ENonterminals :: (IO ()), imports_Syn_ENonterminals :: ([PP_Doc]), initStates_Syn_ENonterminals :: (Map NontermIdent Int), output_Syn_ENonterminals :: (PP_Doc), output_dcl_Syn_ENonterminals :: (PP_Doc), semFunBndDefs_Syn_ENonterminals :: (Seq PP_Doc), semFunBndTps_Syn_ENonterminals :: (Seq PP_Doc), visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminals #-}
wrap_ENonterminals :: T_ENonterminals  -> Inh_ENonterminals  -> (Syn_ENonterminals )
wrap_ENonterminals (T_ENonterminals act) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminals_s11 sem arg)
        return (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_ENonterminals_vIn10  = T_ENonterminals_vIn10 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (Derivings) (String -> String -> String -> Bool -> String) (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (Options) (Map NontermIdent Attributes) (PP_Doc) (TypeSyns) (Set NontermIdent)
data T_ENonterminals_vOut10  = T_ENonterminals_vOut10 ([PP_Doc]) ([PP_Doc]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (Map NontermIdent Int) (PP_Doc) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminals_Cons #-}
sem_ENonterminals_Cons :: T_ENonterminal  -> T_ENonterminals  -> T_ENonterminals 
sem_ENonterminals_Cons arg_hd_ arg_tl_ = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_ENonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_tl_))
         (T_ENonterminal_vOut7 _hdIappendCommon _hdIappendMain _hdIchildvisit _hdIerrors _hdIfromToStates _hdIgenProdIO _hdIimports _hdIinitStates _hdIoutput _hdIoutput_dcl _hdIsemFunBndDefs _hdIsemFunBndTps _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_ENonterminal_s8 _hdX8 (T_ENonterminal_vIn7 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOconstructorTypeMap _hdOdclModuleHeader _hdOderivings _hdOiclModuleHeader _hdOimportBlocks _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOoptions _hdOsynmap _hdOtextBlocks _hdOtypeSyns _hdOwrappers)
         (T_ENonterminals_vOut10 _tlIappendCommon _tlIappendMain _tlIchildvisit _tlIerrors _tlIfromToStates _tlIgenProdIO _tlIimports _tlIinitStates _tlIoutput _tlIoutput_dcl _tlIsemFunBndDefs _tlIsemFunBndTps _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_ENonterminals_s11 _tlX11 (T_ENonterminals_vIn10 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOconstructorTypeMap _tlOdclModuleHeader _tlOderivings _tlOiclModuleHeader _tlOimportBlocks _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOoptions _tlOsynmap _tlOtextBlocks _tlOtypeSyns _tlOwrappers)
         _lhsOappendCommon :: [PP_Doc]
         _lhsOappendCommon = rule177 _hdIappendCommon _tlIappendCommon
         _lhsOappendMain :: [PP_Doc]
         _lhsOappendMain = rule178 _hdIappendMain _tlIappendMain
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule179 _hdIchildvisit _tlIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule180 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule181 _hdIfromToStates _tlIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule182 _hdIgenProdIO _tlIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule183 _hdIimports _tlIimports
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule184 _hdIinitStates _tlIinitStates
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule185 _hdIoutput _tlIoutput
         _lhsOoutput_dcl :: PP_Doc
         _lhsOoutput_dcl = rule186 _hdIoutput_dcl _tlIoutput_dcl
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule187 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule188 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule189 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule190 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule191 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule192 _lhsIallFromToStates
         _hdOallInitStates = rule193 _lhsIallInitStates
         _hdOallVisitKinds = rule194 _lhsIallVisitKinds
         _hdOallchildvisit = rule195 _lhsIallchildvisit
         _hdOavisitdefs = rule196 _lhsIavisitdefs
         _hdOavisituses = rule197 _lhsIavisituses
         _hdOconstructorTypeMap = rule198 _lhsIconstructorTypeMap
         _hdOdclModuleHeader = rule199 _lhsIdclModuleHeader
         _hdOderivings = rule200 _lhsIderivings
         _hdOiclModuleHeader = rule201 _lhsIiclModuleHeader
         _hdOimportBlocks = rule202 _lhsIimportBlocks
         _hdOinhmap = rule203 _lhsIinhmap
         _hdOlocalAttrTypes = rule204 _lhsIlocalAttrTypes
         _hdOmainFile = rule205 _lhsImainFile
         _hdOmainName = rule206 _lhsImainName
         _hdOoptions = rule207 _lhsIoptions
         _hdOsynmap = rule208 _lhsIsynmap
         _hdOtextBlocks = rule209 _lhsItextBlocks
         _hdOtypeSyns = rule210 _lhsItypeSyns
         _hdOwrappers = rule211 _lhsIwrappers
         _tlOallFromToStates = rule212 _lhsIallFromToStates
         _tlOallInitStates = rule213 _lhsIallInitStates
         _tlOallVisitKinds = rule214 _lhsIallVisitKinds
         _tlOallchildvisit = rule215 _lhsIallchildvisit
         _tlOavisitdefs = rule216 _lhsIavisitdefs
         _tlOavisituses = rule217 _lhsIavisituses
         _tlOconstructorTypeMap = rule218 _lhsIconstructorTypeMap
         _tlOdclModuleHeader = rule219 _lhsIdclModuleHeader
         _tlOderivings = rule220 _lhsIderivings
         _tlOiclModuleHeader = rule221 _lhsIiclModuleHeader
         _tlOimportBlocks = rule222 _lhsIimportBlocks
         _tlOinhmap = rule223 _lhsIinhmap
         _tlOlocalAttrTypes = rule224 _lhsIlocalAttrTypes
         _tlOmainFile = rule225 _lhsImainFile
         _tlOmainName = rule226 _lhsImainName
         _tlOoptions = rule227 _lhsIoptions
         _tlOsynmap = rule228 _lhsIsynmap
         _tlOtextBlocks = rule229 _lhsItextBlocks
         _tlOtypeSyns = rule230 _lhsItypeSyns
         _tlOwrappers = rule231 _lhsIwrappers
         __result_ = T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule177 #-}
   rule177 = \ ((_hdIappendCommon) ::  PP_Doc ) ((_tlIappendCommon) :: [PP_Doc]) ->
     _hdIappendCommon : _tlIappendCommon
   {-# INLINE rule178 #-}
   rule178 = \ ((_hdIappendMain) ::  PP_Doc ) ((_tlIappendMain) :: [PP_Doc]) ->
     _hdIappendMain : _tlIappendMain
   {-# INLINE rule179 #-}
   rule179 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule180 #-}
   rule180 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule181 #-}
   rule181 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule182 #-}
   rule182 = \ ((_hdIgenProdIO) :: IO ()) ((_tlIgenProdIO) :: IO ()) ->
     _hdIgenProdIO >> _tlIgenProdIO
   {-# INLINE rule183 #-}
   rule183 = \ ((_hdIimports) :: [PP_Doc]) ((_tlIimports) :: [PP_Doc]) ->
     _hdIimports ++ _tlIimports
   {-# INLINE rule184 #-}
   rule184 = \ ((_hdIinitStates) :: Map NontermIdent Int) ((_tlIinitStates) :: Map NontermIdent Int) ->
     _hdIinitStates `mappend` _tlIinitStates
   {-# INLINE rule185 #-}
   rule185 = \ ((_hdIoutput) :: PP_Doc) ((_tlIoutput) :: PP_Doc) ->
     _hdIoutput >-< _tlIoutput
   {-# INLINE rule186 #-}
   rule186 = \ ((_hdIoutput_dcl) :: PP_Doc) ((_tlIoutput_dcl) :: PP_Doc) ->
     _hdIoutput_dcl >-< _tlIoutput_dcl
   {-# INLINE rule187 #-}
   rule187 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule188 #-}
   rule188 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule189 #-}
   rule189 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule190 #-}
   rule190 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule191 #-}
   rule191 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
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
   rule198 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule203 #-}
   rule203 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule228 #-}
   rule228 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_ENonterminals_Nil #-}
sem_ENonterminals_Nil ::  T_ENonterminals 
sem_ENonterminals_Nil  = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIderivings _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _lhsOappendCommon :: [PP_Doc]
         _lhsOappendCommon = rule232  ()
         _lhsOappendMain :: [PP_Doc]
         _lhsOappendMain = rule233  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule234  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule235  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule236  ()
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule237  ()
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule238  ()
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule239  ()
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule240  ()
         _lhsOoutput_dcl :: PP_Doc
         _lhsOoutput_dcl = rule241  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule242  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule243  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule244  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule245  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule246  ()
         __result_ = T_ENonterminals_vOut10 _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOoutput_dcl _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule232 #-}
   rule232 = \  (_ :: ()) ->
     []
   {-# INLINE rule233 #-}
   rule233 = \  (_ :: ()) ->
     []
   {-# INLINE rule234 #-}
   rule234 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule235 #-}
   rule235 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule236 #-}
   rule236 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule237 #-}
   rule237 = \  (_ :: ()) ->
     return ()
   {-# INLINE rule238 #-}
   rule238 = \  (_ :: ()) ->
     []
   {-# INLINE rule239 #-}
   rule239 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule240 #-}
   rule240 = \  (_ :: ()) ->
     empty
   {-# INLINE rule241 #-}
   rule241 = \  (_ :: ()) ->
     empty
   {-# INLINE rule242 #-}
   rule242 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule243 #-}
   rule243 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule244 #-}
   rule244 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule245 #-}
   rule245 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule246 #-}
   rule246 = \  (_ :: ()) ->
     Map.empty

-- EProduction -------------------------------------------------
-- wrapper
data Inh_EProduction  = Inh_EProduction { allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProduction :: (Map NontermIdent Attributes), allInitStates_Inh_EProduction :: (Map NontermIdent Int), allSynmap_Inh_EProduction :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allstates_Inh_EProduction :: (Set StateIdentifier), avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProduction :: (ClassContext), constructorTypeMap_Inh_EProduction :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String), importBlocks_Inh_EProduction :: (PP_Doc), inhmap_Inh_EProduction :: (Attributes), initial_Inh_EProduction :: (StateIdentifier), localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProduction :: (String), mainName_Inh_EProduction :: (String), nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), nt_Inh_EProduction :: (NontermIdent), ntType_Inh_EProduction :: (Type), options_Inh_EProduction :: (Options), params_Inh_EProduction :: ([Identifier]), prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), rename_Inh_EProduction :: (Bool), synmap_Inh_EProduction :: (Attributes), textBlocks_Inh_EProduction :: (PP_Doc) }
data Syn_EProduction  = Syn_EProduction { allvisits_Syn_EProduction :: ([VisitStateState]), childvisit_Syn_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), count_Syn_EProduction :: (Int), datatype_Syn_EProduction :: (PP_Doc), errors_Syn_EProduction :: (Seq Error), fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_EProduction :: (IO ()), imports_Syn_EProduction :: ([PP_Doc]), recordtype_Syn_EProduction :: (PP_Doc), semFunBndDefs_Syn_EProduction :: (Seq PP_Doc), semFunBndTps_Syn_EProduction :: (Seq PP_Doc), sem_nt_Syn_EProduction :: (PP_Doc), sem_prod_Syn_EProduction :: (PP_Doc), sem_prod_tys_Syn_EProduction :: (PP_Doc), t_visits_Syn_EProduction :: (PP_Doc), visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProduction #-}
wrap_EProduction :: T_EProduction  -> Inh_EProduction  -> (Syn_EProduction )
wrap_EProduction (T_EProduction act) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProduction_s14 sem arg)
        return (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_EProduction_vIn13  = T_EProduction_vIn13 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Bool) (Attributes) (PP_Doc)
data T_EProduction_vOut13  = T_EProduction_vOut13 ([VisitStateState]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Int) (PP_Doc) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProduction_EProduction #-}
sem_EProduction_EProduction :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_ERules  -> T_EChildren  -> T_Visits  -> T_EProduction 
sem_EProduction_EProduction arg_con_ arg_params_ arg_constraints_ arg_rules_ arg_children_ arg_visits_ = T_EProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_EProduction_v13 
      v13 = \ (T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _rulesX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_rules_))
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_children_))
         _visitsX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_visits_))
         (T_ERules_vOut22 _rulesIerrors _rulesImrules _rulesIruledefs _rulesIruleuses _rulesIsem_rules _rulesIusedArgs) = inv_ERules_s23 _rulesX23 (T_ERules_vIn22 _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOconstructorTypeMap _rulesOdclModuleHeader _rulesOiclModuleHeader _rulesOimportBlocks _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOnt _rulesOoptions _rulesOruleKinds _rulesOsynmap _rulesOtextBlocks _rulesOusageInfo)
         (T_EChildren_vOut4 _childrenIargnamesw _childrenIargpats _childrenIargtps _childrenIchildTypes _childrenIchildintros _childrenIdatatype _childrenIrecfields _childrenIrecordtype _childrenIterminaldefs _childrenIusedArgs) = inv_EChildren_s5 _childrenX5 (T_EChildren_vIn4 _childrenOallInitStates _childrenOcon _childrenOconstructorTypeMap _childrenOdclModuleHeader _childrenOiclModuleHeader _childrenOimportBlocks _childrenOmainFile _childrenOmainName _childrenOnt _childrenOoptions _childrenOtextBlocks)
         (T_Visits_vOut55 _visitsIallvisits _visitsIchildvisit _visitsIerrors _visitsIfromToStates _visitsIintramap _visitsIlazyIntras _visitsIruleKinds _visitsIruleUsage _visitsIsem_visit _visitsIt_visits _visitsIusedArgs _visitsIvisitKinds _visitsIvisitdefs _visitsIvisituses) = inv_Visits_s56 _visitsX56 (T_Visits_vIn55 _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOchildintros _visitsOcon _visitsOinhmap _visitsOmrules _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs)
         _childrenOcon = rule247 arg_con_
         _rulesOcon = rule248 arg_con_
         _visitsOcon = rule249 arg_con_
         _lhsOdatatype :: PP_Doc
         _lhsOdatatype = rule250 _childrenIdatatype _classPP1 _lhsInt _lhsIoptions _lhsIrename _quantPP1 arg_con_
         _lhsOrecordtype :: PP_Doc
         _lhsOrecordtype = rule251 _childrenIrecordtype _classPP1 _quantPP1
         _classPP1 = rule252 arg_constraints_
         _quantPP1 = rule253 arg_params_
         _lhsOcount :: Int
         _lhsOcount = rule254  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule255 _childrenIargnamesw _childrenIargpats _childrenIrecfields _lhsIconstructorTypeMap _lhsInt _lhsIrename arg_con_
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule256 _semFunBndDef
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule257 _semFunBndTp
         _semFunBndDef = rule258 _semFunBndNm _semname
         _semFunBndTp = rule259 _semFunBndNm _sem_tp
         _semFunBndNm = rule260 _lhsInt arg_con_
         _t_type = rule261 _lhsInt
         _t_params = rule262 _lhsIparams
         _usedArgs = rule263 _childrenIusedArgs _rulesIusedArgs _visitsIusedArgs
         _args = rule264 _childrenIargpats _usedArgs
         _semname = rule265 _lhsInt arg_con_
         _sem_tp = rule266 _childrenIargtps _classPP2 _quantPP2 _t_params _t_type
         _classPP2 = rule267 _lhsIclassCtxs arg_constraints_
         _quantPP2 = rule268 _lhsIparams arg_params_
         _lhsOsem_prod_tys :: PP_Doc
         _lhsOsem_prod_tys = rule269 _sem_tp _semname
         _sem_prod = rule270 _args _lhsIinitial _mbInitializer _mkSemBody _outerlet _sem_tp _semname _t_type
         _mkSemBody = rule271  ()
         _mbInitializer = rule272 _lhsIoptions
         _outerlet = rule273 _rulesIsem_rules _statefns
         _statefns = rule274 _genstfn _lhsIallstates
         _genstfn = rule275 _addbang _lhsIinitial _lhsInextVisits _lhsInt _lhsIprevVisits _stargs _stks _stvs
         _stargs = rule276 _addbang _childTypes _lazyIntras _lhsIallInhmap _lhsIallSynmap _lhsIoptions _localAttrTypes _visitsIintramap
         _stks = rule277 _lhsInt _stvisits _t_params
         _stvisits = rule278 _visitsIallvisits
         _stvs = rule279 _visitsIsem_visit
         _visitsOmrules = rule280 _rulesImrules
         _visitsOchildintros = rule281 _childrenIchildintros
         _rulesOusageInfo = rule282 _visitsIruleUsage
         _rulesOruleKinds = rule283 _visitsIruleKinds
         _visitsOallintramap = rule284 _visitsIintramap
         _visitsOterminaldefs = rule285 _childrenIterminaldefs
         _visitsOruledefs = rule286 _rulesIruledefs
         _visitsOruleuses = rule287 _rulesIruleuses
         _lazyIntras = rule288 _visitsIlazyIntras
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule289 _moduleName
         _moduleName = rule290 _lhsImainName _suffix
         _suffix = rule291 _lhsInt arg_con_
         _outputfile = rule292 _lhsImainFile _suffix
         _ppMonadImports = rule293  ()
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule294 _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainName _outputfile _ppMonadImports _sem_prod _semname _suffix
         _addbang = rule295 _lhsIoptions
         _childTypes = rule296 _childrenIchildTypes _lhsIntType
         _localAttrTypes = rule297 _lhsIlocalAttrTypes arg_con_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule298 _visitsIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule299 _rulesIerrors _visitsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule300 _visitsIfromToStates
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule301 _visitsIt_visits
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule302 _visitsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule303 _visitsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule304 _visitsIvisituses
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule305 _visitsIallvisits
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule306 _sem_prod
         _rulesOallInhmap = rule307 _lhsIallInhmap
         _rulesOallSynmap = rule308 _lhsIallSynmap
         _rulesOchildTypes = rule309 _childTypes
         _rulesOconstructorTypeMap = rule310 _lhsIconstructorTypeMap
         _rulesOdclModuleHeader = rule311 _lhsIdclModuleHeader
         _rulesOiclModuleHeader = rule312 _lhsIiclModuleHeader
         _rulesOimportBlocks = rule313 _lhsIimportBlocks
         _rulesOinhmap = rule314 _lhsIinhmap
         _rulesOlazyIntras = rule315 _lazyIntras
         _rulesOlocalAttrTypes = rule316 _localAttrTypes
         _rulesOmainFile = rule317 _lhsImainFile
         _rulesOmainName = rule318 _lhsImainName
         _rulesOnt = rule319 _lhsInt
         _rulesOoptions = rule320 _lhsIoptions
         _rulesOsynmap = rule321 _lhsIsynmap
         _rulesOtextBlocks = rule322 _lhsItextBlocks
         _childrenOallInitStates = rule323 _lhsIallInitStates
         _childrenOconstructorTypeMap = rule324 _lhsIconstructorTypeMap
         _childrenOdclModuleHeader = rule325 _lhsIdclModuleHeader
         _childrenOiclModuleHeader = rule326 _lhsIiclModuleHeader
         _childrenOimportBlocks = rule327 _lhsIimportBlocks
         _childrenOmainFile = rule328 _lhsImainFile
         _childrenOmainName = rule329 _lhsImainName
         _childrenOnt = rule330 _lhsInt
         _childrenOoptions = rule331 _lhsIoptions
         _childrenOtextBlocks = rule332 _lhsItextBlocks
         _visitsOallFromToStates = rule333 _lhsIallFromToStates
         _visitsOallInhmap = rule334 _lhsIallInhmap
         _visitsOallInitStates = rule335 _lhsIallInitStates
         _visitsOallSynmap = rule336 _lhsIallSynmap
         _visitsOallVisitKinds = rule337 _lhsIallVisitKinds
         _visitsOallchildvisit = rule338 _lhsIallchildvisit
         _visitsOavisitdefs = rule339 _lhsIavisitdefs
         _visitsOavisituses = rule340 _lhsIavisituses
         _visitsOchildTypes = rule341 _childTypes
         _visitsOinhmap = rule342 _lhsIinhmap
         _visitsOnextVisits = rule343 _lhsInextVisits
         _visitsOnt = rule344 _lhsInt
         _visitsOoptions = rule345 _lhsIoptions
         _visitsOparams = rule346 _lhsIparams
         _visitsOprevVisits = rule347 _lhsIprevVisits
         _visitsOsynmap = rule348 _lhsIsynmap
         __result_ = T_EProduction_vOut13 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProduction_s14 v13
   {-# INLINE rule247 #-}
   {-# LINE 72 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule247 = \ con_ ->
                                 {-# LINE 72 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 con_
                                 {-# LINE 2369 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule248 #-}
   {-# LINE 73 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule248 = \ con_ ->
                                 {-# LINE 73 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 con_
                                 {-# LINE 2375 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule249 #-}
   {-# LINE 74 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule249 = \ con_ ->
                                 {-# LINE 74 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 con_
                                 {-# LINE 2381 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule250 #-}
   {-# LINE 215 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule250 = \ ((_childrenIdatatype) :: [PP_Doc]) _classPP1 ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIrename) :: Bool) _quantPP1 con_ ->
                                 {-# LINE 215 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _quantPP1     >#< _classPP1
                                 >#< conname _lhsIrename _lhsInt con_
                                 >#< ppConFields (dataRecords _lhsIoptions) _childrenIdatatype
                                 {-# LINE 2389 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule251 #-}
   {-# LINE 218 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule251 = \ ((_childrenIrecordtype) :: [PP_Doc]) _classPP1 _quantPP1 ->
                                   {-# LINE 218 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   _quantPP1     >#< _classPP1
                                   >#< ppConFields True _childrenIrecordtype
                                   {-# LINE 2396 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule252 #-}
   {-# LINE 220 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule252 = \ constraints_ ->
                                 {-# LINE 220 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppClasses (classConstrsToDocs constraints_)
                                 {-# LINE 2402 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule253 #-}
   {-# LINE 221 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule253 = \ params_ ->
                                 {-# LINE 221 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppQuants params_
                                 {-# LINE 2408 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule254 #-}
   {-# LINE 320 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule254 = \  (_ :: ()) ->
                                              {-# LINE 320 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                              1
                                              {-# LINE 2414 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule255 #-}
   {-# LINE 325 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule255 = \ ((_childrenIargnamesw) :: [PP_Doc]) ((_childrenIargpats) ::  [PP_Doc] ) ((_childrenIrecfields) ::  [Identifier] ) ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ((_lhsInt) :: NontermIdent) ((_lhsIrename) :: Bool) con_ ->
                               {-# LINE 325 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               let lhs   = "sem_" >|< _lhsInt
                                   rhs   = "=" >#< "sem_" >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                                   cnnm  = conname _lhsIrename _lhsInt con_
                               in  if isRecordConstructor _lhsInt _lhsIconstructorTypeMap
                                     then  lhs >#< "{" >#< cnnm >#< "|" >#<
                                           pp_block "" "" "," (zipWith (\l r -> l >#< "=" >#< r) _childrenIrecfields _childrenIargpats) >#< "}" >#< rhs
                                     else  lhs >#< "(" >#< cnnm >#< ppSpaced _childrenIargpats >#< ")" >#< rhs
                               {-# LINE 2426 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule256 #-}
   {-# LINE 591 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule256 = \ _semFunBndDef ->
                        {-# LINE 591 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        Seq.singleton _semFunBndDef
                        {-# LINE 2432 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule257 #-}
   {-# LINE 592 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule257 = \ _semFunBndTp ->
                        {-# LINE 592 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        Seq.singleton _semFunBndTp
                        {-# LINE 2438 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule258 #-}
   {-# LINE 593 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule258 = \ _semFunBndNm _semname ->
                        {-# LINE 593 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndNm     >#< "=" >#< _semname
                        {-# LINE 2444 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule259 #-}
   {-# LINE 594 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule259 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 594 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        _semFunBndNm     >#< "::" >#< _sem_tp
                        {-# LINE 2450 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule260 #-}
   {-# LINE 595 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule260 = \ ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 595 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        lateSemConLabel _lhsInt con_
                        {-# LINE 2456 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule261 #-}
   {-# LINE 659 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule261 = \ ((_lhsInt) :: NontermIdent) ->
                                 {-# LINE 659 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 "T_" >|< _lhsInt
                                 {-# LINE 2462 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule262 #-}
   {-# LINE 660 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule262 = \ ((_lhsIparams) :: [Identifier]) ->
                                 {-# LINE 660 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppSpaced _lhsIparams
                                 {-# LINE 2468 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule263 #-}
   {-# LINE 661 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule263 = \ ((_childrenIusedArgs) :: Set String) ((_rulesIusedArgs) :: Set String) ((_visitsIusedArgs) :: Set String) ->
                                 {-# LINE 661 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _childrenIusedArgs `Set.union` _visitsIusedArgs `Set.union` _rulesIusedArgs
                                 {-# LINE 2474 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule264 #-}
   {-# LINE 664 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule264 = \ ((_childrenIargpats) ::  [PP_Doc] ) _usedArgs ->
                                 {-# LINE 664 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 map (\x -> let (name,arg) = case show x of
                                                         ""       -> ("", empty)
                                                         '!':name -> ("arg_" ++ name, "!arg_" >|< name)
                                                         name     -> ("arg_" ++ name, "arg_"  >|< name)
                                            in  if null name || name `Set.member` _usedArgs
                                                              then arg
                                                              else text "_") _childrenIargpats
                                 {-# LINE 2486 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule265 #-}
   {-# LINE 671 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule265 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                 {-# LINE 671 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 "sem_" ++ show _lhsInt ++ "_" ++ show con_
                                 {-# LINE 2492 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule266 #-}
   {-# LINE 672 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule266 = \ ((_childrenIargtps) ::  [PP_Doc] ) _classPP2 _quantPP2 _t_params _t_type ->
                                 {-# LINE 672 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _quantPP2     >#< _classPP2     >#< ppSpaced _childrenIargtps
                                 >#< (if length _childrenIargtps > 0 then "->" else "")
                                 >#< _t_type     >#< _t_params
                                 {-# LINE 2500 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule267 #-}
   {-# LINE 675 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule267 = \ ((_lhsIclassCtxs) :: ClassContext) constraints_ ->
                                 {-# LINE 675 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppClasses (classCtxsToDocs _lhsIclassCtxs ++ classConstrsToDocs constraints_)
                                 {-# LINE 2506 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule268 #-}
   {-# LINE 676 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule268 = \ ((_lhsIparams) :: [Identifier]) params_ ->
                                 {-# LINE 676 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 ppQuants (_lhsIparams ++ params_)
                                 {-# LINE 2512 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule269 #-}
   {-# LINE 678 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule269 = \ _sem_tp _semname ->
                                     {-# LINE 678 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _semname     >#< " ::" >#< _sem_tp
                                     {-# LINE 2518 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule270 #-}
   {-# LINE 680 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule270 = \ _args ((_lhsIinitial) :: StateIdentifier) _mbInitializer _mkSemBody _outerlet _sem_tp _semname _t_type ->
                                 {-# LINE 680 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _semname     >#< " ::" >#< _sem_tp
                                 >-< _mkSemBody     (_semname     >#< ppSpaced _args     >#< "=" >#< _t_type    )
                                                    _mbInitializer     _outerlet     ("lift" >#< "st" >|< _lhsIinitial)
                                 {-# LINE 2526 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule271 #-}
   {-# LINE 683 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule271 = \  (_ :: ()) ->
                                  {-# LINE 683 "./src-ag/ExecutionPlan2Clean.ag" #-}
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
                                  {-# LINE 2542 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule272 #-}
   {-# LINE 695 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule272 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 695 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                        if parallelInvoke _lhsIoptions
                                        then (Nothing :: Maybe PP_Doc)
                                        else Nothing
                                        {-# LINE 2550 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule273 #-}
   {-# LINE 701 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule273 = \ ((_rulesIsem_rules) :: PP_Doc) _statefns ->
                                 {-# LINE 701 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 vlist _statefns     >-< _rulesIsem_rules
                                 {-# LINE 2556 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule274 #-}
   {-# LINE 702 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule274 = \ _genstfn ((_lhsIallstates) :: Set StateIdentifier) ->
                                 {-# LINE 702 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 map _genstfn     $ Set.toList _lhsIallstates
                                 {-# LINE 2562 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule275 #-}
   {-# LINE 703 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule275 = \ _addbang ((_lhsIinitial) :: StateIdentifier) ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ((_lhsInt) :: NontermIdent) ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) _stargs _stks _stvs ->
                                 {-# LINE 703 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 \st -> let nextVisitInfo = Map.findWithDefault ManyVis st _lhsInextVisits
                                            prevVisitInfo = Map.findWithDefault ManyVis st _lhsIprevVisits
                                            stNm = "st" >|< st
                                            lhs  = bang stNm >#< "=" >#<
                                                   (
                                                     if st == _lhsIinitial
                                                     then empty
                                                     else "\\" >#< _stargs     st >#< "->"
                                                   )
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
                                 {-# LINE 2586 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule276 #-}
   {-# LINE 731 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule276 = \ _addbang _childTypes _lazyIntras ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIoptions) :: Options) _localAttrTypes ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                                 {-# LINE 731 "./src-ag/ExecutionPlan2Clean.ag" #-}
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
                                 {-# LINE 2606 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule277 #-}
   {-# LINE 747 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule277 = \ ((_lhsInt) :: NontermIdent) _stvisits _t_params ->
                                 {-# LINE 747 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 \st -> if null (_stvisits     st)
                                        then empty
                                        else "k" >|< st >#< "::" >#< "K_" >|< _lhsInt >|< "_s" >|< st >#< _t_params     >#< "t" >#< "->" >#< "t"
                                             >-< vlist (map (\(v,f,t) -> "k" >|< st >#< "K_" >|< _lhsInt >|< "_v" >|< v >#< "="
                                                                    >#< "v" >|< v) $ _stvisits     st)
                                 {-# LINE 2616 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule278 #-}
   {-# LINE 752 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule278 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
                                 {-# LINE 752 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 \st -> filter (\(v,f,t) -> f == st) _visitsIallvisits
                                 {-# LINE 2622 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule279 #-}
   {-# LINE 753 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule279 = \ ((_visitsIsem_visit) ::  [(StateIdentifier,Bool -> PP_Doc)] ) ->
                                 {-# LINE 753 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 \st inlinePragma -> vlist [ppf inlinePragma | (f,ppf) <- _visitsIsem_visit, f == st]
                                 {-# LINE 2628 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule280 #-}
   {-# LINE 754 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule280 = \ ((_rulesImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                                  {-# LINE 754 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  _rulesImrules
                                  {-# LINE 2634 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule281 #-}
   {-# LINE 942 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule281 = \ ((_childrenIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                       {-# LINE 942 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       _childrenIchildintros
                                       {-# LINE 2640 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule282 #-}
   {-# LINE 1277 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule282 = \ ((_visitsIruleUsage) :: Map Identifier Int) ->
                                                   {-# LINE 1277 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                   _visitsIruleUsage
                                                   {-# LINE 2646 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule283 #-}
   {-# LINE 1292 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule283 = \ ((_visitsIruleKinds) :: Map Identifier (Set VisitKind)) ->
                      {-# LINE 1292 "./src-ag/ExecutionPlan2Clean.ag" #-}
                      _visitsIruleKinds
                      {-# LINE 2652 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule284 #-}
   {-# LINE 1321 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule284 = \ ((_visitsIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                          {-# LINE 1321 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          _visitsIintramap
                          {-# LINE 2658 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule285 #-}
   {-# LINE 1322 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule285 = \ ((_childrenIterminaldefs) :: Set String) ->
                          {-# LINE 1322 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          _childrenIterminaldefs
                          {-# LINE 2664 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule286 #-}
   {-# LINE 1346 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule286 = \ ((_rulesIruledefs) :: Map Identifier (Set String)) ->
                                    {-# LINE 1346 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    _rulesIruledefs
                                    {-# LINE 2670 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule287 #-}
   {-# LINE 1347 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule287 = \ ((_rulesIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                    {-# LINE 1347 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    _rulesIruleuses
                                    {-# LINE 2676 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule288 #-}
   {-# LINE 1401 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule288 = \ ((_visitsIlazyIntras) :: Set String) ->
                     {-# LINE 1401 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     _visitsIlazyIntras
                     {-# LINE 2682 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule289 #-}
   {-# LINE 1486 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule289 = \ _moduleName ->
                                   {-# LINE 1486 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   [pp $ "import " ++ _moduleName    ]
                                   {-# LINE 2688 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule290 #-}
   {-# LINE 1487 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule290 = \ ((_lhsImainName) :: String) _suffix ->
                                   {-# LINE 1487 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   _lhsImainName ++ _suffix
                                   {-# LINE 2694 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule291 #-}
   {-# LINE 1488 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule291 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                   {-# LINE 1488 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   '_' : show _lhsInt ++ ('_' : show con_)
                                   {-# LINE 2700 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule292 #-}
   {-# LINE 1489 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule292 = \ ((_lhsImainFile) :: String) _suffix ->
                                   {-# LINE 1489 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ _suffix    )
                                   {-# LINE 2706 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule293 #-}
   {-# LINE 1490 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule293 = \  (_ :: ()) ->
                                        {-# LINE 1490 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                        pp "import qualified Control.Monad.Identity"
                                        {-# LINE 2712 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule294 #-}
   {-# LINE 1491 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule294 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) _outputfile _ppMonadImports _sem_prod _semname _suffix ->
                                   {-# LINE 1491 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   writeModule _outputfile
                                       [ pp $ _lhsIiclModuleHeader _lhsImainName _suffix     _semname     True
                                       , _lhsIimportBlocks
                                       , _ppMonadImports
                                       , pp $ "import " ++ _lhsImainName ++ "_common"
                                       , _sem_prod
                                       ]
                                   {-# LINE 2724 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule295 #-}
   {-# LINE 1567 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule295 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1567 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 2730 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule296 #-}
   {-# LINE 1617 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule296 = \ ((_childrenIchildTypes) :: Map Identifier Type) ((_lhsIntType) :: Type) ->
                     {-# LINE 1617 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                     {-# LINE 2736 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule297 #-}
   {-# LINE 1634 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule297 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) con_ ->
                           {-# LINE 1634 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                           {-# LINE 2742 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule298 #-}
   rule298 = \ ((_visitsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _visitsIchildvisit
   {-# INLINE rule299 #-}
   rule299 = \ ((_rulesIerrors) :: Seq Error) ((_visitsIerrors) :: Seq Error) ->
     _rulesIerrors Seq.>< _visitsIerrors
   {-# INLINE rule300 #-}
   rule300 = \ ((_visitsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _visitsIfromToStates
   {-# INLINE rule301 #-}
   rule301 = \ ((_visitsIt_visits) :: PP_Doc) ->
     _visitsIt_visits
   {-# INLINE rule302 #-}
   rule302 = \ ((_visitsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _visitsIvisitKinds
   {-# INLINE rule303 #-}
   rule303 = \ ((_visitsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisitdefs
   {-# INLINE rule304 #-}
   rule304 = \ ((_visitsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisituses
   {-# INLINE rule305 #-}
   rule305 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
     _visitsIallvisits
   {-# INLINE rule306 #-}
   rule306 = \ _sem_prod ->
     _sem_prod
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule309 #-}
   rule309 = \ _childTypes ->
     _childTypes
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule315 #-}
   rule315 = \ _lazyIntras ->
     _lazyIntras
   {-# INLINE rule316 #-}
   rule316 = \ _localAttrTypes ->
     _localAttrTypes
   {-# INLINE rule317 #-}
   rule317 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule318 #-}
   rule318 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule325 #-}
   rule325 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule327 #-}
   rule327 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule331 #-}
   rule331 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule333 #-}
   rule333 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule335 #-}
   rule335 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule336 #-}
   rule336 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule337 #-}
   rule337 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule338 #-}
   rule338 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule339 #-}
   rule339 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule340 #-}
   rule340 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule341 #-}
   rule341 = \ _childTypes ->
     _childTypes
   {-# INLINE rule342 #-}
   rule342 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule343 #-}
   rule343 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule344 #-}
   rule344 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule345 #-}
   rule345 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule346 #-}
   rule346 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule347 #-}
   rule347 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule348 #-}
   rule348 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap

-- EProductions ------------------------------------------------
-- wrapper
data Inh_EProductions  = Inh_EProductions { allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProductions :: (Map NontermIdent Attributes), allInitStates_Inh_EProductions :: (Map NontermIdent Int), allSynmap_Inh_EProductions :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), allstates_Inh_EProductions :: (Set StateIdentifier), avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProductions :: (ClassContext), constructorTypeMap_Inh_EProductions :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String), importBlocks_Inh_EProductions :: (PP_Doc), inhmap_Inh_EProductions :: (Attributes), initial_Inh_EProductions :: (StateIdentifier), localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProductions :: (String), mainName_Inh_EProductions :: (String), nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), nt_Inh_EProductions :: (NontermIdent), ntType_Inh_EProductions :: (Type), options_Inh_EProductions :: (Options), params_Inh_EProductions :: ([Identifier]), prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), rename_Inh_EProductions :: (Bool), synmap_Inh_EProductions :: (Attributes), textBlocks_Inh_EProductions :: (PP_Doc) }
data Syn_EProductions  = Syn_EProductions { allvisits_Syn_EProductions :: ([VisitStateState]), childvisit_Syn_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), count_Syn_EProductions :: (Int), datatype_Syn_EProductions :: ([PP_Doc]), errors_Syn_EProductions :: (Seq Error), fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)), genProdIO_Syn_EProductions :: (IO ()), imports_Syn_EProductions :: ([PP_Doc]), recordtype_Syn_EProductions :: (PP_Doc), semFunBndDefs_Syn_EProductions :: (Seq PP_Doc), semFunBndTps_Syn_EProductions :: (Seq PP_Doc), sem_nt_Syn_EProductions :: (PP_Doc), sem_prod_Syn_EProductions :: (PP_Doc), sem_prod_tys_Syn_EProductions :: (PP_Doc), t_visits_Syn_EProductions :: (PP_Doc), visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProductions #-}
wrap_EProductions :: T_EProductions  -> Inh_EProductions  -> (Syn_EProductions )
wrap_EProductions (T_EProductions act) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProductions_s17 sem arg)
        return (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
data T_EProductions_vIn16  = T_EProductions_vIn16 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Bool) (Attributes) (PP_Doc)
data T_EProductions_vOut16  = T_EProductions_vOut16 ([VisitStateState]) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Int) ([PP_Doc]) (Seq Error) (Map VisitIdentifier (Int,Int)) (IO ()) ([PP_Doc]) (PP_Doc) (Seq PP_Doc) (Seq PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (PP_Doc) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProductions_Cons #-}
sem_EProductions_Cons :: T_EProduction  -> T_EProductions  -> T_EProductions 
sem_EProductions_Cons arg_hd_ arg_tl_ = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_EProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_tl_))
         (T_EProduction_vOut13 _hdIallvisits _hdIchildvisit _hdIcount _hdIdatatype _hdIerrors _hdIfromToStates _hdIgenProdIO _hdIimports _hdIrecordtype _hdIsemFunBndDefs _hdIsemFunBndTps _hdIsem_nt _hdIsem_prod _hdIsem_prod_tys _hdIt_visits _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_EProduction_s14 _hdX14 (T_EProduction_vIn13 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOclassCtxs _hdOconstructorTypeMap _hdOdclModuleHeader _hdOiclModuleHeader _hdOimportBlocks _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOprevVisits _hdOrename _hdOsynmap _hdOtextBlocks)
         (T_EProductions_vOut16 _tlIallvisits _tlIchildvisit _tlIcount _tlIdatatype _tlIerrors _tlIfromToStates _tlIgenProdIO _tlIimports _tlIrecordtype _tlIsemFunBndDefs _tlIsemFunBndTps _tlIsem_nt _tlIsem_prod _tlIsem_prod_tys _tlIt_visits _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_EProductions_s17 _tlX17 (T_EProductions_vIn16 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOclassCtxs _tlOconstructorTypeMap _tlOdclModuleHeader _tlOiclModuleHeader _tlOimportBlocks _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOprevVisits _tlOrename _tlOsynmap _tlOtextBlocks)
         _lhsOrecordtype :: PP_Doc
         _lhsOrecordtype = rule349 _hdIrecordtype
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule350 _hdIallvisits
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule351 _hdIt_visits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule352 _hdIchildvisit _tlIchildvisit
         _lhsOcount :: Int
         _lhsOcount = rule353 _hdIcount _tlIcount
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule354 _hdIdatatype _tlIdatatype
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule355 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule356 _hdIfromToStates _tlIfromToStates
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule357 _hdIgenProdIO _tlIgenProdIO
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule358 _hdIimports _tlIimports
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule359 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule360 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule361 _hdIsem_nt _tlIsem_nt
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule362 _hdIsem_prod _tlIsem_prod
         _lhsOsem_prod_tys :: PP_Doc
         _lhsOsem_prod_tys = rule363 _hdIsem_prod_tys _tlIsem_prod_tys
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule364 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule365 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule366 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule367 _lhsIallFromToStates
         _hdOallInhmap = rule368 _lhsIallInhmap
         _hdOallInitStates = rule369 _lhsIallInitStates
         _hdOallSynmap = rule370 _lhsIallSynmap
         _hdOallVisitKinds = rule371 _lhsIallVisitKinds
         _hdOallchildvisit = rule372 _lhsIallchildvisit
         _hdOallstates = rule373 _lhsIallstates
         _hdOavisitdefs = rule374 _lhsIavisitdefs
         _hdOavisituses = rule375 _lhsIavisituses
         _hdOclassCtxs = rule376 _lhsIclassCtxs
         _hdOconstructorTypeMap = rule377 _lhsIconstructorTypeMap
         _hdOdclModuleHeader = rule378 _lhsIdclModuleHeader
         _hdOiclModuleHeader = rule379 _lhsIiclModuleHeader
         _hdOimportBlocks = rule380 _lhsIimportBlocks
         _hdOinhmap = rule381 _lhsIinhmap
         _hdOinitial = rule382 _lhsIinitial
         _hdOlocalAttrTypes = rule383 _lhsIlocalAttrTypes
         _hdOmainFile = rule384 _lhsImainFile
         _hdOmainName = rule385 _lhsImainName
         _hdOnextVisits = rule386 _lhsInextVisits
         _hdOnt = rule387 _lhsInt
         _hdOntType = rule388 _lhsIntType
         _hdOoptions = rule389 _lhsIoptions
         _hdOparams = rule390 _lhsIparams
         _hdOprevVisits = rule391 _lhsIprevVisits
         _hdOrename = rule392 _lhsIrename
         _hdOsynmap = rule393 _lhsIsynmap
         _hdOtextBlocks = rule394 _lhsItextBlocks
         _tlOallFromToStates = rule395 _lhsIallFromToStates
         _tlOallInhmap = rule396 _lhsIallInhmap
         _tlOallInitStates = rule397 _lhsIallInitStates
         _tlOallSynmap = rule398 _lhsIallSynmap
         _tlOallVisitKinds = rule399 _lhsIallVisitKinds
         _tlOallchildvisit = rule400 _lhsIallchildvisit
         _tlOallstates = rule401 _lhsIallstates
         _tlOavisitdefs = rule402 _lhsIavisitdefs
         _tlOavisituses = rule403 _lhsIavisituses
         _tlOclassCtxs = rule404 _lhsIclassCtxs
         _tlOconstructorTypeMap = rule405 _lhsIconstructorTypeMap
         _tlOdclModuleHeader = rule406 _lhsIdclModuleHeader
         _tlOiclModuleHeader = rule407 _lhsIiclModuleHeader
         _tlOimportBlocks = rule408 _lhsIimportBlocks
         _tlOinhmap = rule409 _lhsIinhmap
         _tlOinitial = rule410 _lhsIinitial
         _tlOlocalAttrTypes = rule411 _lhsIlocalAttrTypes
         _tlOmainFile = rule412 _lhsImainFile
         _tlOmainName = rule413 _lhsImainName
         _tlOnextVisits = rule414 _lhsInextVisits
         _tlOnt = rule415 _lhsInt
         _tlOntType = rule416 _lhsIntType
         _tlOoptions = rule417 _lhsIoptions
         _tlOparams = rule418 _lhsIparams
         _tlOprevVisits = rule419 _lhsIprevVisits
         _tlOrename = rule420 _lhsIrename
         _tlOsynmap = rule421 _lhsIsynmap
         _tlOtextBlocks = rule422 _lhsItextBlocks
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule349 #-}
   {-# LINE 224 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule349 = \ ((_hdIrecordtype) :: PP_Doc) ->
                             {-# LINE 224 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             _hdIrecordtype
                             {-# LINE 3038 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 385 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule350 = \ ((_hdIallvisits) :: [VisitStateState]) ->
                           {-# LINE 385 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           _hdIallvisits
                           {-# LINE 3044 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 450 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule351 = \ ((_hdIt_visits) :: PP_Doc) ->
                          {-# LINE 450 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          _hdIt_visits
                          {-# LINE 3050 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule352 #-}
   rule352 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule353 #-}
   rule353 = \ ((_hdIcount) :: Int) ((_tlIcount) :: Int) ->
     _hdIcount + _tlIcount
   {-# INLINE rule354 #-}
   rule354 = \ ((_hdIdatatype) :: PP_Doc) ((_tlIdatatype) :: [PP_Doc]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule355 #-}
   rule355 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule356 #-}
   rule356 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule357 #-}
   rule357 = \ ((_hdIgenProdIO) :: IO ()) ((_tlIgenProdIO) :: IO ()) ->
     _hdIgenProdIO >> _tlIgenProdIO
   {-# INLINE rule358 #-}
   rule358 = \ ((_hdIimports) :: [PP_Doc]) ((_tlIimports) :: [PP_Doc]) ->
     _hdIimports ++ _tlIimports
   {-# INLINE rule359 #-}
   rule359 = \ ((_hdIsemFunBndDefs) :: Seq PP_Doc) ((_tlIsemFunBndDefs) :: Seq PP_Doc) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule360 #-}
   rule360 = \ ((_hdIsemFunBndTps) :: Seq PP_Doc) ((_tlIsemFunBndTps) :: Seq PP_Doc) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule361 #-}
   rule361 = \ ((_hdIsem_nt) :: PP_Doc) ((_tlIsem_nt) :: PP_Doc) ->
     _hdIsem_nt >-< _tlIsem_nt
   {-# INLINE rule362 #-}
   rule362 = \ ((_hdIsem_prod) :: PP_Doc) ((_tlIsem_prod) :: PP_Doc) ->
     _hdIsem_prod >-< _tlIsem_prod
   {-# INLINE rule363 #-}
   rule363 = \ ((_hdIsem_prod_tys) :: PP_Doc) ((_tlIsem_prod_tys) :: PP_Doc) ->
     _hdIsem_prod_tys >-< _tlIsem_prod_tys
   {-# INLINE rule364 #-}
   rule364 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule365 #-}
   rule365 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule366 #-}
   rule366 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule367 #-}
   rule367 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule368 #-}
   rule368 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
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
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule411 #-}
   rule411 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
{-# NOINLINE sem_EProductions_Nil #-}
sem_EProductions_Nil ::  T_EProductions 
sem_EProductions_Nil  = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _lhsOrecordtype :: PP_Doc
         _lhsOrecordtype = rule423  ()
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule424  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule425  ()
         _lhsOcount :: Int
         _lhsOcount = rule426  ()
         _lhsOdatatype :: [PP_Doc]
         _lhsOdatatype = rule427  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule428  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule429  ()
         _lhsOgenProdIO :: IO ()
         _lhsOgenProdIO = rule430  ()
         _lhsOimports :: [PP_Doc]
         _lhsOimports = rule431  ()
         _lhsOsemFunBndDefs :: Seq PP_Doc
         _lhsOsemFunBndDefs = rule432  ()
         _lhsOsemFunBndTps :: Seq PP_Doc
         _lhsOsemFunBndTps = rule433  ()
         _lhsOsem_nt :: PP_Doc
         _lhsOsem_nt = rule434  ()
         _lhsOsem_prod :: PP_Doc
         _lhsOsem_prod = rule435  ()
         _lhsOsem_prod_tys :: PP_Doc
         _lhsOsem_prod_tys = rule436  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule437  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule438  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule439  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule440  ()
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOrecordtype _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOsem_prod_tys _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule423 #-}
   {-# LINE 225 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule423 = \  (_ :: ()) ->
                             {-# LINE 225 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             empty
                             {-# LINE 3315 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule424 #-}
   {-# LINE 386 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule424 = \  (_ :: ()) ->
                           {-# LINE 386 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           error "Every nonterminal should have at least 1 production"
                           {-# LINE 3321 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule425 #-}
   rule425 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule426 #-}
   rule426 = \  (_ :: ()) ->
     0
   {-# INLINE rule427 #-}
   rule427 = \  (_ :: ()) ->
     []
   {-# INLINE rule428 #-}
   rule428 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule429 #-}
   rule429 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule430 #-}
   rule430 = \  (_ :: ()) ->
     return ()
   {-# INLINE rule431 #-}
   rule431 = \  (_ :: ()) ->
     []
   {-# INLINE rule432 #-}
   rule432 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule433 #-}
   rule433 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     empty
   {-# INLINE rule435 #-}
   rule435 = \  (_ :: ()) ->
     empty
   {-# INLINE rule436 #-}
   rule436 = \  (_ :: ()) ->
     empty
   {-# INLINE rule437 #-}
   rule437 = \  (_ :: ()) ->
     empty
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule439 #-}
   rule439 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule440 #-}
   rule440 = \  (_ :: ()) ->
     Map.empty

-- ERule -------------------------------------------------------
-- wrapper
data Inh_ERule  = Inh_ERule { allInhmap_Inh_ERule :: (Map NontermIdent Attributes), allSynmap_Inh_ERule :: (Map NontermIdent Attributes), childTypes_Inh_ERule :: (Map Identifier Type), con_Inh_ERule :: (ConstructorIdent), constructorTypeMap_Inh_ERule :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_ERule :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_ERule :: (String -> String -> String -> Bool -> String), importBlocks_Inh_ERule :: (PP_Doc), inhmap_Inh_ERule :: (Attributes), lazyIntras_Inh_ERule :: (Set String), localAttrTypes_Inh_ERule :: (Map Identifier Type), mainFile_Inh_ERule :: (String), mainName_Inh_ERule :: (String), nt_Inh_ERule :: (NontermIdent), options_Inh_ERule :: (Options), ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)), synmap_Inh_ERule :: (Attributes), textBlocks_Inh_ERule :: (PP_Doc), usageInfo_Inh_ERule :: (Map Identifier Int) }
data Syn_ERule  = Syn_ERule { errors_Syn_ERule :: (Seq Error), mrules_Syn_ERule :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), ruledefs_Syn_ERule :: (Map Identifier (Set String)), ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERule :: (PP_Doc), usedArgs_Syn_ERule :: (Set String) }
{-# INLINABLE wrap_ERule #-}
wrap_ERule :: T_ERule  -> Inh_ERule  -> (Syn_ERule )
wrap_ERule (T_ERule act) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
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
data T_ERule_vIn19  = T_ERule_vIn19 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (Attributes) (Set String) (Map Identifier Type) (String) (String) (NontermIdent) (Options) (Map Identifier (Set VisitKind)) (Attributes) (PP_Doc) (Map Identifier Int)
data T_ERule_vOut19  = T_ERule_vOut19 (Seq Error) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc) (Set String)
{-# NOINLINE sem_ERule_ERule #-}
sem_ERule_ERule :: (Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Maybe Error) -> T_ERule 
sem_ERule_ERule arg_name_ arg_pattern_ arg_rhs_ _ _ arg_explicit_ arg_pure_ arg_mbError_ = T_ERule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_ERule_v19 
      v19 = \ (T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX29 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut40 _patternIattrTypes _patternIattrs _patternIcopy _patternIisUnderscore _patternIsem_lhs) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap)
         (T_Expression_vOut28 _rhsIattrs _rhsIpos _rhsIsemfunc _rhsItks) = inv_Expression_s29 _rhsX29 (T_Expression_vIn28 _rhsOoptions)
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule441 _usedArgs_augmented_f1 _usedArgs_augmented_syn
         _usedArgs_augmented_f1 = rule442 _rhsIattrs
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule443 _rulecode _used
         _rulecode = rule444 _endpragma _genpragma _lambda _pragma _rhsIpos _rhsIsemfunc
         _pragma = rule445 _rhsIpos
         _endpragma = rule446 _lhsImainFile
         _genpragma = rule447 _haspos _lhsIoptions arg_explicit_
         _haspos = rule448 _rhsIpos
         _lambda = rule449 _argPats _lhsIoptions _rhsIattrs arg_name_
         _argPats = rule450 _addbang1 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIlazyIntras _lhsIlocalAttrTypes _lhsIoptions _rhsIattrs
         _argExprs = rule451 _rhsIattrs
         _stepcode = rule452 _argExprs _lhsIoptions _patternIattrTypes _patternIsem_lhs _rhsIattrs arg_name_ arg_pure_
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule453 _stepcode arg_name_
         _used = rule454 _lhsIusageInfo arg_name_
         _kinds = rule455 _lhsIruleKinds arg_name_
         _anyLazyKind = rule456 _kinds
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule457 _patternIattrs arg_name_
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule458 _rhsIattrs arg_name_
         _addbang = rule459 _lhsIoptions
         _addbang1 = rule460 _addbang _anyLazyKind
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule461 _used arg_mbError_
         _usedArgs_augmented_syn = rule462  ()
         _patternOallInhmap = rule463 _lhsIallInhmap
         _patternOallSynmap = rule464 _lhsIallSynmap
         _patternOanyLazyKind = rule465 _anyLazyKind
         _patternOinhmap = rule466 _lhsIinhmap
         _patternOlocalAttrTypes = rule467 _lhsIlocalAttrTypes
         _patternOoptions = rule468 _lhsIoptions
         _patternOsynmap = rule469 _lhsIsynmap
         _rhsOoptions = rule470 _lhsIoptions
         __result_ = T_ERule_vOut19 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERule_s20 v19
   {-# INLINE rule441 #-}
   rule441 = \ _usedArgs_augmented_f1 _usedArgs_augmented_syn ->
     foldr ($) _usedArgs_augmented_syn [_usedArgs_augmented_f1]
   {-# INLINE rule442 #-}
   rule442 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                        Set.union $ Map.keysSet $ Map.mapKeys (\a -> "arg_" ++ a) $ Map.filter isNothing _rhsIattrs
   {-# INLINE rule443 #-}
   {-# LINE 1011 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule443 = \ _rulecode _used ->
                          {-# LINE 1011 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          if _used     == 0
                          then empty
                          else _rulecode
                          {-# LINE 3464 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule444 #-}
   {-# LINE 1014 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule444 = \ _endpragma _genpragma _lambda _pragma ((_rhsIpos) :: Pos) ((_rhsIsemfunc) :: PP_Doc) ->
                          {-# LINE 1014 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          ( if _genpragma
                            then _pragma
                            else empty
                          )
                          >-< _lambda
                          >-< indent ((column _rhsIpos - 2) `max` 2)
                                ( if _genpragma
                                  then _pragma     >-< _rhsIsemfunc >-< _endpragma
                                  else _rhsIsemfunc
                                )
                          {-# LINE 3479 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule445 #-}
   {-# LINE 1026 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule445 = \ ((_rhsIpos) :: Pos) ->
                           {-# LINE 1026 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           "/*# LINE" >#< show (line _rhsIpos) >#< show (file _rhsIpos) >#< "#*/"
                           {-# LINE 3485 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule446 #-}
   {-# LINE 1027 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule446 = \ ((_lhsImainFile) :: String) ->
                           {-# LINE 1027 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           ppWithLineNr (\ln -> "/*# LINE " ++ show (ln+1) ++ " " ++ show _lhsImainFile ++ "#*/")
                           {-# LINE 3491 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule447 #-}
   {-# LINE 1028 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule447 = \ _haspos ((_lhsIoptions) :: Options) explicit_ ->
                           {-# LINE 1028 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           genLinePragmas _lhsIoptions && explicit_ && _haspos
                           {-# LINE 3497 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule448 #-}
   {-# LINE 1029 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule448 = \ ((_rhsIpos) :: Pos) ->
                           {-# LINE 1029 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           line _rhsIpos > 0 && column _rhsIpos >= 0 && not (null (file _rhsIpos))
                           {-# LINE 3503 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule449 #-}
   {-# LINE 1042 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule449 = \ _argPats ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                           {-# LINE 1042 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           name_ >#< _argPats     >#< dummyPat _lhsIoptions (Map.null _rhsIattrs) >#< "="
                           {-# LINE 3509 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule450 #-}
   {-# LINE 1044 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule450 = \ _addbang1 ((_lhsIallInhmap) :: Map NontermIdent Attributes) ((_lhsIallSynmap) :: Map NontermIdent Attributes) ((_lhsIchildTypes) :: Map Identifier Type) ((_lhsIlazyIntras) :: Set String) ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIoptions) :: Options) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                           {-# LINE 1044 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           ppSpaced [ let match | str `Set.member` _lhsIlazyIntras = pp str
                                                | otherwise                        = _addbang1     (pp str)
                                      in case mbAttr of
                                           Just (AttrSyn child nm) | child == _LOC && not (noPerStateTypeSigs _lhsIoptions) ->
                                             case Map.lookup nm _lhsIlocalAttrTypes of
                                               Just tp -> pp_parens (pp_parens match)
                                               Nothing -> match
                                           Just attr | not (noPerRuleTypeSigs _lhsIoptions) ->
                                             case lookupAttrType attr _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes of
                                               Just tpDoc -> pp_parens (pp_parens match)
                                               Nothing    -> match
                                           _ -> match
                                    | (str,mbAttr) <- Map.assocs _rhsIattrs
                                    ]
                           {-# LINE 3528 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule451 #-}
   {-# LINE 1058 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule451 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) ->
                           {-# LINE 1058 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           ppSpaced [ case mbAttr of
                                        Nothing -> "arg_" >|< str
                                        _       -> text str
                                    | (str,mbAttr) <- Map.assocs _rhsIattrs
                                    ]
                           {-# LINE 3538 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 1063 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule452 = \ _argExprs ((_lhsIoptions) :: Options) ((_patternIattrTypes) :: PP_Doc) ((_patternIsem_lhs) ::  PP_Doc ) ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ pure_ ->
                           {-# LINE 1063 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           \kind fmtMode -> if kind `compatibleRule` pure_
                                            then Right $ let oper | pure_     = "="
                                                                  | otherwise = "<-"
                                                             decl = _patternIsem_lhs >#< oper >#< name_ >#< _argExprs     >#< dummyArg _lhsIoptions (Map.null _rhsIattrs)
                                                             tp   = if pure_ && not (noPerRuleTypeSigs _lhsIoptions)
                                                                      then _patternIattrTypes
                                                                      else empty
                                                         in fmtDecl pure_ fmtMode (tp >-< decl)
                                            else Left $ IncompatibleRuleKind name_ kind
                           {-# LINE 3552 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule453 #-}
   {-# LINE 1073 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule453 = \ _stepcode name_ ->
                           {-# LINE 1073 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.singleton name_ _stepcode
                           {-# LINE 3558 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule454 #-}
   {-# LINE 1279 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule454 = \ ((_lhsIusageInfo) :: Map Identifier Int) name_ ->
                                                 {-# LINE 1279 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                 Map.findWithDefault 0 name_ _lhsIusageInfo
                                                 {-# LINE 3564 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule455 #-}
   {-# LINE 1295 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule455 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) name_ ->
                {-# LINE 1295 "./src-ag/ExecutionPlan2Clean.ag" #-}
                Map.findWithDefault Set.empty name_ _lhsIruleKinds
                {-# LINE 3570 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule456 #-}
   {-# LINE 1296 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule456 = \ _kinds ->
                      {-# LINE 1296 "./src-ag/ExecutionPlan2Clean.ag" #-}
                      Set.fold (\k r -> isLazyKind k || r) False _kinds
                      {-# LINE 3576 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule457 #-}
   {-# LINE 1342 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule457 = \ ((_patternIattrs) :: Set String) name_ ->
                           {-# LINE 1342 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.singleton name_ _patternIattrs
                           {-# LINE 3582 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule458 #-}
   {-# LINE 1343 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule458 = \ ((_rhsIattrs) :: Map String (Maybe NonLocalAttr)) name_ ->
                           {-# LINE 1343 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.singleton name_ _rhsIattrs
                           {-# LINE 3588 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule459 #-}
   {-# LINE 1564 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule459 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1564 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 3594 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule460 #-}
   {-# LINE 1575 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule460 = \ _addbang _anyLazyKind ->
                                                     {-# LINE 1575 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     if _anyLazyKind     then id else _addbang
                                                     {-# LINE 3600 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule461 #-}
   {-# LINE 1681 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule461 = \ _used mbError_ ->
                 {-# LINE 1681 "./src-ag/ExecutionPlan2Clean.ag" #-}
                 case mbError_ of
                   Just e | _used     > 0 -> Seq.singleton e
                   _                      -> Seq.empty
                 {-# LINE 3608 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule462 #-}
   rule462 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule465 #-}
   rule465 = \ _anyLazyKind ->
     _anyLazyKind
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- ERules ------------------------------------------------------
-- wrapper
data Inh_ERules  = Inh_ERules { allInhmap_Inh_ERules :: (Map NontermIdent Attributes), allSynmap_Inh_ERules :: (Map NontermIdent Attributes), childTypes_Inh_ERules :: (Map Identifier Type), con_Inh_ERules :: (ConstructorIdent), constructorTypeMap_Inh_ERules :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_ERules :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_ERules :: (String -> String -> String -> Bool -> String), importBlocks_Inh_ERules :: (PP_Doc), inhmap_Inh_ERules :: (Attributes), lazyIntras_Inh_ERules :: (Set String), localAttrTypes_Inh_ERules :: (Map Identifier Type), mainFile_Inh_ERules :: (String), mainName_Inh_ERules :: (String), nt_Inh_ERules :: (NontermIdent), options_Inh_ERules :: (Options), ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)), synmap_Inh_ERules :: (Attributes), textBlocks_Inh_ERules :: (PP_Doc), usageInfo_Inh_ERules :: (Map Identifier Int) }
data Syn_ERules  = Syn_ERules { errors_Syn_ERules :: (Seq Error), mrules_Syn_ERules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), ruledefs_Syn_ERules :: (Map Identifier (Set String)), ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))), sem_rules_Syn_ERules :: (PP_Doc), usedArgs_Syn_ERules :: (Set String) }
{-# INLINABLE wrap_ERules #-}
wrap_ERules :: T_ERules  -> Inh_ERules  -> (Syn_ERules )
wrap_ERules (T_ERules act) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
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
data T_ERules_vIn22  = T_ERules_vIn22 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (Attributes) (Set String) (Map Identifier Type) (String) (String) (NontermIdent) (Options) (Map Identifier (Set VisitKind)) (Attributes) (PP_Doc) (Map Identifier Int)
data T_ERules_vOut22  = T_ERules_vOut22 (Seq Error) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (PP_Doc) (Set String)
{-# NOINLINE sem_ERules_Cons #-}
sem_ERules_Cons :: T_ERule  -> T_ERules  -> T_ERules 
sem_ERules_Cons arg_hd_ arg_tl_ = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_ERule (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_tl_))
         (T_ERule_vOut19 _hdIerrors _hdImrules _hdIruledefs _hdIruleuses _hdIsem_rules _hdIusedArgs) = inv_ERule_s20 _hdX20 (T_ERule_vIn19 _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOconstructorTypeMap _hdOdclModuleHeader _hdOiclModuleHeader _hdOimportBlocks _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOnt _hdOoptions _hdOruleKinds _hdOsynmap _hdOtextBlocks _hdOusageInfo)
         (T_ERules_vOut22 _tlIerrors _tlImrules _tlIruledefs _tlIruleuses _tlIsem_rules _tlIusedArgs) = inv_ERules_s23 _tlX23 (T_ERules_vIn22 _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOconstructorTypeMap _tlOdclModuleHeader _tlOiclModuleHeader _tlOimportBlocks _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOnt _tlOoptions _tlOruleKinds _tlOsynmap _tlOtextBlocks _tlOusageInfo)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule471 _hdIerrors _tlIerrors
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule472 _hdImrules _tlImrules
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule473 _hdIruledefs _tlIruledefs
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule474 _hdIruleuses _tlIruleuses
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule475 _hdIsem_rules _tlIsem_rules
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule476 _hdIusedArgs _tlIusedArgs
         _hdOallInhmap = rule477 _lhsIallInhmap
         _hdOallSynmap = rule478 _lhsIallSynmap
         _hdOchildTypes = rule479 _lhsIchildTypes
         _hdOcon = rule480 _lhsIcon
         _hdOconstructorTypeMap = rule481 _lhsIconstructorTypeMap
         _hdOdclModuleHeader = rule482 _lhsIdclModuleHeader
         _hdOiclModuleHeader = rule483 _lhsIiclModuleHeader
         _hdOimportBlocks = rule484 _lhsIimportBlocks
         _hdOinhmap = rule485 _lhsIinhmap
         _hdOlazyIntras = rule486 _lhsIlazyIntras
         _hdOlocalAttrTypes = rule487 _lhsIlocalAttrTypes
         _hdOmainFile = rule488 _lhsImainFile
         _hdOmainName = rule489 _lhsImainName
         _hdOnt = rule490 _lhsInt
         _hdOoptions = rule491 _lhsIoptions
         _hdOruleKinds = rule492 _lhsIruleKinds
         _hdOsynmap = rule493 _lhsIsynmap
         _hdOtextBlocks = rule494 _lhsItextBlocks
         _hdOusageInfo = rule495 _lhsIusageInfo
         _tlOallInhmap = rule496 _lhsIallInhmap
         _tlOallSynmap = rule497 _lhsIallSynmap
         _tlOchildTypes = rule498 _lhsIchildTypes
         _tlOcon = rule499 _lhsIcon
         _tlOconstructorTypeMap = rule500 _lhsIconstructorTypeMap
         _tlOdclModuleHeader = rule501 _lhsIdclModuleHeader
         _tlOiclModuleHeader = rule502 _lhsIiclModuleHeader
         _tlOimportBlocks = rule503 _lhsIimportBlocks
         _tlOinhmap = rule504 _lhsIinhmap
         _tlOlazyIntras = rule505 _lhsIlazyIntras
         _tlOlocalAttrTypes = rule506 _lhsIlocalAttrTypes
         _tlOmainFile = rule507 _lhsImainFile
         _tlOmainName = rule508 _lhsImainName
         _tlOnt = rule509 _lhsInt
         _tlOoptions = rule510 _lhsIoptions
         _tlOruleKinds = rule511 _lhsIruleKinds
         _tlOsynmap = rule512 _lhsIsynmap
         _tlOtextBlocks = rule513 _lhsItextBlocks
         _tlOusageInfo = rule514 _lhsIusageInfo
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule471 #-}
   rule471 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule472 #-}
   rule472 = \ ((_hdImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ((_tlImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _hdImrules `Map.union` _tlImrules
   {-# INLINE rule473 #-}
   rule473 = \ ((_hdIruledefs) :: Map Identifier (Set String)) ((_tlIruledefs) :: Map Identifier (Set String)) ->
     _hdIruledefs `uwSetUnion` _tlIruledefs
   {-# INLINE rule474 #-}
   rule474 = \ ((_hdIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ((_tlIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _hdIruleuses `uwMapUnion` _tlIruleuses
   {-# INLINE rule475 #-}
   rule475 = \ ((_hdIsem_rules) :: PP_Doc) ((_tlIsem_rules) :: PP_Doc) ->
     _hdIsem_rules >-< _tlIsem_rules
   {-# INLINE rule476 #-}
   rule476 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule510 #-}
   rule510 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule511 #-}
   rule511 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
{-# NOINLINE sem_ERules_Nil #-}
sem_ERules_Nil ::  T_ERules 
sem_ERules_Nil  = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsInt _lhsIoptions _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule515  ()
         _lhsOmrules :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
         _lhsOmrules = rule516  ()
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule517  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule518  ()
         _lhsOsem_rules :: PP_Doc
         _lhsOsem_rules = rule519  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule520  ()
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule515 #-}
   rule515 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule516 #-}
   rule516 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule517 #-}
   rule517 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule518 #-}
   rule518 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule519 #-}
   rule519 = \  (_ :: ()) ->
     empty
   {-# INLINE rule520 #-}
   rule520 = \  (_ :: ()) ->
     Set.empty

-- ExecutionPlan -----------------------------------------------
-- wrapper
data Inh_ExecutionPlan  = Inh_ExecutionPlan { constructorTypeMap_Inh_ExecutionPlan :: (Map NontermIdent ConstructorType), dclModuleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String), iclModuleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String), importBlocks_Inh_ExecutionPlan :: (PP_Doc), inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainBlocksDoc_Inh_ExecutionPlan :: (PP_Doc), mainFile_Inh_ExecutionPlan :: (String), mainName_Inh_ExecutionPlan :: (String), options_Inh_ExecutionPlan :: (Options), synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), textBlockMap_Inh_ExecutionPlan :: (Map BlockInfo PP_Doc), textBlocks_Inh_ExecutionPlan :: (PP_Doc) }
data Syn_ExecutionPlan  = Syn_ExecutionPlan { errors_Syn_ExecutionPlan :: (Seq Error), genIO_Syn_ExecutionPlan :: (IO ()), output_Syn_ExecutionPlan :: (PP_Doc), output_dcl_Syn_ExecutionPlan :: (PP_Doc) }
{-# INLINABLE wrap_ExecutionPlan #-}
wrap_ExecutionPlan :: T_ExecutionPlan  -> Inh_ExecutionPlan  -> (Syn_ExecutionPlan )
wrap_ExecutionPlan (T_ExecutionPlan act) (Inh_ExecutionPlan _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_ExecutionPlan_vIn25 _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks
        (T_ExecutionPlan_vOut25 _lhsOerrors _lhsOgenIO _lhsOoutput _lhsOoutput_dcl) <- return (inv_ExecutionPlan_s26 sem arg)
        return (Syn_ExecutionPlan _lhsOerrors _lhsOgenIO _lhsOoutput _lhsOoutput_dcl)
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
data T_ExecutionPlan_vIn25  = T_ExecutionPlan_vIn25 (Map NontermIdent ConstructorType) (String -> String -> String -> Bool -> String) (String -> String -> String -> Bool -> String) (PP_Doc) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (PP_Doc) (String) (String) (Options) (Map NontermIdent Attributes) (Map BlockInfo PP_Doc) (PP_Doc)
data T_ExecutionPlan_vOut25  = T_ExecutionPlan_vOut25 (Seq Error) (IO ()) (PP_Doc) (PP_Doc)
{-# NOINLINE sem_ExecutionPlan_ExecutionPlan #-}
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals  -> (TypeSyns) -> (Set NontermIdent) -> (Derivings) -> T_ExecutionPlan 
sem_ExecutionPlan_ExecutionPlan arg_nonts_ arg_typeSyns_ arg_wrappers_ arg_derivings_ = T_ExecutionPlan (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_ExecutionPlan_v25 
      v25 = \ (T_ExecutionPlan_vIn25 _lhsIconstructorTypeMap _lhsIdclModuleHeader _lhsIiclModuleHeader _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsIoptions _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_nonts_))
         (T_ENonterminals_vOut10 _nontsIappendCommon _nontsIappendMain _nontsIchildvisit _nontsIerrors _nontsIfromToStates _nontsIgenProdIO _nontsIimports _nontsIinitStates _nontsIoutput _nontsIoutput_dcl _nontsIsemFunBndDefs _nontsIsemFunBndTps _nontsIvisitKinds _nontsIvisitdefs _nontsIvisituses) = inv_ENonterminals_s11 _nontsX11 (T_ENonterminals_vIn10 _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOconstructorTypeMap _nontsOdclModuleHeader _nontsOderivings _nontsOiclModuleHeader _nontsOimportBlocks _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOoptions _nontsOsynmap _nontsOtextBlocks _nontsOtypeSyns _nontsOwrappers)
         _lhsOoutput :: PP_Doc
         _lhsOoutput = rule521 _commonExtra _nontsIoutput _wrappersExtra
         _lhsOoutput_dcl :: PP_Doc
         _lhsOoutput_dcl = rule522 _nontsIoutput_dcl
         _nontsOwrappers = rule523 arg_wrappers_
         _nontsOtypeSyns = rule524 arg_typeSyns_
         _nontsOderivings = rule525 arg_derivings_
         _wrappersExtra = rule526 _lateSemBndDef _lhsIoptions
         _commonExtra = rule527 _lateSemBndTp _lhsIoptions
         _lateSemBndTp = rule528 _lhsImainName _nontsIsemFunBndTps
         _lateSemBndDef = rule529 _lhsImainName _nontsIsemFunBndDefs
         _nontsOallchildvisit = rule530 _nontsIchildvisit
         _nontsOavisitdefs = rule531 _nontsIvisitdefs
         _nontsOavisituses = rule532 _nontsIvisituses
         _lhsOgenIO :: IO ()
         _lhsOgenIO = rule533 _genCommonModule _genMainModule _nontsIgenProdIO
         _mainModuleFile = rule534 _lhsImainFile
         _ppMonadImports = rule535  ()
         _genMainModule = rule536 _lhsIiclModuleHeader _lhsImainBlocksDoc _lhsImainName _mainModuleFile _nontsIappendMain _nontsIimports _ppMonadImports _wrappersExtra
         _commonFile = rule537 _lhsImainFile
         _genCommonModule = rule538 _commonExtra _commonFile _lhsIiclModuleHeader _lhsIimportBlocks _lhsImainName _lhsItextBlocks _nontsIappendCommon _ppMonadImports
         _nontsOallFromToStates = rule539 _nontsIfromToStates
         _nontsOallVisitKinds = rule540 _nontsIvisitKinds
         _nontsOallInitStates = rule541 _nontsIinitStates
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule542 _nontsIerrors
         _nontsOconstructorTypeMap = rule543 _lhsIconstructorTypeMap
         _nontsOdclModuleHeader = rule544 _lhsIdclModuleHeader
         _nontsOiclModuleHeader = rule545 _lhsIiclModuleHeader
         _nontsOimportBlocks = rule546 _lhsIimportBlocks
         _nontsOinhmap = rule547 _lhsIinhmap
         _nontsOlocalAttrTypes = rule548 _lhsIlocalAttrTypes
         _nontsOmainFile = rule549 _lhsImainFile
         _nontsOmainName = rule550 _lhsImainName
         _nontsOoptions = rule551 _lhsIoptions
         _nontsOsynmap = rule552 _lhsIsynmap
         _nontsOtextBlocks = rule553 _lhsItextBlocks
         __result_ = T_ExecutionPlan_vOut25 _lhsOerrors _lhsOgenIO _lhsOoutput _lhsOoutput_dcl
         in __result_ )
     in C_ExecutionPlan_s26 v25
   {-# INLINE rule521 #-}
   {-# LINE 91 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule521 = \ _commonExtra ((_nontsIoutput) :: PP_Doc) _wrappersExtra ->
                                 {-# LINE 91 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _nontsIoutput >-< _commonExtra     >-< _wrappersExtra
                                 {-# LINE 3988 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule522 #-}
   {-# LINE 92 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule522 = \ ((_nontsIoutput_dcl) :: PP_Doc) ->
                                     {-# LINE 92 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _nontsIoutput_dcl
                                     {-# LINE 3994 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule523 #-}
   {-# LINE 99 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule523 = \ wrappers_ ->
                                     {-# LINE 99 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     wrappers_
                                     {-# LINE 4000 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule524 #-}
   {-# LINE 159 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule524 = \ typeSyns_ ->
                                     {-# LINE 159 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     typeSyns_
                                     {-# LINE 4006 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule525 #-}
   {-# LINE 160 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule525 = \ derivings_ ->
                                      {-# LINE 160 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                      derivings_
                                      {-# LINE 4012 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule526 #-}
   {-# LINE 599 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule526 = \ _lateSemBndDef ((_lhsIoptions) :: Options) ->
                        {-# LINE 599 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndDef
                        else empty
                        {-# LINE 4020 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule527 #-}
   {-# LINE 602 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule527 = \ _lateSemBndTp ((_lhsIoptions) :: Options) ->
                        {-# LINE 602 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndTp
                        else empty
                        {-# LINE 4028 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule528 #-}
   {-# LINE 605 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule528 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndTps) :: Seq PP_Doc) ->
                       {-# LINE 605 "./src-ag/ExecutionPlan2Clean.ag" #-}
                       "::" >#< lateBindingTypeNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                        >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndTps)
                       {-# LINE 4035 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule529 #-}
   {-# LINE 607 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule529 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndDefs) :: Seq PP_Doc) ->
                        {-# LINE 607 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        lateBindingFieldNm _lhsImainName >#< "::" >#< lateBindingTypeNm _lhsImainName
                        >-< lateBindingFieldNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                        >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndDefs )
                        {-# LINE 4043 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule530 #-}
   {-# LINE 1223 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule530 = \ ((_nontsIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                          {-# LINE 1223 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          _nontsIchildvisit
                                          {-# LINE 4049 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule531 #-}
   {-# LINE 1367 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule531 = \ ((_nontsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1367 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       _nontsIvisitdefs
                                       {-# LINE 4055 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule532 #-}
   {-# LINE 1368 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule532 = \ ((_nontsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
                                       {-# LINE 1368 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                       _nontsIvisituses
                                       {-# LINE 4061 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule533 #-}
   {-# LINE 1439 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule533 = \ _genCommonModule _genMainModule ((_nontsIgenProdIO) :: IO ()) ->
                                          {-# LINE 1439 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          do _genMainModule
                                             _genCommonModule
                                             _nontsIgenProdIO
                                          {-# LINE 4069 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule534 #-}
   {-# LINE 1442 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule534 = \ ((_lhsImainFile) :: String) ->
                                          {-# LINE 1442 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          _lhsImainFile
                                          {-# LINE 4075 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule535 #-}
   {-# LINE 1443 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule535 = \  (_ :: ()) ->
                                          {-# LINE 1443 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          pp "import qualified Control.Monad.Identity"
                                          {-# LINE 4081 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule536 #-}
   {-# LINE 1444 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule536 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ((_lhsImainBlocksDoc) :: PP_Doc) ((_lhsImainName) :: String) _mainModuleFile ((_nontsIappendMain) :: [PP_Doc]) ((_nontsIimports) :: [PP_Doc]) _ppMonadImports _wrappersExtra ->
                                          {-# LINE 1444 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          writeModule _mainModuleFile
                                            ( [ pp $ _lhsIiclModuleHeader _lhsImainName "" "" False
                                              , _ppMonadImports
                                              , pp $ "import " ++ _lhsImainName ++ "_common"
                                              ]
                                              ++ _nontsIimports
                                              ++ [_lhsImainBlocksDoc]
                                              ++ [_wrappersExtra    ]
                                              ++ _nontsIappendMain
                                            )
                                          {-# LINE 4096 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule537 #-}
   {-# LINE 1454 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule537 = \ ((_lhsImainFile) :: String) ->
                                          {-# LINE 1454 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                                          {-# LINE 4102 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule538 #-}
   {-# LINE 1455 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule538 = \ _commonExtra _commonFile ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ((_lhsIimportBlocks) :: PP_Doc) ((_lhsImainName) :: String) ((_lhsItextBlocks) :: PP_Doc) ((_nontsIappendCommon) :: [PP_Doc]) _ppMonadImports ->
                                          {-# LINE 1455 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                          writeModule _commonFile
                                            ( [ pp $ _lhsIiclModuleHeader _lhsImainName "_common" "" True
                                              , _ppMonadImports
                                              , _lhsIimportBlocks
                                              , _lhsItextBlocks
                                              , _commonExtra
                                              ]
                                              ++ _nontsIappendCommon
                                            )
                                          {-# LINE 4116 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule539 #-}
   {-# LINE 1603 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule539 = \ ((_nontsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
                            {-# LINE 1603 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            _nontsIfromToStates
                            {-# LINE 4122 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule540 #-}
   {-# LINE 1647 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule540 = \ ((_nontsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
                          {-# LINE 1647 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          _nontsIvisitKinds
                          {-# LINE 4128 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule541 #-}
   {-# LINE 1661 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule541 = \ ((_nontsIinitStates) :: Map NontermIdent Int) ->
                          {-# LINE 1661 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          _nontsIinitStates
                          {-# LINE 4134 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule542 #-}
   rule542 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule543 #-}
   rule543 = \ ((_lhsIconstructorTypeMap) :: Map NontermIdent ConstructorType) ->
     _lhsIconstructorTypeMap
   {-# INLINE rule544 #-}
   rule544 = \ ((_lhsIdclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIdclModuleHeader
   {-# INLINE rule545 #-}
   rule545 = \ ((_lhsIiclModuleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsIiclModuleHeader
   {-# INLINE rule546 #-}
   rule546 = \ ((_lhsIimportBlocks) :: PP_Doc) ->
     _lhsIimportBlocks
   {-# INLINE rule547 #-}
   rule547 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule548 #-}
   rule548 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule549 #-}
   rule549 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule552 #-}
   rule552 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule553 #-}
   rule553 = \ ((_lhsItextBlocks) :: PP_Doc) ->
     _lhsItextBlocks

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
         _lhsOtks = rule554 arg_tks_
         _lhsOpos :: Pos
         _lhsOpos = rule555 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule556 _inhhstoken arg_tks_
         _lhsOsemfunc :: PP_Doc
         _lhsOsemfunc = rule557 _inhhstoken arg_tks_
         _inhhstoken = rule558 _lhsIoptions
         __result_ = T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks
         in __result_ )
     in C_Expression_s29 v28
   {-# INLINE rule554 #-}
   {-# LINE 1077 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule554 = \ tks_ ->
                           {-# LINE 1077 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           tks_
                           {-# LINE 4226 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule555 #-}
   {-# LINE 1120 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule555 = \ pos_ ->
                                        {-# LINE 1120 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                        pos_
                                        {-# LINE 4232 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule556 #-}
   {-# LINE 1207 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule556 = \ _inhhstoken tks_ ->
                               {-# LINE 1207 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               Map.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) _inhhstoken    )) tks_
                               {-# LINE 4238 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule557 #-}
   {-# LINE 1208 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule557 = \ _inhhstoken tks_ ->
                               {-# LINE 1208 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) _inhhstoken    )) tks_
                               {-# LINE 4244 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule558 #-}
   {-# LINE 1209 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule558 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 1209 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  Inh_HsToken _lhsIoptions
                                  {-# LINE 4250 "dist/build/ExecutionPlan2Clean.hs"#-}

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
         _lhsOattrs = rule559 arg_var_
         _tok = rule560 arg_pos_ arg_var_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule561 _tok
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule559 #-}
   {-# LINE 1166 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule559 = \ var_ ->
                              {-# LINE 1166 "./src-ag/ExecutionPlan2Clean.ag" #-}
                              Map.singleton (fieldname var_) Nothing
                              {-# LINE 4307 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule560 #-}
   {-# LINE 1412 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule560 = \ pos_ var_ ->
                          {-# LINE 1412 "./src-ag/ExecutionPlan2Clean.ag" #-}
                          (pos_,fieldname var_)
                          {-# LINE 4313 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule561 #-}
   rule561 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _mbAttr = rule562 arg_attr_ arg_field_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule563 _lhsIoptions _mbAttr arg_attr_ arg_field_
         _addTrace = rule564 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule565 _addTrace _lhsIoptions arg_attr_ arg_field_ arg_pos_
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule562 #-}
   {-# LINE 1167 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule562 = \ attr_ field_ ->
                              {-# LINE 1167 "./src-ag/ExecutionPlan2Clean.ag" #-}
                              if field_ == _INST || field_ == _FIELD || field_ == _INST'
                              then Nothing
                              else Just $ mkNonLocalAttr (field_ == _LHS) field_ attr_
                              {-# LINE 4340 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule563 #-}
   {-# LINE 1170 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule563 = \ ((_lhsIoptions) :: Options) _mbAttr attr_ field_ ->
                              {-# LINE 1170 "./src-ag/ExecutionPlan2Clean.ag" #-}
                              Map.singleton (attrname _lhsIoptions True field_ attr_) _mbAttr
                              {-# LINE 4346 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule564 #-}
   {-# LINE 1416 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule564 = \ attr_ field_ rdesc_ ->
                        {-# LINE 1416 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(trace_n " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                          Nothing -> id
                        {-# LINE 4354 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule565 #-}
   {-# LINE 1419 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule565 = \ _addTrace ((_lhsIoptions) :: Options) attr_ field_ pos_ ->
                   {-# LINE 1419 "./src-ag/ExecutionPlan2Clean.ag" #-}
                   (pos_, _addTrace     $ attrname _lhsIoptions True field_ attr_)
                   {-# LINE 4360 "dist/build/ExecutionPlan2Clean.hs"#-}
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule566 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule567  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule566 #-}
   {-# LINE 1421 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule566 = \ pos_ value_ ->
                         {-# LINE 1421 "./src-ag/ExecutionPlan2Clean.ag" #-}
                         (pos_, value_)
                         {-# LINE 4380 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule567 #-}
   rule567 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule568 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule569  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule568 #-}
   {-# LINE 1423 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule568 = \ pos_ value_ ->
                           {-# LINE 1423 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 4406 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule569 #-}
   rule569 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule570 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule571  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule570 #-}
   {-# LINE 1428 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule570 = \ pos_ value_ ->
                           {-# LINE 1428 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 4429 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule572 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule573  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule572 #-}
   {-# LINE 1429 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule572 = \ pos_ ->
                           {-# LINE 1429 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           (pos_, "")
                           {-# LINE 4452 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule573 #-}
   rule573 = \  (_ :: ()) ->
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
         _lhsOtks = rule574 _hdItok _tlItks
         _hdOoptions = rule575 _lhsIoptions
         _tlOoptions = rule576 _lhsIoptions
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule574 #-}
   {-# LINE 1408 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule574 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 1408 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 4510 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule575 #-}
   rule575 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule577  ()
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule577 #-}
   {-# LINE 1409 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule577 = \  (_ :: ()) ->
                     {-# LINE 1409 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     []
                     {-# LINE 4534 "dist/build/ExecutionPlan2Clean.hs"#-}

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
         _tokensOoptions = rule578 _lhsIoptions
         __result_ = T_HsTokensRoot_vOut37 
         in __result_ )
     in C_HsTokensRoot_s38 v37
   {-# INLINE rule578 #-}
   rule578 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

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
         _lhsOsem_lhs = rule579 _addbang1 _patsIsem_lhs arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule580  ()
         _addbang = rule581 _lhsIoptions
         _addbang1 = rule582 _addbang _lhsIanyLazyKind
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule583 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule584 _patsIattrs
         _copy = rule585 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule586 _copy
         _patsOallInhmap = rule587 _lhsIallInhmap
         _patsOallSynmap = rule588 _lhsIallSynmap
         _patsOanyLazyKind = rule589 _lhsIanyLazyKind
         _patsOinhmap = rule590 _lhsIinhmap
         _patsOlocalAttrTypes = rule591 _lhsIlocalAttrTypes
         _patsOoptions = rule592 _lhsIoptions
         _patsOsynmap = rule593 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule579 #-}
   {-# LINE 1134 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule579 = \ _addbang1 ((_patsIsem_lhs) :: [PP_Doc]) name_ ->
                                  {-# LINE 1134 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  _addbang1     $ pp_parens $ name_ >#< hv_sp _patsIsem_lhs
                                  {-# LINE 4654 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule580 #-}
   {-# LINE 1141 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule580 = \  (_ :: ()) ->
                                    {-# LINE 1141 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    False
                                    {-# LINE 4660 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule581 #-}
   {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule581 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4666 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule582 #-}
   {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule582 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4672 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule583 #-}
   rule583 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule584 #-}
   rule584 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule585 #-}
   rule585 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule586 #-}
   rule586 = \ _copy ->
     _copy
   {-# INLINE rule587 #-}
   rule587 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule589 #-}
   rule589 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule590 #-}
   rule590 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule591 #-}
   rule591 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule592 #-}
   rule592 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule593 #-}
   rule593 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _lhsOsem_lhs = rule594 _addbang1 _patsIsem_lhs
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule595  ()
         _addbang = rule596 _lhsIoptions
         _addbang1 = rule597 _addbang _lhsIanyLazyKind
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule598 _patsIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule599 _patsIattrs
         _copy = rule600 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule601 _copy
         _patsOallInhmap = rule602 _lhsIallInhmap
         _patsOallSynmap = rule603 _lhsIallSynmap
         _patsOanyLazyKind = rule604 _lhsIanyLazyKind
         _patsOinhmap = rule605 _lhsIinhmap
         _patsOlocalAttrTypes = rule606 _lhsIlocalAttrTypes
         _patsOoptions = rule607 _lhsIoptions
         _patsOsynmap = rule608 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule594 #-}
   {-# LINE 1133 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule594 = \ _addbang1 ((_patsIsem_lhs) :: [PP_Doc]) ->
                                  {-# LINE 1133 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  _addbang1     $ pp_block "(" ")" "," _patsIsem_lhs
                                  {-# LINE 4743 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule595 #-}
   {-# LINE 1142 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule595 = \  (_ :: ()) ->
                                    {-# LINE 1142 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    False
                                    {-# LINE 4749 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule596 #-}
   {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule596 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4755 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule597 #-}
   {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule597 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4761 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule598 #-}
   rule598 = \ ((_patsIattrTypes) :: PP_Doc) ->
     _patsIattrTypes
   {-# INLINE rule599 #-}
   rule599 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule600 #-}
   rule600 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule601 #-}
   rule601 = \ _copy ->
     _copy
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule605 #-}
   rule605 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule607 #-}
   rule607 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule608 #-}
   rule608 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _varPat = rule609 _lhsIoptions arg_attr_ arg_field_
         _patExpr = rule610 _patIisUnderscore _patIsem_lhs _varPat
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule611 _addbang1 _patExpr
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule612  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule613 _lhsIoptions _patIattrs arg_attr_ arg_field_
         _mbTp = rule614 _lhsIlocalAttrTypes _lhsIsynmap arg_attr_ arg_field_
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule615  ()
         _addbang = rule616 _lhsIoptions
         _addbang1 = rule617 _addbang _lhsIanyLazyKind
         _copy = rule618 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule619 _copy
         _patOallInhmap = rule620 _lhsIallInhmap
         _patOallSynmap = rule621 _lhsIallSynmap
         _patOanyLazyKind = rule622 _lhsIanyLazyKind
         _patOinhmap = rule623 _lhsIinhmap
         _patOlocalAttrTypes = rule624 _lhsIlocalAttrTypes
         _patOoptions = rule625 _lhsIoptions
         _patOsynmap = rule626 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule609 #-}
   {-# LINE 1128 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule609 = \ ((_lhsIoptions) :: Options) attr_ field_ ->
                                  {-# LINE 1128 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  text $ attrname _lhsIoptions False field_ attr_
                                  {-# LINE 4835 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule610 #-}
   {-# LINE 1129 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule610 = \ ((_patIisUnderscore) :: Bool) ((_patIsem_lhs) ::  PP_Doc ) _varPat ->
                                  {-# LINE 1129 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  if _patIisUnderscore
                                  then _varPat
                                  else _varPat     >|< "@" >|< _patIsem_lhs
                                  {-# LINE 4843 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule611 #-}
   {-# LINE 1132 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule611 = \ _addbang1 _patExpr ->
                                  {-# LINE 1132 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  _addbang1     _patExpr
                                  {-# LINE 4849 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule612 #-}
   {-# LINE 1143 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule612 = \  (_ :: ()) ->
                                    {-# LINE 1143 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    False
                                    {-# LINE 4855 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule613 #-}
   {-# LINE 1149 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule613 = \ ((_lhsIoptions) :: Options) ((_patIattrs) :: Set String) attr_ field_ ->
                    {-# LINE 1149 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    Set.insert (attrname _lhsIoptions False field_ attr_) _patIattrs
                    {-# LINE 4861 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule614 #-}
   {-# LINE 1154 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule614 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ((_lhsIsynmap) :: Attributes) attr_ field_ ->
                    {-# LINE 1154 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    if field_ == _LHS
                    then Map.lookup attr_ _lhsIsynmap
                    else if field_ == _LOC
                         then Map.lookup attr_ _lhsIlocalAttrTypes
                         else Nothing
                    {-# LINE 4871 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule615 #-}
   {-# LINE 1159 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule615 = \  (_ :: ()) ->
                    {-# LINE 1159 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    empty
                    {-# LINE 4877 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule616 #-}
   {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule616 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1571 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                     {-# LINE 4883 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule617 #-}
   {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule617 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1576 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 4889 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule618 #-}
   rule618 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule619 #-}
   rule619 = \ _copy ->
     _copy
   {-# INLINE rule620 #-}
   rule620 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule621 #-}
   rule621 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule623 #-}
   rule623 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule624 #-}
   rule624 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule625 #-}
   rule625 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule626 #-}
   rule626 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _lhsOsem_lhs = rule627 _patIsem_lhs
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule628 _patIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule629 _patIattrs
         _copy = rule630 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule631 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule632 _patIisUnderscore
         _patOallInhmap = rule633 _lhsIallInhmap
         _patOallSynmap = rule634 _lhsIallSynmap
         _patOanyLazyKind = rule635 _lhsIanyLazyKind
         _patOinhmap = rule636 _lhsIinhmap
         _patOlocalAttrTypes = rule637 _lhsIlocalAttrTypes
         _patOoptions = rule638 _lhsIoptions
         _patOsynmap = rule639 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule627 #-}
   {-# LINE 1136 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule627 = \ ((_patIsem_lhs) ::  PP_Doc ) ->
                                  {-# LINE 1136 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  text "~" >|< pp_parens _patIsem_lhs
                                  {-# LINE 4952 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule628 #-}
   rule628 = \ ((_patIattrTypes) :: PP_Doc) ->
     _patIattrTypes
   {-# INLINE rule629 #-}
   rule629 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule630 #-}
   rule630 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule631 #-}
   rule631 = \ _copy ->
     _copy
   {-# INLINE rule632 #-}
   rule632 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule633 #-}
   rule633 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule634 #-}
   rule634 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule635 #-}
   rule635 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule636 #-}
   rule636 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule637 #-}
   rule637 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule638 #-}
   rule638 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOsem_lhs ::  PP_Doc 
         _lhsOsem_lhs = rule640  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule641  ()
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule642  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule643  ()
         _copy = rule644 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule645 _copy
         __result_ = T_Pattern_vOut40 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule640 #-}
   {-# LINE 1135 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule640 = \  (_ :: ()) ->
                                  {-# LINE 1135 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  text "_"
                                  {-# LINE 5015 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule641 #-}
   {-# LINE 1144 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule641 = \  (_ :: ()) ->
                                    {-# LINE 1144 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                    True
                                    {-# LINE 5021 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule642 #-}
   rule642 = \  (_ :: ()) ->
     empty
   {-# INLINE rule643 #-}
   rule643 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule644 #-}
   rule644 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule645 #-}
   rule645 = \ _copy ->
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
         _lhsOattrTypes = rule646 _hdIattrTypes _tlIattrTypes
         _lhsOattrs :: Set String
         _lhsOattrs = rule647 _hdIattrs _tlIattrs
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule648 _hdIsem_lhs _tlIsem_lhs
         _copy = rule649 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule650 _copy
         _hdOallInhmap = rule651 _lhsIallInhmap
         _hdOallSynmap = rule652 _lhsIallSynmap
         _hdOanyLazyKind = rule653 _lhsIanyLazyKind
         _hdOinhmap = rule654 _lhsIinhmap
         _hdOlocalAttrTypes = rule655 _lhsIlocalAttrTypes
         _hdOoptions = rule656 _lhsIoptions
         _hdOsynmap = rule657 _lhsIsynmap
         _tlOallInhmap = rule658 _lhsIallInhmap
         _tlOallSynmap = rule659 _lhsIallSynmap
         _tlOanyLazyKind = rule660 _lhsIanyLazyKind
         _tlOinhmap = rule661 _lhsIinhmap
         _tlOlocalAttrTypes = rule662 _lhsIlocalAttrTypes
         _tlOoptions = rule663 _lhsIoptions
         _tlOsynmap = rule664 _lhsIsynmap
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule646 #-}
   rule646 = \ ((_hdIattrTypes) :: PP_Doc) ((_tlIattrTypes) :: PP_Doc) ->
     _hdIattrTypes >-< _tlIattrTypes
   {-# INLINE rule647 #-}
   rule647 = \ ((_hdIattrs) :: Set String) ((_tlIattrs) :: Set String) ->
     _hdIattrs `Set.union` _tlIattrs
   {-# INLINE rule648 #-}
   rule648 = \ ((_hdIsem_lhs) ::  PP_Doc ) ((_tlIsem_lhs) :: [PP_Doc]) ->
     _hdIsem_lhs : _tlIsem_lhs
   {-# INLINE rule649 #-}
   rule649 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule650 #-}
   rule650 = \ _copy ->
     _copy
   {-# INLINE rule651 #-}
   rule651 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule652 #-}
   rule652 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule653 #-}
   rule653 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule654 #-}
   rule654 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule655 #-}
   rule655 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule656 #-}
   rule656 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule657 #-}
   rule657 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule658 #-}
   rule658 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule659 #-}
   rule659 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule660 #-}
   rule660 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule661 #-}
   rule661 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule662 #-}
   rule662 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule663 #-}
   rule663 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule664 #-}
   rule664 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrTypes :: PP_Doc
         _lhsOattrTypes = rule665  ()
         _lhsOattrs :: Set String
         _lhsOattrs = rule666  ()
         _lhsOsem_lhs :: [PP_Doc]
         _lhsOsem_lhs = rule667  ()
         _copy = rule668  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule669 _copy
         __result_ = T_Patterns_vOut43 _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule665 #-}
   rule665 = \  (_ :: ()) ->
     empty
   {-# INLINE rule666 #-}
   rule666 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule667 #-}
   rule667 = \  (_ :: ()) ->
     []
   {-# INLINE rule668 #-}
   rule668 = \  (_ :: ()) ->
     []
   {-# INLINE rule669 #-}
   rule669 = \ _copy ->
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
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOruledefs _stepsOruleuses)
         _lhsOallvisits ::  VisitStateState 
         _lhsOallvisits = rule670 arg_from_ arg_ident_ arg_to_
         _nameT_visit = rule671 _lhsInt arg_ident_
         _nameTIn_visit = rule672 _lhsInt arg_ident_
         _nameTOut_visit = rule673 _lhsInt arg_ident_
         _nameTNext_visit = rule674 _lhsInt arg_to_
         _nextVisitInfo = rule675 _lhsInextVisits arg_to_
         _typecon = rule676 _lhsIoptions arg_kind_
         _t_params = rule677 _lhsIparams
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule678 _addbang1 _inhpart _lhsIoptions _nameTIn_visit _nameTNext_visit _nameTOut_visit _nameT_visit _nextVisitInfo _synpart _t_params _typecon
         _inhpart = rule679 _lhsIinhmap _ppTypeList arg_inh_
         _synpart = rule680 _lhsIsynmap _ppTypeList arg_syn_
         _ppTypeList = rule681 _addbang1
         _lhsOsem_visit ::   (StateIdentifier,Bool -> PP_Doc)  
         _lhsOsem_visit = rule682 _addbang _inhpats _lhsIoptions _nameTIn_visit _stepsClosing _stepsInitial _stepsIsem_steps arg_from_ arg_ident_
         _stepsInitial = rule683 arg_kind_
         _stepsClosing = rule684 _addbang _nextStBuild _resultval arg_kind_
         _vname = rule685 arg_ident_
         _inhpats = rule686 _lhsIoptions arg_inh_
         _inhargs = rule687 _lhsIoptions arg_inh_
         _synargs = rule688 _lhsIoptions arg_syn_
         _nextargsMp = rule689 _lhsIallintramap arg_to_
         _nextargs = rule690 _nextargsMp
         _nextst = rule691 _lhsIoptions _nextargs _nextargsMp arg_to_
         _resultval = rule692 _nameTOut_visit _nextStRef _synargs
         (_nextStBuild,_nextStRef) = rule693 _addbang _nextVisitInfo _nextst
         _stepsOkind = rule694 arg_kind_
         _stepsOfmtMode = rule695 arg_kind_
         _stepsOindex = rule696  ()
         _prevVisitInfo = rule697 _lhsInextVisits arg_from_
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule698 _invokecode arg_ident_
         _invokecode = rule699 _addbang _inhargs _lhsInt _lhsIoptions _nameTIn_visit _nameTOut_visit _nextVisitInfo _prevVisitInfo arg_from_ arg_ident_ arg_kind_ arg_syn_ arg_to_
         _thisintra = rule700 _defsAsMap _nextintra _uses
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule701 _thisintra arg_from_
         _nextintra = rule702 _lhsIallintramap arg_to_
         _uses = rule703 _lhsIoptions _stepsIuses arg_syn_
         _inhVarNms = rule704 _lhsIoptions arg_inh_
         _defs = rule705 _inhVarNms _lhsIterminaldefs _stepsIdefs
         _defsAsMap = rule706 _defs
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule707 arg_ident_ arg_syn_
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule708 arg_ident_ arg_inh_
         _lazyIntrasInh = rule709 _inhVarNms _stepsIdefs arg_kind_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule710 _lazyIntrasInh _stepsIlazyIntras
         _addbang = rule711 _lhsIoptions
         _addbang1 = rule712 _addbang arg_kind_
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule713 arg_from_ arg_ident_ arg_to_
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule714 arg_ident_ arg_kind_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule715 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule716 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule717 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule718 _stepsIusedArgs
         _stepsOallFromToStates = rule719 _lhsIallFromToStates
         _stepsOallInitStates = rule720 _lhsIallInitStates
         _stepsOallVisitKinds = rule721 _lhsIallVisitKinds
         _stepsOallchildvisit = rule722 _lhsIallchildvisit
         _stepsOavisitdefs = rule723 _lhsIavisitdefs
         _stepsOavisituses = rule724 _lhsIavisituses
         _stepsOchildTypes = rule725 _lhsIchildTypes
         _stepsOchildintros = rule726 _lhsIchildintros
         _stepsOmrules = rule727 _lhsImrules
         _stepsOoptions = rule728 _lhsIoptions
         _stepsOruledefs = rule729 _lhsIruledefs
         _stepsOruleuses = rule730 _lhsIruleuses
         __result_ = T_Visit_vOut46 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visit_s47 v46
   {-# INLINE rule670 #-}
   {-# LINE 382 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule670 = \ from_ ident_ to_ ->
                            {-# LINE 382 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 5316 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule671 #-}
   {-# LINE 453 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule671 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 453 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  conNmTVisit _lhsInt ident_
                                  {-# LINE 5322 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule672 #-}
   {-# LINE 454 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule672 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 454 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  conNmTVisitIn _lhsInt ident_
                                  {-# LINE 5328 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule673 #-}
   {-# LINE 455 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule673 = \ ((_lhsInt) :: NontermIdent) ident_ ->
                                  {-# LINE 455 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  conNmTVisitOut _lhsInt ident_
                                  {-# LINE 5334 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule674 #-}
   {-# LINE 456 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule674 = \ ((_lhsInt) :: NontermIdent) to_ ->
                                  {-# LINE 456 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  conNmTNextVisit _lhsInt to_
                                  {-# LINE 5340 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule675 #-}
   {-# LINE 458 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule675 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) to_ ->
                                  {-# LINE 458 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  Map.findWithDefault ManyVis to_ _lhsInextVisits
                                  {-# LINE 5346 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule676 #-}
   {-# LINE 459 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule676 = \ ((_lhsIoptions) :: Options) kind_ ->
                                  {-# LINE 459 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  case kind_ of
                                    VisitPure _  -> empty
                                    VisitMonadic -> ppMonadType _lhsIoptions
                                  {-# LINE 5354 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule677 #-}
   {-# LINE 463 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule677 = \ ((_lhsIparams) :: [Identifier]) ->
                            {-# LINE 463 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            ppSpaced _lhsIparams
                            {-# LINE 5360 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule678 #-}
   {-# LINE 464 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule678 = \ _addbang1 _inhpart ((_lhsIoptions) :: Options) _nameTIn_visit _nameTNext_visit _nameTOut_visit _nameT_visit _nextVisitInfo _synpart _t_params _typecon ->
                           {-# LINE 464 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           "::" >#< _nameT_visit     >#< _t_params     >#< ":==" >#<
                             pp_parens (_nameTIn_visit     >#< _t_params    )
                               >#< ( if dummyTokenVisit _lhsIoptions
                                     then "->" >#< dummyType _lhsIoptions True
                                     else empty
                                   )
                               >#< "->" >#< _typecon     >#< pp_parens (_nameTOut_visit     >#< _t_params    )
                           >-< "::" >#< _nameTIn_visit     >#< _t_params     >#< "=" >#< _nameTIn_visit     >#<
                                _inhpart
                           >-< "::" >#< _nameTOut_visit     >#< _t_params     >#< "=" >#< _nameTOut_visit     >#<
                                _synpart     >#< case _nextVisitInfo     of
                                                   NoneVis    -> empty
                                                   _          -> _addbang1     $ pp_parens (_nameTNext_visit     >#< _t_params    )
                           {-# LINE 5378 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule679 #-}
   {-# LINE 478 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule679 = \ ((_lhsIinhmap) :: Attributes) _ppTypeList inh_ ->
                            {-# LINE 478 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            _ppTypeList     inh_ _lhsIinhmap
                            {-# LINE 5384 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule680 #-}
   {-# LINE 479 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule680 = \ ((_lhsIsynmap) :: Attributes) _ppTypeList syn_ ->
                            {-# LINE 479 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            _ppTypeList     syn_ _lhsIsynmap
                            {-# LINE 5390 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule681 #-}
   {-# LINE 480 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule681 = \ _addbang1 ->
                             {-# LINE 480 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             \s m -> ppSpaced $ map (\i -> _addbang1     $ pp_parens $ case Map.lookup i m of
                                                                                        Just tp -> ppTp tp ) $ Set.toList s
                             {-# LINE 5397 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule682 #-}
   {-# LINE 773 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule682 = \ _addbang _inhpats ((_lhsIoptions) :: Options) _nameTIn_visit _stepsClosing _stepsInitial ((_stepsIsem_steps) :: PP_Doc) from_ ident_ ->
                            {-# LINE 773 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            ( from_
                            , \_ ->
                                 "v" >|< ident_ >#< (_addbang     $ pp_parens (_nameTIn_visit     >#< _inhpats    )) >#< "="
                                 >#< ( if dummyTokenVisit _lhsIoptions
                                       then pp $ dummyPat _lhsIoptions True
                                       else empty
                                     )
                                 >-< indent 10 (_stepsInitial
                                 >-< indent 4 _stepsIsem_steps) >-< indent 10 _stepsClosing
                            )
                            {-# LINE 5412 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule683 #-}
   {-# LINE 791 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule683 = \ kind_ ->
                               {-# LINE 791 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               case kind_ of
                                 VisitPure False -> text "let"
                                 VisitPure True  -> empty
                                 VisitMonadic    -> text "do"
                               {-# LINE 5421 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule684 #-}
   {-# LINE 795 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule684 = \ _addbang _nextStBuild _resultval kind_ ->
                                {-# LINE 795 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                let decls =  _nextStBuild
                                             >-<  _addbang     (pp resultValName) >#< "=" >#< _resultval
                                in case kind_ of
                                     VisitPure False -> indent 4 decls
                                                        >-< "in" >#< resultValName
                                     VisitPure True  -> "let" >#< decls
                                                        >-< indent 1 ("in" >#< resultValName)
                                     VisitMonadic    -> "let" >#< decls
                                                        >-< "lift" >#< resultValName
                                {-# LINE 5435 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule685 #-}
   {-# LINE 804 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule685 = \ ident_ ->
                            {-# LINE 804 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            "v" >|< ident_
                            {-# LINE 5441 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule686 #-}
   {-# LINE 805 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule686 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 805 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            ppSpaced $ map (\arg -> pp $ attrname _lhsIoptions True _LHS arg) $ Set.toList inh_
                            {-# LINE 5447 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule687 #-}
   {-# LINE 806 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule687 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 806 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            \chn -> ppSpaced $ map (attrname _lhsIoptions False chn) $ Set.toList inh_
                            {-# LINE 5453 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule688 #-}
   {-# LINE 807 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule688 = \ ((_lhsIoptions) :: Options) syn_ ->
                            {-# LINE 807 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            ppSpaced $ map (\arg -> attrname _lhsIoptions False _LHS arg) $ Set.toList syn_
                            {-# LINE 5459 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule689 #-}
   {-# LINE 808 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule689 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                             {-# LINE 808 "./src-ag/ExecutionPlan2Clean.ag" #-}
                             maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                             {-# LINE 5465 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule690 #-}
   {-# LINE 809 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule690 = \ _nextargsMp ->
                            {-# LINE 809 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            ppSpaced $ Map.keys $ _nextargsMp
                            {-# LINE 5471 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule691 #-}
   {-# LINE 810 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule691 = \ ((_lhsIoptions) :: Options) _nextargs _nextargsMp to_ ->
                            {-# LINE 810 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            "st" >|< to_ >#< _nextargs     >#< dummyArg _lhsIoptions (Map.null _nextargsMp    )
                            {-# LINE 5477 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule692 #-}
   {-# LINE 811 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule692 = \ _nameTOut_visit _nextStRef _synargs ->
                            {-# LINE 811 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            _nameTOut_visit     >#< _synargs     >#< _nextStRef
                            {-# LINE 5483 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule693 #-}
   {-# LINE 813 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule693 = \ _addbang _nextVisitInfo _nextst ->
                {-# LINE 813 "./src-ag/ExecutionPlan2Clean.ag" #-}
                case _nextVisitInfo     of
                  NoneVis  -> (empty, empty)
                  _        -> (_addbang     (pp nextStName) >#< "=" >#< _nextst    , pp nextStName)
                {-# LINE 5491 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule694 #-}
   {-# LINE 827 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule694 = \ kind_ ->
                                  {-# LINE 827 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                  kind_
                                  {-# LINE 5497 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule695 #-}
   {-# LINE 868 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule695 = \ kind_ ->
                    {-# LINE 868 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    case kind_ of
                      VisitPure False -> FormatLetDecl
                      VisitPure True  -> FormatLetLine
                      VisitMonadic    -> FormatDo
                    {-# LINE 5506 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule696 #-}
   {-# LINE 917 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule696 = \  (_ :: ()) ->
                                     {-# LINE 917 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     0
                                     {-# LINE 5512 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule697 #-}
   {-# LINE 1227 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule697 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) from_ ->
                           {-# LINE 1227 "./src-ag/ExecutionPlan2Clean.ag" #-}
                           Map.findWithDefault ManyVis from_ _lhsInextVisits
                           {-# LINE 5518 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule698 #-}
   {-# LINE 1228 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule698 = \ _invokecode ident_ ->
                        {-# LINE 1228 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        Map.singleton ident_ _invokecode
                        {-# LINE 5524 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule699 #-}
   {-# LINE 1229 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule699 = \ _addbang _inhargs ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) _nameTIn_visit _nameTOut_visit _nextVisitInfo _prevVisitInfo from_ ident_ kind_ syn_ to_ ->
                        {-# LINE 1229 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        \chn kind ->
                          if kind `compatibleKind` kind_
                          then Right $
                            let pat | isLazyKind kind_ = pat0
                                    | otherwise        = _addbang     pat0
                                pat0  = pp_parens pat1
                                pat1  = _nameTOut_visit     >#< (ppSpaced $ map (attrname _lhsIoptions True chn) $ Set.toList syn_)
                                                            >#< cont
                                cont  = case _nextVisitInfo     of
                                          NoneVis  -> empty
                                          _        -> ch1
                                ch0   = text $ stname chn from_
                                ch1   = text $ stname chn to_
                                expr  = case (kind, kind_) of
                                          (VisitPure _,  VisitPure _)  -> expr0
                                          (VisitPure _,  VisitMonadic) -> unMon _lhsIoptions >#< expr0
                                          (VisitMonadic, VisitPure _)  -> "lift" >#< expr0
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
                        {-# LINE 5558 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule700 #-}
   {-# LINE 1325 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule700 = \ _defsAsMap _nextintra _uses ->
                            {-# LINE 1325 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            (_uses     `Map.union` _nextintra    ) `Map.difference` _defsAsMap
                            {-# LINE 5564 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule701 #-}
   {-# LINE 1326 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule701 = \ _thisintra from_ ->
                            {-# LINE 1326 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Map.singleton from_ _thisintra
                            {-# LINE 5570 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule702 #-}
   {-# LINE 1327 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule702 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) to_ ->
                            {-# LINE 1327 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 5576 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule703 #-}
   {-# LINE 1328 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule703 = \ ((_lhsIoptions) :: Options) ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) syn_ ->
                            {-# LINE 1328 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            let mp1 = _stepsIuses
                                mp2 = Map.fromList [ (lhsname _lhsIoptions False i, Just (AttrSyn _LHS i)) | i <- Set.elems syn_ ]
                            in mp1 `Map.union` mp2
                            {-# LINE 5584 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule704 #-}
   {-# LINE 1331 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule704 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 1331 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Set.map (lhsname _lhsIoptions True) inh_
                            {-# LINE 5590 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule705 #-}
   {-# LINE 1332 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule705 = \ _inhVarNms ((_lhsIterminaldefs) :: Set String) ((_stepsIdefs) :: Set String) ->
                            {-# LINE 1332 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            _stepsIdefs `Set.union` _inhVarNms     `Set.union` _lhsIterminaldefs
                            {-# LINE 5596 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule706 #-}
   {-# LINE 1333 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule706 = \ _defs ->
                            {-# LINE 1333 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Map.fromList [ (a, Nothing) | a <- Set.elems _defs     ]
                            {-# LINE 5602 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule707 #-}
   {-# LINE 1357 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule707 = \ ident_ syn_ ->
                            {-# LINE 1357 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 5608 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule708 #-}
   {-# LINE 1358 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule708 = \ ident_ inh_ ->
                            {-# LINE 1358 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 5614 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule709 #-}
   {-# LINE 1390 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule709 = \ _inhVarNms ((_stepsIdefs) :: Set String) kind_ ->
                        {-# LINE 1390 "./src-ag/ExecutionPlan2Clean.ag" #-}
                        case kind_ of
                          VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                          _               -> Set.empty
                        {-# LINE 5622 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule710 #-}
   {-# LINE 1393 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule710 = \ _lazyIntrasInh ((_stepsIlazyIntras) :: Set String) ->
                     {-# LINE 1393 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                     {-# LINE 5628 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule711 #-}
   {-# LINE 1565 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule711 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1565 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 5634 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule712 #-}
   {-# LINE 1573 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule712 = \ _addbang kind_ ->
                                                     {-# LINE 1573 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                     if isLazyKind kind_ then id else _addbang
                                                     {-# LINE 5640 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule713 #-}
   {-# LINE 1600 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule713 = \ from_ ident_ to_ ->
                       {-# LINE 1600 "./src-ag/ExecutionPlan2Clean.ag" #-}
                       Map.singleton ident_ (from_, to_)
                       {-# LINE 5646 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule714 #-}
   {-# LINE 1644 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule714 = \ ident_ kind_ ->
                     {-# LINE 1644 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     Map.singleton ident_ kind_
                     {-# LINE 5652 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule715 #-}
   rule715 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule716 #-}
   rule716 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule717 #-}
   rule717 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule718 #-}
   rule718 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule721 #-}
   rule721 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule724 #-}
   rule724 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule725 #-}
   rule725 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule729 #-}
   rule729 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule730 #-}
   rule730 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses

-- VisitStep ---------------------------------------------------
-- wrapper
data Inh_VisitStep  = Inh_VisitStep { allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitStep :: (Map NontermIdent Int), allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitStep :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitStep :: (Map Identifier Type), childintros_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), fmtMode_Inh_VisitStep :: (FormatMode), index_Inh_VisitStep :: (Int), isLast_Inh_VisitStep :: (Bool), kind_Inh_VisitStep :: (VisitKind), mrules_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), options_Inh_VisitStep :: (Options), ruledefs_Inh_VisitStep :: (Map Identifier (Set String)), ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))) }
data Syn_VisitStep  = Syn_VisitStep { defs_Syn_VisitStep :: (Set String), errors_Syn_VisitStep :: (Seq Error), index_Syn_VisitStep :: (Int), isLast_Syn_VisitStep :: (Bool), lazyIntras_Syn_VisitStep :: (Set String), ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitStep :: (Map Identifier Int), sem_steps_Syn_VisitStep :: (PP_Doc), usedArgs_Syn_VisitStep :: (Set String), uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitStep #-}
wrap_VisitStep :: T_VisitStep  -> Inh_VisitStep  -> (Syn_VisitStep )
wrap_VisitStep (T_VisitStep act) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses
        (T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitStep_s50 sem arg)
        return (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
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
data T_VisitStep_vIn49  = T_VisitStep_vIn49 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (FormatMode) (Int) (Bool) (VisitKind) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Options) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr)))
data T_VisitStep_vOut49  = T_VisitStep_vOut49 (Set String) (Seq Error) (Int) (Bool) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitStep_Sem #-}
sem_VisitStep_Sem :: (Identifier) -> T_VisitStep 
sem_VisitStep_Sem arg_name_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _ruleItf = rule731 _lhsImrules arg_name_
         _lhsOerrors :: Seq Error
         (_lhsOerrors,_sem_steps) = rule732 _lhsIfmtMode _lhsIkind _ruleItf
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule733 arg_name_
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule734 _lhsIkind arg_name_
         _lhsOdefs :: Set String
         _lhsOdefs = rule735 _lhsIruledefs arg_name_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule736 _lhsIruleuses arg_name_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule737  ()
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule738 _sem_steps
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule739  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule740  ()
         _lhsOindex :: Int
         _lhsOindex = rule741 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule742 _lhsIisLast
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule731 #-}
   {-# LINE 836 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule731 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) name_ ->
                               {-# LINE 836 "./src-ag/ExecutionPlan2Clean.ag" #-}
                               Map.findWithDefault (error $ "Rule "  ++ show name_  ++ " not found") name_ _lhsImrules
                               {-# LINE 5774 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule732 #-}
   {-# LINE 837 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule732 = \ ((_lhsIfmtMode) :: FormatMode) ((_lhsIkind) :: VisitKind) _ruleItf ->
                                               {-# LINE 837 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                               case _ruleItf     _lhsIkind _lhsIfmtMode of
                                                 Left e     -> (Seq.singleton e, empty)
                                                 Right stmt -> (Seq.empty, stmt)
                                               {-# LINE 5782 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule733 #-}
   {-# LINE 1278 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule733 = \ name_ ->
                                                 {-# LINE 1278 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                 Map.singleton name_ 1
                                                 {-# LINE 5788 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule734 #-}
   {-# LINE 1288 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule734 = \ ((_lhsIkind) :: VisitKind) name_ ->
                    {-# LINE 1288 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    Map.singleton name_ (Set.singleton _lhsIkind)
                    {-# LINE 5794 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule735 #-}
   {-# LINE 1373 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule735 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) name_ ->
                            {-# LINE 1373 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                            {-# LINE 5800 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule736 #-}
   {-# LINE 1374 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule736 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) name_ ->
                            {-# LINE 1374 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                            {-# LINE 5806 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule737 #-}
   rule737 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule738 #-}
   rule738 = \ _sem_steps ->
     _sem_steps
   {-# INLINE rule739 #-}
   rule739 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule740 #-}
   rule740 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule741 #-}
   rule741 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule742 #-}
   rule742 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
{-# NOINLINE sem_VisitStep_ChildVisit #-}
sem_VisitStep_ChildVisit :: (Identifier) -> (NontermIdent) -> (VisitIdentifier) -> T_VisitStep 
sem_VisitStep_ChildVisit arg_child_ _ arg_visit_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _visitItf = rule743 _lhsIallchildvisit arg_visit_
         _lhsOerrors :: Seq Error
         (_lhsOerrors,_patPP,_exprPP) = rule744 _lhsIkind _visitItf arg_child_
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule745 _exprPP _lhsIfmtMode _lhsIkind _patPP
         _convToMonad = rule746 _callKind
         _callKind = rule747 _lhsIallVisitKinds arg_visit_
         _lhsOdefs :: Set String
         _lhsOdefs = rule748 _lhsIavisitdefs _lhsIoptions _to arg_child_ arg_visit_
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule749 _from _lhsIavisituses _lhsIoptions arg_child_ arg_visit_
         _addbang = rule750 _lhsIoptions
         (_from,_to) = rule751 _lhsIallFromToStates arg_visit_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule752  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule753  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule754  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule755  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule756  ()
         _lhsOindex :: Int
         _lhsOindex = rule757 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule758 _lhsIisLast
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule743 #-}
   {-# LINE 845 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule743 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) visit_ ->
                                {-# LINE 845 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                Map.findWithDefault (error $ "Visit " ++ show visit_ ++ " not found") visit_ _lhsIallchildvisit
                                {-# LINE 5867 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule744 #-}
   {-# LINE 846 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule744 = \ ((_lhsIkind) :: VisitKind) _visitItf child_ ->
                                                       {-# LINE 846 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                       case _visitItf     child_ _lhsIkind of
                                                         Left e           -> (Seq.singleton e, empty, empty)
                                                         Right (pat,expr) -> (Seq.empty, pat, expr)
                                                       {-# LINE 5875 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule745 #-}
   {-# LINE 850 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule745 = \ _exprPP ((_lhsIfmtMode) :: FormatMode) ((_lhsIkind) :: VisitKind) _patPP ->
                                 {-# LINE 850 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 let decl = case _lhsIkind of
                                              VisitPure _  -> _patPP     >#< "=" >#< _exprPP
                                              VisitMonadic -> _exprPP     >#< ">>= \\" >#< _patPP     >#< "->"
                                 in fmtDecl False _lhsIfmtMode decl
                                 {-# LINE 5884 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule746 #-}
   {-# LINE 854 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule746 = \ _callKind ->
                                   {-# LINE 854 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                   case _callKind     of
                                     VisitPure _  -> text "lift"
                                     VisitMonadic -> empty
                                   {-# LINE 5892 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule747 #-}
   {-# LINE 857 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule747 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) visit_ ->
                                 {-# LINE 857 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 Map.findWithDefault (error "visit kind should be in the map") visit_ _lhsIallVisitKinds
                                 {-# LINE 5898 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule748 #-}
   {-# LINE 1375 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule748 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_lhsIoptions) :: Options) _to child_ visit_ ->
                            {-# LINE 1375 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            Set.insert (stname child_ _to) $ maybe (error "Visit not found") (Set.map $ attrname _lhsIoptions True child_) $ Map.lookup visit_ _lhsIavisitdefs
                            {-# LINE 5904 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule749 #-}
   {-# LINE 1376 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule749 = \ _from ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ((_lhsIoptions) :: Options) child_ visit_ ->
                            {-# LINE 1376 "./src-ag/ExecutionPlan2Clean.ag" #-}
                            let convert attrs = Map.fromList [ (attrname _lhsIoptions False child_ attr, Just $ mkNonLocalAttr True child_ attr) | attr <- Set.elems attrs ]
                            in Map.insert (stname child_ _from) Nothing $ convert $
                                 maybe (error "Visit not found") id $ Map.lookup visit_ _lhsIavisituses
                            {-# LINE 5912 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule750 #-}
   {-# LINE 1570 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule750 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1570 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                                    {-# LINE 5918 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule751 #-}
   {-# LINE 1606 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule751 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) visit_ ->
                         {-# LINE 1606 "./src-ag/ExecutionPlan2Clean.ag" #-}
                         Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                         {-# LINE 5924 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule752 #-}
   rule752 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule753 #-}
   rule753 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule754 #-}
   rule754 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule755 #-}
   rule755 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule756 #-}
   rule756 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule757 #-}
   rule757 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule758 #-}
   rule758 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
{-# NOINLINE sem_VisitStep_PureGroup #-}
sem_VisitStep_PureGroup :: T_VisitSteps  -> (Bool) -> T_VisitStep 
sem_VisitStep_PureGroup arg_steps_ arg_ordered_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOruledefs _stepsOruleuses)
         _stepsOkind = rule759 arg_ordered_
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule760 _lhsIfmtMode _stepsIsem_steps
         _stepsOfmtMode = rule761 _lhsIfmtMode
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule762 _stepsIdefs _stepsIlazyIntras arg_ordered_
         _lhsOdefs :: Set String
         _lhsOdefs = rule763 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule764 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule765 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule766 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule767 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule768 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule769 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule770 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule771 _stepsIisLast
         _stepsOallFromToStates = rule772 _lhsIallFromToStates
         _stepsOallInitStates = rule773 _lhsIallInitStates
         _stepsOallVisitKinds = rule774 _lhsIallVisitKinds
         _stepsOallchildvisit = rule775 _lhsIallchildvisit
         _stepsOavisitdefs = rule776 _lhsIavisitdefs
         _stepsOavisituses = rule777 _lhsIavisituses
         _stepsOchildTypes = rule778 _lhsIchildTypes
         _stepsOchildintros = rule779 _lhsIchildintros
         _stepsOindex = rule780 _lhsIindex
         _stepsOmrules = rule781 _lhsImrules
         _stepsOoptions = rule782 _lhsIoptions
         _stepsOruledefs = rule783 _lhsIruledefs
         _stepsOruleuses = rule784 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule759 #-}
   {-# LINE 831 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule759 = \ ordered_ ->
                 {-# LINE 831 "./src-ag/ExecutionPlan2Clean.ag" #-}
                 VisitPure ordered_
                 {-# LINE 6000 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule760 #-}
   {-# LINE 859 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule760 = \ ((_lhsIfmtMode) :: FormatMode) ((_stepsIsem_steps) :: PP_Doc) ->
                                 {-# LINE 859 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 case _lhsIfmtMode of
                                   FormatDo -> "let" >#< _stepsIsem_steps
                                   _        -> _stepsIsem_steps
                                 {-# LINE 6008 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule761 #-}
   {-# LINE 874 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule761 = \ ((_lhsIfmtMode) :: FormatMode) ->
                    {-# LINE 874 "./src-ag/ExecutionPlan2Clean.ag" #-}
                    case _lhsIfmtMode of
                      FormatDo      -> FormatLetDecl
                      mode          -> mode
                    {-# LINE 6016 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule762 #-}
   {-# LINE 1396 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule762 = \ ((_stepsIdefs) :: Set String) ((_stepsIlazyIntras) :: Set String) ordered_ ->
                     {-# LINE 1396 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     if ordered_
                     then _stepsIlazyIntras
                     else _stepsIdefs
                     {-# LINE 6024 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule763 #-}
   rule763 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule764 #-}
   rule764 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule765 #-}
   rule765 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule766 #-}
   rule766 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule767 #-}
   rule767 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule768 #-}
   rule768 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule769 #-}
   rule769 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule770 #-}
   rule770 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule771 #-}
   rule771 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule772 #-}
   rule772 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule773 #-}
   rule773 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule774 #-}
   rule774 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule775 #-}
   rule775 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule776 #-}
   rule776 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule777 #-}
   rule777 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule778 #-}
   rule778 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule779 #-}
   rule779 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule780 #-}
   rule780 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule781 #-}
   rule781 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule782 #-}
   rule782 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule783 #-}
   rule783 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule784 #-}
   rule784 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitStep_Sim #-}
sem_VisitStep_Sim :: T_VisitSteps  -> T_VisitStep 
sem_VisitStep_Sim arg_steps_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIruleKinds _stepsIruleUsage _stepsIsem_steps _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOruledefs _stepsOruleuses)
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule785 _stepsIsem_steps
         _stepsOindex = rule786  ()
         _lhsOindex :: Int
         _lhsOindex = rule787 _lhsIindex
         _isMonadic = rule788 _lhsIkind
         _lhsOdefs :: Set String
         _lhsOdefs = rule789 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule790 _stepsIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule791 _stepsIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule792 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule793 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule794 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule795 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule796 _stepsIvisitKinds
         _lhsOisLast :: Bool
         _lhsOisLast = rule797 _stepsIisLast
         _stepsOallFromToStates = rule798 _lhsIallFromToStates
         _stepsOallInitStates = rule799 _lhsIallInitStates
         _stepsOallVisitKinds = rule800 _lhsIallVisitKinds
         _stepsOallchildvisit = rule801 _lhsIallchildvisit
         _stepsOavisitdefs = rule802 _lhsIavisitdefs
         _stepsOavisituses = rule803 _lhsIavisituses
         _stepsOchildTypes = rule804 _lhsIchildTypes
         _stepsOchildintros = rule805 _lhsIchildintros
         _stepsOfmtMode = rule806 _lhsIfmtMode
         _stepsOkind = rule807 _lhsIkind
         _stepsOmrules = rule808 _lhsImrules
         _stepsOoptions = rule809 _lhsIoptions
         _stepsOruledefs = rule810 _lhsIruledefs
         _stepsOruleuses = rule811 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule785 #-}
   {-# LINE 858 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule785 = \ ((_stepsIsem_steps) :: PP_Doc) ->
                                 {-# LINE 858 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 _stepsIsem_steps
                                 {-# LINE 6146 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule786 #-}
   {-# LINE 918 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule786 = \  (_ :: ()) ->
                                     {-# LINE 918 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     0
                                     {-# LINE 6152 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule787 #-}
   {-# LINE 919 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule787 = \ ((_lhsIindex) :: Int) ->
                                     {-# LINE 919 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                     _lhsIindex
                                     {-# LINE 6158 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule788 #-}
   {-# LINE 930 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule788 = \ ((_lhsIkind) :: VisitKind) ->
                                         {-# LINE 930 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                         case _lhsIkind of
                                           VisitMonadic -> True
                                           _            -> False
                                         {-# LINE 6166 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule789 #-}
   rule789 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule790 #-}
   rule790 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule791 #-}
   rule791 = \ ((_stepsIlazyIntras) :: Set String) ->
     _stepsIlazyIntras
   {-# INLINE rule792 #-}
   rule792 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule793 #-}
   rule793 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule794 #-}
   rule794 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule795 #-}
   rule795 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule796 #-}
   rule796 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule797 #-}
   rule797 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule798 #-}
   rule798 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule799 #-}
   rule799 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule800 #-}
   rule800 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule801 #-}
   rule801 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule802 #-}
   rule802 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule803 #-}
   rule803 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule804 #-}
   rule804 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule805 #-}
   rule805 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule806 #-}
   rule806 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule807 #-}
   rule807 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule808 #-}
   rule808 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule809 #-}
   rule809 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule810 #-}
   rule810 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule811 #-}
   rule811 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitStep_ChildIntro #-}
sem_VisitStep_ChildIntro :: (Identifier) -> T_VisitStep 
sem_VisitStep_ChildIntro arg_child_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _attachItf = rule812 _lhsIchildintros arg_child_
         _lhsOerrors :: Seq Error
         _lhsOsem_steps :: PP_Doc
         _lhsOdefs :: Set String
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         (_lhsOerrors,_lhsOsem_steps,_lhsOdefs,_lhsOuses) = rule813 _attachItf _lhsIfmtMode _lhsIkind
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule814  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule815  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule816  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule817  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule818  ()
         _lhsOindex :: Int
         _lhsOindex = rule819 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule820 _lhsIisLast
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule812 #-}
   {-# LINE 840 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule812 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) child_ ->
                                 {-# LINE 840 "./src-ag/ExecutionPlan2Clean.ag" #-}
                                 Map.findWithDefault (error $ "Child " ++ show child_ ++ " not found") child_ _lhsIchildintros
                                 {-# LINE 6271 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule813 #-}
   {-# LINE 842 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule813 = \ _attachItf ((_lhsIfmtMode) :: FormatMode) ((_lhsIkind) :: VisitKind) ->
                     {-# LINE 842 "./src-ag/ExecutionPlan2Clean.ag" #-}
                     case _attachItf     _lhsIkind _lhsIfmtMode of
                       Left e                   -> (Seq.singleton e, empty, Set.empty, Map.empty)
                       Right (code, defs, uses) -> (Seq.empty, code, defs, uses)
                     {-# LINE 6279 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule814 #-}
   rule814 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule815 #-}
   rule815 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule816 #-}
   rule816 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule817 #-}
   rule817 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule818 #-}
   rule818 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule819 #-}
   rule819 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule820 #-}
   rule820 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast

-- VisitSteps --------------------------------------------------
-- wrapper
data Inh_VisitSteps  = Inh_VisitSteps { allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitSteps :: (Map NontermIdent Int), allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind), allchildvisit_Inh_VisitSteps :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))), avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitSteps :: (Map Identifier Type), childintros_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))), fmtMode_Inh_VisitSteps :: (FormatMode), index_Inh_VisitSteps :: (Int), kind_Inh_VisitSteps :: (VisitKind), mrules_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)), options_Inh_VisitSteps :: (Options), ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)), ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))) }
data Syn_VisitSteps  = Syn_VisitSteps { defs_Syn_VisitSteps :: (Set String), errors_Syn_VisitSteps :: (Seq Error), index_Syn_VisitSteps :: (Int), isLast_Syn_VisitSteps :: (Bool), lazyIntras_Syn_VisitSteps :: (Set String), ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitSteps :: (Map Identifier Int), sem_steps_Syn_VisitSteps :: (PP_Doc), size_Syn_VisitSteps :: (Int), usedArgs_Syn_VisitSteps :: (Set String), uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitSteps #-}
wrap_VisitSteps :: T_VisitSteps  -> Inh_VisitSteps  -> (Syn_VisitSteps )
wrap_VisitSteps (T_VisitSteps act) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses
        (T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitSteps_s53 sem arg)
        return (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
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
data T_VisitSteps_vIn52  = T_VisitSteps_vIn52 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) (FormatMode) (Int) (VisitKind) (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) (Options) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr)))
data T_VisitSteps_vOut52  = T_VisitSteps_vOut52 (Set String) (Seq Error) (Int) (Bool) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (PP_Doc) (Int) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitSteps_Cons #-}
sem_VisitSteps_Cons :: T_VisitStep  -> T_VisitSteps  -> T_VisitSteps 
sem_VisitSteps_Cons arg_hd_ arg_tl_ = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_VisitStep (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_tl_))
         (T_VisitStep_vOut49 _hdIdefs _hdIerrors _hdIindex _hdIisLast _hdIlazyIntras _hdIruleKinds _hdIruleUsage _hdIsem_steps _hdIusedArgs _hdIuses _hdIvisitKinds) = inv_VisitStep_s50 _hdX50 (T_VisitStep_vIn49 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOfmtMode _hdOindex _hdOisLast _hdOkind _hdOmrules _hdOoptions _hdOruledefs _hdOruleuses)
         (T_VisitSteps_vOut52 _tlIdefs _tlIerrors _tlIindex _tlIisLast _tlIlazyIntras _tlIruleKinds _tlIruleUsage _tlIsem_steps _tlIsize _tlIusedArgs _tlIuses _tlIvisitKinds) = inv_VisitSteps_s53 _tlX53 (T_VisitSteps_vIn52 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOfmtMode _tlOindex _tlOkind _tlOmrules _tlOoptions _tlOruledefs _tlOruleuses)
         _lhsOsize :: Int
         _lhsOsize = rule821 _tlIsize
         _hdOindex = rule822 _lhsIindex
         _tlOindex = rule823 _lhsIindex
         _lhsOindex :: Int
         _lhsOindex = rule824 _tlIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule825  ()
         _hdOisLast = rule826 _tlIisLast
         _lhsOdefs :: Set String
         _lhsOdefs = rule827 _hdIdefs _tlIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule828 _hdIerrors _tlIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule829 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule830 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule831 _hdIruleUsage _tlIruleUsage
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule832 _hdIsem_steps _tlIsem_steps
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule833 _hdIusedArgs _tlIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule834 _hdIuses _tlIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule835 _hdIvisitKinds _tlIvisitKinds
         _hdOallFromToStates = rule836 _lhsIallFromToStates
         _hdOallInitStates = rule837 _lhsIallInitStates
         _hdOallVisitKinds = rule838 _lhsIallVisitKinds
         _hdOallchildvisit = rule839 _lhsIallchildvisit
         _hdOavisitdefs = rule840 _lhsIavisitdefs
         _hdOavisituses = rule841 _lhsIavisituses
         _hdOchildTypes = rule842 _lhsIchildTypes
         _hdOchildintros = rule843 _lhsIchildintros
         _hdOfmtMode = rule844 _lhsIfmtMode
         _hdOkind = rule845 _lhsIkind
         _hdOmrules = rule846 _lhsImrules
         _hdOoptions = rule847 _lhsIoptions
         _hdOruledefs = rule848 _lhsIruledefs
         _hdOruleuses = rule849 _lhsIruleuses
         _tlOallFromToStates = rule850 _lhsIallFromToStates
         _tlOallInitStates = rule851 _lhsIallInitStates
         _tlOallVisitKinds = rule852 _lhsIallVisitKinds
         _tlOallchildvisit = rule853 _lhsIallchildvisit
         _tlOavisitdefs = rule854 _lhsIavisitdefs
         _tlOavisituses = rule855 _lhsIavisituses
         _tlOchildTypes = rule856 _lhsIchildTypes
         _tlOchildintros = rule857 _lhsIchildintros
         _tlOfmtMode = rule858 _lhsIfmtMode
         _tlOkind = rule859 _lhsIkind
         _tlOmrules = rule860 _lhsImrules
         _tlOoptions = rule861 _lhsIoptions
         _tlOruledefs = rule862 _lhsIruledefs
         _tlOruleuses = rule863 _lhsIruleuses
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule821 #-}
   {-# LINE 909 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule821 = \ ((_tlIsize) :: Int) ->
                      {-# LINE 909 "./src-ag/ExecutionPlan2Clean.ag" #-}
                      1 + _tlIsize
                      {-# LINE 6406 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule822 #-}
   {-# LINE 914 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule822 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 914 "./src-ag/ExecutionPlan2Clean.ag" #-}
                _lhsIindex
                {-# LINE 6412 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule823 #-}
   {-# LINE 915 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule823 = \ ((_lhsIindex) :: Int) ->
                {-# LINE 915 "./src-ag/ExecutionPlan2Clean.ag" #-}
                1 + _lhsIindex
                {-# LINE 6418 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule824 #-}
   {-# LINE 916 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule824 = \ ((_tlIindex) :: Int) ->
                {-# LINE 916 "./src-ag/ExecutionPlan2Clean.ag" #-}
                _tlIindex
                {-# LINE 6424 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule825 #-}
   {-# LINE 926 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule825 = \  (_ :: ()) ->
                         {-# LINE 926 "./src-ag/ExecutionPlan2Clean.ag" #-}
                         False
                         {-# LINE 6430 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule826 #-}
   {-# LINE 927 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule826 = \ ((_tlIisLast) :: Bool) ->
                         {-# LINE 927 "./src-ag/ExecutionPlan2Clean.ag" #-}
                         _tlIisLast
                         {-# LINE 6436 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule827 #-}
   rule827 = \ ((_hdIdefs) :: Set String) ((_tlIdefs) :: Set String) ->
     _hdIdefs `Set.union` _tlIdefs
   {-# INLINE rule828 #-}
   rule828 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule829 #-}
   rule829 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule830 #-}
   rule830 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule831 #-}
   rule831 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule832 #-}
   rule832 = \ ((_hdIsem_steps) :: PP_Doc) ((_tlIsem_steps) :: PP_Doc) ->
     _hdIsem_steps >-< _tlIsem_steps
   {-# INLINE rule833 #-}
   rule833 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule834 #-}
   rule834 = \ ((_hdIuses) :: Map String (Maybe NonLocalAttr)) ((_tlIuses) :: Map String (Maybe NonLocalAttr)) ->
     _hdIuses `Map.union` _tlIuses
   {-# INLINE rule835 #-}
   rule835 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule836 #-}
   rule836 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule837 #-}
   rule837 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule838 #-}
   rule838 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule839 #-}
   rule839 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule840 #-}
   rule840 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule841 #-}
   rule841 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule842 #-}
   rule842 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule843 #-}
   rule843 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule844 #-}
   rule844 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule845 #-}
   rule845 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule846 #-}
   rule846 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule847 #-}
   rule847 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule848 #-}
   rule848 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule849 #-}
   rule849 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule850 #-}
   rule850 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule851 #-}
   rule851 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule852 #-}
   rule852 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule853 #-}
   rule853 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule854 #-}
   rule854 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule855 #-}
   rule855 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule856 #-}
   rule856 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule857 #-}
   rule857 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule858 #-}
   rule858 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule859 #-}
   rule859 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule860 #-}
   rule860 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule861 #-}
   rule861 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule862 #-}
   rule862 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule863 #-}
   rule863 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitSteps_Nil #-}
sem_VisitSteps_Nil ::  T_VisitSteps 
sem_VisitSteps_Nil  = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOsize :: Int
         _lhsOsize = rule864  ()
         _lhsOisLast :: Bool
         _lhsOisLast = rule865  ()
         _lhsOdefs :: Set String
         _lhsOdefs = rule866  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule867  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule868  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule869  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule870  ()
         _lhsOsem_steps :: PP_Doc
         _lhsOsem_steps = rule871  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule872  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule873  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule874  ()
         _lhsOindex :: Int
         _lhsOindex = rule875 _lhsIindex
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule864 #-}
   {-# LINE 908 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule864 = \  (_ :: ()) ->
                      {-# LINE 908 "./src-ag/ExecutionPlan2Clean.ag" #-}
                      0
                      {-# LINE 6587 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule865 #-}
   {-# LINE 925 "./src-ag/ExecutionPlan2Clean.ag" #-}
   rule865 = \  (_ :: ()) ->
                         {-# LINE 925 "./src-ag/ExecutionPlan2Clean.ag" #-}
                         True
                         {-# LINE 6593 "dist/build/ExecutionPlan2Clean.hs"#-}
   {-# INLINE rule866 #-}
   rule866 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule867 #-}
   rule867 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule868 #-}
   rule868 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule869 #-}
   rule869 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule870 #-}
   rule870 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule871 #-}
   rule871 = \  (_ :: ()) ->
     empty
   {-# INLINE rule872 #-}
   rule872 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule873 #-}
   rule873 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule874 #-}
   rule874 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule875 #-}
   rule875 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex

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
         _lhsOallvisits = rule876 _hdIallvisits _tlIallvisits
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule877 _hdIchildvisit _tlIchildvisit
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule878 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule879 _hdIfromToStates _tlIfromToStates
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule880 _hdIintramap _tlIintramap
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule881 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule882 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule883 _hdIruleUsage _tlIruleUsage
         _lhsOsem_visit ::  [(StateIdentifier,Bool -> PP_Doc)] 
         _lhsOsem_visit = rule884 _hdIsem_visit _tlIsem_visit
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule885 _hdIt_visits _tlIt_visits
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule886 _hdIusedArgs _tlIusedArgs
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule887 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule888 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule889 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule890 _lhsIallFromToStates
         _hdOallInhmap = rule891 _lhsIallInhmap
         _hdOallInitStates = rule892 _lhsIallInitStates
         _hdOallSynmap = rule893 _lhsIallSynmap
         _hdOallVisitKinds = rule894 _lhsIallVisitKinds
         _hdOallchildvisit = rule895 _lhsIallchildvisit
         _hdOallintramap = rule896 _lhsIallintramap
         _hdOavisitdefs = rule897 _lhsIavisitdefs
         _hdOavisituses = rule898 _lhsIavisituses
         _hdOchildTypes = rule899 _lhsIchildTypes
         _hdOchildintros = rule900 _lhsIchildintros
         _hdOcon = rule901 _lhsIcon
         _hdOinhmap = rule902 _lhsIinhmap
         _hdOmrules = rule903 _lhsImrules
         _hdOnextVisits = rule904 _lhsInextVisits
         _hdOnt = rule905 _lhsInt
         _hdOoptions = rule906 _lhsIoptions
         _hdOparams = rule907 _lhsIparams
         _hdOprevVisits = rule908 _lhsIprevVisits
         _hdOruledefs = rule909 _lhsIruledefs
         _hdOruleuses = rule910 _lhsIruleuses
         _hdOsynmap = rule911 _lhsIsynmap
         _hdOterminaldefs = rule912 _lhsIterminaldefs
         _tlOallFromToStates = rule913 _lhsIallFromToStates
         _tlOallInhmap = rule914 _lhsIallInhmap
         _tlOallInitStates = rule915 _lhsIallInitStates
         _tlOallSynmap = rule916 _lhsIallSynmap
         _tlOallVisitKinds = rule917 _lhsIallVisitKinds
         _tlOallchildvisit = rule918 _lhsIallchildvisit
         _tlOallintramap = rule919 _lhsIallintramap
         _tlOavisitdefs = rule920 _lhsIavisitdefs
         _tlOavisituses = rule921 _lhsIavisituses
         _tlOchildTypes = rule922 _lhsIchildTypes
         _tlOchildintros = rule923 _lhsIchildintros
         _tlOcon = rule924 _lhsIcon
         _tlOinhmap = rule925 _lhsIinhmap
         _tlOmrules = rule926 _lhsImrules
         _tlOnextVisits = rule927 _lhsInextVisits
         _tlOnt = rule928 _lhsInt
         _tlOoptions = rule929 _lhsIoptions
         _tlOparams = rule930 _lhsIparams
         _tlOprevVisits = rule931 _lhsIprevVisits
         _tlOruledefs = rule932 _lhsIruledefs
         _tlOruleuses = rule933 _lhsIruleuses
         _tlOsynmap = rule934 _lhsIsynmap
         _tlOterminaldefs = rule935 _lhsIterminaldefs
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule876 #-}
   rule876 = \ ((_hdIallvisits) ::  VisitStateState ) ((_tlIallvisits) :: [VisitStateState]) ->
     _hdIallvisits : _tlIallvisits
   {-# INLINE rule877 #-}
   rule877 = \ ((_hdIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ((_tlIchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _hdIchildvisit `Map.union` _tlIchildvisit
   {-# INLINE rule878 #-}
   rule878 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule879 #-}
   rule879 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule880 #-}
   rule880 = \ ((_hdIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ((_tlIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _hdIintramap `uwMapUnion` _tlIintramap
   {-# INLINE rule881 #-}
   rule881 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule882 #-}
   rule882 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule883 #-}
   rule883 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule884 #-}
   rule884 = \ ((_hdIsem_visit) ::   (StateIdentifier,Bool -> PP_Doc)  ) ((_tlIsem_visit) ::  [(StateIdentifier,Bool -> PP_Doc)] ) ->
     _hdIsem_visit : _tlIsem_visit
   {-# INLINE rule885 #-}
   rule885 = \ ((_hdIt_visits) :: PP_Doc) ((_tlIt_visits) :: PP_Doc) ->
     _hdIt_visits >-< _tlIt_visits
   {-# INLINE rule886 #-}
   rule886 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule887 #-}
   rule887 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule888 #-}
   rule888 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule889 #-}
   rule889 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule890 #-}
   rule890 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule891 #-}
   rule891 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule892 #-}
   rule892 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule893 #-}
   rule893 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule894 #-}
   rule894 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule895 #-}
   rule895 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule896 #-}
   rule896 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule897 #-}
   rule897 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule898 #-}
   rule898 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule899 #-}
   rule899 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule900 #-}
   rule900 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule901 #-}
   rule901 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule902 #-}
   rule902 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule903 #-}
   rule903 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule904 #-}
   rule904 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule905 #-}
   rule905 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule906 #-}
   rule906 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule907 #-}
   rule907 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule908 #-}
   rule908 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule909 #-}
   rule909 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule910 #-}
   rule910 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule911 #-}
   rule911 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule912 #-}
   rule912 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
   {-# INLINE rule913 #-}
   rule913 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule914 #-}
   rule914 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule915 #-}
   rule915 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule916 #-}
   rule916 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule917 #-}
   rule917 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule918 #-}
   rule918 = \ ((_lhsIallchildvisit) :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
     _lhsIallchildvisit
   {-# INLINE rule919 #-}
   rule919 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule920 #-}
   rule920 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule921 #-}
   rule921 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule922 #-}
   rule922 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule923 #-}
   rule923 = \ ((_lhsIchildintros) :: Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
     _lhsIchildintros
   {-# INLINE rule924 #-}
   rule924 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule925 #-}
   rule925 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule926 #-}
   rule926 = \ ((_lhsImrules) :: Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
     _lhsImrules
   {-# INLINE rule927 #-}
   rule927 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule928 #-}
   rule928 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule929 #-}
   rule929 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule930 #-}
   rule930 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule931 #-}
   rule931 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule932 #-}
   rule932 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule933 #-}
   rule933 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule934 #-}
   rule934 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule935 #-}
   rule935 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
{-# NOINLINE sem_Visits_Nil #-}
sem_Visits_Nil ::  T_Visits 
sem_Visits_Nil  = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIcon _lhsIinhmap _lhsImrules _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule936  ()
         _lhsOchildvisit :: Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         _lhsOchildvisit = rule937  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule938  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule939  ()
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule940  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule941  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule942  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule943  ()
         _lhsOsem_visit ::  [(StateIdentifier,Bool -> PP_Doc)] 
         _lhsOsem_visit = rule944  ()
         _lhsOt_visits :: PP_Doc
         _lhsOt_visits = rule945  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule946  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule947  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule948  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule949  ()
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule936 #-}
   rule936 = \  (_ :: ()) ->
     []
   {-# INLINE rule937 #-}
   rule937 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule938 #-}
   rule938 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule939 #-}
   rule939 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule940 #-}
   rule940 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule941 #-}
   rule941 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule942 #-}
   rule942 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule943 #-}
   rule943 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule944 #-}
   rule944 = \  (_ :: ()) ->
     []
   {-# INLINE rule945 #-}
   rule945 = \  (_ :: ()) ->
     empty
   {-# INLINE rule946 #-}
   rule946 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule947 #-}
   rule947 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule948 #-}
   rule948 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule949 #-}
   rule949 = \  (_ :: ()) ->
     Map.empty
