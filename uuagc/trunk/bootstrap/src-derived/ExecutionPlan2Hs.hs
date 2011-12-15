

-- UUAGC 0.9.40.1 (src-ag/ExecutionPlan2Hs.ag)
module ExecutionPlan2Hs where
{-# LINE 7 "src-ag/ExecutionPlan2Hs.ag" #-}

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

{-# LINE 2 "src-ag/ExecutionPlan.ag" #-}

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

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 52 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 58 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 64 "dist/build/ExecutionPlan2Hs.hs" #-}
{-# LINE 161 "src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 81 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 190 "src-ag/ExecutionPlan2Hs.ag" #-}

-- first parameter indicates: generate a record or not
ppConFields :: Bool -> [PP_Doc] -> PP_Doc
ppConFields True  flds = ppListSep "{" "}" ", " flds
ppConFields False flds = ppSpaced flds
{-# LINE 89 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 216 "src-ag/ExecutionPlan2Hs.ag" #-}

ppTp :: Type -> PP_Doc
ppTp (Haskell s)      = text s
ppTp (NT nt args for) = prefix >|< nt >#< ppSpaced (map pp_parens args) where
  prefix | for       = text "T_"
         | otherwise = empty
{-# LINE 98 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 333 "src-ag/ExecutionPlan2Hs.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 102 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 429 "src-ag/ExecutionPlan2Hs.ag" #-}

conNmTVisit nt vId      = "T_" >|< nt >|< "_v"    >|< vId
conNmTVisitIn nt vId    = "T_" >|< nt >|< "_vIn"  >|< vId
conNmTVisitOut nt vId   = "T_" >|< nt >|< "_vOut" >|< vId
conNmTNextVisit nt stId = "T_" >|< nt >|< "_s"    >|< stId

ppMonadType :: Options -> PP_Doc
ppMonadType opts
  | parallelInvoke opts = text "IO"
  | otherwise           = text "Identity"
{-# LINE 115 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 579 "src-ag/ExecutionPlan2Hs.ag" #-}

ppDefor :: Type -> PP_Doc
ppDefor (NT nt args _) = "T_" >|< nt >#< ppSpaced (map pp_parens args)
ppDefor (Haskell s)    = text s
{-# LINE 122 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 694 "src-ag/ExecutionPlan2Hs.ag" #-}

mklet :: (PP a, PP b, PP c) => a -> b -> c -> PP_Doc
mklet prefix defs body =
  prefix >#< "let"
  >-< indent 3 defs
  >-< indent 2 "in" >#< body
{-# LINE 131 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 760 "src-ag/ExecutionPlan2Hs.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 140 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 831 "src-ag/ExecutionPlan2Hs.ag" #-}

parResultName :: String
parResultName = "__outcome_"

fmtDecl :: PP a => Bool -> FormatMode -> a -> PP_Doc
fmtDecl declPure fmt decl = case fmt of
  FormatLetDecl -> pp decl
  FormatLetLine -> "let" >#< decl >#< "in"
  FormatDo | declPure  -> "let" >#< decl
           | otherwise -> pp decl
{-# LINE 153 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 946 "src-ag/ExecutionPlan2Hs.ag" #-}

stname :: Identifier -> Int -> String
stname child st = "_" ++ getName child ++ "X" ++ show st

-- should actually return some conversion info
compatibleAttach :: VisitKind -> NontermIdent -> Options -> Bool
compatibleAttach _ _ _ = True

unMon :: Options -> PP_Doc
unMon options
  | parallelInvoke options = text "System.IO.Unsafe.unsafePerformIO"    -- IO monad
  | otherwise              = text "Control.Monad.Identity.runIdentity"  -- identity monad
{-# LINE 168 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1050 "src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 195 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1076 "src-ag/ExecutionPlan2Hs.ag" #-}

-- rules are "deinlined" to prevent needless code duplication.
-- if there is only a bit of duplication, we allow ghc to decide if it is worth it.
-- if the duplication crosses this threshold, however, we tell ghc definitely not to inline it.
ruleInlineThresholdSoft :: Int
ruleInlineThresholdSoft = 3

ruleInlineThresholdHard :: Int
ruleInlineThresholdHard = 5

reallyOftenUsedThreshold :: Int
reallyOftenUsedThreshold = 12
{-# LINE 210 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1142 "src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 243 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1228 "src-ag/ExecutionPlan2Hs.ag" #-}

-- a `compatibleKind` b  means: can kind b be invoked from a
compatibleKind :: VisitKind -> VisitKind -> Bool
compatibleKind _              _             = True

compatibleRule :: VisitKind -> Bool -> Bool
compatibleRule (VisitPure _) False = False
compatibleRule _             _     = True
{-# LINE 254 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1250 "src-ag/ExecutionPlan2Hs.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 259 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1273 "src-ag/ExecutionPlan2Hs.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 268 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1490 "src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 288 "dist/build/ExecutionPlan2Hs.hs" #-}

{-# LINE 1637 "src-ag/ExecutionPlan2Hs.ag" #-}

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
{-# LINE 330 "dist/build/ExecutionPlan2Hs.hs" #-}
-- EChild ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInitStates        : Map NontermIdent Int
         con                  : ConstructorIdent
         importBlocks         : PP_Doc
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         pragmaBlocks         : String
         textBlocks           : PP_Doc
      synthesized attributes:
         argnamesw            :  PP_Doc 
         argpats              :   PP_Doc  
         argtps               :   PP_Doc  
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         datatype             : PP_Doc
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
            local tpDoc       : _
            local strNm       : _
            local field       : _
            local addStrict   : _
            local argpats     : _
            local isDefor     : _
            local valcode     : _
            local aroundcode  : _
            local introcode   : _
            local nt          : _
            local addbang     : _
            local initSt      : _
      alternative ETerm:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local tpDoc       : _
            local strNm       : _
            local field       : _
            local addStrict   : _
            local argpats     : _
            local addbang     : _
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
                             PP_Doc ->
                             String ->
                             String ->
                             (String -> String -> String -> Bool -> String) ->
                             NontermIdent ->
                             Options ->
                             String ->
                             PP_Doc ->
                             ( ( PP_Doc ),(  PP_Doc  ),(  PP_Doc  ),(Map Identifier Type),(Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),PP_Doc,(Set String)))
data Inh_EChild = Inh_EChild {allInitStates_Inh_EChild :: (Map NontermIdent Int),con_Inh_EChild :: ConstructorIdent,importBlocks_Inh_EChild :: PP_Doc,mainFile_Inh_EChild :: String,mainName_Inh_EChild :: String,moduleHeader_Inh_EChild :: (String -> String -> String -> Bool -> String),nt_Inh_EChild :: NontermIdent,options_Inh_EChild :: Options,pragmaBlocks_Inh_EChild :: String,textBlocks_Inh_EChild :: PP_Doc}
data Syn_EChild = Syn_EChild {argnamesw_Syn_EChild :: ( PP_Doc ),argpats_Syn_EChild :: (  PP_Doc  ),argtps_Syn_EChild :: (  PP_Doc  ),childTypes_Syn_EChild :: (Map Identifier Type),childintros_Syn_EChild :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),datatype_Syn_EChild :: PP_Doc,terminaldefs_Syn_EChild :: (Set String)}
wrap_EChild :: T_EChild ->
               Inh_EChild ->
               Syn_EChild
wrap_EChild (T_EChild sem) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
    (let ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs) = sem _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
     in  (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs))
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
                 _lhsIimportBlocks
                 _lhsImainFile
                 _lhsImainName
                 _lhsImoduleHeader
                 _lhsInt
                 _lhsIoptions
                 _lhsIpragmaBlocks
                 _lhsItextBlocks ->
                   (let _lhsOdatatype :: PP_Doc
                        _lhsOargnamesw :: ( PP_Doc )
                        _lhsOargtps :: (  PP_Doc  )
                        _lhsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _lhsOchildTypes :: (Map Identifier Type)
                        _lhsOterminaldefs :: (Set String)
                        _lhsOargpats :: (  PP_Doc  )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 204, column 7)
                        _tpDoc =
                            ({-# LINE 204 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                             {-# LINE 441 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 205, column 7)
                        _strNm =
                            ({-# LINE 205 "src-ag/ExecutionPlan2Hs.ag" #-}
                             recordFieldname _lhsInt _lhsIcon name_
                             {-# LINE 447 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 206, column 7)
                        _field =
                            ({-# LINE 206 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if dataRecords _lhsIoptions
                             then _strNm     >#< "::" >#< _tpDoc
                             else _tpDoc
                             {-# LINE 455 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 209, column 7)
                        _addStrict =
                            ({-# LINE 209 "src-ag/ExecutionPlan2Hs.ag" #-}
                             \x -> if strictData _lhsIoptions then "!" >|< x else x
                             {-# LINE 461 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 210, column 13)
                        _lhsOdatatype =
                            ({-# LINE 210 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildAttr -> empty
                               _         -> _field
                             {-# LINE 469 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 301, column 12)
                        _lhsOargnamesw =
                            ({-# LINE 301 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildSyntax     -> "(" >#< "sem_" >|< _nt     >#< name_ >|< "_" >#< ")"
                               ChildAttr       -> empty
                               ChildReplace tp -> "(" >#< "sem_" >|< extractNonterminal tp >#< name_ >|< "_" >#< ")"
                             {-# LINE 478 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 568, column 12)
                        _lhsOargtps =
                            ({-# LINE 568 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildSyntax     -> ppDefor tp_ >#< "->"
                               ChildReplace tp -> ppDefor tp >#< "->"
                               _               -> empty
                             {-# LINE 487 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 572, column 12)
                        _argpats =
                            ({-# LINE 572 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildSyntax    -> name_ >|< "_"
                               ChildReplace _ -> name_ >|< "_"
                               _              -> empty
                             {-# LINE 496 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 900, column 12)
                        _lhsOchildintros =
                            ({-# LINE 900 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.singleton name_ _introcode
                             {-# LINE 502 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 901, column 12)
                        _isDefor =
                            ({-# LINE 901 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case tp_ of
                               NT _ _ defor -> defor
                               _            -> False
                             {-# LINE 510 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 904, column 12)
                        _valcode =
                            ({-# LINE 904 "src-ag/ExecutionPlan2Hs.ag" #-}
                             case kind_ of
                               ChildSyntax -> name_ >|< "_"
                               ChildAttr   ->
                                              let prefix | not _isDefor     = if lateHigherOrderBinding _lhsIoptions
                                                                              then lateSemNtLabel _nt     >#< lhsname True idLateBindingAttr
                                                                              else "sem_" >|< _nt
                                                         | otherwise        = empty
                                              in pp_parens (prefix >#< instname name_)
                               ChildReplace _ ->
                                                 pp_parens (instname name_ >#< name_ >|< "_")
                             {-# LINE 525 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 915, column 12)
                        _aroundcode =
                            ({-# LINE 915 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if hasAround_
                             then locname name_ >|< "_around"
                             else empty
                             {-# LINE 533 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 918, column 12)
                        _introcode =
                            ({-# LINE 918 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                             {-# LINE 564 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 944, column 12)
                        _nt =
                            ({-# LINE 944 "src-ag/ExecutionPlan2Hs.ag" #-}
                             extractNonterminal tp_
                             {-# LINE 570 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1518, column 37)
                        _addbang =
                            ({-# LINE 1518 "src-ag/ExecutionPlan2Hs.ag" #-}
                             \x -> if bangpats _lhsIoptions then "!" >|< x else x
                             {-# LINE 576 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1570, column 3)
                        _lhsOchildTypes =
                            ({-# LINE 1570 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.singleton name_ tp_
                             {-# LINE 582 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1614, column 3)
                        _initSt =
                            ({-# LINE 1614 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                             {-# LINE 588 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1284, column 42)
                        _lhsOterminaldefs =
                            ({-# LINE 1284 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Set.empty
                             {-# LINE 594 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (from local)
                        _lhsOargpats =
                            ({-# LINE 563 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _argpats
                             {-# LINE 600 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ___node =
                            (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs)
                    in  ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))))
sem_EChild_ETerm :: Identifier ->
                    Type ->
                    T_EChild
sem_EChild_ETerm name_ tp_ =
    (T_EChild (\ _lhsIallInitStates
                 _lhsIcon
                 _lhsIimportBlocks
                 _lhsImainFile
                 _lhsImainName
                 _lhsImoduleHeader
                 _lhsInt
                 _lhsIoptions
                 _lhsIpragmaBlocks
                 _lhsItextBlocks ->
                   (let _lhsOdatatype :: PP_Doc
                        _lhsOargnamesw :: ( PP_Doc )
                        _lhsOargtps :: (  PP_Doc  )
                        _lhsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _lhsOterminaldefs :: (Set String)
                        _lhsOchildTypes :: (Map Identifier Type)
                        _lhsOargpats :: (  PP_Doc  )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 204, column 7)
                        _tpDoc =
                            ({-# LINE 204 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _addStrict     $ pp_parens $ ppTp $ removeDeforested tp_
                             {-# LINE 630 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 205, column 7)
                        _strNm =
                            ({-# LINE 205 "src-ag/ExecutionPlan2Hs.ag" #-}
                             recordFieldname _lhsInt _lhsIcon name_
                             {-# LINE 636 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 206, column 7)
                        _field =
                            ({-# LINE 206 "src-ag/ExecutionPlan2Hs.ag" #-}
                             if dataRecords _lhsIoptions
                             then _strNm     >#< "::" >#< _tpDoc
                             else _tpDoc
                             {-# LINE 644 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 209, column 7)
                        _addStrict =
                            ({-# LINE 209 "src-ag/ExecutionPlan2Hs.ag" #-}
                             \x -> if strictData _lhsIoptions then "!" >|< x else x
                             {-# LINE 650 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 214, column 13)
                        _lhsOdatatype =
                            ({-# LINE 214 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _field
                             {-# LINE 656 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 305, column 12)
                        _lhsOargnamesw =
                            ({-# LINE 305 "src-ag/ExecutionPlan2Hs.ag" #-}
                             text $ fieldname name_
                             {-# LINE 662 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 576, column 12)
                        _lhsOargtps =
                            ({-# LINE 576 "src-ag/ExecutionPlan2Hs.ag" #-}
                             (pp_parens $ show tp_) >#< "->"
                             {-# LINE 668 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 577, column 12)
                        _argpats =
                            ({-# LINE 577 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _addbang     $ text $ fieldname name_
                             {-# LINE 674 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 899, column 12)
                        _lhsOchildintros =
                            ({-# LINE 899 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.singleton name_ (\_ _ -> Right (empty, Set.empty, Map.empty))
                             {-# LINE 680 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1287, column 3)
                        _lhsOterminaldefs =
                            ({-# LINE 1287 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Set.singleton $ fieldname name_
                             {-# LINE 686 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1519, column 37)
                        _addbang =
                            ({-# LINE 1519 "src-ag/ExecutionPlan2Hs.ag" #-}
                             \x -> if bangpats _lhsIoptions then "!" >|< x else x
                             {-# LINE 692 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- "src-ag/ExecutionPlan2Hs.ag"(line 1570, column 3)
                        _lhsOchildTypes =
                            ({-# LINE 1570 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.singleton name_ tp_
                             {-# LINE 698 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (from local)
                        _lhsOargpats =
                            ({-# LINE 563 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _argpats
                             {-# LINE 704 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ___node =
                            (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs)
                    in  ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))))
-- EChildren ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInitStates        : Map NontermIdent Int
         con                  : ConstructorIdent
         importBlocks         : PP_Doc
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         pragmaBlocks         : String
         textBlocks           : PP_Doc
      synthesized attributes:
         argnamesw            : [PP_Doc]
         argpats              :  [PP_Doc] 
         argtps               :  [PP_Doc] 
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         datatype             : [PP_Doc]
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
                                   PP_Doc ->
                                   String ->
                                   String ->
                                   (String -> String -> String -> Bool -> String) ->
                                   NontermIdent ->
                                   Options ->
                                   String ->
                                   PP_Doc ->
                                   ( ([PP_Doc]),( [PP_Doc] ),( [PP_Doc] ),(Map Identifier Type),(Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),([PP_Doc]),(Set String)))
data Inh_EChildren = Inh_EChildren {allInitStates_Inh_EChildren :: (Map NontermIdent Int),con_Inh_EChildren :: ConstructorIdent,importBlocks_Inh_EChildren :: PP_Doc,mainFile_Inh_EChildren :: String,mainName_Inh_EChildren :: String,moduleHeader_Inh_EChildren :: (String -> String -> String -> Bool -> String),nt_Inh_EChildren :: NontermIdent,options_Inh_EChildren :: Options,pragmaBlocks_Inh_EChildren :: String,textBlocks_Inh_EChildren :: PP_Doc}
data Syn_EChildren = Syn_EChildren {argnamesw_Syn_EChildren :: ([PP_Doc]),argpats_Syn_EChildren :: ( [PP_Doc] ),argtps_Syn_EChildren :: ( [PP_Doc] ),childTypes_Syn_EChildren :: (Map Identifier Type),childintros_Syn_EChildren :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),datatype_Syn_EChildren :: ([PP_Doc]),terminaldefs_Syn_EChildren :: (Set String)}
wrap_EChildren :: T_EChildren ->
                  Inh_EChildren ->
                  Syn_EChildren
wrap_EChildren (T_EChildren sem) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
    (let ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs) = sem _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
     in  (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs))
sem_EChildren_Cons :: T_EChild ->
                      T_EChildren ->
                      T_EChildren
sem_EChildren_Cons (T_EChild hd_) (T_EChildren tl_) =
    (T_EChildren (\ _lhsIallInitStates
                    _lhsIcon
                    _lhsIimportBlocks
                    _lhsImainFile
                    _lhsImainName
                    _lhsImoduleHeader
                    _lhsInt
                    _lhsIoptions
                    _lhsIpragmaBlocks
                    _lhsItextBlocks ->
                      (let _lhsOargnamesw :: ([PP_Doc])
                           _lhsOargpats :: ( [PP_Doc] )
                           _lhsOargtps :: ( [PP_Doc] )
                           _lhsOchildTypes :: (Map Identifier Type)
                           _lhsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _lhsOdatatype :: ([PP_Doc])
                           _lhsOterminaldefs :: (Set String)
                           _hdOallInitStates :: (Map NontermIdent Int)
                           _hdOcon :: ConstructorIdent
                           _hdOimportBlocks :: PP_Doc
                           _hdOmainFile :: String
                           _hdOmainName :: String
                           _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                           _hdOnt :: NontermIdent
                           _hdOoptions :: Options
                           _hdOpragmaBlocks :: String
                           _hdOtextBlocks :: PP_Doc
                           _tlOallInitStates :: (Map NontermIdent Int)
                           _tlOcon :: ConstructorIdent
                           _tlOimportBlocks :: PP_Doc
                           _tlOmainFile :: String
                           _tlOmainName :: String
                           _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                           _tlOnt :: NontermIdent
                           _tlOoptions :: Options
                           _tlOpragmaBlocks :: String
                           _tlOtextBlocks :: PP_Doc
                           _hdIargnamesw :: ( PP_Doc )
                           _hdIargpats :: (  PP_Doc  )
                           _hdIargtps :: (  PP_Doc  )
                           _hdIchildTypes :: (Map Identifier Type)
                           _hdIchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _hdIdatatype :: PP_Doc
                           _hdIterminaldefs :: (Set String)
                           _tlIargnamesw :: ([PP_Doc])
                           _tlIargpats :: ( [PP_Doc] )
                           _tlIargtps :: ( [PP_Doc] )
                           _tlIchildTypes :: (Map Identifier Type)
                           _tlIchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _tlIdatatype :: ([PP_Doc])
                           _tlIterminaldefs :: (Set String)
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 298, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 298 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargnamesw : _tlIargnamesw
                                {-# LINE 821 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 565, column 30)
                           _lhsOargpats =
                               ({-# LINE 565 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargpats : _tlIargpats
                                {-# LINE 827 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 564, column 30)
                           _lhsOargtps =
                               ({-# LINE 564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIargtps : _tlIargtps
                                {-# LINE 833 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1563, column 40)
                           _lhsOchildTypes =
                               ({-# LINE 1563 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIchildTypes `mappend` _tlIchildTypes
                                {-# LINE 839 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 890, column 41)
                           _lhsOchildintros =
                               ({-# LINE 890 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIchildintros `Map.union` _tlIchildintros
                                {-# LINE 845 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 199, column 31)
                           _lhsOdatatype =
                               ({-# LINE 199 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIdatatype : _tlIdatatype
                                {-# LINE 851 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1284, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 1284 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _hdIterminaldefs `Set.union` _tlIterminaldefs
                                {-# LINE 857 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOallInitStates =
                               ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 863 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOcon =
                               ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIcon
                                {-# LINE 869 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOimportBlocks =
                               ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIimportBlocks
                                {-# LINE 875 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOmainFile =
                               ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImainFile
                                {-# LINE 881 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOmainName =
                               ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImainName
                                {-# LINE 887 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOmoduleHeader =
                               ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImoduleHeader
                                {-# LINE 893 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOnt =
                               ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsInt
                                {-# LINE 899 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOoptions =
                               ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 905 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOpragmaBlocks =
                               ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIpragmaBlocks
                                {-# LINE 911 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _hdOtextBlocks =
                               ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsItextBlocks
                                {-# LINE 917 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOallInitStates =
                               ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 923 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOcon =
                               ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIcon
                                {-# LINE 929 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOimportBlocks =
                               ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIimportBlocks
                                {-# LINE 935 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOmainFile =
                               ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImainFile
                                {-# LINE 941 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOmainName =
                               ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImainName
                                {-# LINE 947 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOmoduleHeader =
                               ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImoduleHeader
                                {-# LINE 953 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOnt =
                               ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsInt
                                {-# LINE 959 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOoptions =
                               ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 965 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOpragmaBlocks =
                               ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIpragmaBlocks
                                {-# LINE 971 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _tlOtextBlocks =
                               ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsItextBlocks
                                {-# LINE 977 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ( _hdIargnamesw,_hdIargpats,_hdIargtps,_hdIchildTypes,_hdIchildintros,_hdIdatatype,_hdIterminaldefs) =
                               hd_ _hdOallInitStates _hdOcon _hdOimportBlocks _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOtextBlocks
                           ( _tlIargnamesw,_tlIargpats,_tlIargtps,_tlIchildTypes,_tlIchildintros,_tlIdatatype,_tlIterminaldefs) =
                               tl_ _tlOallInitStates _tlOcon _tlOimportBlocks _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOtextBlocks
                           ___node =
                               (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs)
                       in  ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))))
sem_EChildren_Nil :: T_EChildren
sem_EChildren_Nil =
    (T_EChildren (\ _lhsIallInitStates
                    _lhsIcon
                    _lhsIimportBlocks
                    _lhsImainFile
                    _lhsImainName
                    _lhsImoduleHeader
                    _lhsInt
                    _lhsIoptions
                    _lhsIpragmaBlocks
                    _lhsItextBlocks ->
                      (let _lhsOargnamesw :: ([PP_Doc])
                           _lhsOargpats :: ( [PP_Doc] )
                           _lhsOargtps :: ( [PP_Doc] )
                           _lhsOchildTypes :: (Map Identifier Type)
                           _lhsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _lhsOdatatype :: ([PP_Doc])
                           _lhsOterminaldefs :: (Set String)
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 298, column 32)
                           _lhsOargnamesw =
                               ({-# LINE 298 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 1009 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 565, column 30)
                           _lhsOargpats =
                               ({-# LINE 565 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 1015 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 564, column 30)
                           _lhsOargtps =
                               ({-# LINE 564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 1021 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1563, column 40)
                           _lhsOchildTypes =
                               ({-# LINE 1563 "src-ag/ExecutionPlan2Hs.ag" #-}
                                mempty
                                {-# LINE 1027 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 890, column 41)
                           _lhsOchildintros =
                               ({-# LINE 890 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 1033 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 199, column 31)
                           _lhsOdatatype =
                               ({-# LINE 199 "src-ag/ExecutionPlan2Hs.ag" #-}
                                []
                                {-# LINE 1039 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1284, column 42)
                           _lhsOterminaldefs =
                               ({-# LINE 1284 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 1045 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ___node =
                               (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOchildintros _lhsOdatatype _lhsOterminaldefs)
                       in  ( _lhsOargnamesw,_lhsOargpats,_lhsOargtps,_lhsOchildTypes,_lhsOchildintros,_lhsOdatatype,_lhsOterminaldefs))))
-- ENonterminal ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         derivings            : Derivings
         importBlocks         : PP_Doc
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         pragmaBlocks         : String
         synmap               : Map NontermIdent Attributes
         textBlocks           : PP_Doc
         typeSyns             : TypeSyns
         wrappers             : Set NontermIdent
      synthesized attributes:
         appendCommon         :  PP_Doc 
         appendMain           :  PP_Doc 
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         genProdIO            : IO ()
         imports              : [PP_Doc]
         initStates           : Map NontermIdent Int
         output               : PP_Doc
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
            local classPP     : _
            local aliasPre    : _
            local datatype    : _
            local derivings   : _
            local fsemname    : _
            local semname     : _
            local frecarg     : _
            local sem_tp      : _
            local quantPP     : _
            local sem_nt      : _
            local inlineNt    : _
            local semPragma   : _
            local outedges    : _
            local inedges     : _
            local allstates   : _
            local stvisits    : _
            local t_type      : _
            local t_params    : _
            local t_init      : _
            local t_states    : _
            local k_type      : _
            local k_states    : _
            local wr_inh      : _
            local wr_syn      : _
            local genwrap     : _
            local synAttrs    : _
            local wr_inhs     : _
            local wr_inhs1    : _
            local wr_filter   : _
            local wr_syns     : _
            local inhlist     : _
            local inhlist1    : _
            local synlist     : _
            local wrapname    : _
            local inhname     : _
            local synname     : _
            local firstVisitInfo : _
            local wrapper     : _
            local wrapPragma  : _
            local semFunBndDef : _
            local semFunBndTp : _
            local semFunBndNm : _
            local addbang     : _
            local addbangWrap : _
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
                                         (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         Derivings ->
                                         PP_Doc ->
                                         (Map NontermIdent Attributes) ->
                                         (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                         String ->
                                         String ->
                                         (String -> String -> String -> Bool -> String) ->
                                         Options ->
                                         String ->
                                         (Map NontermIdent Attributes) ->
                                         PP_Doc ->
                                         TypeSyns ->
                                         (Set NontermIdent) ->
                                         ( ( PP_Doc ),( PP_Doc ),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),(Seq Error),(Map VisitIdentifier (Int,Int)),(IO ()),([PP_Doc]),(Map NontermIdent Int),PP_Doc,(Seq PP_Doc),(Seq PP_Doc),(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_ENonterminal = Inh_ENonterminal {allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_ENonterminal :: (Map NontermIdent Int),allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)),derivings_Inh_ENonterminal :: Derivings,importBlocks_Inh_ENonterminal :: PP_Doc,inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes),localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainFile_Inh_ENonterminal :: String,mainName_Inh_ENonterminal :: String,moduleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String),options_Inh_ENonterminal :: Options,pragmaBlocks_Inh_ENonterminal :: String,synmap_Inh_ENonterminal :: (Map NontermIdent Attributes),textBlocks_Inh_ENonterminal :: PP_Doc,typeSyns_Inh_ENonterminal :: TypeSyns,wrappers_Inh_ENonterminal :: (Set NontermIdent)}
data Syn_ENonterminal = Syn_ENonterminal {appendCommon_Syn_ENonterminal :: ( PP_Doc ),appendMain_Syn_ENonterminal :: ( PP_Doc ),childvisit_Syn_ENonterminal :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),errors_Syn_ENonterminal :: (Seq Error),fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)),genProdIO_Syn_ENonterminal :: (IO ()),imports_Syn_ENonterminal :: ([PP_Doc]),initStates_Syn_ENonterminal :: (Map NontermIdent Int),output_Syn_ENonterminal :: PP_Doc,semFunBndDefs_Syn_ENonterminal :: (Seq PP_Doc),semFunBndTps_Syn_ENonterminal :: (Seq PP_Doc),visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind),visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier))}
wrap_ENonterminal :: T_ENonterminal ->
                     Inh_ENonterminal ->
                     Syn_ENonterminal
wrap_ENonterminal (T_ENonterminal sem) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
    (let ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOinitStates,_lhsOoutput,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
     in  (Syn_ENonterminal _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
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
                       _lhsIderivings
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsIoptions
                       _lhsIpragmaBlocks
                       _lhsIsynmap
                       _lhsItextBlocks
                       _lhsItypeSyns
                       _lhsIwrappers ->
                         (let _prodsOrename :: Bool
                              _prodsOnt :: NontermIdent
                              _prodsOparams :: ([Identifier])
                              _prodsOclassCtxs :: ClassContext
                              _lhsOoutput :: PP_Doc
                              _prodsOinhmap :: Attributes
                              _prodsOsynmap :: Attributes
                              _prodsOallInhmap :: (Map NontermIdent Attributes)
                              _prodsOallSynmap :: (Map NontermIdent Attributes)
                              _lhsOsemFunBndDefs :: (Seq PP_Doc)
                              _lhsOsemFunBndTps :: (Seq PP_Doc)
                              _prodsOinitial :: StateIdentifier
                              _prodsOallstates :: (Set StateIdentifier)
                              _lhsOappendMain :: ( PP_Doc )
                              _lhsOappendCommon :: ( PP_Doc )
                              _prodsOnextVisits :: (Map StateIdentifier StateCtx)
                              _prodsOprevVisits :: (Map StateIdentifier StateCtx)
                              _prodsOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _lhsOinitStates :: (Map NontermIdent Int)
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
                              _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                              _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                              _prodsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                              _prodsOallInitStates :: (Map NontermIdent Int)
                              _prodsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                              _prodsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _prodsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _prodsOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _prodsOimportBlocks :: PP_Doc
                              _prodsOmainFile :: String
                              _prodsOmainName :: String
                              _prodsOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _prodsOntType :: Type
                              _prodsOoptions :: Options
                              _prodsOpragmaBlocks :: String
                              _prodsOtextBlocks :: PP_Doc
                              _prodsIallvisits :: ([VisitStateState])
                              _prodsIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _prodsIcount :: Int
                              _prodsIdatatype :: ([PP_Doc])
                              _prodsIerrors :: (Seq Error)
                              _prodsIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _prodsIgenProdIO :: (IO ())
                              _prodsIimports :: ([PP_Doc])
                              _prodsIsemFunBndDefs :: (Seq PP_Doc)
                              _prodsIsemFunBndTps :: (Seq PP_Doc)
                              _prodsIsem_nt :: PP_Doc
                              _prodsIsem_prod :: PP_Doc
                              _prodsIt_visits :: PP_Doc
                              _prodsIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _prodsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _prodsIvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 55, column 18)
                              _prodsOrename =
                                  ({-# LINE 55 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   rename _lhsIoptions
                                   {-# LINE 1274 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 63, column 18)
                              _prodsOnt =
                                  ({-# LINE 63 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   nt_
                                   {-# LINE 1280 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 75, column 3)
                              _prodsOparams =
                                  ({-# LINE 75 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   params_
                                   {-# LINE 1286 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 79, column 3)
                              _prodsOclassCtxs =
                                  ({-# LINE 79 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   classCtxs_
                                   {-# LINE 1292 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 96, column 18)
                              _lhsOoutput =
                                  ({-# LINE 96 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   {-# LINE 1324 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 123, column 18)
                              _hasWrapper =
                                  ({-# LINE 123 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   nt_ `Set.member` _lhsIwrappers
                                   {-# LINE 1330 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 136, column 18)
                              _classPP =
                                  ({-# LINE 136 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   ppClasses $ classCtxsToDocs classCtxs_
                                   {-# LINE 1336 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 137, column 18)
                              _aliasPre =
                                  ({-# LINE 137 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "type" >#< _classPP     >#< nt_ >#< _t_params     >#< "="
                                   {-# LINE 1342 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 138, column 18)
                              _datatype =
                                  ({-# LINE 138 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   {-# LINE 1363 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 155, column 18)
                              _derivings =
                                  ({-# LINE 155 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Nothing -> empty
                                      Just s  -> if   Set.null s
                                                 then empty
                                                 else "deriving" >#< (pp_parens $ ppCommas $ map pp $ Set.toList s)
                                   {-# LINE 1373 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 228, column 18)
                              _fsemname =
                                  ({-# LINE 228 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \x -> "sem_" ++ show x
                                   {-# LINE 1379 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 229, column 18)
                              _semname =
                                  ({-# LINE 229 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _fsemname     nt_
                                   {-# LINE 1385 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 230, column 18)
                              _frecarg =
                                  ({-# LINE 230 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \t x -> case t of
                                              NT nt _ _ -> pp_parens (_fsemname nt >#< x)
                                              _         -> pp x
                                   {-# LINE 1393 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 236, column 18)
                              _sem_tp =
                                  ({-# LINE 236 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _quantPP     >#< _classPP     >#< nt_ >#< _t_params     >#< "->" >#< _t_type     >#< _t_params
                                   {-# LINE 1399 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 237, column 18)
                              _quantPP =
                                  ({-# LINE 237 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   ppQuants params_
                                   {-# LINE 1405 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 238, column 18)
                              _sem_nt =
                                  ({-# LINE 238 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   {-# LINE 1447 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 278, column 17)
                              _inlineNt =
                                  ({-# LINE 278 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   not (lateHigherOrderBinding _lhsIoptions) && not recursive_ && (_prodsIcount == 1 || (aggressiveInlinePragmas _lhsIoptions && not _hasWrapper    ))
                                   {-# LINE 1453 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 279, column 17)
                              _semPragma =
                                  ({-# LINE 279 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   if noInlinePragmas _lhsIoptions
                                   then empty
                                   else if _inlineNt
                                        then ppInline _semname
                                        else if helpInlining _lhsIoptions && not (lateHigherOrderBinding _lhsIoptions)
                                             then ppInlinable _semname
                                             else ppNoInline _semname
                                   {-# LINE 1465 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 325, column 19)
                              (Just _prodsOinhmap) =
                                  ({-# LINE 325 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 1471 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 326, column 19)
                              (Just _prodsOsynmap) =
                                  ({-# LINE 326 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 1477 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 327, column 18)
                              _prodsOallInhmap =
                                  ({-# LINE 327 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 1483 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 328, column 18)
                              _prodsOallSynmap =
                                  ({-# LINE 328 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 1489 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 349, column 18)
                              _outedges =
                                  ({-# LINE 349 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 1495 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 350, column 18)
                              _inedges =
                                  ({-# LINE 350 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 1501 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 351, column 18)
                              _allstates =
                                  ({-# LINE 351 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 1507 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 352, column 18)
                              _stvisits =
                                  ({-# LINE 352 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1513 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 353, column 18)
                              _t_type =
                                  ({-# LINE 353 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "T_" >|< nt_
                                   {-# LINE 1519 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 354, column 18)
                              _t_params =
                                  ({-# LINE 354 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   ppSpaced params_
                                   {-# LINE 1525 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 355, column 18)
                              _t_init =
                                  ({-# LINE 355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "newtype" >#< _t_type     >#< _t_params     >#< "=" >#< _t_type     >#<
                                     pp_braces (
                                       "attach_">|< _t_type     >#< "::"
                                         >#< ppMonadType _lhsIoptions >#< pp_parens (_t_type     >|< "_s" >|< initial_ >#< _t_params    ))
                                   {-# LINE 1534 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 359, column 18)
                              _t_states =
                                  ({-# LINE 359 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   {-# LINE 1554 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 377, column 18)
                              _k_type =
                                  ({-# LINE 377 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "K_" ++ show nt_
                                   {-# LINE 1560 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 378, column 18)
                              _k_states =
                                  ({-# LINE 378 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   {-# LINE 1579 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 445, column 18)
                              _wr_inh =
                                  ({-# LINE 445 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _genwrap     "Inh" _wr_inhs
                                   {-# LINE 1585 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 446, column 18)
                              _wr_syn =
                                  ({-# LINE 446 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _genwrap     "Syn" _wr_syns
                                   {-# LINE 1591 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 447, column 18)
                              _genwrap =
                                  ({-# LINE 447 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \nm attr -> "data" >#< nm >|< "_" >|< nt_ >#< _t_params     >#< "=" >#< nm >|< "_" >|< nt_ >#< "{"
                                               >#< (ppCommas $ map (\(i,t) -> i >|< "_" >|< nm >|< "_" >|< nt_ >#< "::"
                                               >#< (_addbang     $ pp_parens $ typeToHaskellString (Just nt_) [] t)) attr) >#< "}"
                                   {-# LINE 1599 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 450, column 18)
                              _synAttrs =
                                  ({-# LINE 450 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   fromJust $ Map.lookup nt_ _lhsIinhmap
                                   {-# LINE 1605 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 451, column 18)
                              _wr_inhs =
                                  ({-# LINE 451 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.toList $ _wr_filter     $ _synAttrs
                                   {-# LINE 1611 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 452, column 18)
                              _wr_inhs1 =
                                  ({-# LINE 452 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.toList _synAttrs
                                   {-# LINE 1617 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 453, column 18)
                              _wr_filter =
                                  ({-# LINE 453 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                                   then Map.delete idLateBindingAttr
                                   else id
                                   {-# LINE 1625 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 456, column 18)
                              _wr_syns =
                                  ({-# LINE 456 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                   {-# LINE 1631 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 457, column 18)
                              _inhlist =
                                  ({-# LINE 457 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   map (lhsname True . fst) _wr_inhs
                                   {-# LINE 1637 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 458, column 18)
                              _inhlist1 =
                                  ({-# LINE 458 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   map (lhsname True . fst) _wr_inhs1
                                   {-# LINE 1643 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 459, column 18)
                              _synlist =
                                  ({-# LINE 459 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   map (lhsname False . fst) _wr_syns
                                   {-# LINE 1649 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 460, column 18)
                              _wrapname =
                                  ({-# LINE 460 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "wrap_" ++ show nt_
                                   {-# LINE 1655 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 461, column 18)
                              _inhname =
                                  ({-# LINE 461 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "Inh_" ++ show nt_
                                   {-# LINE 1661 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 462, column 18)
                              _synname =
                                  ({-# LINE 462 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   "Syn_" ++ show nt_
                                   {-# LINE 1667 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 463, column 18)
                              _firstVisitInfo =
                                  ({-# LINE 463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.findWithDefault ManyVis initial_ nextVisits_
                                   {-# LINE 1673 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 464, column 18)
                              _wrapper =
                                  ({-# LINE 464 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                   >-< if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                                       then indent 2 ("where" >#< lhsname True idLateBindingAttr >#< "=" >#< lateBindingFieldNm _lhsImainName)
                                       else empty
                                   {-# LINE 1718 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 505, column 18)
                              _wrapPragma =
                                  ({-# LINE 505 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   if parallelInvoke _lhsIoptions && not (monadicWrappers _lhsIoptions)
                                   then ppNoInline _wrapname
                                   else if noInlinePragmas _lhsIoptions
                                        then empty
                                        else ppInlinable _wrapname
                                   {-# LINE 1728 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 517, column 3)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 517 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                                   {-# LINE 1734 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 518, column 3)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 518 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                                   {-# LINE 1740 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 519, column 3)
                              _semFunBndDef =
                                  ({-# LINE 519 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _semFunBndNm     >#< "=" >#< _semname
                                   {-# LINE 1746 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 520, column 3)
                              _semFunBndTp =
                                  ({-# LINE 520 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _semFunBndNm     >#< "::" >#< _sem_tp
                                   {-# LINE 1752 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 521, column 3)
                              _semFunBndNm =
                                  ({-# LINE 521 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   lateSemNtLabel nt_
                                   {-# LINE 1758 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 559, column 18)
                              _prodsOinitial =
                                  ({-# LINE 559 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   initial_
                                   {-# LINE 1764 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 560, column 18)
                              _prodsOallstates =
                                  ({-# LINE 560 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _allstates
                                   {-# LINE 1770 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1450, column 18)
                              _lhsOappendMain =
                                  ({-# LINE 1450 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   (if nt_ `Set.member` _lhsIwrappers
                                    then     _wr_inh
                                         >-< _wr_syn
                                         >-< _wrapper
                                    else empty)
                                   >-< _sem_nt
                                   {-# LINE 1781 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1456, column 18)
                              _lhsOappendCommon =
                                  ({-# LINE 1456 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   (if dataTypes _lhsIoptions then _datatype     else empty)
                                   >-< _t_init
                                   >-< _t_states
                                   >-< _k_states
                                   >-< _prodsIt_visits
                                   {-# LINE 1791 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1516, column 37)
                              _addbang =
                                  ({-# LINE 1516 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                   {-# LINE 1797 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1524, column 37)
                              _addbangWrap =
                                  ({-# LINE 1524 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   if strictWrap _lhsIoptions then _addbang     else id
                                   {-# LINE 1803 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1536, column 3)
                              _prodsOnextVisits =
                                  ({-# LINE 1536 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   nextVisits_
                                   {-# LINE 1809 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1537, column 3)
                              _prodsOprevVisits =
                                  ({-# LINE 1537 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   prevVisits_
                                   {-# LINE 1815 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1581, column 3)
                              _prodsOlocalAttrTypes =
                                  ({-# LINE 1581 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                                   {-# LINE 1821 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1608, column 3)
                              _lhsOinitStates =
                                  ({-# LINE 1608 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.singleton nt_ initial_
                                   {-# LINE 1827 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 1622, column 3)
                              _ntType =
                                  ({-# LINE 1622 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   NT nt_ (map show params_) False
                                   {-# LINE 1833 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIchildvisit
                                   {-# LINE 1839 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIerrors
                                   {-# LINE 1845 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIfromToStates
                                   {-# LINE 1851 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1464, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 1464 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIgenProdIO
                                   {-# LINE 1857 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1463, column 47)
                              _lhsOimports =
                                  ({-# LINE 1463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIimports
                                   {-# LINE 1863 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIvisitKinds
                                   {-# LINE 1869 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIvisitdefs
                                   {-# LINE 1875 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _prodsIvisituses
                                   {-# LINE 1881 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallFromToStates =
                                  ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 1887 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallInitStates =
                                  ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 1893 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallVisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 1899 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallchildvisit =
                                  ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 1905 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisitdefs =
                                  ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 1911 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOavisituses =
                                  ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 1917 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOimportBlocks =
                                  ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 1923 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainFile =
                                  ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 1929 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmainName =
                                  ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 1935 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOmoduleHeader =
                                  ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 1941 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (from local)
                              _prodsOntType =
                                  ({-# LINE 1620 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _ntType
                                   {-# LINE 1947 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOoptions =
                                  ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1953 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOpragmaBlocks =
                                  ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 1959 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOtextBlocks =
                                  ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 1965 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              ( _prodsIallvisits,_prodsIchildvisit,_prodsIcount,_prodsIdatatype,_prodsIerrors,_prodsIfromToStates,_prodsIgenProdIO,_prodsIimports,_prodsIsemFunBndDefs,_prodsIsemFunBndTps,_prodsIsem_nt,_prodsIsem_prod,_prodsIt_visits,_prodsIvisitKinds,_prodsIvisitdefs,_prodsIvisituses) =
                                  prods_ _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallchildvisit _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOclassCtxs _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOmoduleHeader _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOpragmaBlocks _prodsOprevVisits _prodsOrename _prodsOsynmap _prodsOtextBlocks
                              ___node =
                                  (Syn_ENonterminal _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                          in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOinitStates,_lhsOoutput,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- ENonterminals -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         derivings            : Derivings
         importBlocks         : PP_Doc
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         pragmaBlocks         : String
         synmap               : Map NontermIdent Attributes
         textBlocks           : PP_Doc
         typeSyns             : TypeSyns
         wrappers             : Set NontermIdent
      synthesized attributes:
         appendCommon         : [PP_Doc]
         appendMain           : [PP_Doc]
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         genProdIO            : IO ()
         imports              : [PP_Doc]
         initStates           : Map NontermIdent Int
         output               : PP_Doc
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
                                           (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                           (Map VisitIdentifier (Set Identifier)) ->
                                           (Map VisitIdentifier (Set Identifier)) ->
                                           Derivings ->
                                           PP_Doc ->
                                           (Map NontermIdent Attributes) ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                           String ->
                                           String ->
                                           (String -> String -> String -> Bool -> String) ->
                                           Options ->
                                           String ->
                                           (Map NontermIdent Attributes) ->
                                           PP_Doc ->
                                           TypeSyns ->
                                           (Set NontermIdent) ->
                                           ( ([PP_Doc]),([PP_Doc]),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),(Seq Error),(Map VisitIdentifier (Int,Int)),(IO ()),([PP_Doc]),(Map NontermIdent Int),PP_Doc,(Seq PP_Doc),(Seq PP_Doc),(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_ENonterminals = Inh_ENonterminals {allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_ENonterminals :: (Map NontermIdent Int),allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)),derivings_Inh_ENonterminals :: Derivings,importBlocks_Inh_ENonterminals :: PP_Doc,inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes),localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainFile_Inh_ENonterminals :: String,mainName_Inh_ENonterminals :: String,moduleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String),options_Inh_ENonterminals :: Options,pragmaBlocks_Inh_ENonterminals :: String,synmap_Inh_ENonterminals :: (Map NontermIdent Attributes),textBlocks_Inh_ENonterminals :: PP_Doc,typeSyns_Inh_ENonterminals :: TypeSyns,wrappers_Inh_ENonterminals :: (Set NontermIdent)}
data Syn_ENonterminals = Syn_ENonterminals {appendCommon_Syn_ENonterminals :: ([PP_Doc]),appendMain_Syn_ENonterminals :: ([PP_Doc]),childvisit_Syn_ENonterminals :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),errors_Syn_ENonterminals :: (Seq Error),fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)),genProdIO_Syn_ENonterminals :: (IO ()),imports_Syn_ENonterminals :: ([PP_Doc]),initStates_Syn_ENonterminals :: (Map NontermIdent Int),output_Syn_ENonterminals :: PP_Doc,semFunBndDefs_Syn_ENonterminals :: (Seq PP_Doc),semFunBndTps_Syn_ENonterminals :: (Seq PP_Doc),visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind),visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier))}
wrap_ENonterminals :: T_ENonterminals ->
                      Inh_ENonterminals ->
                      Syn_ENonterminals
wrap_ENonterminals (T_ENonterminals sem) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
    (let ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOinitStates,_lhsOoutput,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
     in  (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
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
                        _lhsIderivings
                        _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlocks
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOappendCommon :: ([PP_Doc])
                               _lhsOappendMain :: ([PP_Doc])
                               _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _lhsOerrors :: (Seq Error)
                               _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                               _lhsOgenProdIO :: (IO ())
                               _lhsOimports :: ([PP_Doc])
                               _lhsOinitStates :: (Map NontermIdent Int)
                               _lhsOoutput :: PP_Doc
                               _lhsOsemFunBndDefs :: (Seq PP_Doc)
                               _lhsOsemFunBndTps :: (Seq PP_Doc)
                               _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                               _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                               _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _hdOallInitStates :: (Map NontermIdent Int)
                               _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _hdOderivings :: Derivings
                               _hdOimportBlocks :: PP_Doc
                               _hdOinhmap :: (Map NontermIdent Attributes)
                               _hdOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _hdOmainFile :: String
                               _hdOmainName :: String
                               _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _hdOoptions :: Options
                               _hdOpragmaBlocks :: String
                               _hdOsynmap :: (Map NontermIdent Attributes)
                               _hdOtextBlocks :: PP_Doc
                               _hdOtypeSyns :: TypeSyns
                               _hdOwrappers :: (Set NontermIdent)
                               _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _tlOallInitStates :: (Map NontermIdent Int)
                               _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _tlOderivings :: Derivings
                               _tlOimportBlocks :: PP_Doc
                               _tlOinhmap :: (Map NontermIdent Attributes)
                               _tlOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _tlOmainFile :: String
                               _tlOmainName :: String
                               _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _tlOoptions :: Options
                               _tlOpragmaBlocks :: String
                               _tlOsynmap :: (Map NontermIdent Attributes)
                               _tlOtextBlocks :: PP_Doc
                               _tlOtypeSyns :: TypeSyns
                               _tlOwrappers :: (Set NontermIdent)
                               _hdIappendCommon :: ( PP_Doc )
                               _hdIappendMain :: ( PP_Doc )
                               _hdIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _hdIerrors :: (Seq Error)
                               _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _hdIgenProdIO :: (IO ())
                               _hdIimports :: ([PP_Doc])
                               _hdIinitStates :: (Map NontermIdent Int)
                               _hdIoutput :: PP_Doc
                               _hdIsemFunBndDefs :: (Seq PP_Doc)
                               _hdIsemFunBndTps :: (Seq PP_Doc)
                               _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                               _tlIappendCommon :: ([PP_Doc])
                               _tlIappendMain :: ([PP_Doc])
                               _tlIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _tlIerrors :: (Seq Error)
                               _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _tlIgenProdIO :: (IO ())
                               _tlIimports :: ([PP_Doc])
                               _tlIinitStates :: (Map NontermIdent Int)
                               _tlIoutput :: PP_Doc
                               _tlIsemFunBndDefs :: (Seq PP_Doc)
                               _tlIsemFunBndTps :: (Seq PP_Doc)
                               _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1447, column 51)
                               _lhsOappendCommon =
                                   ({-# LINE 1447 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIappendCommon : _tlIappendCommon
                                    {-# LINE 2157 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1447, column 51)
                               _lhsOappendMain =
                                   ({-# LINE 1447 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIappendMain : _tlIappendMain
                                    {-# LINE 2163 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIchildvisit `Map.union` _tlIchildvisit
                                    {-# LINE 2169 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIerrors Seq.>< _tlIerrors
                                    {-# LINE 2175 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                               _lhsOfromToStates =
                                   ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIfromToStates `mappend` _tlIfromToStates
                                    {-# LINE 2181 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1464, column 49)
                               _lhsOgenProdIO =
                                   ({-# LINE 1464 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIgenProdIO >> _tlIgenProdIO
                                    {-# LINE 2187 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1463, column 47)
                               _lhsOimports =
                                   ({-# LINE 1463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIimports ++ _tlIimports
                                    {-# LINE 2193 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1603, column 50)
                               _lhsOinitStates =
                                   ({-# LINE 1603 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIinitStates `mappend` _tlIinitStates
                                    {-# LINE 2199 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 90, column 45)
                               _lhsOoutput =
                                   ({-# LINE 90 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIoutput >-< _tlIoutput
                                    {-# LINE 2205 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                               _lhsOsemFunBndDefs =
                                   ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
                                    {-# LINE 2211 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                               _lhsOsemFunBndTps =
                                   ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
                                    {-# LINE 2217 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                               _lhsOvisitKinds =
                                   ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIvisitKinds `mappend` _tlIvisitKinds
                                    {-# LINE 2223 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                    {-# LINE 2229 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _hdIvisituses `uwSetUnion` _tlIvisituses
                                    {-# LINE 2235 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallFromToStates =
                                   ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallFromToStates
                                    {-# LINE 2241 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallInitStates =
                                   ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallInitStates
                                    {-# LINE 2247 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallVisitKinds =
                                   ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallVisitKinds
                                    {-# LINE 2253 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallchildvisit =
                                   ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 2259 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisitdefs =
                                   ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 2265 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOavisituses =
                                   ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 2271 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOderivings =
                                   ({-# LINE 129 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 2277 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOimportBlocks =
                                   ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 2283 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOinhmap =
                                   ({-# LINE 312 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 2289 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOlocalAttrTypes =
                                   ({-# LINE 1576 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 2295 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainFile =
                                   ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 2301 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmainName =
                                   ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 2307 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmoduleHeader =
                                   ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 2313 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOoptions =
                                   ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2319 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOpragmaBlocks =
                                   ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 2325 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOsynmap =
                                   ({-# LINE 313 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 2331 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtextBlocks =
                                   ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 2337 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtypeSyns =
                                   ({-# LINE 128 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 2343 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOwrappers =
                                   ({-# LINE 89 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 2349 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallFromToStates =
                                   ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallFromToStates
                                    {-# LINE 2355 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallInitStates =
                                   ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallInitStates
                                    {-# LINE 2361 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallVisitKinds =
                                   ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallVisitKinds
                                    {-# LINE 2367 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallchildvisit =
                                   ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIallchildvisit
                                    {-# LINE 2373 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisitdefs =
                                   ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisitdefs
                                    {-# LINE 2379 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOavisituses =
                                   ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIavisituses
                                    {-# LINE 2385 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOderivings =
                                   ({-# LINE 129 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 2391 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOimportBlocks =
                                   ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 2397 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOinhmap =
                                   ({-# LINE 312 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 2403 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOlocalAttrTypes =
                                   ({-# LINE 1576 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 2409 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainFile =
                                   ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 2415 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmainName =
                                   ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 2421 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmoduleHeader =
                                   ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 2427 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOoptions =
                                   ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2433 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOpragmaBlocks =
                                   ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 2439 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOsynmap =
                                   ({-# LINE 313 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 2445 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtextBlocks =
                                   ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 2451 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtypeSyns =
                                   ({-# LINE 128 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 2457 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOwrappers =
                                   ({-# LINE 89 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 2463 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               ( _hdIappendCommon,_hdIappendMain,_hdIchildvisit,_hdIerrors,_hdIfromToStates,_hdIgenProdIO,_hdIimports,_hdIinitStates,_hdIoutput,_hdIsemFunBndDefs,_hdIsemFunBndTps,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                                   hd_ _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOderivings _hdOimportBlocks _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOoptions _hdOpragmaBlocks _hdOsynmap _hdOtextBlocks _hdOtypeSyns _hdOwrappers
                               ( _tlIappendCommon,_tlIappendMain,_tlIchildvisit,_tlIerrors,_tlIfromToStates,_tlIgenProdIO,_tlIimports,_tlIinitStates,_tlIoutput,_tlIsemFunBndDefs,_tlIsemFunBndTps,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                                   tl_ _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOderivings _tlOimportBlocks _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOoptions _tlOpragmaBlocks _tlOsynmap _tlOtextBlocks _tlOtypeSyns _tlOwrappers
                               ___node =
                                   (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                           in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOinitStates,_lhsOoutput,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
sem_ENonterminals_Nil :: T_ENonterminals
sem_ENonterminals_Nil =
    (T_ENonterminals (\ _lhsIallFromToStates
                        _lhsIallInitStates
                        _lhsIallVisitKinds
                        _lhsIallchildvisit
                        _lhsIavisitdefs
                        _lhsIavisituses
                        _lhsIderivings
                        _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlocks
                        _lhsItypeSyns
                        _lhsIwrappers ->
                          (let _lhsOappendCommon :: ([PP_Doc])
                               _lhsOappendMain :: ([PP_Doc])
                               _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _lhsOerrors :: (Seq Error)
                               _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                               _lhsOgenProdIO :: (IO ())
                               _lhsOimports :: ([PP_Doc])
                               _lhsOinitStates :: (Map NontermIdent Int)
                               _lhsOoutput :: PP_Doc
                               _lhsOsemFunBndDefs :: (Seq PP_Doc)
                               _lhsOsemFunBndTps :: (Seq PP_Doc)
                               _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                               _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1447, column 51)
                               _lhsOappendCommon =
                                   ({-# LINE 1447 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 2511 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1447, column 51)
                               _lhsOappendMain =
                                   ({-# LINE 1447 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 2517 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                               _lhsOchildvisit =
                                   ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 2523 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Seq.empty
                                    {-# LINE 2529 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                               _lhsOfromToStates =
                                   ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    mempty
                                    {-# LINE 2535 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1464, column 49)
                               _lhsOgenProdIO =
                                   ({-# LINE 1464 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    return ()
                                    {-# LINE 2541 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1463, column 47)
                               _lhsOimports =
                                   ({-# LINE 1463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    []
                                    {-# LINE 2547 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1603, column 50)
                               _lhsOinitStates =
                                   ({-# LINE 1603 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    mempty
                                    {-# LINE 2553 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 90, column 45)
                               _lhsOoutput =
                                   ({-# LINE 90 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    empty
                                    {-# LINE 2559 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                               _lhsOsemFunBndDefs =
                                   ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Seq.empty
                                    {-# LINE 2565 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                               _lhsOsemFunBndTps =
                                   ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Seq.empty
                                    {-# LINE 2571 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                               _lhsOvisitKinds =
                                   ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    mempty
                                    {-# LINE 2577 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                               _lhsOvisitdefs =
                                   ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 2583 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                               _lhsOvisituses =
                                   ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    Map.empty
                                    {-# LINE 2589 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               ___node =
                                   (Syn_ENonterminals _lhsOappendCommon _lhsOappendMain _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                           in  ( _lhsOappendCommon,_lhsOappendMain,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOinitStates,_lhsOoutput,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- EProduction -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         allstates            : Set StateIdentifier
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         classCtxs            : ClassContext
         importBlocks         : PP_Doc
         inhmap               : Attributes
         initial              : StateIdentifier
         localAttrTypes       : Map ConstructorIdent (Map Identifier Type)
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         ntType               : Type
         options              : Options
         params               : [Identifier]
         pragmaBlocks         : String
         prevVisits           : Map StateIdentifier StateCtx
         rename               : Bool
         synmap               : Attributes
         textBlocks           : PP_Doc
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         count                : Int
         datatype             : PP_Doc
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         genProdIO            : IO ()
         imports              : [PP_Doc]
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
            local classPP1    : _
            local quantPP1    : _
            local semFunBndDef : _
            local semFunBndTp : _
            local semFunBndNm : _
            local t_type      : _
            local t_params    : _
            local args        : _
            local semname     : _
            local sem_tp      : _
            local classPP2    : _
            local quantPP2    : _
            local sem_prod    : _
            local mkSemBody   : _
            local mbInitializer : _
            local scc         : _
            local semInlinePragma : _
            local outerlet    : _
            local statefns    : _
            local genstfn     : _
            local stargs      : _
            local stks        : _
            local stvisits    : _
            local stvs        : _
            local lazyIntras  : _
            local moduleName  : _
            local suffix      : _
            local outputfile  : _
            local ppMonadImports : _
            local addbang     : _
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
                                       (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                       (Set StateIdentifier) ->
                                       (Map VisitIdentifier (Set Identifier)) ->
                                       (Map VisitIdentifier (Set Identifier)) ->
                                       ClassContext ->
                                       PP_Doc ->
                                       Attributes ->
                                       StateIdentifier ->
                                       (Map ConstructorIdent (Map Identifier Type)) ->
                                       String ->
                                       String ->
                                       (String -> String -> String -> Bool -> String) ->
                                       (Map StateIdentifier StateCtx) ->
                                       NontermIdent ->
                                       Type ->
                                       Options ->
                                       ([Identifier]) ->
                                       String ->
                                       (Map StateIdentifier StateCtx) ->
                                       Bool ->
                                       Attributes ->
                                       PP_Doc ->
                                       ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),Int,PP_Doc,(Seq Error),(Map VisitIdentifier (Int,Int)),(IO ()),([PP_Doc]),(Seq PP_Doc),(Seq PP_Doc),PP_Doc,PP_Doc,PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_EProduction = Inh_EProduction {allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_EProduction :: (Map NontermIdent Attributes),allInitStates_Inh_EProduction :: (Map NontermIdent Int),allSynmap_Inh_EProduction :: (Map NontermIdent Attributes),allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),allstates_Inh_EProduction :: (Set StateIdentifier),avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)),classCtxs_Inh_EProduction :: ClassContext,importBlocks_Inh_EProduction :: PP_Doc,inhmap_Inh_EProduction :: Attributes,initial_Inh_EProduction :: StateIdentifier,localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)),mainFile_Inh_EProduction :: String,mainName_Inh_EProduction :: String,moduleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String),nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx),nt_Inh_EProduction :: NontermIdent,ntType_Inh_EProduction :: Type,options_Inh_EProduction :: Options,params_Inh_EProduction :: ([Identifier]),pragmaBlocks_Inh_EProduction :: String,prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx),rename_Inh_EProduction :: Bool,synmap_Inh_EProduction :: Attributes,textBlocks_Inh_EProduction :: PP_Doc}
data Syn_EProduction = Syn_EProduction {allvisits_Syn_EProduction :: ([VisitStateState]),childvisit_Syn_EProduction :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),count_Syn_EProduction :: Int,datatype_Syn_EProduction :: PP_Doc,errors_Syn_EProduction :: (Seq Error),fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)),genProdIO_Syn_EProduction :: (IO ()),imports_Syn_EProduction :: ([PP_Doc]),semFunBndDefs_Syn_EProduction :: (Seq PP_Doc),semFunBndTps_Syn_EProduction :: (Seq PP_Doc),sem_nt_Syn_EProduction :: PP_Doc,sem_prod_Syn_EProduction :: PP_Doc,t_visits_Syn_EProduction :: PP_Doc,visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind),visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier))}
wrap_EProduction :: T_EProduction ->
                    Inh_EProduction ->
                    Syn_EProduction
wrap_EProduction (T_EProduction sem) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
     in  (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
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
                      _lhsIclassCtxs
                      _lhsIimportBlocks
                      _lhsIinhmap
                      _lhsIinitial
                      _lhsIlocalAttrTypes
                      _lhsImainFile
                      _lhsImainName
                      _lhsImoduleHeader
                      _lhsInextVisits
                      _lhsInt
                      _lhsIntType
                      _lhsIoptions
                      _lhsIparams
                      _lhsIpragmaBlocks
                      _lhsIprevVisits
                      _lhsIrename
                      _lhsIsynmap
                      _lhsItextBlocks ->
                        (let _childrenOcon :: ConstructorIdent
                             _rulesOcon :: ConstructorIdent
                             _visitsOcon :: ConstructorIdent
                             _lhsOdatatype :: PP_Doc
                             _lhsOcount :: Int
                             _lhsOsem_nt :: PP_Doc
                             _lhsOsemFunBndDefs :: (Seq PP_Doc)
                             _lhsOsemFunBndTps :: (Seq PP_Doc)
                             _visitsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                             _visitsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                             _rulesOusageInfo :: (Map Identifier Int)
                             _rulesOruleKinds :: (Map Identifier (Set VisitKind))
                             _visitsOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                             _visitsOterminaldefs :: (Set String)
                             _visitsOruledefs :: (Map Identifier (Set String))
                             _visitsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                             _lhsOimports :: ([PP_Doc])
                             _lhsOgenProdIO :: (IO ())
                             _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
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
                             _rulesOimportBlocks :: PP_Doc
                             _rulesOinhmap :: Attributes
                             _rulesOlazyIntras :: (Set String)
                             _rulesOlocalAttrTypes :: (Map Identifier Type)
                             _rulesOmainFile :: String
                             _rulesOmainName :: String
                             _rulesOmoduleHeader :: (String -> String -> String -> Bool -> String)
                             _rulesOnt :: NontermIdent
                             _rulesOoptions :: Options
                             _rulesOpragmaBlocks :: String
                             _rulesOsynmap :: Attributes
                             _rulesOtextBlocks :: PP_Doc
                             _childrenOallInitStates :: (Map NontermIdent Int)
                             _childrenOimportBlocks :: PP_Doc
                             _childrenOmainFile :: String
                             _childrenOmainName :: String
                             _childrenOmoduleHeader :: (String -> String -> String -> Bool -> String)
                             _childrenOnt :: NontermIdent
                             _childrenOoptions :: Options
                             _childrenOpragmaBlocks :: String
                             _childrenOtextBlocks :: PP_Doc
                             _visitsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                             _visitsOallInhmap :: (Map NontermIdent Attributes)
                             _visitsOallInitStates :: (Map NontermIdent Int)
                             _visitsOallSynmap :: (Map NontermIdent Attributes)
                             _visitsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                             _visitsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
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
                             _rulesImrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                             _rulesIruledefs :: (Map Identifier (Set String))
                             _rulesIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                             _rulesIsem_rules :: PP_Doc
                             _childrenIargnamesw :: ([PP_Doc])
                             _childrenIargpats :: ( [PP_Doc] )
                             _childrenIargtps :: ( [PP_Doc] )
                             _childrenIchildTypes :: (Map Identifier Type)
                             _childrenIchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                             _childrenIdatatype :: ([PP_Doc])
                             _childrenIterminaldefs :: (Set String)
                             _visitsIallvisits :: ([VisitStateState])
                             _visitsIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                             _visitsIerrors :: (Seq Error)
                             _visitsIfromToStates :: (Map VisitIdentifier (Int,Int))
                             _visitsIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                             _visitsIlazyIntras :: (Set String)
                             _visitsIruleKinds :: (Map Identifier (Set VisitKind))
                             _visitsIruleUsage :: (Map Identifier Int)
                             _visitsIsem_visit :: ( [(StateIdentifier,Bool -> PP_Doc)] )
                             _visitsIt_visits :: PP_Doc
                             _visitsIvisitKinds :: (Map VisitIdentifier VisitKind)
                             _visitsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                             _visitsIvisituses :: (Map VisitIdentifier (Set Identifier))
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 69, column 17)
                             _childrenOcon =
                                 ({-# LINE 69 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  con_
                                  {-# LINE 2857 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 70, column 17)
                             _rulesOcon =
                                 ({-# LINE 70 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  con_
                                  {-# LINE 2863 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 71, column 17)
                             _visitsOcon =
                                 ({-# LINE 71 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  con_
                                  {-# LINE 2869 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 184, column 17)
                             _lhsOdatatype =
                                 ({-# LINE 184 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _quantPP1     >#< _classPP1
                                  >#< conname _lhsIrename _lhsInt con_
                                  >#< ppConFields (dataRecords _lhsIoptions) _childrenIdatatype
                                  {-# LINE 2877 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 187, column 17)
                             _classPP1 =
                                 ({-# LINE 187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppClasses (classConstrsToDocs constraints_)
                                  {-# LINE 2883 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 188, column 17)
                             _quantPP1 =
                                 ({-# LINE 188 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppQuants params_
                                  {-# LINE 2889 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 289, column 32)
                             _lhsOcount =
                                 ({-# LINE 289 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  1
                                  {-# LINE 2895 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 294, column 17)
                             _lhsOsem_nt =
                                 ({-# LINE 294 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "sem_" >|< _lhsInt >#< "(" >#< conname _lhsIrename _lhsInt con_ >#< ppSpaced _childrenIargpats >#< ")"
                                  >#< "=" >#< "sem_" >|< _lhsInt >|< "_" >|< con_ >#< ppSpaced _childrenIargnamesw
                                  {-# LINE 2902 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 524, column 3)
                             _lhsOsemFunBndDefs =
                                 ({-# LINE 524 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  Seq.singleton _semFunBndDef
                                  {-# LINE 2908 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 525, column 3)
                             _lhsOsemFunBndTps =
                                 ({-# LINE 525 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  Seq.singleton _semFunBndTp
                                  {-# LINE 2914 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 526, column 3)
                             _semFunBndDef =
                                 ({-# LINE 526 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _semFunBndNm     >#< "=" >#< _semname
                                  {-# LINE 2920 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 527, column 3)
                             _semFunBndTp =
                                 ({-# LINE 527 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _semFunBndNm     >#< "::" >#< _sem_tp
                                  {-# LINE 2926 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 528, column 3)
                             _semFunBndNm =
                                 ({-# LINE 528 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  lateSemConLabel _lhsInt con_
                                  {-# LINE 2932 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 586, column 17)
                             _t_type =
                                 ({-# LINE 586 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "T_" >|< _lhsInt
                                  {-# LINE 2938 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 587, column 17)
                             _t_params =
                                 ({-# LINE 587 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppSpaced _lhsIparams
                                  {-# LINE 2944 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 588, column 17)
                             _args =
                                 ({-# LINE 588 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIargpats
                                  {-# LINE 2950 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 589, column 17)
                             _semname =
                                 ({-# LINE 589 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "sem_" ++ show _lhsInt ++ "_" ++ show con_
                                  {-# LINE 2956 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 590, column 17)
                             _sem_tp =
                                 ({-# LINE 590 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _quantPP2     >#< _classPP2     >#< ppSpaced _childrenIargtps >#< _t_type     >#< _t_params
                                  {-# LINE 2962 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 591, column 17)
                             _classPP2 =
                                 ({-# LINE 591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppClasses (classCtxsToDocs _lhsIclassCtxs ++ classConstrsToDocs constraints_)
                                  {-# LINE 2968 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 592, column 17)
                             _quantPP2 =
                                 ({-# LINE 592 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  ppQuants (_lhsIparams ++ params_)
                                  {-# LINE 2974 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 593, column 17)
                             _sem_prod =
                                 ({-# LINE 593 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _semInlinePragma
                                  >-< _semname     >#< "::" >#< _sem_tp
                                  >-< _mkSemBody     (_semname     >#< ppSpaced _args     >#< "=" >#< _scc     >#< _t_type    )
                                                     _mbInitializer     _outerlet     ("return" >#< "st" >|< _lhsIinitial)
                                  {-# LINE 2983 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 597, column 17)
                             _mkSemBody =
                                 ({-# LINE 597 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                  {-# LINE 2999 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 609, column 17)
                             _mbInitializer =
                                 ({-# LINE 609 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  if parallelInvoke _lhsIoptions
                                  then (Nothing :: Maybe PP_Doc)
                                  else Nothing
                                  {-# LINE 3007 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 615, column 17)
                             _scc =
                                 ({-# LINE 615 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  if genCostCentres _lhsIoptions
                                  then ppCostCentre _semname
                                  else empty
                                  {-# LINE 3015 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 618, column 17)
                             _semInlinePragma =
                                 ({-# LINE 618 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  if noInlinePragmas _lhsIoptions
                                  then empty
                                  else ppNoInline _semname
                                  {-# LINE 3023 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 621, column 17)
                             _outerlet =
                                 ({-# LINE 621 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  vlist _statefns     >-< _rulesIsem_rules
                                  {-# LINE 3029 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 622, column 17)
                             _statefns =
                                 ({-# LINE 622 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  map _genstfn     $ Set.toList _lhsIallstates
                                  {-# LINE 3035 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 623, column 17)
                             _genstfn =
                                 ({-# LINE 623 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                  {-# LINE 3073 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 665, column 17)
                             _stargs =
                                 ({-# LINE 665 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                  {-# LINE 3093 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 681, column 17)
                             _stks =
                                 ({-# LINE 681 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> if null (_stvisits     st)
                                         then empty
                                         else ( if not (noInlinePragmas _lhsIoptions) && helpInlining _lhsIoptions
                                                then ppNoInline ("k" >|< st)
                                                else empty
                                              )
                                              >-< "k" >|< st >#< "::" >#< "K_" >|< _lhsInt >|< "_s" >|< st >#< "t" >#< "->" >#< "t"
                                              >-< vlist (map (\(v,f,t) -> "k" >|< st >#< "K_" >|< _lhsInt >|< "_v" >|< v >#< "="
                                                                     >#< "v" >|< v) $ _stvisits     st)
                                  {-# LINE 3107 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 690, column 17)
                             _stvisits =
                                 ({-# LINE 690 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st -> filter (\(v,f,t) -> f == st) _visitsIallvisits
                                  {-# LINE 3113 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 691, column 17)
                             _stvs =
                                 ({-# LINE 691 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \st inlinePragma -> vlist [ppf inlinePragma | (f,ppf) <- _visitsIsem_visit, f == st]
                                  {-# LINE 3119 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 692, column 17)
                             _visitsOmrules =
                                 ({-# LINE 692 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesImrules
                                  {-# LINE 3125 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 896, column 17)
                             _visitsOchildintros =
                                 ({-# LINE 896 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIchildintros
                                  {-# LINE 3131 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1246, column 32)
                             _rulesOusageInfo =
                                 ({-# LINE 1246 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIruleUsage
                                  {-# LINE 3137 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1261, column 3)
                             _rulesOruleKinds =
                                 ({-# LINE 1261 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIruleKinds
                                  {-# LINE 3143 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1290, column 3)
                             _visitsOallintramap =
                                 ({-# LINE 1290 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIintramap
                                  {-# LINE 3149 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1291, column 3)
                             _visitsOterminaldefs =
                                 ({-# LINE 1291 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childrenIterminaldefs
                                  {-# LINE 3155 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1315, column 17)
                             _visitsOruledefs =
                                 ({-# LINE 1315 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesIruledefs
                                  {-# LINE 3161 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1316, column 17)
                             _visitsOruleuses =
                                 ({-# LINE 1316 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesIruleuses
                                  {-# LINE 3167 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1370, column 3)
                             _lazyIntras =
                                 ({-# LINE 1370 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIlazyIntras
                                  {-# LINE 3173 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1467, column 17)
                             _lhsOimports =
                                 ({-# LINE 1467 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  [pp $ "import " ++ _moduleName    ]
                                  {-# LINE 3179 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1468, column 17)
                             _moduleName =
                                 ({-# LINE 1468 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainName ++ _suffix
                                  {-# LINE 3185 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1469, column 17)
                             _suffix =
                                 ({-# LINE 1469 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  "_" ++ show _lhsInt ++ "_" ++ show con_
                                  {-# LINE 3191 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1470, column 17)
                             _outputfile =
                                 ({-# LINE 1470 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ _suffix    )
                                  {-# LINE 3197 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1471, column 17)
                             _ppMonadImports =
                                 ({-# LINE 1471 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  if parallelInvoke _lhsIoptions
                                  then pp "import qualified System.IO.Unsafe(unsafePerformIO)"
                                       >-< pp "import System.IO(IO)"
                                       >-< pp "import Control.Concurrent(newEmptyMVar,forkIO,putMVar,takeMVar)"
                                  else pp "import Control.Monad.Identity"
                                  {-# LINE 3207 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1476, column 17)
                             _lhsOgenProdIO =
                                 ({-# LINE 1476 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                  {-# LINE 3225 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1517, column 37)
                             _addbang =
                                 ({-# LINE 1517 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                  {-# LINE 3231 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1567, column 3)
                             _childTypes =
                                 ({-# LINE 1567 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                                  {-# LINE 3237 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- "src-ag/ExecutionPlan2Hs.ag"(line 1584, column 3)
                             _localAttrTypes =
                                 ({-# LINE 1584 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                                  {-# LINE 3243 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                             _lhsOchildvisit =
                                 ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIchildvisit
                                  {-# LINE 3249 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                             _lhsOerrors =
                                 ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _rulesIerrors Seq.>< _visitsIerrors
                                  {-# LINE 3255 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                             _lhsOfromToStates =
                                 ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIfromToStates
                                  {-# LINE 3261 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 394, column 59)
                             _lhsOt_visits =
                                 ({-# LINE 394 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIt_visits
                                  {-# LINE 3267 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                             _lhsOvisitKinds =
                                 ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIvisitKinds
                                  {-# LINE 3273 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                             _lhsOvisitdefs =
                                 ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIvisitdefs
                                  {-# LINE 3279 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                             _lhsOvisituses =
                                 ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIvisituses
                                  {-# LINE 3285 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (up)
                             _lhsOallvisits =
                                 ({-# LINE 338 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _visitsIallvisits
                                  {-# LINE 3291 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (from local)
                             _lhsOsem_prod =
                                 ({-# LINE 553 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _sem_prod
                                  {-# LINE 3297 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOallInhmap =
                                 ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallInhmap
                                  {-# LINE 3303 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOallSynmap =
                                 ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallSynmap
                                  {-# LINE 3309 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOchildTypes =
                                 ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childTypes
                                  {-# LINE 3315 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOimportBlocks =
                                 ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIimportBlocks
                                  {-# LINE 3321 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOinhmap =
                                 ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIinhmap
                                  {-# LINE 3327 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOlazyIntras =
                                 ({-# LINE 1356 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lazyIntras
                                  {-# LINE 3333 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (from local)
                             _rulesOlocalAttrTypes =
                                 ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _localAttrTypes
                                  {-# LINE 3339 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOmainFile =
                                 ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainFile
                                  {-# LINE 3345 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOmainName =
                                 ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainName
                                  {-# LINE 3351 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOmoduleHeader =
                                 ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImoduleHeader
                                  {-# LINE 3357 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOnt =
                                 ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInt
                                  {-# LINE 3363 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOoptions =
                                 ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 3369 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOpragmaBlocks =
                                 ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIpragmaBlocks
                                  {-# LINE 3375 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOsynmap =
                                 ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIsynmap
                                  {-# LINE 3381 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _rulesOtextBlocks =
                                 ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsItextBlocks
                                  {-# LINE 3387 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOallInitStates =
                                 ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallInitStates
                                  {-# LINE 3393 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOimportBlocks =
                                 ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIimportBlocks
                                  {-# LINE 3399 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOmainFile =
                                 ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainFile
                                  {-# LINE 3405 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOmainName =
                                 ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImainName
                                  {-# LINE 3411 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOmoduleHeader =
                                 ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsImoduleHeader
                                  {-# LINE 3417 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOnt =
                                 ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInt
                                  {-# LINE 3423 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOoptions =
                                 ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 3429 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOpragmaBlocks =
                                 ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIpragmaBlocks
                                  {-# LINE 3435 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _childrenOtextBlocks =
                                 ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsItextBlocks
                                  {-# LINE 3441 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallFromToStates =
                                 ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallFromToStates
                                  {-# LINE 3447 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallInhmap =
                                 ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallInhmap
                                  {-# LINE 3453 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallInitStates =
                                 ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallInitStates
                                  {-# LINE 3459 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallSynmap =
                                 ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallSynmap
                                  {-# LINE 3465 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallVisitKinds =
                                 ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallVisitKinds
                                  {-# LINE 3471 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallchildvisit =
                                 ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIallchildvisit
                                  {-# LINE 3477 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisitdefs =
                                 ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIavisitdefs
                                  {-# LINE 3483 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOavisituses =
                                 ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIavisituses
                                  {-# LINE 3489 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (from local)
                             _visitsOchildTypes =
                                 ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _childTypes
                                  {-# LINE 3495 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOinhmap =
                                 ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIinhmap
                                  {-# LINE 3501 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnextVisits =
                                 ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInextVisits
                                  {-# LINE 3507 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnt =
                                 ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsInt
                                  {-# LINE 3513 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOoptions =
                                 ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 3519 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOparams =
                                 ({-# LINE 73 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIparams
                                  {-# LINE 3525 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOprevVisits =
                                 ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIprevVisits
                                  {-# LINE 3531 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOsynmap =
                                 ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                  _lhsIsynmap
                                  {-# LINE 3537 "dist/build/ExecutionPlan2Hs" #-}
                                  )
                             ( _rulesIerrors,_rulesImrules,_rulesIruledefs,_rulesIruleuses,_rulesIsem_rules) =
                                 rules_ _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOimportBlocks _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOmoduleHeader _rulesOnt _rulesOoptions _rulesOpragmaBlocks _rulesOruleKinds _rulesOsynmap _rulesOtextBlocks _rulesOusageInfo
                             ( _childrenIargnamesw,_childrenIargpats,_childrenIargtps,_childrenIchildTypes,_childrenIchildintros,_childrenIdatatype,_childrenIterminaldefs) =
                                 children_ _childrenOallInitStates _childrenOcon _childrenOimportBlocks _childrenOmainFile _childrenOmainName _childrenOmoduleHeader _childrenOnt _childrenOoptions _childrenOpragmaBlocks _childrenOtextBlocks
                             ( _visitsIallvisits,_visitsIchildvisit,_visitsIerrors,_visitsIfromToStates,_visitsIintramap,_visitsIlazyIntras,_visitsIruleKinds,_visitsIruleUsage,_visitsIsem_visit,_visitsIt_visits,_visitsIvisitKinds,_visitsIvisitdefs,_visitsIvisituses) =
                                 visits_ _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallchildvisit _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOchildintros _visitsOcon _visitsOinhmap _visitsOmrules _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs
                             ___node =
                                 (Syn_EProduction _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                         in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- EProductions ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         allstates            : Set StateIdentifier
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         classCtxs            : ClassContext
         importBlocks         : PP_Doc
         inhmap               : Attributes
         initial              : StateIdentifier
         localAttrTypes       : Map ConstructorIdent (Map Identifier Type)
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nextVisits           : Map StateIdentifier StateCtx
         nt                   : NontermIdent
         ntType               : Type
         options              : Options
         params               : [Identifier]
         pragmaBlocks         : String
         prevVisits           : Map StateIdentifier StateCtx
         rename               : Bool
         synmap               : Attributes
         textBlocks           : PP_Doc
      synthesized attributes:
         allvisits            : [VisitStateState]
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         count                : Int
         datatype             : [PP_Doc]
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         genProdIO            : IO ()
         imports              : [PP_Doc]
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
                                         (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                         (Set StateIdentifier) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         (Map VisitIdentifier (Set Identifier)) ->
                                         ClassContext ->
                                         PP_Doc ->
                                         Attributes ->
                                         StateIdentifier ->
                                         (Map ConstructorIdent (Map Identifier Type)) ->
                                         String ->
                                         String ->
                                         (String -> String -> String -> Bool -> String) ->
                                         (Map StateIdentifier StateCtx) ->
                                         NontermIdent ->
                                         Type ->
                                         Options ->
                                         ([Identifier]) ->
                                         String ->
                                         (Map StateIdentifier StateCtx) ->
                                         Bool ->
                                         Attributes ->
                                         PP_Doc ->
                                         ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),Int,([PP_Doc]),(Seq Error),(Map VisitIdentifier (Int,Int)),(IO ()),([PP_Doc]),(Seq PP_Doc),(Seq PP_Doc),PP_Doc,PP_Doc,PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_EProductions = Inh_EProductions {allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_EProductions :: (Map NontermIdent Attributes),allInitStates_Inh_EProductions :: (Map NontermIdent Int),allSynmap_Inh_EProductions :: (Map NontermIdent Attributes),allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),allstates_Inh_EProductions :: (Set StateIdentifier),avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)),classCtxs_Inh_EProductions :: ClassContext,importBlocks_Inh_EProductions :: PP_Doc,inhmap_Inh_EProductions :: Attributes,initial_Inh_EProductions :: StateIdentifier,localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)),mainFile_Inh_EProductions :: String,mainName_Inh_EProductions :: String,moduleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String),nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx),nt_Inh_EProductions :: NontermIdent,ntType_Inh_EProductions :: Type,options_Inh_EProductions :: Options,params_Inh_EProductions :: ([Identifier]),pragmaBlocks_Inh_EProductions :: String,prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx),rename_Inh_EProductions :: Bool,synmap_Inh_EProductions :: Attributes,textBlocks_Inh_EProductions :: PP_Doc}
data Syn_EProductions = Syn_EProductions {allvisits_Syn_EProductions :: ([VisitStateState]),childvisit_Syn_EProductions :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),count_Syn_EProductions :: Int,datatype_Syn_EProductions :: ([PP_Doc]),errors_Syn_EProductions :: (Seq Error),fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)),genProdIO_Syn_EProductions :: (IO ()),imports_Syn_EProductions :: ([PP_Doc]),semFunBndDefs_Syn_EProductions :: (Seq PP_Doc),semFunBndTps_Syn_EProductions :: (Seq PP_Doc),sem_nt_Syn_EProductions :: PP_Doc,sem_prod_Syn_EProductions :: PP_Doc,t_visits_Syn_EProductions :: PP_Doc,visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind),visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier))}
wrap_EProductions :: T_EProductions ->
                     Inh_EProductions ->
                     Syn_EProductions
wrap_EProductions (T_EProductions sem) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
    (let ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses) = sem _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallchildvisit _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
     in  (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses))
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
                       _lhsIclassCtxs
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsInextVisits
                       _lhsInt
                       _lhsIntType
                       _lhsIoptions
                       _lhsIparams
                       _lhsIpragmaBlocks
                       _lhsIprevVisits
                       _lhsIrename
                       _lhsIsynmap
                       _lhsItextBlocks ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOt_visits :: PP_Doc
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _lhsOcount :: Int
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
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
                              _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _hdOallstates :: (Set StateIdentifier)
                              _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _hdOclassCtxs :: ClassContext
                              _hdOimportBlocks :: PP_Doc
                              _hdOinhmap :: Attributes
                              _hdOinitial :: StateIdentifier
                              _hdOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _hdOmainFile :: String
                              _hdOmainName :: String
                              _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _hdOnextVisits :: (Map StateIdentifier StateCtx)
                              _hdOnt :: NontermIdent
                              _hdOntType :: Type
                              _hdOoptions :: Options
                              _hdOparams :: ([Identifier])
                              _hdOpragmaBlocks :: String
                              _hdOprevVisits :: (Map StateIdentifier StateCtx)
                              _hdOrename :: Bool
                              _hdOsynmap :: Attributes
                              _hdOtextBlocks :: PP_Doc
                              _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                              _tlOallInhmap :: (Map NontermIdent Attributes)
                              _tlOallInitStates :: (Map NontermIdent Int)
                              _tlOallSynmap :: (Map NontermIdent Attributes)
                              _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                              _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _tlOallstates :: (Set StateIdentifier)
                              _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                              _tlOclassCtxs :: ClassContext
                              _tlOimportBlocks :: PP_Doc
                              _tlOinhmap :: Attributes
                              _tlOinitial :: StateIdentifier
                              _tlOlocalAttrTypes :: (Map ConstructorIdent (Map Identifier Type))
                              _tlOmainFile :: String
                              _tlOmainName :: String
                              _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                              _tlOnextVisits :: (Map StateIdentifier StateCtx)
                              _tlOnt :: NontermIdent
                              _tlOntType :: Type
                              _tlOoptions :: Options
                              _tlOparams :: ([Identifier])
                              _tlOpragmaBlocks :: String
                              _tlOprevVisits :: (Map StateIdentifier StateCtx)
                              _tlOrename :: Bool
                              _tlOsynmap :: Attributes
                              _tlOtextBlocks :: PP_Doc
                              _hdIallvisits :: ([VisitStateState])
                              _hdIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _hdIcount :: Int
                              _hdIdatatype :: PP_Doc
                              _hdIerrors :: (Seq Error)
                              _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _hdIgenProdIO :: (IO ())
                              _hdIimports :: ([PP_Doc])
                              _hdIsemFunBndDefs :: (Seq PP_Doc)
                              _hdIsemFunBndTps :: (Seq PP_Doc)
                              _hdIsem_nt :: PP_Doc
                              _hdIsem_prod :: PP_Doc
                              _hdIt_visits :: PP_Doc
                              _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                              _tlIallvisits :: ([VisitStateState])
                              _tlIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _tlIcount :: Int
                              _tlIdatatype :: ([PP_Doc])
                              _tlIerrors :: (Seq Error)
                              _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                              _tlIgenProdIO :: (IO ())
                              _tlIimports :: ([PP_Doc])
                              _tlIsemFunBndDefs :: (Seq PP_Doc)
                              _tlIsemFunBndTps :: (Seq PP_Doc)
                              _tlIsem_nt :: PP_Doc
                              _tlIsem_prod :: PP_Doc
                              _tlIt_visits :: PP_Doc
                              _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                              _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 344, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 344 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIallvisits
                                   {-# LINE 3781 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 397, column 10)
                              _lhsOt_visits =
                                  ({-# LINE 397 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIt_visits
                                   {-# LINE 3787 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIchildvisit `Map.union` _tlIchildvisit
                                   {-# LINE 3793 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 288, column 43)
                              _lhsOcount =
                                  ({-# LINE 288 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIcount + _tlIcount
                                   {-# LINE 3799 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 179, column 34)
                              _lhsOdatatype =
                                  ({-# LINE 179 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIdatatype : _tlIdatatype
                                   {-# LINE 3805 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIerrors Seq.>< _tlIerrors
                                   {-# LINE 3811 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIfromToStates `mappend` _tlIfromToStates
                                   {-# LINE 3817 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1464, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 1464 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIgenProdIO >> _tlIgenProdIO
                                   {-# LINE 3823 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1463, column 47)
                              _lhsOimports =
                                  ({-# LINE 1463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIimports ++ _tlIimports
                                   {-# LINE 3829 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
                                   {-# LINE 3835 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
                                   {-# LINE 3841 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 292, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 292 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsem_nt >-< _tlIsem_nt
                                   {-# LINE 3847 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 554, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 554 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIsem_prod >-< _tlIsem_prod
                                   {-# LINE 3853 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIvisitKinds `mappend` _tlIvisitKinds
                                   {-# LINE 3859 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                                   {-# LINE 3865 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _hdIvisituses `uwSetUnion` _tlIvisituses
                                   {-# LINE 3871 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallFromToStates =
                                  ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 3877 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallInhmap =
                                  ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallInhmap
                                   {-# LINE 3883 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallInitStates =
                                  ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 3889 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallSynmap =
                                  ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallSynmap
                                   {-# LINE 3895 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallVisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 3901 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallchildvisit =
                                  ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 3907 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallstates =
                                  ({-# LINE 557 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 3913 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisitdefs =
                                  ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 3919 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOavisituses =
                                  ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 3925 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOclassCtxs =
                                  ({-# LINE 77 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIclassCtxs
                                   {-# LINE 3931 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOimportBlocks =
                                  ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 3937 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhmap =
                                  ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 3943 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinitial =
                                  ({-# LINE 556 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 3949 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOlocalAttrTypes =
                                  ({-# LINE 1577 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIlocalAttrTypes
                                   {-# LINE 3955 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainFile =
                                  ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 3961 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmainName =
                                  ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 3967 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmoduleHeader =
                                  ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 3973 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnextVisits =
                                  ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInextVisits
                                   {-# LINE 3979 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnt =
                                  ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInt
                                   {-# LINE 3985 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOntType =
                                  ({-# LINE 1620 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIntType
                                   {-# LINE 3991 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 3997 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOparams =
                                  ({-# LINE 73 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIparams
                                   {-# LINE 4003 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOpragmaBlocks =
                                  ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 4009 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOprevVisits =
                                  ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIprevVisits
                                   {-# LINE 4015 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOrename =
                                  ({-# LINE 52 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIrename
                                   {-# LINE 4021 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynmap =
                                  ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 4027 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOtextBlocks =
                                  ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 4033 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallFromToStates =
                                  ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallFromToStates
                                   {-# LINE 4039 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallInhmap =
                                  ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallInhmap
                                   {-# LINE 4045 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallInitStates =
                                  ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallInitStates
                                   {-# LINE 4051 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallSynmap =
                                  ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallSynmap
                                   {-# LINE 4057 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallVisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallVisitKinds
                                   {-# LINE 4063 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallchildvisit =
                                  ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallchildvisit
                                   {-# LINE 4069 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallstates =
                                  ({-# LINE 557 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIallstates
                                   {-# LINE 4075 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisitdefs =
                                  ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisitdefs
                                   {-# LINE 4081 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOavisituses =
                                  ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIavisituses
                                   {-# LINE 4087 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOclassCtxs =
                                  ({-# LINE 77 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIclassCtxs
                                   {-# LINE 4093 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOimportBlocks =
                                  ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIimportBlocks
                                   {-# LINE 4099 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhmap =
                                  ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinhmap
                                   {-# LINE 4105 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinitial =
                                  ({-# LINE 556 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIinitial
                                   {-# LINE 4111 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOlocalAttrTypes =
                                  ({-# LINE 1577 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIlocalAttrTypes
                                   {-# LINE 4117 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainFile =
                                  ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainFile
                                   {-# LINE 4123 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmainName =
                                  ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImainName
                                   {-# LINE 4129 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmoduleHeader =
                                  ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsImoduleHeader
                                   {-# LINE 4135 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnextVisits =
                                  ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInextVisits
                                   {-# LINE 4141 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnt =
                                  ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsInt
                                   {-# LINE 4147 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOntType =
                                  ({-# LINE 1620 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIntType
                                   {-# LINE 4153 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 4159 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOparams =
                                  ({-# LINE 73 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIparams
                                   {-# LINE 4165 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOpragmaBlocks =
                                  ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIpragmaBlocks
                                   {-# LINE 4171 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOprevVisits =
                                  ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIprevVisits
                                   {-# LINE 4177 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOrename =
                                  ({-# LINE 52 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIrename
                                   {-# LINE 4183 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynmap =
                                  ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsIsynmap
                                   {-# LINE 4189 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOtextBlocks =
                                  ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   _lhsItextBlocks
                                   {-# LINE 4195 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              ( _hdIallvisits,_hdIchildvisit,_hdIcount,_hdIdatatype,_hdIerrors,_hdIfromToStates,_hdIgenProdIO,_hdIimports,_hdIsemFunBndDefs,_hdIsemFunBndTps,_hdIsem_nt,_hdIsem_prod,_hdIt_visits,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                                  hd_ _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallstates _hdOavisitdefs _hdOavisituses _hdOclassCtxs _hdOimportBlocks _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOpragmaBlocks _hdOprevVisits _hdOrename _hdOsynmap _hdOtextBlocks
                              ( _tlIallvisits,_tlIchildvisit,_tlIcount,_tlIdatatype,_tlIerrors,_tlIfromToStates,_tlIgenProdIO,_tlIimports,_tlIsemFunBndDefs,_tlIsemFunBndTps,_tlIsem_nt,_tlIsem_prod,_tlIt_visits,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                                  tl_ _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallstates _tlOavisitdefs _tlOavisituses _tlOclassCtxs _tlOimportBlocks _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOpragmaBlocks _tlOprevVisits _tlOrename _tlOsynmap _tlOtextBlocks
                              ___node =
                                  (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
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
                       _lhsIclassCtxs
                       _lhsIimportBlocks
                       _lhsIinhmap
                       _lhsIinitial
                       _lhsIlocalAttrTypes
                       _lhsImainFile
                       _lhsImainName
                       _lhsImoduleHeader
                       _lhsInextVisits
                       _lhsInt
                       _lhsIntType
                       _lhsIoptions
                       _lhsIparams
                       _lhsIpragmaBlocks
                       _lhsIprevVisits
                       _lhsIrename
                       _lhsIsynmap
                       _lhsItextBlocks ->
                         (let _lhsOallvisits :: ([VisitStateState])
                              _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                              _lhsOcount :: Int
                              _lhsOdatatype :: ([PP_Doc])
                              _lhsOerrors :: (Seq Error)
                              _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                              _lhsOgenProdIO :: (IO ())
                              _lhsOimports :: ([PP_Doc])
                              _lhsOsemFunBndDefs :: (Seq PP_Doc)
                              _lhsOsemFunBndTps :: (Seq PP_Doc)
                              _lhsOsem_nt :: PP_Doc
                              _lhsOsem_prod :: PP_Doc
                              _lhsOt_visits :: PP_Doc
                              _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                              _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                              _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                              -- "src-ag/ExecutionPlan2Hs.ag"(line 345, column 10)
                              _lhsOallvisits =
                                  ({-# LINE 345 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   error "Every nonterminal should have at least 1 production"
                                   {-# LINE 4253 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                              _lhsOchildvisit =
                                  ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 4259 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 288, column 43)
                              _lhsOcount =
                                  ({-# LINE 288 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   0
                                   {-# LINE 4265 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 179, column 34)
                              _lhsOdatatype =
                                  ({-# LINE 179 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   []
                                   {-# LINE 4271 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                              _lhsOerrors =
                                  ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Seq.empty
                                   {-# LINE 4277 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                              _lhsOfromToStates =
                                  ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   mempty
                                   {-# LINE 4283 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1464, column 49)
                              _lhsOgenProdIO =
                                  ({-# LINE 1464 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   return ()
                                   {-# LINE 4289 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1463, column 47)
                              _lhsOimports =
                                  ({-# LINE 1463 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   []
                                   {-# LINE 4295 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                              _lhsOsemFunBndDefs =
                                  ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Seq.empty
                                   {-# LINE 4301 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 515, column 92)
                              _lhsOsemFunBndTps =
                                  ({-# LINE 515 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Seq.empty
                                   {-# LINE 4307 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 292, column 44)
                              _lhsOsem_nt =
                                  ({-# LINE 292 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 4313 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 554, column 34)
                              _lhsOsem_prod =
                                  ({-# LINE 554 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 4319 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 394, column 59)
                              _lhsOt_visits =
                                  ({-# LINE 394 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   empty
                                   {-# LINE 4325 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                              _lhsOvisitKinds =
                                  ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   mempty
                                   {-# LINE 4331 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                              _lhsOvisitdefs =
                                  ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 4337 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                              _lhsOvisituses =
                                  ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                                   Map.empty
                                   {-# LINE 4343 "dist/build/ExecutionPlan2Hs" #-}
                                   )
                              ___node =
                                  (Syn_EProductions _lhsOallvisits _lhsOchildvisit _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOgenProdIO _lhsOimports _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOsem_prod _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                          in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOcount,_lhsOdatatype,_lhsOerrors,_lhsOfromToStates,_lhsOgenProdIO,_lhsOimports,_lhsOsemFunBndDefs,_lhsOsemFunBndTps,_lhsOsem_nt,_lhsOsem_prod,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- ERule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         childTypes           : Map Identifier Type
         con                  : ConstructorIdent
         importBlocks         : PP_Doc
         inhmap               : Attributes
         lazyIntras           : Set String
         localAttrTypes       : Map Identifier Type
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         pragmaBlocks         : String
         ruleKinds            : Map Identifier (Set VisitKind)
         synmap               : Attributes
         textBlocks           : PP_Doc
         usageInfo            : Map Identifier Int
      synthesized attributes:
         errors               : Seq Error
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
            local rulePragma  : _
            local scc         : _
            local pragma      : _
            local endpragma   : _
            local genpragma   : _
            local haspos      : _
            local lambda      : _
            local argPats     : _
            local argExprs    : _
            local stepcode    : _
            local used        : _
            local kinds       : _
            local anyLazyKind : _
            local addbang     : _
            local addbang1    : _
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
                           PP_Doc ->
                           Attributes ->
                           (Set String) ->
                           (Map Identifier Type) ->
                           String ->
                           String ->
                           (String -> String -> String -> Bool -> String) ->
                           NontermIdent ->
                           Options ->
                           String ->
                           (Map Identifier (Set VisitKind)) ->
                           Attributes ->
                           PP_Doc ->
                           (Map Identifier Int) ->
                           ( (Seq Error),(Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),(Map Identifier (Set String)),(Map Identifier (Map String (Maybe NonLocalAttr))),PP_Doc))
data Inh_ERule = Inh_ERule {allInhmap_Inh_ERule :: (Map NontermIdent Attributes),allSynmap_Inh_ERule :: (Map NontermIdent Attributes),childTypes_Inh_ERule :: (Map Identifier Type),con_Inh_ERule :: ConstructorIdent,importBlocks_Inh_ERule :: PP_Doc,inhmap_Inh_ERule :: Attributes,lazyIntras_Inh_ERule :: (Set String),localAttrTypes_Inh_ERule :: (Map Identifier Type),mainFile_Inh_ERule :: String,mainName_Inh_ERule :: String,moduleHeader_Inh_ERule :: (String -> String -> String -> Bool -> String),nt_Inh_ERule :: NontermIdent,options_Inh_ERule :: Options,pragmaBlocks_Inh_ERule :: String,ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)),synmap_Inh_ERule :: Attributes,textBlocks_Inh_ERule :: PP_Doc,usageInfo_Inh_ERule :: (Map Identifier Int)}
data Syn_ERule = Syn_ERule {errors_Syn_ERule :: (Seq Error),mrules_Syn_ERule :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),ruledefs_Syn_ERule :: (Map Identifier (Set String)),ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))),sem_rules_Syn_ERule :: PP_Doc}
wrap_ERule :: T_ERule ->
              Inh_ERule ->
              Syn_ERule
wrap_ERule (T_ERule sem) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
    (let ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
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
                _lhsIimportBlocks
                _lhsIinhmap
                _lhsIlazyIntras
                _lhsIlocalAttrTypes
                _lhsImainFile
                _lhsImainName
                _lhsImoduleHeader
                _lhsInt
                _lhsIoptions
                _lhsIpragmaBlocks
                _lhsIruleKinds
                _lhsIsynmap
                _lhsItextBlocks
                _lhsIusageInfo ->
                  (let _lhsOsem_rules :: PP_Doc
                       _lhsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                       _patternIisUnderscore :: Bool
                       _patternIsem_lhs :: ( PP_Doc )
                       _rhsIattrs :: (Map String (Maybe NonLocalAttr))
                       _rhsIpos :: Pos
                       _rhsIsemfunc :: PP_Doc
                       _rhsItks :: ([HsToken])
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 965, column 6)
                       _lhsOsem_rules =
                           ({-# LINE 965 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if _used     == 0
                            then empty
                            else _rulePragma     >-< _rulecode
                            {-# LINE 4492 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 968, column 6)
                       _rulecode =
                           ({-# LINE 968 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 4507 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 980, column 7)
                       _rulePragma =
                           ({-# LINE 980 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 4534 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1002, column 7)
                       _scc =
                           ({-# LINE 1002 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if genCostCentres _lhsIoptions && explicit_ && pure_ && not (noPerRuleCostCentres _lhsIoptions)
                            then ppCostCentre (name_ >|< "_" >|< line _rhsIpos >|< "_" >|< _lhsInt >|< "_" >|< _lhsIcon)
                            else empty
                            {-# LINE 4542 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1005, column 7)
                       _pragma =
                           ({-# LINE 1005 "src-ag/ExecutionPlan2Hs.ag" #-}
                            "{-# LINE" >#< show (line _rhsIpos) >#< show (file _rhsIpos) >#< "#-}"
                            {-# LINE 4548 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1006, column 7)
                       _endpragma =
                           ({-# LINE 1006 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppWithLineNr (\ln -> "{-# LINE " ++ show (ln+1) ++ " " ++ show _lhsImainFile ++ "#-}")
                            {-# LINE 4554 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1007, column 7)
                       _genpragma =
                           ({-# LINE 1007 "src-ag/ExecutionPlan2Hs.ag" #-}
                            genLinePragmas _lhsIoptions && explicit_ && _haspos
                            {-# LINE 4560 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1008, column 7)
                       _haspos =
                           ({-# LINE 1008 "src-ag/ExecutionPlan2Hs.ag" #-}
                            line _rhsIpos > 0 && column _rhsIpos >= 0 && not (null (file _rhsIpos))
                            {-# LINE 4566 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1017, column 7)
                       _lambda =
                           ({-# LINE 1017 "src-ag/ExecutionPlan2Hs.ag" #-}
                            name_ >#< "=" >#< "\\" >#< _argPats     >#< dummyPat _lhsIoptions (Map.null _rhsIattrs) >#< "->"
                            {-# LINE 4572 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1019, column 7)
                       _argPats =
                           ({-# LINE 1019 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 4591 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1033, column 7)
                       _argExprs =
                           ({-# LINE 1033 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ Map.keys _rhsIattrs
                            {-# LINE 4597 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1034, column 7)
                       _stepcode =
                           ({-# LINE 1034 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \kind fmtMode -> if kind `compatibleRule` pure_
                                             then Right $ let oper | pure_     = "="
                                                                   | otherwise = "<-"
                                                              decl = _patternIsem_lhs >#< oper >#< name_ >#< _argExprs     >#< dummyArg _lhsIoptions (Map.null _rhsIattrs)
                                                              tp   = if pure_ && not (noPerRuleTypeSigs _lhsIoptions)
                                                                     then _patternIattrTypes
                                                                     else empty
                                                          in fmtDecl pure_ fmtMode (tp >-< decl)
                                             else Left $ IncompatibleRuleKind name_ kind
                            {-# LINE 4611 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1044, column 7)
                       _lhsOmrules =
                           ({-# LINE 1044 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ _stepcode
                            {-# LINE 4617 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1248, column 32)
                       _used =
                           ({-# LINE 1248 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.findWithDefault 0 name_ _lhsIusageInfo
                            {-# LINE 4623 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1264, column 3)
                       _kinds =
                           ({-# LINE 1264 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.findWithDefault Set.empty name_ _lhsIruleKinds
                            {-# LINE 4629 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1265, column 3)
                       _anyLazyKind =
                           ({-# LINE 1265 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Set.fold (\k r -> isLazyKind k || r) False _kinds
                            {-# LINE 4635 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1311, column 11)
                       _lhsOruledefs =
                           ({-# LINE 1311 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ _patternIattrs
                            {-# LINE 4641 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1312, column 11)
                       _lhsOruleuses =
                           ({-# LINE 1312 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton name_ _rhsIattrs
                            {-# LINE 4647 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1514, column 37)
                       _addbang =
                           ({-# LINE 1514 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \x -> if bangpats _lhsIoptions then "!" >|< x else x
                            {-# LINE 4653 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1525, column 37)
                       _addbang1 =
                           ({-# LINE 1525 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if _anyLazyKind     then id else _addbang
                            {-# LINE 4659 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1631, column 3)
                       _lhsOerrors =
                           ({-# LINE 1631 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case mbError_ of
                              Just e | _used     > 0 -> Seq.singleton e
                              _                      -> Seq.empty
                            {-# LINE 4667 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOallInhmap =
                           ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallInhmap
                            {-# LINE 4673 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOallSynmap =
                           ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallSynmap
                            {-# LINE 4679 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (from local)
                       _patternOanyLazyKind =
                           ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _anyLazyKind
                            {-# LINE 4685 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOinhmap =
                           ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIinhmap
                            {-# LINE 4691 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOlocalAttrTypes =
                           ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIlocalAttrTypes
                            {-# LINE 4697 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOoptions =
                           ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIoptions
                            {-# LINE 4703 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _patternOsynmap =
                           ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIsynmap
                            {-# LINE 4709 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       ( _patternIattrTypes,_patternIattrs,_patternIcopy,_patternIisUnderscore,_patternIsem_lhs) =
                           pattern_ _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap
                       ( _rhsIattrs,_rhsIpos,_rhsIsemfunc,_rhsItks) =
                           rhs_
                       ___node =
                           (Syn_ERule _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules)
                   in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
-- ERules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allInhmap            : Map NontermIdent Attributes
         allSynmap            : Map NontermIdent Attributes
         childTypes           : Map Identifier Type
         con                  : ConstructorIdent
         importBlocks         : PP_Doc
         inhmap               : Attributes
         lazyIntras           : Set String
         localAttrTypes       : Map Identifier Type
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nt                   : NontermIdent
         options              : Options
         pragmaBlocks         : String
         ruleKinds            : Map Identifier (Set VisitKind)
         synmap               : Attributes
         textBlocks           : PP_Doc
         usageInfo            : Map Identifier Int
      synthesized attributes:
         errors               : Seq Error
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
                             PP_Doc ->
                             Attributes ->
                             (Set String) ->
                             (Map Identifier Type) ->
                             String ->
                             String ->
                             (String -> String -> String -> Bool -> String) ->
                             NontermIdent ->
                             Options ->
                             String ->
                             (Map Identifier (Set VisitKind)) ->
                             Attributes ->
                             PP_Doc ->
                             (Map Identifier Int) ->
                             ( (Seq Error),(Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),(Map Identifier (Set String)),(Map Identifier (Map String (Maybe NonLocalAttr))),PP_Doc))
data Inh_ERules = Inh_ERules {allInhmap_Inh_ERules :: (Map NontermIdent Attributes),allSynmap_Inh_ERules :: (Map NontermIdent Attributes),childTypes_Inh_ERules :: (Map Identifier Type),con_Inh_ERules :: ConstructorIdent,importBlocks_Inh_ERules :: PP_Doc,inhmap_Inh_ERules :: Attributes,lazyIntras_Inh_ERules :: (Set String),localAttrTypes_Inh_ERules :: (Map Identifier Type),mainFile_Inh_ERules :: String,mainName_Inh_ERules :: String,moduleHeader_Inh_ERules :: (String -> String -> String -> Bool -> String),nt_Inh_ERules :: NontermIdent,options_Inh_ERules :: Options,pragmaBlocks_Inh_ERules :: String,ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)),synmap_Inh_ERules :: Attributes,textBlocks_Inh_ERules :: PP_Doc,usageInfo_Inh_ERules :: (Map Identifier Int)}
data Syn_ERules = Syn_ERules {errors_Syn_ERules :: (Seq Error),mrules_Syn_ERules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),ruledefs_Syn_ERules :: (Map Identifier (Set String)),ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))),sem_rules_Syn_ERules :: PP_Doc}
wrap_ERules :: T_ERules ->
               Inh_ERules ->
               Syn_ERules
wrap_ERules (T_ERules sem) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
    (let ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules) = sem _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
     in  (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules))
sem_ERules_Cons :: T_ERule ->
                   T_ERules ->
                   T_ERules
sem_ERules_Cons (T_ERule hd_) (T_ERules tl_) =
    (T_ERules (\ _lhsIallInhmap
                 _lhsIallSynmap
                 _lhsIchildTypes
                 _lhsIcon
                 _lhsIimportBlocks
                 _lhsIinhmap
                 _lhsIlazyIntras
                 _lhsIlocalAttrTypes
                 _lhsImainFile
                 _lhsImainName
                 _lhsImoduleHeader
                 _lhsInt
                 _lhsIoptions
                 _lhsIpragmaBlocks
                 _lhsIruleKinds
                 _lhsIsynmap
                 _lhsItextBlocks
                 _lhsIusageInfo ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                        _lhsOruledefs :: (Map Identifier (Set String))
                        _lhsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _lhsOsem_rules :: PP_Doc
                        _hdOallInhmap :: (Map NontermIdent Attributes)
                        _hdOallSynmap :: (Map NontermIdent Attributes)
                        _hdOchildTypes :: (Map Identifier Type)
                        _hdOcon :: ConstructorIdent
                        _hdOimportBlocks :: PP_Doc
                        _hdOinhmap :: Attributes
                        _hdOlazyIntras :: (Set String)
                        _hdOlocalAttrTypes :: (Map Identifier Type)
                        _hdOmainFile :: String
                        _hdOmainName :: String
                        _hdOmoduleHeader :: (String -> String -> String -> Bool -> String)
                        _hdOnt :: NontermIdent
                        _hdOoptions :: Options
                        _hdOpragmaBlocks :: String
                        _hdOruleKinds :: (Map Identifier (Set VisitKind))
                        _hdOsynmap :: Attributes
                        _hdOtextBlocks :: PP_Doc
                        _hdOusageInfo :: (Map Identifier Int)
                        _tlOallInhmap :: (Map NontermIdent Attributes)
                        _tlOallSynmap :: (Map NontermIdent Attributes)
                        _tlOchildTypes :: (Map Identifier Type)
                        _tlOcon :: ConstructorIdent
                        _tlOimportBlocks :: PP_Doc
                        _tlOinhmap :: Attributes
                        _tlOlazyIntras :: (Set String)
                        _tlOlocalAttrTypes :: (Map Identifier Type)
                        _tlOmainFile :: String
                        _tlOmainName :: String
                        _tlOmoduleHeader :: (String -> String -> String -> Bool -> String)
                        _tlOnt :: NontermIdent
                        _tlOoptions :: Options
                        _tlOpragmaBlocks :: String
                        _tlOruleKinds :: (Map Identifier (Set VisitKind))
                        _tlOsynmap :: Attributes
                        _tlOtextBlocks :: PP_Doc
                        _tlOusageInfo :: (Map Identifier Int)
                        _hdIerrors :: (Seq Error)
                        _hdImrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                        _hdIruledefs :: (Map Identifier (Set String))
                        _hdIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _hdIsem_rules :: PP_Doc
                        _tlIerrors :: (Seq Error)
                        _tlImrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                        _tlIruledefs :: (Map Identifier (Set String))
                        _tlIruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _tlIsem_rules :: PP_Doc
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                        _lhsOerrors =
                            ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIerrors Seq.>< _tlIerrors
                             {-# LINE 4862 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 962, column 32)
                        _lhsOmrules =
                            ({-# LINE 962 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdImrules `Map.union` _tlImrules
                             {-# LINE 4868 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1304, column 34)
                        _lhsOruledefs =
                            ({-# LINE 1304 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruledefs `uwSetUnion` _tlIruledefs
                             {-# LINE 4874 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1305, column 34)
                        _lhsOruleuses =
                            ({-# LINE 1305 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruleuses `uwMapUnion` _tlIruleuses
                             {-# LINE 4880 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 961, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 961 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIsem_rules >-< _tlIsem_rules
                             {-# LINE 4886 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInhmap =
                            ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 4892 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallSynmap =
                            ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 4898 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildTypes =
                            ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 4904 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOcon =
                            ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIcon
                             {-# LINE 4910 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOimportBlocks =
                            ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIimportBlocks
                             {-# LINE 4916 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOinhmap =
                            ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 4922 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOlazyIntras =
                            ({-# LINE 1356 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIlazyIntras
                             {-# LINE 4928 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOlocalAttrTypes =
                            ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIlocalAttrTypes
                             {-# LINE 4934 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmainFile =
                            ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImainFile
                             {-# LINE 4940 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmainName =
                            ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImainName
                             {-# LINE 4946 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmoduleHeader =
                            ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImoduleHeader
                             {-# LINE 4952 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnt =
                            ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 4958 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 4964 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOpragmaBlocks =
                            ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIpragmaBlocks
                             {-# LINE 4970 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruleKinds =
                            ({-# LINE 1259 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleKinds
                             {-# LINE 4976 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOsynmap =
                            ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 4982 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOtextBlocks =
                            ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsItextBlocks
                             {-# LINE 4988 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOusageInfo =
                            ({-# LINE 1244 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIusageInfo
                             {-# LINE 4994 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInhmap =
                            ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 5000 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallSynmap =
                            ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 5006 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildTypes =
                            ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 5012 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOcon =
                            ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIcon
                             {-# LINE 5018 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOimportBlocks =
                            ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIimportBlocks
                             {-# LINE 5024 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOinhmap =
                            ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 5030 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOlazyIntras =
                            ({-# LINE 1356 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIlazyIntras
                             {-# LINE 5036 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOlocalAttrTypes =
                            ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIlocalAttrTypes
                             {-# LINE 5042 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmainFile =
                            ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImainFile
                             {-# LINE 5048 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmainName =
                            ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImainName
                             {-# LINE 5054 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmoduleHeader =
                            ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImoduleHeader
                             {-# LINE 5060 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnt =
                            ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 5066 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 5072 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOpragmaBlocks =
                            ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIpragmaBlocks
                             {-# LINE 5078 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruleKinds =
                            ({-# LINE 1259 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleKinds
                             {-# LINE 5084 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOsynmap =
                            ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 5090 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOtextBlocks =
                            ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsItextBlocks
                             {-# LINE 5096 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOusageInfo =
                            ({-# LINE 1244 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIusageInfo
                             {-# LINE 5102 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ( _hdIerrors,_hdImrules,_hdIruledefs,_hdIruleuses,_hdIsem_rules) =
                            hd_ _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOimportBlocks _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOruleKinds _hdOsynmap _hdOtextBlocks _hdOusageInfo
                        ( _tlIerrors,_tlImrules,_tlIruledefs,_tlIruleuses,_tlIsem_rules) =
                            tl_ _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOimportBlocks _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOruleKinds _tlOsynmap _tlOtextBlocks _tlOusageInfo
                        ___node =
                            (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules)
                    in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
sem_ERules_Nil :: T_ERules
sem_ERules_Nil =
    (T_ERules (\ _lhsIallInhmap
                 _lhsIallSynmap
                 _lhsIchildTypes
                 _lhsIcon
                 _lhsIimportBlocks
                 _lhsIinhmap
                 _lhsIlazyIntras
                 _lhsIlocalAttrTypes
                 _lhsImainFile
                 _lhsImainName
                 _lhsImoduleHeader
                 _lhsInt
                 _lhsIoptions
                 _lhsIpragmaBlocks
                 _lhsIruleKinds
                 _lhsIsynmap
                 _lhsItextBlocks
                 _lhsIusageInfo ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                        _lhsOruledefs :: (Map Identifier (Set String))
                        _lhsOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                        _lhsOsem_rules :: PP_Doc
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                        _lhsOerrors =
                            ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Seq.empty
                             {-# LINE 5140 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 962, column 32)
                        _lhsOmrules =
                            ({-# LINE 962 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 5146 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1304, column 34)
                        _lhsOruledefs =
                            ({-# LINE 1304 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 5152 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1305, column 34)
                        _lhsOruleuses =
                            ({-# LINE 1305 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 5158 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 961, column 35)
                        _lhsOsem_rules =
                            ({-# LINE 961 "src-ag/ExecutionPlan2Hs.ag" #-}
                             empty
                             {-# LINE 5164 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ___node =
                            (Syn_ERules _lhsOerrors _lhsOmrules _lhsOruledefs _lhsOruleuses _lhsOsem_rules)
                    in  ( _lhsOerrors,_lhsOmrules,_lhsOruledefs,_lhsOruleuses,_lhsOsem_rules))))
-- ExecutionPlan -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         inhmap               : Map NontermIdent Attributes
         localAttrTypes       : Map NontermIdent (Map ConstructorIdent (Map Identifier Type))
         mainBlocksDoc        : PP_Doc
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         pragmaBlocks         : String
         synmap               : Map NontermIdent Attributes
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         errors               : Seq Error
         genIO                : IO ()
         output               : PP_Doc
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
            local mainModuleFile : _
            local ppMonadImports : _
            local genMainModule : _
            local commonFile  : _
            local genCommonModule : _
-}
-- cata
sem_ExecutionPlan :: ExecutionPlan ->
                     T_ExecutionPlan
sem_ExecutionPlan (ExecutionPlan _nonts _typeSyns _wrappers _derivings) =
    (sem_ExecutionPlan_ExecutionPlan (sem_ENonterminals _nonts) _typeSyns _wrappers _derivings)
-- semantic domain
newtype T_ExecutionPlan = T_ExecutionPlan (PP_Doc ->
                                           (Map NontermIdent Attributes) ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
                                           PP_Doc ->
                                           String ->
                                           String ->
                                           (String -> String -> String -> Bool -> String) ->
                                           Options ->
                                           String ->
                                           (Map NontermIdent Attributes) ->
                                           (Map BlockInfo PP_Doc) ->
                                           PP_Doc ->
                                           ( (Seq Error),(IO ()),PP_Doc))
data Inh_ExecutionPlan = Inh_ExecutionPlan {importBlocks_Inh_ExecutionPlan :: PP_Doc,inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes),localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))),mainBlocksDoc_Inh_ExecutionPlan :: PP_Doc,mainFile_Inh_ExecutionPlan :: String,mainName_Inh_ExecutionPlan :: String,moduleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String),options_Inh_ExecutionPlan :: Options,pragmaBlocks_Inh_ExecutionPlan :: String,synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes),textBlockMap_Inh_ExecutionPlan :: (Map BlockInfo PP_Doc),textBlocks_Inh_ExecutionPlan :: PP_Doc}
data Syn_ExecutionPlan = Syn_ExecutionPlan {errors_Syn_ExecutionPlan :: (Seq Error),genIO_Syn_ExecutionPlan :: (IO ()),output_Syn_ExecutionPlan :: PP_Doc}
wrap_ExecutionPlan :: T_ExecutionPlan ->
                      Inh_ExecutionPlan ->
                      Syn_ExecutionPlan
wrap_ExecutionPlan (T_ExecutionPlan sem) (Inh_ExecutionPlan _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks) =
    (let ( _lhsOerrors,_lhsOgenIO,_lhsOoutput) = sem _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainBlocksDoc _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlockMap _lhsItextBlocks
     in  (Syn_ExecutionPlan _lhsOerrors _lhsOgenIO _lhsOoutput))
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals ->
                                   TypeSyns ->
                                   (Set NontermIdent) ->
                                   Derivings ->
                                   T_ExecutionPlan
sem_ExecutionPlan_ExecutionPlan (T_ENonterminals nonts_) typeSyns_ wrappers_ derivings_ =
    (T_ExecutionPlan (\ _lhsIimportBlocks
                        _lhsIinhmap
                        _lhsIlocalAttrTypes
                        _lhsImainBlocksDoc
                        _lhsImainFile
                        _lhsImainName
                        _lhsImoduleHeader
                        _lhsIoptions
                        _lhsIpragmaBlocks
                        _lhsIsynmap
                        _lhsItextBlockMap
                        _lhsItextBlocks ->
                          (let _lhsOoutput :: PP_Doc
                               _nontsOwrappers :: (Set NontermIdent)
                               _nontsOtypeSyns :: TypeSyns
                               _nontsOderivings :: Derivings
                               _nontsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _nontsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _nontsOavisituses :: (Map VisitIdentifier (Set Identifier))
                               _lhsOgenIO :: (IO ())
                               _nontsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                               _nontsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                               _nontsOallInitStates :: (Map NontermIdent Int)
                               _lhsOerrors :: (Seq Error)
                               _nontsOimportBlocks :: PP_Doc
                               _nontsOinhmap :: (Map NontermIdent Attributes)
                               _nontsOlocalAttrTypes :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type)))
                               _nontsOmainFile :: String
                               _nontsOmainName :: String
                               _nontsOmoduleHeader :: (String -> String -> String -> Bool -> String)
                               _nontsOoptions :: Options
                               _nontsOpragmaBlocks :: String
                               _nontsOsynmap :: (Map NontermIdent Attributes)
                               _nontsOtextBlocks :: PP_Doc
                               _nontsIappendCommon :: ([PP_Doc])
                               _nontsIappendMain :: ([PP_Doc])
                               _nontsIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                               _nontsIerrors :: (Seq Error)
                               _nontsIfromToStates :: (Map VisitIdentifier (Int,Int))
                               _nontsIgenProdIO :: (IO ())
                               _nontsIimports :: ([PP_Doc])
                               _nontsIinitStates :: (Map NontermIdent Int)
                               _nontsIoutput :: PP_Doc
                               _nontsIsemFunBndDefs :: (Seq PP_Doc)
                               _nontsIsemFunBndTps :: (Seq PP_Doc)
                               _nontsIvisitKinds :: (Map VisitIdentifier VisitKind)
                               _nontsIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                               _nontsIvisituses :: (Map VisitIdentifier (Set Identifier))
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 87, column 19)
                               _lhsOoutput =
                                   ({-# LINE 87 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIoutput >-< _commonExtra     >-< _wrappersExtra
                                    {-# LINE 5291 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 93, column 19)
                               _nontsOwrappers =
                                   ({-# LINE 93 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    wrappers_
                                    {-# LINE 5297 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 132, column 19)
                               _nontsOtypeSyns =
                                   ({-# LINE 132 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    typeSyns_
                                    {-# LINE 5303 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 133, column 19)
                               _nontsOderivings =
                                   ({-# LINE 133 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    derivings_
                                    {-# LINE 5309 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 532, column 3)
                               _wrappersExtra =
                                   ({-# LINE 532 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                                    then _lateSemBndDef
                                    else empty
                                    {-# LINE 5317 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 535, column 3)
                               _commonExtra =
                                   ({-# LINE 535 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    if kennedyWarren _lhsIoptions && lateHigherOrderBinding _lhsIoptions
                                    then _lateSemBndTp
                                    else empty
                                    {-# LINE 5325 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 538, column 3)
                               _lateSemBndTp =
                                   ({-# LINE 538 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    "data" >#< lateBindingTypeNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                                     >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndTps)
                                    {-# LINE 5332 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 540, column 3)
                               _lateSemBndDef =
                                   ({-# LINE 540 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    ( if noInlinePragmas _lhsIoptions
                                      then empty
                                      else if helpInlining _lhsIoptions && Set.size wrappers_ == 1
                                           then ppInline $ lateBindingFieldNm _lhsImainName
                                           else ppNoInline $ lateBindingFieldNm _lhsImainName
                                    )
                                    >-< lateBindingFieldNm _lhsImainName >#< "::" >#< lateBindingTypeNm _lhsImainName
                                    >-< lateBindingFieldNm _lhsImainName >#< "=" >#< lateBindingTypeNm _lhsImainName
                                    >-< (indent 2 $ pp_block "{" "}" "," $ toList _nontsIsemFunBndDefs )
                                    {-# LINE 5346 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1192, column 19)
                               _nontsOallchildvisit =
                                   ({-# LINE 1192 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIchildvisit
                                    {-# LINE 5352 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1336, column 19)
                               _nontsOavisitdefs =
                                   ({-# LINE 1336 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIvisitdefs
                                    {-# LINE 5358 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1337, column 19)
                               _nontsOavisituses =
                                   ({-# LINE 1337 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIvisituses
                                    {-# LINE 5364 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1408, column 19)
                               _lhsOgenIO =
                                   ({-# LINE 1408 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    do _genMainModule
                                       _genCommonModule
                                       _nontsIgenProdIO
                                    {-# LINE 5372 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1411, column 19)
                               _mainModuleFile =
                                   ({-# LINE 1411 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 5378 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1412, column 19)
                               _ppMonadImports =
                                   ({-# LINE 1412 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    ( if tupleAsDummyToken _lhsIoptions
                                      then empty
                                      else pp "import GHC.Prim"
                                    )
                                    >-< if parallelInvoke _lhsIoptions
                                        then pp "import qualified System.IO.Unsafe(unsafePerformIO)"
                                             >-< pp "import System.IO(IO)"
                                             >-< pp "import Control.Concurrent(newEmptyMVar,forkIO,putMVar,takeMVar)"
                                        else pp "import Control.Monad.Identity"
                                    {-# LINE 5392 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1421, column 19)
                               _genMainModule =
                                   ({-# LINE 1421 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                    {-# LINE 5409 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1433, column 19)
                               _commonFile =
                                   ({-# LINE 1433 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    replaceBaseName _lhsImainFile (takeBaseName _lhsImainFile ++ "_common")
                                    {-# LINE 5415 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1434, column 19)
                               _genCommonModule =
                                   ({-# LINE 1434 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                                    {-# LINE 5431 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1553, column 3)
                               _nontsOallFromToStates =
                                   ({-# LINE 1553 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIfromToStates
                                    {-# LINE 5437 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1597, column 3)
                               _nontsOallVisitKinds =
                                   ({-# LINE 1597 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIvisitKinds
                                    {-# LINE 5443 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- "src-ag/ExecutionPlan2Hs.ag"(line 1611, column 3)
                               _nontsOallInitStates =
                                   ({-# LINE 1611 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIinitStates
                                    {-# LINE 5449 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                               _lhsOerrors =
                                   ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _nontsIerrors
                                    {-# LINE 5455 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOimportBlocks =
                                   ({-# LINE 34 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIimportBlocks
                                    {-# LINE 5461 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOinhmap =
                                   ({-# LINE 312 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIinhmap
                                    {-# LINE 5467 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOlocalAttrTypes =
                                   ({-# LINE 1576 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIlocalAttrTypes
                                    {-# LINE 5473 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainFile =
                                   ({-# LINE 38 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainFile
                                    {-# LINE 5479 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmainName =
                                   ({-# LINE 39 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImainName
                                    {-# LINE 5485 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOmoduleHeader =
                                   ({-# LINE 37 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsImoduleHeader
                                    {-# LINE 5491 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOoptions =
                                   ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 5497 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOpragmaBlocks =
                                   ({-# LINE 35 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIpragmaBlocks
                                    {-# LINE 5503 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOsynmap =
                                   ({-# LINE 313 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsIsynmap
                                    {-# LINE 5509 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               -- copy rule (down)
                               _nontsOtextBlocks =
                                   ({-# LINE 36 "src-ag/ExecutionPlan2Hs.ag" #-}
                                    _lhsItextBlocks
                                    {-# LINE 5515 "dist/build/ExecutionPlan2Hs" #-}
                                    )
                               ( _nontsIappendCommon,_nontsIappendMain,_nontsIchildvisit,_nontsIerrors,_nontsIfromToStates,_nontsIgenProdIO,_nontsIimports,_nontsIinitStates,_nontsIoutput,_nontsIsemFunBndDefs,_nontsIsemFunBndTps,_nontsIvisitKinds,_nontsIvisitdefs,_nontsIvisituses) =
                                   nonts_ _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOallchildvisit _nontsOavisitdefs _nontsOavisituses _nontsOderivings _nontsOimportBlocks _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOmoduleHeader _nontsOoptions _nontsOpragmaBlocks _nontsOsynmap _nontsOtextBlocks _nontsOtypeSyns _nontsOwrappers
                               ___node =
                                   (Syn_ExecutionPlan _lhsOerrors _lhsOgenIO _lhsOoutput)
                           in  ( _lhsOerrors,_lhsOgenIO,_lhsOoutput))))
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
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1048, column 16)
                       _lhsOtks =
                           ({-# LINE 1048 "src-ag/ExecutionPlan2Hs.ag" #-}
                            tks_
                            {-# LINE 5562 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1091, column 29)
                       _lhsOpos =
                           ({-# LINE 1091 "src-ag/ExecutionPlan2Hs.ag" #-}
                            pos_
                            {-# LINE 5568 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1177, column 16)
                       _lhsOattrs =
                           ({-# LINE 1177 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.unions $ map (\tok -> attrs_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 5574 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1178, column 16)
                       _lhsOsemfunc =
                           ({-# LINE 1178 "src-ag/ExecutionPlan2Hs.ag" #-}
                            vlist $ showTokens $ map (\tok -> tok_Syn_HsToken (wrap_HsToken (sem_HsToken tok) Inh_HsToken)) tks_
                            {-# LINE 5580 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       ___node =
                           (Syn_Expression _lhsOattrs _lhsOpos _lhsOsemfunc _lhsOtks)
                   in  ( _lhsOattrs,_lhsOpos,_lhsOsemfunc,_lhsOtks)))
-- HsToken -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         attrs                : Map String (Maybe NonLocalAttr)
         tok                  : (Pos,String)
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local mbAttr      : _
            local addTrace    : _
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tok         : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken ->
               T_HsToken
sem_HsToken (AGField _field _attr _pos _rdesc) =
    (sem_HsToken_AGField _field _attr _pos _rdesc)
sem_HsToken (AGLocal _var _pos _rdesc) =
    (sem_HsToken_AGLocal _var _pos _rdesc)
sem_HsToken (CharToken _value _pos) =
    (sem_HsToken_CharToken _value _pos)
sem_HsToken (Err _mesg _pos) =
    (sem_HsToken_Err _mesg _pos)
sem_HsToken (HsToken _value _pos) =
    (sem_HsToken_HsToken _value _pos)
sem_HsToken (StrToken _value _pos) =
    (sem_HsToken_StrToken _value _pos)
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
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGField field_ attr_ pos_ rdesc_ =
    (T_HsToken (let _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    _lhsOtok :: ((Pos,String))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1137, column 15)
                    _mbAttr =
                        ({-# LINE 1137 "src-ag/ExecutionPlan2Hs.ag" #-}
                         if field_ == _INST || field_ == _FIELD || field_ == _INST'
                         then Nothing
                         else Just $ mkNonLocalAttr (field_ == _LHS) field_ attr_
                         {-# LINE 5658 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1140, column 15)
                    _lhsOattrs =
                        ({-# LINE 1140 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.singleton (attrname True field_ attr_) _mbAttr
                         {-# LINE 5664 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1385, column 8)
                    _addTrace =
                        ({-# LINE 1385 "src-ag/ExecutionPlan2Hs.ag" #-}
                         case rdesc_ of
                           Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                           Nothing -> id
                         {-# LINE 5672 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1388, column 8)
                    _lhsOtok =
                        ({-# LINE 1388 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, _addTrace     $ attrname True field_ attr_)
                         {-# LINE 5678 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGLocal var_ pos_ rdesc_ =
    (T_HsToken (let _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    _lhsOtok :: ((Pos,String))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1136, column 15)
                    _lhsOattrs =
                        ({-# LINE 1136 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.singleton (fieldname var_) Nothing
                         {-# LINE 5694 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1381, column 15)
                    _tok =
                        ({-# LINE 1381 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_,fieldname var_)
                         {-# LINE 5700 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- copy rule (from local)
                    _lhsOtok =
                        ({-# LINE 1383 "src-ag/ExecutionPlan2Hs.ag" #-}
                         _tok
                         {-# LINE 5706 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken
sem_HsToken_CharToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1392, column 16)
                    _lhsOtok =
                        ({-# LINE 1392 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, if null value_
                                   then ""
                                   else showCharShort (head value_)
                         )
                         {-# LINE 5724 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1134, column 37)
                    _lhsOattrs =
                        ({-# LINE 1134 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.empty
                         {-# LINE 5730 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken
sem_HsToken_Err mesg_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1398, column 16)
                    _lhsOtok =
                        ({-# LINE 1398 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, "")
                         {-# LINE 5745 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1134, column 37)
                    _lhsOattrs =
                        ({-# LINE 1134 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.empty
                         {-# LINE 5751 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken
sem_HsToken_HsToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1390, column 14)
                    _lhsOtok =
                        ({-# LINE 1390 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, value_)
                         {-# LINE 5766 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1134, column 37)
                    _lhsOattrs =
                        ({-# LINE 1134 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.empty
                         {-# LINE 5772 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
                in  ( _lhsOattrs,_lhsOtok)))
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken
sem_HsToken_StrToken value_ pos_ =
    (T_HsToken (let _lhsOtok :: ((Pos,String))
                    _lhsOattrs :: (Map String (Maybe NonLocalAttr))
                    -- "src-ag/ExecutionPlan2Hs.ag"(line 1397, column 16)
                    _lhsOtok =
                        ({-# LINE 1397 "src-ag/ExecutionPlan2Hs.ag" #-}
                         (pos_, showStrShort value_)
                         {-# LINE 5787 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1134, column 37)
                    _lhsOattrs =
                        ({-# LINE 1134 "src-ag/ExecutionPlan2Hs.ag" #-}
                         Map.empty
                         {-# LINE 5793 "dist/build/ExecutionPlan2Hs" #-}
                         )
                    ___node =
                        (Syn_HsToken _lhsOattrs _lhsOtok)
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
                     -- "src-ag/ExecutionPlan2Hs.ag"(line 1377, column 10)
                     _lhsOtks =
                         ({-# LINE 1377 "src-ag/ExecutionPlan2Hs.ag" #-}
                          _hdItok : _tlItks
                          {-# LINE 5836 "dist/build/ExecutionPlan2Hs" #-}
                          )
                     ( _hdIattrs,_hdItok) =
                         hd_
                     ( _tlItks) =
                         tl_
                     ___node =
                         (Syn_HsTokens _lhsOtks)
                 in  ( _lhsOtks)))
sem_HsTokens_Nil :: T_HsTokens
sem_HsTokens_Nil =
    (T_HsTokens (let _lhsOtks :: ([(Pos,String)])
                     -- "src-ag/ExecutionPlan2Hs.ag"(line 1378, column 10)
                     _lhsOtks =
                         ({-# LINE 1378 "src-ag/ExecutionPlan2Hs.ag" #-}
                          []
                          {-# LINE 5852 "dist/build/ExecutionPlan2Hs" #-}
                          )
                     ___node =
                         (Syn_HsTokens _lhsOtks)
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
                         ___node =
                             (Syn_HsTokensRoot)
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
         isUnderscore         : Bool
         sem_lhs              :  PP_Doc 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local varPat      : _
            local patExpr     : _
            local mbTp        : _
            local addbang     : _
            local addbang1    : _
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local addbang     : _
            local addbang1    : _
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local addbang     : _
            local addbang1    : _
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
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
                               ( PP_Doc,(Set String),Pattern,Bool,( PP_Doc )))
data Inh_Pattern = Inh_Pattern {allInhmap_Inh_Pattern :: (Map NontermIdent Attributes),allSynmap_Inh_Pattern :: (Map NontermIdent Attributes),anyLazyKind_Inh_Pattern :: Bool,inhmap_Inh_Pattern :: Attributes,localAttrTypes_Inh_Pattern :: (Map Identifier Type),options_Inh_Pattern :: Options,synmap_Inh_Pattern :: Attributes}
data Syn_Pattern = Syn_Pattern {attrTypes_Syn_Pattern :: PP_Doc,attrs_Syn_Pattern :: (Set String),copy_Syn_Pattern :: Pattern,isUnderscore_Syn_Pattern :: Bool,sem_lhs_Syn_Pattern :: ( PP_Doc )}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
    (let ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs) = sem _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
     in  (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs))
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
                         _patIisUnderscore :: Bool
                         _patIsem_lhs :: ( PP_Doc )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1099, column 17)
                         _varPat =
                             ({-# LINE 1099 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text $ attrname False field_ attr_
                              {-# LINE 6002 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1100, column 17)
                         _patExpr =
                             ({-# LINE 1100 "src-ag/ExecutionPlan2Hs.ag" #-}
                              if _patIisUnderscore
                              then _varPat
                              else _varPat     >|< "@" >|< _patIsem_lhs
                              {-# LINE 6010 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1103, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1103 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _addbang1     _patExpr
                              {-# LINE 6016 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1114, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1114 "src-ag/ExecutionPlan2Hs.ag" #-}
                              False
                              {-# LINE 6022 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1120, column 3)
                         _lhsOattrs =
                             ({-# LINE 1120 "src-ag/ExecutionPlan2Hs.ag" #-}
                              Set.insert (attrname False field_ attr_) _patIattrs
                              {-# LINE 6028 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1125, column 3)
                         _mbTp =
                             ({-# LINE 1125 "src-ag/ExecutionPlan2Hs.ag" #-}
                              if field_ == _LHS
                              then Map.lookup attr_ _lhsIsynmap
                              else if field_ == _LOC
                                   then Map.lookup attr_ _lhsIlocalAttrTypes
                                   else Nothing
                              {-# LINE 6038 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1130, column 3)
                         _lhsOattrTypes =
                             ({-# LINE 1130 "src-ag/ExecutionPlan2Hs.ag" #-}
                              maybe empty (\tp -> (attrname False field_ attr_) >#< "::" >#< ppTp tp) _mbTp
                              >-< _patIattrTypes
                              {-# LINE 6045 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1521, column 37)
                         _addbang =
                             ({-# LINE 1521 "src-ag/ExecutionPlan2Hs.ag" #-}
                              \x -> if bangpats _lhsIoptions then "!" >|< x else x
                              {-# LINE 6051 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1526, column 37)
                         _addbang1 =
                             ({-# LINE 1526 "src-ag/ExecutionPlan2Hs.ag" #-}
                              if _lhsIanyLazyKind then id else _addbang
                              {-# LINE 6057 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Alias field_ attr_ _patIcopy
                              {-# LINE 6063 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 6069 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOallInhmap =
                             ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 6075 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOallSynmap =
                             ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 6081 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOanyLazyKind =
                             ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 6087 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOinhmap =
                             ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIinhmap
                              {-# LINE 6093 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOlocalAttrTypes =
                             ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 6099 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOoptions =
                             ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIoptions
                              {-# LINE 6105 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOsynmap =
                             ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIsynmap
                              {-# LINE 6111 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         ( _patIattrTypes,_patIattrs,_patIcopy,_patIisUnderscore,_patIsem_lhs) =
                             pat_ _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap
                         ___node =
                             (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs))))
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
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1105, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1105 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _addbang1     $ pp_parens $ name_ >#< hv_sp _patsIsem_lhs
                              {-# LINE 6149 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1112, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1112 "src-ag/ExecutionPlan2Hs.ag" #-}
                              False
                              {-# LINE 6155 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1521, column 37)
                         _addbang =
                             ({-# LINE 1521 "src-ag/ExecutionPlan2Hs.ag" #-}
                              \x -> if bangpats _lhsIoptions then "!" >|< x else x
                              {-# LINE 6161 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1526, column 37)
                         _addbang1 =
                             ({-# LINE 1526 "src-ag/ExecutionPlan2Hs.ag" #-}
                              if _lhsIanyLazyKind then id else _addbang
                              {-# LINE 6167 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrTypes
                              {-# LINE 6173 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                         _lhsOattrs =
                             ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrs
                              {-# LINE 6179 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Constr name_ _patsIcopy
                              {-# LINE 6185 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 6191 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallInhmap =
                             ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 6197 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallSynmap =
                             ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 6203 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOanyLazyKind =
                             ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 6209 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinhmap =
                             ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIinhmap
                              {-# LINE 6215 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOlocalAttrTypes =
                             ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 6221 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOoptions =
                             ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIoptions
                              {-# LINE 6227 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsynmap =
                             ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIsynmap
                              {-# LINE 6233 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         ( _patsIattrTypes,_patsIattrs,_patsIcopy,_patsIsem_lhs) =
                             pats_ _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap
                         ___node =
                             (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs))))
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
                         _patIisUnderscore :: Bool
                         _patIsem_lhs :: ( PP_Doc )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1107, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1107 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text "~" >|< pp_parens _patIsem_lhs
                              {-# LINE 6271 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patIattrTypes
                              {-# LINE 6277 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                         _lhsOattrs =
                             ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patIattrs
                              {-# LINE 6283 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Irrefutable _patIcopy
                              {-# LINE 6289 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 6295 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (up)
                         _lhsOisUnderscore =
                             ({-# LINE 1110 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patIisUnderscore
                              {-# LINE 6301 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOallInhmap =
                             ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 6307 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOallSynmap =
                             ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 6313 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOanyLazyKind =
                             ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 6319 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOinhmap =
                             ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIinhmap
                              {-# LINE 6325 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOlocalAttrTypes =
                             ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 6331 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOoptions =
                             ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIoptions
                              {-# LINE 6337 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patOsynmap =
                             ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIsynmap
                              {-# LINE 6343 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         ( _patIattrTypes,_patIattrs,_patIcopy,_patIisUnderscore,_patIsem_lhs) =
                             pat_ _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap
                         ___node =
                             (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs))))
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
                         _patsIsem_lhs :: ([PP_Doc])
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1104, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1104 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _addbang1     $ pp_block "(" ")" "," _patsIsem_lhs
                              {-# LINE 6381 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1113, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1113 "src-ag/ExecutionPlan2Hs.ag" #-}
                              False
                              {-# LINE 6387 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1521, column 37)
                         _addbang =
                             ({-# LINE 1521 "src-ag/ExecutionPlan2Hs.ag" #-}
                              \x -> if bangpats _lhsIoptions then "!" >|< x else x
                              {-# LINE 6393 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1526, column 37)
                         _addbang1 =
                             ({-# LINE 1526 "src-ag/ExecutionPlan2Hs.ag" #-}
                              if _lhsIanyLazyKind then id else _addbang
                              {-# LINE 6399 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrTypes
                              {-# LINE 6405 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                         _lhsOattrs =
                             ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _patsIattrs
                              {-# LINE 6411 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Product pos_ _patsIcopy
                              {-# LINE 6417 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 6423 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallInhmap =
                             ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallInhmap
                              {-# LINE 6429 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOallSynmap =
                             ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIallSynmap
                              {-# LINE 6435 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOanyLazyKind =
                             ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIanyLazyKind
                              {-# LINE 6441 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOinhmap =
                             ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIinhmap
                              {-# LINE 6447 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOlocalAttrTypes =
                             ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIlocalAttrTypes
                              {-# LINE 6453 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOoptions =
                             ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIoptions
                              {-# LINE 6459 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- copy rule (down)
                         _patsOsynmap =
                             ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                              _lhsIsynmap
                              {-# LINE 6465 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         ( _patsIattrTypes,_patsIattrs,_patsIcopy,_patsIsem_lhs) =
                             pats_ _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap
                         ___node =
                             (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs))))
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
                         _lhsOcopy :: Pattern
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1106, column 17)
                         _lhsOsem_lhs =
                             ({-# LINE 1106 "src-ag/ExecutionPlan2Hs.ag" #-}
                              text "_"
                              {-# LINE 6491 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- "src-ag/ExecutionPlan2Hs.ag"(line 1115, column 16)
                         _lhsOisUnderscore =
                             ({-# LINE 1115 "src-ag/ExecutionPlan2Hs.ag" #-}
                              True
                              {-# LINE 6497 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                         _lhsOattrTypes =
                             ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                              empty
                              {-# LINE 6503 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                         _lhsOattrs =
                             ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                              Set.empty
                              {-# LINE 6509 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _copy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              Underscore pos_
                              {-# LINE 6515 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         -- self rule
                         _lhsOcopy =
                             ({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 6521 "dist/build/ExecutionPlan2Hs" #-}
                              )
                         ___node =
                             (Syn_Pattern _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOisUnderscore _lhsOsem_lhs)
                     in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOisUnderscore,_lhsOsem_lhs))))
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
                                 ( PP_Doc,(Set String),Patterns,([PP_Doc])))
data Inh_Patterns = Inh_Patterns {allInhmap_Inh_Patterns :: (Map NontermIdent Attributes),allSynmap_Inh_Patterns :: (Map NontermIdent Attributes),anyLazyKind_Inh_Patterns :: Bool,inhmap_Inh_Patterns :: Attributes,localAttrTypes_Inh_Patterns :: (Map Identifier Type),options_Inh_Patterns :: Options,synmap_Inh_Patterns :: Attributes}
data Syn_Patterns = Syn_Patterns {attrTypes_Syn_Patterns :: PP_Doc,attrs_Syn_Patterns :: (Set String),copy_Syn_Patterns :: Patterns,sem_lhs_Syn_Patterns :: ([PP_Doc])}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
    (let ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOsem_lhs) = sem _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
     in  (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs))
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
                          _hdIisUnderscore :: Bool
                          _hdIsem_lhs :: ( PP_Doc )
                          _tlIattrTypes :: PP_Doc
                          _tlIattrs :: (Set String)
                          _tlIcopy :: Patterns
                          _tlIsem_lhs :: ([PP_Doc])
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                          _lhsOattrTypes =
                              ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _hdIattrTypes >-< _tlIattrTypes
                               {-# LINE 6616 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                          _lhsOattrs =
                              ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _hdIattrs `Set.union` _tlIattrs
                               {-# LINE 6622 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1095, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 1095 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _hdIsem_lhs : _tlIsem_lhs
                               {-# LINE 6628 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               (:) _hdIcopy _tlIcopy
                               {-# LINE 6634 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 6640 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallInhmap =
                              ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIallInhmap
                               {-# LINE 6646 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallSynmap =
                              ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIallSynmap
                               {-# LINE 6652 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOanyLazyKind =
                              ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIanyLazyKind
                               {-# LINE 6658 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhmap =
                              ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIinhmap
                               {-# LINE 6664 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOlocalAttrTypes =
                              ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIlocalAttrTypes
                               {-# LINE 6670 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOoptions =
                              ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIoptions
                               {-# LINE 6676 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynmap =
                              ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIsynmap
                               {-# LINE 6682 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallInhmap =
                              ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIallInhmap
                               {-# LINE 6688 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallSynmap =
                              ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIallSynmap
                               {-# LINE 6694 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOanyLazyKind =
                              ({-# LINE 1267 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIanyLazyKind
                               {-# LINE 6700 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhmap =
                              ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIinhmap
                               {-# LINE 6706 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOlocalAttrTypes =
                              ({-# LINE 1578 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIlocalAttrTypes
                               {-# LINE 6712 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOoptions =
                              ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIoptions
                               {-# LINE 6718 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynmap =
                              ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                               _lhsIsynmap
                               {-# LINE 6724 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          ( _hdIattrTypes,_hdIattrs,_hdIcopy,_hdIisUnderscore,_hdIsem_lhs) =
                              hd_ _hdOallInhmap _hdOallSynmap _hdOanyLazyKind _hdOinhmap _hdOlocalAttrTypes _hdOoptions _hdOsynmap
                          ( _tlIattrTypes,_tlIattrs,_tlIcopy,_tlIsem_lhs) =
                              tl_ _tlOallInhmap _tlOallSynmap _tlOanyLazyKind _tlOinhmap _tlOlocalAttrTypes _tlOoptions _tlOsynmap
                          ___node =
                              (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs)
                      in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOsem_lhs))))
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
                          _lhsOsem_lhs :: ([PP_Doc])
                          _lhsOcopy :: Patterns
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1123, column 40)
                          _lhsOattrTypes =
                              ({-# LINE 1123 "src-ag/ExecutionPlan2Hs.ag" #-}
                               empty
                               {-# LINE 6750 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1118, column 36)
                          _lhsOattrs =
                              ({-# LINE 1118 "src-ag/ExecutionPlan2Hs.ag" #-}
                               Set.empty
                               {-# LINE 6756 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1095, column 29)
                          _lhsOsem_lhs =
                              ({-# LINE 1095 "src-ag/ExecutionPlan2Hs.ag" #-}
                               []
                               {-# LINE 6762 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- self rule
                          _copy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               []
                               {-# LINE 6768 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          -- self rule
                          _lhsOcopy =
                              ({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 6774 "dist/build/ExecutionPlan2Hs" #-}
                               )
                          ___node =
                              (Syn_Patterns _lhsOattrTypes _lhsOattrs _lhsOcopy _lhsOsem_lhs)
                      in  ( _lhsOattrTypes,_lhsOattrs,_lhsOcopy,_lhsOsem_lhs))))
-- Visit -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         allintramap          : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         con                  : ConstructorIdent
         inhmap               : Attributes
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         intramap             : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_visit            :   (StateIdentifier,Bool -> PP_Doc)  
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
            local nameT_visit : _
            local nameTIn_visit : _
            local nameTOut_visit : _
            local nameTNext_visit : _
            local nextVisitInfo : _
            local typecon     : _
            local t_params    : _
            local inhpart     : _
            local synpart     : _
            local ppTypeList  : _
            local stepsInitial : _
            local stepsClosing : _
            local vname       : _
            local inhpats     : _
            local inhargs     : _
            local synargs     : _
            local nextargsMp  : _
            local nextargs    : _
            local nextst      : _
            local resultval   : _
            local nextStBuild : _
            local nextStRef   : _
            local prevVisitInfo : _
            local invokecode  : _
            local thisintra   : _
            local nextintra   : _
            local uses        : _
            local inhVarNms   : _
            local defs        : _
            local defsAsMap   : _
            local lazyIntrasInh : _
            local addbang     : _
            local addbang1    : _
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
                           (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                           (Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                           (Map VisitIdentifier (Set Identifier)) ->
                           (Map VisitIdentifier (Set Identifier)) ->
                           (Map Identifier Type) ->
                           (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                           ConstructorIdent ->
                           Attributes ->
                           (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                           (Map StateIdentifier StateCtx) ->
                           NontermIdent ->
                           Options ->
                           ([Identifier]) ->
                           (Map StateIdentifier StateCtx) ->
                           (Map Identifier (Set String)) ->
                           (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                           Attributes ->
                           (Set String) ->
                           ( ( VisitStateState ),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),(Seq Error),(Map VisitIdentifier (Int,Int)),(Map StateIdentifier (Map String (Maybe NonLocalAttr))),(Set String),(Map Identifier (Set VisitKind)),(Map Identifier Int),(  (StateIdentifier,Bool -> PP_Doc)  ),PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_Visit = Inh_Visit {allFromToStates_Inh_Visit :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_Visit :: (Map NontermIdent Attributes),allInitStates_Inh_Visit :: (Map NontermIdent Int),allSynmap_Inh_Visit :: (Map NontermIdent Attributes),allVisitKinds_Inh_Visit :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_Visit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),allintramap_Inh_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),avisitdefs_Inh_Visit :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_Visit :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_Visit :: (Map Identifier Type),childintros_Inh_Visit :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),con_Inh_Visit :: ConstructorIdent,inhmap_Inh_Visit :: Attributes,mrules_Inh_Visit :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),nextVisits_Inh_Visit :: (Map StateIdentifier StateCtx),nt_Inh_Visit :: NontermIdent,options_Inh_Visit :: Options,params_Inh_Visit :: ([Identifier]),prevVisits_Inh_Visit :: (Map StateIdentifier StateCtx),ruledefs_Inh_Visit :: (Map Identifier (Set String)),ruleuses_Inh_Visit :: (Map Identifier (Map String (Maybe NonLocalAttr))),synmap_Inh_Visit :: Attributes,terminaldefs_Inh_Visit :: (Set String)}
data Syn_Visit = Syn_Visit {allvisits_Syn_Visit :: ( VisitStateState ),childvisit_Syn_Visit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),errors_Syn_Visit :: (Seq Error),fromToStates_Syn_Visit :: (Map VisitIdentifier (Int,Int)),intramap_Syn_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),lazyIntras_Syn_Visit :: (Set String),ruleKinds_Syn_Visit :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_Visit :: (Map Identifier Int),sem_visit_Syn_Visit :: (  (StateIdentifier,Bool -> PP_Doc)  ),t_visits_Syn_Visit :: PP_Doc,visitKinds_Syn_Visit :: (Map VisitIdentifier VisitKind),visitdefs_Syn_Visit :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_Visit :: (Map VisitIdentifier (Set Identifier))}
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
                       _lhsOsem_visit :: (  (StateIdentifier,Bool -> PP_Doc)  )
                       _stepsOkind :: VisitKind
                       _stepsOfmtMode :: FormatMode
                       _stepsOindex :: Int
                       _stepsOprevMaxSimRefs :: Int
                       _stepsOuseParallel :: Bool
                       _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
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
                       _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                       _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                       _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                       _stepsOchildTypes :: (Map Identifier Type)
                       _stepsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                       _stepsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                       _stepsIsync_steps :: PP_Doc
                       _stepsIuses :: (Map String (Maybe NonLocalAttr))
                       _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 341, column 11)
                       _lhsOallvisits =
                           ({-# LINE 341 "src-ag/ExecutionPlan2Hs.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 6981 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 400, column 11)
                       _nameT_visit =
                           ({-# LINE 400 "src-ag/ExecutionPlan2Hs.ag" #-}
                            conNmTVisit _lhsInt ident_
                            {-# LINE 6987 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 401, column 11)
                       _nameTIn_visit =
                           ({-# LINE 401 "src-ag/ExecutionPlan2Hs.ag" #-}
                            conNmTVisitIn _lhsInt ident_
                            {-# LINE 6993 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 402, column 11)
                       _nameTOut_visit =
                           ({-# LINE 402 "src-ag/ExecutionPlan2Hs.ag" #-}
                            conNmTVisitOut _lhsInt ident_
                            {-# LINE 6999 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 403, column 11)
                       _nameTNext_visit =
                           ({-# LINE 403 "src-ag/ExecutionPlan2Hs.ag" #-}
                            conNmTNextVisit _lhsInt to_
                            {-# LINE 7005 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 405, column 11)
                       _nextVisitInfo =
                           ({-# LINE 405 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.findWithDefault ManyVis to_ _lhsInextVisits
                            {-# LINE 7011 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 406, column 11)
                       _typecon =
                           ({-# LINE 406 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case kind_ of
                              VisitPure _  -> empty
                              VisitMonadic -> ppMonadType _lhsIoptions
                            {-# LINE 7019 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 410, column 11)
                       _t_params =
                           ({-# LINE 410 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced _lhsIparams
                            {-# LINE 7025 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 411, column 11)
                       _lhsOt_visits =
                           ({-# LINE 411 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 7043 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 424, column 11)
                       _inhpart =
                           ({-# LINE 424 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _ppTypeList     inh_ _lhsIinhmap
                            {-# LINE 7049 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 425, column 11)
                       _synpart =
                           ({-# LINE 425 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _ppTypeList     syn_ _lhsIsynmap
                            {-# LINE 7055 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 426, column 11)
                       _ppTypeList =
                           ({-# LINE 426 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \s m -> ppSpaced $ map (\i -> _addbang1     $ pp_parens $ case Map.lookup i m of
                                                                                       Just tp -> ppTp tp ) $ Set.toList s
                            {-# LINE 7062 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 709, column 11)
                       _lhsOsem_visit =
                           ({-# LINE 709 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 7090 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 734, column 11)
                       _stepsInitial =
                           ({-# LINE 734 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case kind_ of
                              VisitPure False -> text "let"
                              VisitPure True  -> empty
                              VisitMonadic    -> text "do"
                            {-# LINE 7099 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 738, column 11)
                       _stepsClosing =
                           ({-# LINE 738 "src-ag/ExecutionPlan2Hs.ag" #-}
                            let decls =  _nextStBuild
                                         >-<  _addbang     (pp resultValName) >#< "=" >#< _resultval
                            in case kind_ of
                                 VisitPure False -> decls
                                                    >-< "in" >#< resultValName
                                 VisitPure True  -> "let" >#< decls
                                                    >-< indent 1 ("in" >#< resultValName)
                                 VisitMonadic    -> "let" >#< decls
                                                    >-< "return" >#< resultValName
                            {-# LINE 7113 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 747, column 11)
                       _vname =
                           ({-# LINE 747 "src-ag/ExecutionPlan2Hs.ag" #-}
                            "v" >|< ident_
                            {-# LINE 7119 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 748, column 11)
                       _inhpats =
                           ({-# LINE 748 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ map (\arg -> _addbang     $ pp $ attrname True _LHS arg) $ Set.toList inh_
                            {-# LINE 7125 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 749, column 11)
                       _inhargs =
                           ({-# LINE 749 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \chn -> ppSpaced $ map (attrname False chn) $ Set.toList inh_
                            {-# LINE 7131 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 750, column 11)
                       _synargs =
                           ({-# LINE 750 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ map (\arg -> attrname False _LHS arg) $ Set.toList syn_
                            {-# LINE 7137 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 751, column 11)
                       _nextargsMp =
                           ({-# LINE 751 "src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 7143 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 752, column 11)
                       _nextargs =
                           ({-# LINE 752 "src-ag/ExecutionPlan2Hs.ag" #-}
                            ppSpaced $ Map.keys $ _nextargsMp
                            {-# LINE 7149 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 753, column 11)
                       _nextst =
                           ({-# LINE 753 "src-ag/ExecutionPlan2Hs.ag" #-}
                            "st" >|< to_ >#< _nextargs     >#< dummyArg _lhsIoptions (Map.null _nextargsMp    )
                            {-# LINE 7155 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 754, column 11)
                       _resultval =
                           ({-# LINE 754 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _nameTOut_visit     >#< _synargs     >#< _nextStRef
                            {-# LINE 7161 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 755, column 11)
                       (_nextStBuild,_nextStRef) =
                           ({-# LINE 755 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case _nextVisitInfo     of
                              NoneVis  -> (empty, empty)
                              _        -> (_addbang     (pp nextStName) >#< "=" >#< _nextst    , pp nextStName)
                            {-# LINE 7169 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 770, column 20)
                       _stepsOkind =
                           ({-# LINE 770 "src-ag/ExecutionPlan2Hs.ag" #-}
                            kind_
                            {-# LINE 7175 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 821, column 3)
                       _stepsOfmtMode =
                           ({-# LINE 821 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case kind_ of
                              VisitPure False -> FormatLetDecl
                              VisitPure True  -> FormatLetLine
                              VisitMonadic    -> FormatDo
                            {-# LINE 7184 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 859, column 22)
                       _stepsOindex =
                           ({-# LINE 859 "src-ag/ExecutionPlan2Hs.ag" #-}
                            0
                            {-# LINE 7190 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 866, column 22)
                       _stepsOprevMaxSimRefs =
                           ({-# LINE 866 "src-ag/ExecutionPlan2Hs.ag" #-}
                            0
                            {-# LINE 7196 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 882, column 22)
                       _stepsOuseParallel =
                           ({-# LINE 882 "src-ag/ExecutionPlan2Hs.ag" #-}
                            False
                            {-# LINE 7202 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1196, column 6)
                       _prevVisitInfo =
                           ({-# LINE 1196 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.findWithDefault ManyVis from_ _lhsInextVisits
                            {-# LINE 7208 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1197, column 6)
                       _lhsOchildvisit =
                           ({-# LINE 1197 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ _invokecode
                            {-# LINE 7214 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1198, column 6)
                       _invokecode =
                           ({-# LINE 1198 "src-ag/ExecutionPlan2Hs.ag" #-}
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
                            {-# LINE 7248 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1294, column 11)
                       _thisintra =
                           ({-# LINE 1294 "src-ag/ExecutionPlan2Hs.ag" #-}
                            (_uses     `Map.union` _nextintra    ) `Map.difference` _defsAsMap
                            {-# LINE 7254 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1295, column 11)
                       _lhsOintramap =
                           ({-# LINE 1295 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton from_ _thisintra
                            {-# LINE 7260 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1296, column 11)
                       _nextintra =
                           ({-# LINE 1296 "src-ag/ExecutionPlan2Hs.ag" #-}
                            maybe Map.empty id $ Map.lookup to_ _lhsIallintramap
                            {-# LINE 7266 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1297, column 11)
                       _uses =
                           ({-# LINE 1297 "src-ag/ExecutionPlan2Hs.ag" #-}
                            let mp1 = _stepsIuses
                                mp2 = Map.fromList [ (lhsname False i, Just (AttrSyn _LHS i)) | i <- Set.elems syn_ ]
                            in mp1 `Map.union` mp2
                            {-# LINE 7274 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1300, column 11)
                       _inhVarNms =
                           ({-# LINE 1300 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Set.map (lhsname True) inh_
                            {-# LINE 7280 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1301, column 11)
                       _defs =
                           ({-# LINE 1301 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIdefs `Set.union` _inhVarNms     `Set.union` _lhsIterminaldefs
                            {-# LINE 7286 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1302, column 11)
                       _defsAsMap =
                           ({-# LINE 1302 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.fromList [ (a, Nothing) | a <- Set.elems _defs     ]
                            {-# LINE 7292 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1326, column 11)
                       _lhsOvisitdefs =
                           ({-# LINE 1326 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ syn_
                            {-# LINE 7298 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1327, column 11)
                       _lhsOvisituses =
                           ({-# LINE 1327 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ inh_
                            {-# LINE 7304 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1359, column 3)
                       _lazyIntrasInh =
                           ({-# LINE 1359 "src-ag/ExecutionPlan2Hs.ag" #-}
                            case kind_ of
                              VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                              _               -> Set.empty
                            {-# LINE 7312 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1362, column 3)
                       _lhsOlazyIntras =
                           ({-# LINE 1362 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                            {-# LINE 7318 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1515, column 37)
                       _addbang =
                           ({-# LINE 1515 "src-ag/ExecutionPlan2Hs.ag" #-}
                            \x -> if bangpats _lhsIoptions then "!" >|< x else x
                            {-# LINE 7324 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1523, column 37)
                       _addbang1 =
                           ({-# LINE 1523 "src-ag/ExecutionPlan2Hs.ag" #-}
                            if isLazyKind kind_ then id else _addbang
                            {-# LINE 7330 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1550, column 3)
                       _lhsOfromToStates =
                           ({-# LINE 1550 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ (from_, to_)
                            {-# LINE 7336 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- "src-ag/ExecutionPlan2Hs.ag"(line 1594, column 3)
                       _lhsOvisitKinds =
                           ({-# LINE 1594 "src-ag/ExecutionPlan2Hs.ag" #-}
                            Map.singleton ident_ kind_
                            {-# LINE 7342 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                       _lhsOerrors =
                           ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIerrors
                            {-# LINE 7348 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                       _lhsOruleKinds =
                           ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIruleKinds
                            {-# LINE 7354 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                       _lhsOruleUsage =
                           ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _stepsIruleUsage
                            {-# LINE 7360 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallFromToStates =
                           ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallFromToStates
                            {-# LINE 7366 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallInitStates =
                           ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallInitStates
                            {-# LINE 7372 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallVisitKinds =
                           ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallVisitKinds
                            {-# LINE 7378 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOallchildvisit =
                           ({-# LINE 1189 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIallchildvisit
                            {-# LINE 7384 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisitdefs =
                           ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIavisitdefs
                            {-# LINE 7390 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOavisituses =
                           ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIavisituses
                            {-# LINE 7396 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOchildTypes =
                           ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIchildTypes
                            {-# LINE 7402 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOchildintros =
                           ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIchildintros
                            {-# LINE 7408 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOmrules =
                           ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsImrules
                            {-# LINE 7414 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOoptions =
                           ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIoptions
                            {-# LINE 7420 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruledefs =
                           ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIruledefs
                            {-# LINE 7426 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       -- copy rule (down)
                       _stepsOruleuses =
                           ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                            _lhsIruleuses
                            {-# LINE 7432 "dist/build/ExecutionPlan2Hs" #-}
                            )
                       ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIsync_steps,_stepsIuses,_stepsIvisitKinds) =
                           steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                       ___node =
                           (Syn_Visit _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                   in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))
-- VisitStep ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         fmtMode              : FormatMode
         kind                 : VisitKind
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
         sync_steps           : PP_Doc
         uses                 : Map String (Maybe NonLocalAttr)
         visitKinds           : Map VisitIdentifier VisitKind
   alternatives:
      alternative ChildIntro:
         child child          : {Identifier}
         visit 0:
            local attachItf   : _
      alternative ChildVisit:
         child child          : {Identifier}
         child nonterm        : {NontermIdent}
         child visit          : {VisitIdentifier}
         visit 0:
            local visitItf    : _
            local patPP       : _
            local exprPP      : _
            local useParallel : _
            local convToMonad : _
            local callKind    : _
            local addbang     : _
            local from        : _
            local to          : _
      alternative PureGroup:
         child steps          : VisitSteps 
         child ordered        : {Bool}
      alternative Sem:
         child name           : {Identifier}
         visit 0:
            local ruleItf     : _
            local sem_steps   : _
      alternative Sim:
         child steps          : VisitSteps 
         visit 0:
            local useParallel : _
            local isMonadic   : _
-}
-- cata
sem_VisitStep :: VisitStep ->
                 T_VisitStep
sem_VisitStep (ChildIntro _child) =
    (sem_VisitStep_ChildIntro _child)
sem_VisitStep (ChildVisit _child _nonterm _visit) =
    (sem_VisitStep_ChildVisit _child _nonterm _visit)
sem_VisitStep (PureGroup _steps _ordered) =
    (sem_VisitStep_PureGroup (sem_VisitSteps _steps) _ordered)
sem_VisitStep (Sem _name) =
    (sem_VisitStep_Sem _name)
sem_VisitStep (Sim _steps) =
    (sem_VisitStep_Sim (sem_VisitSteps _steps))
-- semantic domain
newtype T_VisitStep = T_VisitStep ((Map VisitIdentifier (Int,Int)) ->
                                   (Map NontermIdent Int) ->
                                   (Map VisitIdentifier VisitKind) ->
                                   (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                   (Map VisitIdentifier (Set Identifier)) ->
                                   (Map VisitIdentifier (Set Identifier)) ->
                                   (Map Identifier Type) ->
                                   (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                   FormatMode ->
                                   Int ->
                                   Bool ->
                                   VisitKind ->
                                   (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                                   Options ->
                                   Int ->
                                   (Map Identifier (Set String)) ->
                                   (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                   Bool ->
                                   ( (Set String),(Seq Error),Int,Bool,(Set String),Int,(Map Identifier (Set VisitKind)),(Map Identifier Int),PP_Doc,PP_Doc,(Map String (Maybe NonLocalAttr)),(Map VisitIdentifier VisitKind)))
data Inh_VisitStep = Inh_VisitStep {allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_VisitStep :: (Map NontermIdent Int),allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_VisitStep :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_VisitStep :: (Map Identifier Type),childintros_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),fmtMode_Inh_VisitStep :: FormatMode,index_Inh_VisitStep :: Int,isLast_Inh_VisitStep :: Bool,kind_Inh_VisitStep :: VisitKind,mrules_Inh_VisitStep :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),options_Inh_VisitStep :: Options,prevMaxSimRefs_Inh_VisitStep :: Int,ruledefs_Inh_VisitStep :: (Map Identifier (Set String)),ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))),useParallel_Inh_VisitStep :: Bool}
data Syn_VisitStep = Syn_VisitStep {defs_Syn_VisitStep :: (Set String),errors_Syn_VisitStep :: (Seq Error),index_Syn_VisitStep :: Int,isLast_Syn_VisitStep :: Bool,lazyIntras_Syn_VisitStep :: (Set String),prevMaxSimRefs_Syn_VisitStep :: Int,ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_VisitStep :: (Map Identifier Int),sem_steps_Syn_VisitStep :: PP_Doc,sync_steps_Syn_VisitStep :: PP_Doc,uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)),visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind)}
wrap_VisitStep :: T_VisitStep ->
                  Inh_VisitStep ->
                  Syn_VisitStep
wrap_VisitStep (T_VisitStep sem) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
    (let ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
     in  (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds))
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
                    _lhsIfmtMode
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
                           _lhsOsync_steps :: PP_Doc
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 783, column 16)
                           _attachItf =
                               ({-# LINE 783 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error $ "Child " ++ show child_ ++ " not found") child_ _lhsIchildintros
                                {-# LINE 7583 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 784, column 16)
                           (_lhsOerrors,_lhsOsem_steps,_lhsOdefs,_lhsOuses) =
                               ({-# LINE 784 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _attachItf     _lhsIkind _lhsIfmtMode of
                                  Left e                   -> (Seq.singleton e, empty, Set.empty, Map.empty)
                                  Right (code, defs, uses) -> (Seq.empty, code, defs, uses)
                                {-# LINE 7591 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 7597 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 7603 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 7609 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                           _lhsOsync_steps =
                               ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                empty
                                {-# LINE 7615 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                mempty
                                {-# LINE 7621 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIindex
                                {-# LINE 7627 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 873 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIisLast
                                {-# LINE 7633 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 7639 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ___node =
                               (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
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
                    _lhsIfmtMode
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
                           _lhsOsync_steps :: PP_Doc
                           _lhsOdefs :: (Set String)
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOlazyIntras :: (Set String)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 788, column 16)
                           _visitItf =
                               ({-# LINE 788 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error $ "Visit " ++ show visit_ ++ " not found") visit_ _lhsIallchildvisit
                                {-# LINE 7683 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 789, column 16)
                           (_lhsOerrors,_patPP,_exprPP) =
                               ({-# LINE 789 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _visitItf     child_ _lhsIkind of
                                  Left e           -> (Seq.singleton e, empty, empty)
                                  Right (pat,expr) -> (Seq.empty, pat, expr)
                                {-# LINE 7691 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 793, column 16)
                           _useParallel =
                               ({-# LINE 793 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIuseParallel && not _lhsIisLast
                                {-# LINE 7697 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 794, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 794 "src-ag/ExecutionPlan2Hs.ag" #-}
                                if _useParallel
                                then _addbang     ("sync_" >|< _lhsIindex) >#< "<- newEmptyMVar"
                                     >-< "forkIO" >#< pp_parens (_convToMonad     >#< pp_parens _exprPP     >#< ">>= \\" >#< _addbang     (pp parResultName) >#< " -> putMVar sync_" >|< _lhsIindex >#< parResultName)
                                else let decl = case _lhsIkind of
                                                  VisitPure _  -> _patPP     >#< "=" >#< _exprPP
                                                  VisitMonadic -> _patPP     >#< "<-" >#< _exprPP
                                     in fmtDecl False _lhsIfmtMode decl
                                {-# LINE 7709 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 801, column 16)
                           _convToMonad =
                               ({-# LINE 801 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _callKind     of
                                  VisitPure _  -> text "return"
                                  VisitMonadic -> empty
                                {-# LINE 7717 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 804, column 16)
                           _callKind =
                               ({-# LINE 804 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error "visit kind should be in the map") visit_ _lhsIallVisitKinds
                                {-# LINE 7723 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 812, column 3)
                           _lhsOsync_steps =
                               ({-# LINE 812 "src-ag/ExecutionPlan2Hs.ag" #-}
                                if _useParallel
                                then _patPP     >#< "<-" >#< "takeMVar sync_" >|< _lhsIindex
                                else empty
                                {-# LINE 7731 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1344, column 16)
                           _lhsOdefs =
                               ({-# LINE 1344 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.insert (stname child_ _to) $ maybe (error "Visit not found") (Set.map $ attrname True child_) $ Map.lookup visit_ _lhsIavisitdefs
                                {-# LINE 7737 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1345, column 16)
                           _lhsOuses =
                               ({-# LINE 1345 "src-ag/ExecutionPlan2Hs.ag" #-}
                                let convert attrs = Map.fromList [ (attrname False child_ attr, Just $ mkNonLocalAttr True child_ attr) | attr <- Set.elems attrs ]
                                in Map.insert (stname child_ _from) Nothing $ convert $
                                     maybe (error "Visit not found") id $ Map.lookup visit_ _lhsIavisituses
                                {-# LINE 7745 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1520, column 37)
                           _addbang =
                               ({-# LINE 1520 "src-ag/ExecutionPlan2Hs.ag" #-}
                                \x -> if bangpats _lhsIoptions then "!" >|< x else x
                                {-# LINE 7751 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1556, column 3)
                           (_from,_to) =
                               ({-# LINE 1556 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                                {-# LINE 7757 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 7763 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 7769 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.empty
                                {-# LINE 7775 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                mempty
                                {-# LINE 7781 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIindex
                                {-# LINE 7787 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 873 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIisLast
                                {-# LINE 7793 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 7799 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ___node =
                               (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
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
                    _lhsIfmtMode
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
                           _lhsOsem_steps :: PP_Doc
                           _stepsOfmtMode :: FormatMode
                           _lhsOlazyIntras :: (Set String)
                           _lhsOdefs :: (Set String)
                           _lhsOerrors :: (Seq Error)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOsync_steps :: PP_Doc
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           _stepsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                           _stepsOallInitStates :: (Map NontermIdent Int)
                           _stepsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                           _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                           _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                           _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                           _stepsOchildTypes :: (Map Identifier Type)
                           _stepsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _stepsOindex :: Int
                           _stepsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                           _stepsIsync_steps :: PP_Doc
                           _stepsIuses :: (Map String (Maybe NonLocalAttr))
                           _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 774, column 3)
                           _stepsOkind =
                               ({-# LINE 774 "src-ag/ExecutionPlan2Hs.ag" #-}
                                VisitPure ordered_
                                {-# LINE 7872 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 806, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 806 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _lhsIfmtMode of
                                  FormatDo -> "let" >#< _stepsIsem_steps
                                  _        -> _stepsIsem_steps
                                {-# LINE 7880 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 827, column 3)
                           _stepsOfmtMode =
                               ({-# LINE 827 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _lhsIfmtMode of
                                  FormatDo      -> FormatLetDecl
                                  mode          -> mode
                                {-# LINE 7888 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1365, column 3)
                           _lhsOlazyIntras =
                               ({-# LINE 1365 "src-ag/ExecutionPlan2Hs.ag" #-}
                                if ordered_
                                then _stepsIlazyIntras
                                else _stepsIdefs
                                {-# LINE 7896 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1339, column 38)
                           _lhsOdefs =
                               ({-# LINE 1339 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIdefs
                                {-# LINE 7902 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                           _lhsOerrors =
                               ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIerrors
                                {-# LINE 7908 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIruleKinds
                                {-# LINE 7914 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIruleUsage
                                {-# LINE 7920 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                           _lhsOsync_steps =
                               ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIsync_steps
                                {-# LINE 7926 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1340, column 38)
                           _lhsOuses =
                               ({-# LINE 1340 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIuses
                                {-# LINE 7932 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIvisitKinds
                                {-# LINE 7938 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOindex =
                               ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIindex
                                {-# LINE 7944 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOisLast =
                               ({-# LINE 873 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIisLast
                                {-# LINE 7950 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIprevMaxSimRefs
                                {-# LINE 7956 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallFromToStates =
                               ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallFromToStates
                                {-# LINE 7962 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallInitStates =
                               ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 7968 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallVisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallVisitKinds
                                {-# LINE 7974 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallchildvisit =
                               ({-# LINE 1189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallchildvisit
                                {-# LINE 7980 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisitdefs =
                               ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisitdefs
                                {-# LINE 7986 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisituses =
                               ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisituses
                                {-# LINE 7992 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildTypes =
                               ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIchildTypes
                                {-# LINE 7998 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildintros =
                               ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIchildintros
                                {-# LINE 8004 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOindex =
                               ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIindex
                                {-# LINE 8010 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOmrules =
                               ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImrules
                                {-# LINE 8016 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOoptions =
                               ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 8022 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 8028 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruledefs =
                               ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruledefs
                                {-# LINE 8034 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruleuses =
                               ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruleuses
                                {-# LINE 8040 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOuseParallel =
                               ({-# LINE 881 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIuseParallel
                                {-# LINE 8046 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIsync_steps,_stepsIuses,_stepsIvisitKinds) =
                               steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                           ___node =
                               (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
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
                    _lhsIfmtMode
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
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOdefs :: (Set String)
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOlazyIntras :: (Set String)
                           _lhsOsem_steps :: PP_Doc
                           _lhsOsync_steps :: PP_Doc
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOindex :: Int
                           _lhsOisLast :: Bool
                           _lhsOprevMaxSimRefs :: Int
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 779, column 16)
                           _ruleItf =
                               ({-# LINE 779 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.findWithDefault (error $ "Rule "  ++ show name_  ++ " not found") name_ _lhsImrules
                                {-# LINE 8090 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 780, column 16)
                           (_lhsOerrors,_sem_steps) =
                               ({-# LINE 780 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _ruleItf     _lhsIkind _lhsIfmtMode of
                                  Left e     -> (Seq.singleton e, empty)
                                  Right stmt -> (Seq.empty, stmt)
                                {-# LINE 8098 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1247, column 32)
                           _lhsOruleUsage =
                               ({-# LINE 1247 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.singleton name_ 1
                                {-# LINE 8104 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1257, column 3)
                           _lhsOruleKinds =
                               ({-# LINE 1257 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Map.singleton name_ (Set.singleton _lhsIkind)
                                {-# LINE 8110 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1342, column 16)
                           _lhsOdefs =
                               ({-# LINE 1342 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruledefs
                                {-# LINE 8116 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 1343, column 16)
                           _lhsOuses =
                               ({-# LINE 1343 "src-ag/ExecutionPlan2Hs.ag" #-}
                                maybe (error "Rule not found") id $ Map.lookup name_ _lhsIruleuses
                                {-# LINE 8122 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                Set.empty
                                {-# LINE 8128 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 777, column 43)
                           _lhsOsem_steps =
                               ({-# LINE 777 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _sem_steps
                                {-# LINE 8134 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                           _lhsOsync_steps =
                               ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                empty
                                {-# LINE 8140 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                mempty
                                {-# LINE 8146 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOindex =
                               ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIindex
                                {-# LINE 8152 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOisLast =
                               ({-# LINE 873 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIisLast
                                {-# LINE 8158 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (chain)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 8164 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ___node =
                               (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
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
                    _lhsIfmtMode
                    _lhsIindex
                    _lhsIisLast
                    _lhsIkind
                    _lhsImrules
                    _lhsIoptions
                    _lhsIprevMaxSimRefs
                    _lhsIruledefs
                    _lhsIruleuses
                    _lhsIuseParallel ->
                      (let _lhsOsem_steps :: PP_Doc
                           _stepsOindex :: Int
                           _lhsOindex :: Int
                           _lhsOprevMaxSimRefs :: Int
                           _lhsOdefs :: (Set String)
                           _lhsOerrors :: (Seq Error)
                           _lhsOlazyIntras :: (Set String)
                           _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                           _lhsOruleUsage :: (Map Identifier Int)
                           _lhsOsync_steps :: PP_Doc
                           _lhsOuses :: (Map String (Maybe NonLocalAttr))
                           _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                           _lhsOisLast :: Bool
                           _stepsOallFromToStates :: (Map VisitIdentifier (Int,Int))
                           _stepsOallInitStates :: (Map NontermIdent Int)
                           _stepsOallVisitKinds :: (Map VisitIdentifier VisitKind)
                           _stepsOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                           _stepsOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                           _stepsOavisituses :: (Map VisitIdentifier (Set Identifier))
                           _stepsOchildTypes :: (Map Identifier Type)
                           _stepsOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                           _stepsOfmtMode :: FormatMode
                           _stepsOkind :: VisitKind
                           _stepsOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                           _stepsIsync_steps :: PP_Doc
                           _stepsIuses :: (Map String (Maybe NonLocalAttr))
                           _stepsIvisitKinds :: (Map VisitIdentifier VisitKind)
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 805, column 16)
                           _lhsOsem_steps =
                               ({-# LINE 805 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIsem_steps >-< _stepsIsync_steps
                                {-# LINE 8236 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 860, column 22)
                           _stepsOindex =
                               ({-# LINE 860 "src-ag/ExecutionPlan2Hs.ag" #-}
                                0
                                {-# LINE 8242 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 861, column 22)
                           _lhsOindex =
                               ({-# LINE 861 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIindex
                                {-# LINE 8248 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 868, column 3)
                           _lhsOprevMaxSimRefs =
                               ({-# LINE 868 "src-ag/ExecutionPlan2Hs.ag" #-}
                                if _useParallel
                                then _lhsIprevMaxSimRefs `max` (_stepsIindex - 1)
                                else _lhsIprevMaxSimRefs
                                {-# LINE 8256 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 883, column 22)
                           _useParallel =
                               ({-# LINE 883 "src-ag/ExecutionPlan2Hs.ag" #-}
                                parallelInvoke _lhsIoptions && _stepsIsize > 1 && _isMonadic
                                {-# LINE 8262 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- "src-ag/ExecutionPlan2Hs.ag"(line 884, column 22)
                           _isMonadic =
                               ({-# LINE 884 "src-ag/ExecutionPlan2Hs.ag" #-}
                                case _lhsIkind of
                                  VisitMonadic -> True
                                  _            -> False
                                {-# LINE 8270 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1339, column 38)
                           _lhsOdefs =
                               ({-# LINE 1339 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIdefs
                                {-# LINE 8276 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                           _lhsOerrors =
                               ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIerrors
                                {-# LINE 8282 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                           _lhsOlazyIntras =
                               ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIlazyIntras
                                {-# LINE 8288 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                           _lhsOruleKinds =
                               ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIruleKinds
                                {-# LINE 8294 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                           _lhsOruleUsage =
                               ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIruleUsage
                                {-# LINE 8300 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                           _lhsOsync_steps =
                               ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIsync_steps
                                {-# LINE 8306 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1340, column 38)
                           _lhsOuses =
                               ({-# LINE 1340 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIuses
                                {-# LINE 8312 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                           _lhsOvisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIvisitKinds
                                {-# LINE 8318 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (up)
                           _lhsOisLast =
                               ({-# LINE 873 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _stepsIisLast
                                {-# LINE 8324 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallFromToStates =
                               ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallFromToStates
                                {-# LINE 8330 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallInitStates =
                               ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallInitStates
                                {-# LINE 8336 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallVisitKinds =
                               ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallVisitKinds
                                {-# LINE 8342 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOallchildvisit =
                               ({-# LINE 1189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIallchildvisit
                                {-# LINE 8348 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisitdefs =
                               ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisitdefs
                                {-# LINE 8354 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOavisituses =
                               ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIavisituses
                                {-# LINE 8360 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildTypes =
                               ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIchildTypes
                                {-# LINE 8366 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOchildintros =
                               ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIchildintros
                                {-# LINE 8372 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOfmtMode =
                               ({-# LINE 819 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIfmtMode
                                {-# LINE 8378 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOkind =
                               ({-# LINE 769 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIkind
                                {-# LINE 8384 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOmrules =
                               ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsImrules
                                {-# LINE 8390 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOoptions =
                               ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIoptions
                                {-# LINE 8396 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOprevMaxSimRefs =
                               ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIprevMaxSimRefs
                                {-# LINE 8402 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruledefs =
                               ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruledefs
                                {-# LINE 8408 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (down)
                           _stepsOruleuses =
                               ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _lhsIruleuses
                                {-# LINE 8414 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           -- copy rule (from local)
                           _stepsOuseParallel =
                               ({-# LINE 881 "src-ag/ExecutionPlan2Hs.ag" #-}
                                _useParallel
                                {-# LINE 8420 "dist/build/ExecutionPlan2Hs" #-}
                                )
                           ( _stepsIdefs,_stepsIerrors,_stepsIindex,_stepsIisLast,_stepsIlazyIntras,_stepsIprevMaxSimRefs,_stepsIruleKinds,_stepsIruleUsage,_stepsIsem_steps,_stepsIsize,_stepsIsync_steps,_stepsIuses,_stepsIvisitKinds) =
                               steps_ _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOallchildvisit _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOchildintros _stepsOfmtMode _stepsOindex _stepsOkind _stepsOmrules _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses _stepsOuseParallel
                           ___node =
                               (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                       in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
-- VisitSteps --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInitStates        : Map NontermIdent Int
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         fmtMode              : FormatMode
         kind                 : VisitKind
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
         sync_steps           : PP_Doc
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
                                     (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                                     (Map VisitIdentifier (Set Identifier)) ->
                                     (Map VisitIdentifier (Set Identifier)) ->
                                     (Map Identifier Type) ->
                                     (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                                     FormatMode ->
                                     Int ->
                                     VisitKind ->
                                     (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                                     Options ->
                                     Int ->
                                     (Map Identifier (Set String)) ->
                                     (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                                     Bool ->
                                     ( (Set String),(Seq Error),Int,Bool,(Set String),Int,(Map Identifier (Set VisitKind)),(Map Identifier Int),PP_Doc,Int,PP_Doc,(Map String (Maybe NonLocalAttr)),(Map VisitIdentifier VisitKind)))
data Inh_VisitSteps = Inh_VisitSteps {allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)),allInitStates_Inh_VisitSteps :: (Map NontermIdent Int),allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_VisitSteps :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_VisitSteps :: (Map Identifier Type),childintros_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),fmtMode_Inh_VisitSteps :: FormatMode,index_Inh_VisitSteps :: Int,kind_Inh_VisitSteps :: VisitKind,mrules_Inh_VisitSteps :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),options_Inh_VisitSteps :: Options,prevMaxSimRefs_Inh_VisitSteps :: Int,ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)),ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))),useParallel_Inh_VisitSteps :: Bool}
data Syn_VisitSteps = Syn_VisitSteps {defs_Syn_VisitSteps :: (Set String),errors_Syn_VisitSteps :: (Seq Error),index_Syn_VisitSteps :: Int,isLast_Syn_VisitSteps :: Bool,lazyIntras_Syn_VisitSteps :: (Set String),prevMaxSimRefs_Syn_VisitSteps :: Int,ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_VisitSteps :: (Map Identifier Int),sem_steps_Syn_VisitSteps :: PP_Doc,size_Syn_VisitSteps :: Int,sync_steps_Syn_VisitSteps :: PP_Doc,uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)),visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind)}
wrap_VisitSteps :: T_VisitSteps ->
                   Inh_VisitSteps ->
                   Syn_VisitSteps
wrap_VisitSteps (T_VisitSteps sem) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel) =
    (let ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds) = sem _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIallchildvisit _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIchildintros _lhsIfmtMode _lhsIindex _lhsIkind _lhsImrules _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses _lhsIuseParallel
     in  (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOuses _lhsOvisitKinds))
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
                     _lhsIfmtMode
                     _lhsIindex
                     _lhsIkind
                     _lhsImrules
                     _lhsIoptions
                     _lhsIprevMaxSimRefs
                     _lhsIruledefs
                     _lhsIruleuses
                     _lhsIuseParallel ->
                       (let _lhsOsize :: Int
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
                            _lhsOsem_steps :: PP_Doc
                            _lhsOsync_steps :: PP_Doc
                            _lhsOuses :: (Map String (Maybe NonLocalAttr))
                            _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                            _lhsOprevMaxSimRefs :: Int
                            _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                            _hdOallInitStates :: (Map NontermIdent Int)
                            _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                            _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                            _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                            _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                            _hdOchildTypes :: (Map Identifier Type)
                            _hdOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                            _hdOfmtMode :: FormatMode
                            _hdOkind :: VisitKind
                            _hdOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
                            _hdOoptions :: Options
                            _hdOprevMaxSimRefs :: Int
                            _hdOruledefs :: (Map Identifier (Set String))
                            _hdOruleuses :: (Map Identifier (Map String (Maybe NonLocalAttr)))
                            _hdOuseParallel :: Bool
                            _tlOallFromToStates :: (Map VisitIdentifier (Int,Int))
                            _tlOallInitStates :: (Map NontermIdent Int)
                            _tlOallVisitKinds :: (Map VisitIdentifier VisitKind)
                            _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                            _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                            _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                            _tlOchildTypes :: (Map Identifier Type)
                            _tlOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                            _tlOfmtMode :: FormatMode
                            _tlOkind :: VisitKind
                            _tlOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                            _hdIsync_steps :: PP_Doc
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
                            _tlIsync_steps :: PP_Doc
                            _tlIuses :: (Map String (Maybe NonLocalAttr))
                            _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 851, column 10)
                            _lhsOsize =
                                ({-# LINE 851 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 1 + _tlIsize
                                 {-# LINE 8597 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 856, column 3)
                            _hdOindex =
                                ({-# LINE 856 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIindex
                                 {-# LINE 8603 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 857, column 3)
                            _tlOindex =
                                ({-# LINE 857 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 1 + _lhsIindex
                                 {-# LINE 8609 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 858, column 3)
                            _lhsOindex =
                                ({-# LINE 858 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _tlIindex
                                 {-# LINE 8615 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 877, column 11)
                            _lhsOisLast =
                                ({-# LINE 877 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 False
                                 {-# LINE 8621 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 878, column 11)
                            _hdOisLast =
                                ({-# LINE 878 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _tlIisLast
                                 {-# LINE 8627 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1339, column 38)
                            _lhsOdefs =
                                ({-# LINE 1339 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIdefs `Set.union` _tlIdefs
                                 {-# LINE 8633 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                            _lhsOerrors =
                                ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIerrors Seq.>< _tlIerrors
                                 {-# LINE 8639 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                            _lhsOlazyIntras =
                                ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIlazyIntras `Set.union` _tlIlazyIntras
                                 {-# LINE 8645 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                            _lhsOruleKinds =
                                ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIruleKinds `unionWithMappend` _tlIruleKinds
                                 {-# LINE 8651 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                            _lhsOruleUsage =
                                ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIruleUsage `unionWithSum` _tlIruleUsage
                                 {-# LINE 8657 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 777, column 43)
                            _lhsOsem_steps =
                                ({-# LINE 777 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIsem_steps >-< _tlIsem_steps
                                 {-# LINE 8663 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                            _lhsOsync_steps =
                                ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIsync_steps >-< _tlIsync_steps
                                 {-# LINE 8669 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1340, column 38)
                            _lhsOuses =
                                ({-# LINE 1340 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIuses `Map.union` _tlIuses
                                 {-# LINE 8675 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                            _lhsOvisitKinds =
                                ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIvisitKinds `mappend` _tlIvisitKinds
                                 {-# LINE 8681 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (up)
                            _lhsOprevMaxSimRefs =
                                ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _tlIprevMaxSimRefs
                                 {-# LINE 8687 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallFromToStates =
                                ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallFromToStates
                                 {-# LINE 8693 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallInitStates =
                                ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallInitStates
                                 {-# LINE 8699 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallVisitKinds =
                                ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallVisitKinds
                                 {-# LINE 8705 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOallchildvisit =
                                ({-# LINE 1189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 8711 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisitdefs =
                                ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 8717 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOavisituses =
                                ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 8723 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOchildTypes =
                                ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildTypes
                                 {-# LINE 8729 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOchildintros =
                                ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 8735 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOfmtMode =
                                ({-# LINE 819 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIfmtMode
                                 {-# LINE 8741 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOkind =
                                ({-# LINE 769 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIkind
                                 {-# LINE 8747 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOmrules =
                                ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsImrules
                                 {-# LINE 8753 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOoptions =
                                ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 8759 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOprevMaxSimRefs =
                                ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIprevMaxSimRefs
                                 {-# LINE 8765 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruledefs =
                                ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 8771 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOruleuses =
                                ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 8777 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _hdOuseParallel =
                                ({-# LINE 881 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIuseParallel
                                 {-# LINE 8783 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallFromToStates =
                                ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallFromToStates
                                 {-# LINE 8789 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallInitStates =
                                ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallInitStates
                                 {-# LINE 8795 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallVisitKinds =
                                ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallVisitKinds
                                 {-# LINE 8801 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOallchildvisit =
                                ({-# LINE 1189 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIallchildvisit
                                 {-# LINE 8807 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisitdefs =
                                ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisitdefs
                                 {-# LINE 8813 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOavisituses =
                                ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIavisituses
                                 {-# LINE 8819 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOchildTypes =
                                ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildTypes
                                 {-# LINE 8825 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOchildintros =
                                ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIchildintros
                                 {-# LINE 8831 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOfmtMode =
                                ({-# LINE 819 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIfmtMode
                                 {-# LINE 8837 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOkind =
                                ({-# LINE 769 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIkind
                                 {-# LINE 8843 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOmrules =
                                ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsImrules
                                 {-# LINE 8849 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOoptions =
                                ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 8855 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (chain)
                            _tlOprevMaxSimRefs =
                                ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _hdIprevMaxSimRefs
                                 {-# LINE 8861 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruledefs =
                                ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruledefs
                                 {-# LINE 8867 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOruleuses =
                                ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIruleuses
                                 {-# LINE 8873 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (down)
                            _tlOuseParallel =
                                ({-# LINE 881 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIuseParallel
                                 {-# LINE 8879 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            ( _hdIdefs,_hdIerrors,_hdIindex,_hdIisLast,_hdIlazyIntras,_hdIprevMaxSimRefs,_hdIruleKinds,_hdIruleUsage,_hdIsem_steps,_hdIsync_steps,_hdIuses,_hdIvisitKinds) =
                                hd_ _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOallchildvisit _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOfmtMode _hdOindex _hdOisLast _hdOkind _hdOmrules _hdOoptions _hdOprevMaxSimRefs _hdOruledefs _hdOruleuses _hdOuseParallel
                            ( _tlIdefs,_tlIerrors,_tlIindex,_tlIisLast,_tlIlazyIntras,_tlIprevMaxSimRefs,_tlIruleKinds,_tlIruleUsage,_tlIsem_steps,_tlIsize,_tlIsync_steps,_tlIuses,_tlIvisitKinds) =
                                tl_ _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOallchildvisit _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOfmtMode _tlOindex _tlOkind _tlOmrules _tlOoptions _tlOprevMaxSimRefs _tlOruledefs _tlOruleuses _tlOuseParallel
                            ___node =
                                (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                        in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
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
                     _lhsIfmtMode
                     _lhsIindex
                     _lhsIkind
                     _lhsImrules
                     _lhsIoptions
                     _lhsIprevMaxSimRefs
                     _lhsIruledefs
                     _lhsIruleuses
                     _lhsIuseParallel ->
                       (let _lhsOsize :: Int
                            _lhsOisLast :: Bool
                            _lhsOdefs :: (Set String)
                            _lhsOerrors :: (Seq Error)
                            _lhsOlazyIntras :: (Set String)
                            _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                            _lhsOruleUsage :: (Map Identifier Int)
                            _lhsOsem_steps :: PP_Doc
                            _lhsOsync_steps :: PP_Doc
                            _lhsOuses :: (Map String (Maybe NonLocalAttr))
                            _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                            _lhsOindex :: Int
                            _lhsOprevMaxSimRefs :: Int
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 850, column 10)
                            _lhsOsize =
                                ({-# LINE 850 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 0
                                 {-# LINE 8924 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- "src-ag/ExecutionPlan2Hs.ag"(line 876, column 11)
                            _lhsOisLast =
                                ({-# LINE 876 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 True
                                 {-# LINE 8930 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1339, column 38)
                            _lhsOdefs =
                                ({-# LINE 1339 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Set.empty
                                 {-# LINE 8936 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                            _lhsOerrors =
                                ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Seq.empty
                                 {-# LINE 8942 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                            _lhsOlazyIntras =
                                ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Set.empty
                                 {-# LINE 8948 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                            _lhsOruleKinds =
                                ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Map.empty
                                 {-# LINE 8954 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                            _lhsOruleUsage =
                                ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Map.empty
                                 {-# LINE 8960 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 777, column 43)
                            _lhsOsem_steps =
                                ({-# LINE 777 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 empty
                                 {-# LINE 8966 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 810, column 44)
                            _lhsOsync_steps =
                                ({-# LINE 810 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 empty
                                 {-# LINE 8972 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1340, column 38)
                            _lhsOuses =
                                ({-# LINE 1340 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 Map.empty
                                 {-# LINE 8978 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                            _lhsOvisitKinds =
                                ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 mempty
                                 {-# LINE 8984 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (chain)
                            _lhsOindex =
                                ({-# LINE 854 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIindex
                                 {-# LINE 8990 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            -- copy rule (chain)
                            _lhsOprevMaxSimRefs =
                                ({-# LINE 865 "src-ag/ExecutionPlan2Hs.ag" #-}
                                 _lhsIprevMaxSimRefs
                                 {-# LINE 8996 "dist/build/ExecutionPlan2Hs" #-}
                                 )
                            ___node =
                                (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsem_steps _lhsOsize _lhsOsync_steps _lhsOuses _lhsOvisitKinds)
                        in  ( _lhsOdefs,_lhsOerrors,_lhsOindex,_lhsOisLast,_lhsOlazyIntras,_lhsOprevMaxSimRefs,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_steps,_lhsOsize,_lhsOsync_steps,_lhsOuses,_lhsOvisitKinds))))
-- Visits ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFromToStates      : Map VisitIdentifier (Int,Int)
         allInhmap            : Map NontermIdent Attributes
         allInitStates        : Map NontermIdent Int
         allSynmap            : Map NontermIdent Attributes
         allVisitKinds        : Map VisitIdentifier VisitKind
         allchildvisit        : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         allintramap          : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         avisitdefs           : Map VisitIdentifier (Set Identifier)
         avisituses           : Map VisitIdentifier (Set Identifier)
         childTypes           : Map Identifier Type
         childintros          : Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))
         con                  : ConstructorIdent
         inhmap               : Attributes
         mrules               : Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)
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
         childvisit           : Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))
         errors               : Seq Error
         fromToStates         : Map VisitIdentifier (Int,Int)
         intramap             : Map StateIdentifier (Map String (Maybe NonLocalAttr))
         lazyIntras           : Set String
         ruleKinds            : Map Identifier (Set VisitKind)
         ruleUsage            : Map Identifier Int
         sem_visit            :  [(StateIdentifier,Bool -> PP_Doc)] 
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
                             (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))) ->
                             (Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
                             (Map VisitIdentifier (Set Identifier)) ->
                             (Map VisitIdentifier (Set Identifier)) ->
                             (Map Identifier Type) ->
                             (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))) ->
                             ConstructorIdent ->
                             Attributes ->
                             (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)) ->
                             (Map StateIdentifier StateCtx) ->
                             NontermIdent ->
                             Options ->
                             ([Identifier]) ->
                             (Map StateIdentifier StateCtx) ->
                             (Map Identifier (Set String)) ->
                             (Map Identifier (Map String (Maybe NonLocalAttr))) ->
                             Attributes ->
                             (Set String) ->
                             ( ([VisitStateState]),(Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),(Seq Error),(Map VisitIdentifier (Int,Int)),(Map StateIdentifier (Map String (Maybe NonLocalAttr))),(Set String),(Map Identifier (Set VisitKind)),(Map Identifier Int),( [(StateIdentifier,Bool -> PP_Doc)] ),PP_Doc,(Map VisitIdentifier VisitKind),(Map VisitIdentifier (Set Identifier)),(Map VisitIdentifier (Set Identifier))))
data Inh_Visits = Inh_Visits {allFromToStates_Inh_Visits :: (Map VisitIdentifier (Int,Int)),allInhmap_Inh_Visits :: (Map NontermIdent Attributes),allInitStates_Inh_Visits :: (Map NontermIdent Int),allSynmap_Inh_Visits :: (Map NontermIdent Attributes),allVisitKinds_Inh_Visits :: (Map VisitIdentifier VisitKind),allchildvisit_Inh_Visits :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),allintramap_Inh_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),avisitdefs_Inh_Visits :: (Map VisitIdentifier (Set Identifier)),avisituses_Inh_Visits :: (Map VisitIdentifier (Set Identifier)),childTypes_Inh_Visits :: (Map Identifier Type),childintros_Inh_Visits :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr)))),con_Inh_Visits :: ConstructorIdent,inhmap_Inh_Visits :: Attributes,mrules_Inh_Visits :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc)),nextVisits_Inh_Visits :: (Map StateIdentifier StateCtx),nt_Inh_Visits :: NontermIdent,options_Inh_Visits :: Options,params_Inh_Visits :: ([Identifier]),prevVisits_Inh_Visits :: (Map StateIdentifier StateCtx),ruledefs_Inh_Visits :: (Map Identifier (Set String)),ruleuses_Inh_Visits :: (Map Identifier (Map String (Maybe NonLocalAttr))),synmap_Inh_Visits :: Attributes,terminaldefs_Inh_Visits :: (Set String)}
data Syn_Visits = Syn_Visits {allvisits_Syn_Visits :: ([VisitStateState]),childvisit_Syn_Visits :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc))),errors_Syn_Visits :: (Seq Error),fromToStates_Syn_Visits :: (Map VisitIdentifier (Int,Int)),intramap_Syn_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))),lazyIntras_Syn_Visits :: (Set String),ruleKinds_Syn_Visits :: (Map Identifier (Set VisitKind)),ruleUsage_Syn_Visits :: (Map Identifier Int),sem_visit_Syn_Visits :: ( [(StateIdentifier,Bool -> PP_Doc)] ),t_visits_Syn_Visits :: PP_Doc,visitKinds_Syn_Visits :: (Map VisitIdentifier VisitKind),visitdefs_Syn_Visits :: (Map VisitIdentifier (Set Identifier)),visituses_Syn_Visits :: (Map VisitIdentifier (Set Identifier))}
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
                        _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _lhsOerrors :: (Seq Error)
                        _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                        _lhsOintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _lhsOlazyIntras :: (Set String)
                        _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                        _lhsOruleUsage :: (Map Identifier Int)
                        _lhsOsem_visit :: ( [(StateIdentifier,Bool -> PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                        _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                        _hdOallFromToStates :: (Map VisitIdentifier (Int,Int))
                        _hdOallInhmap :: (Map NontermIdent Attributes)
                        _hdOallInitStates :: (Map NontermIdent Int)
                        _hdOallSynmap :: (Map NontermIdent Attributes)
                        _hdOallVisitKinds :: (Map VisitIdentifier VisitKind)
                        _hdOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _hdOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _hdOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _hdOavisituses :: (Map VisitIdentifier (Set Identifier))
                        _hdOchildTypes :: (Map Identifier Type)
                        _hdOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _hdOcon :: ConstructorIdent
                        _hdOinhmap :: Attributes
                        _hdOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                        _tlOallchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _tlOallintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _tlOavisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _tlOavisituses :: (Map VisitIdentifier (Set Identifier))
                        _tlOchildTypes :: (Map Identifier Type)
                        _tlOchildintros :: (Map Identifier (VisitKind -> FormatMode -> Either Error (PP_Doc, Set String, Map String (Maybe NonLocalAttr))))
                        _tlOcon :: ConstructorIdent
                        _tlOinhmap :: Attributes
                        _tlOmrules :: (Map Identifier (VisitKind -> FormatMode -> Either Error PP_Doc))
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
                        _hdIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _hdIerrors :: (Seq Error)
                        _hdIfromToStates :: (Map VisitIdentifier (Int,Int))
                        _hdIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _hdIlazyIntras :: (Set String)
                        _hdIruleKinds :: (Map Identifier (Set VisitKind))
                        _hdIruleUsage :: (Map Identifier Int)
                        _hdIsem_visit :: (  (StateIdentifier,Bool -> PP_Doc)  )
                        _hdIt_visits :: PP_Doc
                        _hdIvisitKinds :: (Map VisitIdentifier VisitKind)
                        _hdIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _hdIvisituses :: (Map VisitIdentifier (Set Identifier))
                        _tlIallvisits :: ([VisitStateState])
                        _tlIchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _tlIerrors :: (Seq Error)
                        _tlIfromToStates :: (Map VisitIdentifier (Int,Int))
                        _tlIintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _tlIlazyIntras :: (Set String)
                        _tlIruleKinds :: (Map Identifier (Set VisitKind))
                        _tlIruleUsage :: (Map Identifier Int)
                        _tlIsem_visit :: ( [(StateIdentifier,Bool -> PP_Doc)] )
                        _tlIt_visits :: PP_Doc
                        _tlIvisitKinds :: (Map VisitIdentifier VisitKind)
                        _tlIvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _tlIvisituses :: (Map VisitIdentifier (Set Identifier))
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 336, column 29)
                        _lhsOallvisits =
                            ({-# LINE 336 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIallvisits : _tlIallvisits
                             {-# LINE 9202 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIchildvisit `Map.union` _tlIchildvisit
                             {-# LINE 9208 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                        _lhsOerrors =
                            ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIerrors Seq.>< _tlIerrors
                             {-# LINE 9214 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                        _lhsOfromToStates =
                            ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIfromToStates `mappend` _tlIfromToStates
                             {-# LINE 9220 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1281, column 34)
                        _lhsOintramap =
                            ({-# LINE 1281 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIintramap `uwMapUnion` _tlIintramap
                             {-# LINE 9226 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                        _lhsOlazyIntras =
                            ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIlazyIntras `Set.union` _tlIlazyIntras
                             {-# LINE 9232 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                        _lhsOruleKinds =
                            ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruleKinds `unionWithMappend` _tlIruleKinds
                             {-# LINE 9238 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                        _lhsOruleUsage =
                            ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIruleUsage `unionWithSum` _tlIruleUsage
                             {-# LINE 9244 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 706, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 706 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIsem_visit : _tlIsem_visit
                             {-# LINE 9250 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 394, column 59)
                        _lhsOt_visits =
                            ({-# LINE 394 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIt_visits >-< _tlIt_visits
                             {-# LINE 9256 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                        _lhsOvisitKinds =
                            ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIvisitKinds `mappend` _tlIvisitKinds
                             {-# LINE 9262 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
                             {-# LINE 9268 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                        _lhsOvisituses =
                            ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _hdIvisituses `uwSetUnion` _tlIvisituses
                             {-# LINE 9274 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallFromToStates =
                            ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallFromToStates
                             {-# LINE 9280 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInhmap =
                            ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 9286 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallInitStates =
                            ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInitStates
                             {-# LINE 9292 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallSynmap =
                            ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 9298 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallVisitKinds =
                            ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallVisitKinds
                             {-# LINE 9304 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallchildvisit =
                            ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 9310 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOallintramap =
                            ({-# LINE 1280 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallintramap
                             {-# LINE 9316 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisitdefs =
                            ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 9322 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOavisituses =
                            ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisituses
                             {-# LINE 9328 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildTypes =
                            ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 9334 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOchildintros =
                            ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildintros
                             {-# LINE 9340 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOcon =
                            ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIcon
                             {-# LINE 9346 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOinhmap =
                            ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 9352 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOmrules =
                            ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImrules
                             {-# LINE 9358 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnextVisits =
                            ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInextVisits
                             {-# LINE 9364 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOnt =
                            ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 9370 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 9376 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOparams =
                            ({-# LINE 73 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIparams
                             {-# LINE 9382 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOprevVisits =
                            ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIprevVisits
                             {-# LINE 9388 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruledefs =
                            ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruledefs
                             {-# LINE 9394 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOruleuses =
                            ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleuses
                             {-# LINE 9400 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOsynmap =
                            ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 9406 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _hdOterminaldefs =
                            ({-# LINE 1283 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 9412 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallFromToStates =
                            ({-# LINE 1547 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallFromToStates
                             {-# LINE 9418 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInhmap =
                            ({-# LINE 321 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInhmap
                             {-# LINE 9424 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallInitStates =
                            ({-# LINE 1605 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallInitStates
                             {-# LINE 9430 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallSynmap =
                            ({-# LINE 322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallSynmap
                             {-# LINE 9436 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallVisitKinds =
                            ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallVisitKinds
                             {-# LINE 9442 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallchildvisit =
                            ({-# LINE 1186 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallchildvisit
                             {-# LINE 9448 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOallintramap =
                            ({-# LINE 1280 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIallintramap
                             {-# LINE 9454 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisitdefs =
                            ({-# LINE 1332 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisitdefs
                             {-# LINE 9460 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOavisituses =
                            ({-# LINE 1333 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIavisituses
                             {-# LINE 9466 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildTypes =
                            ({-# LINE 1564 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildTypes
                             {-# LINE 9472 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOchildintros =
                            ({-# LINE 893 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIchildintros
                             {-# LINE 9478 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOcon =
                            ({-# LINE 66 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIcon
                             {-# LINE 9484 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOinhmap =
                            ({-# LINE 319 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIinhmap
                             {-# LINE 9490 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOmrules =
                            ({-# LINE 776 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsImrules
                             {-# LINE 9496 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnextVisits =
                            ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInextVisits
                             {-# LINE 9502 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOnt =
                            ({-# LINE 61 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsInt
                             {-# LINE 9508 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 51 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIoptions
                             {-# LINE 9514 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOparams =
                            ({-# LINE 73 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIparams
                             {-# LINE 9520 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOprevVisits =
                            ({-# LINE 1533 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIprevVisits
                             {-# LINE 9526 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruledefs =
                            ({-# LINE 1307 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruledefs
                             {-# LINE 9532 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOruleuses =
                            ({-# LINE 1308 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIruleuses
                             {-# LINE 9538 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOsynmap =
                            ({-# LINE 320 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIsynmap
                             {-# LINE 9544 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- copy rule (down)
                        _tlOterminaldefs =
                            ({-# LINE 1283 "src-ag/ExecutionPlan2Hs.ag" #-}
                             _lhsIterminaldefs
                             {-# LINE 9550 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ( _hdIallvisits,_hdIchildvisit,_hdIerrors,_hdIfromToStates,_hdIintramap,_hdIlazyIntras,_hdIruleKinds,_hdIruleUsage,_hdIsem_visit,_hdIt_visits,_hdIvisitKinds,_hdIvisitdefs,_hdIvisituses) =
                            hd_ _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallchildvisit _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOchildintros _hdOcon _hdOinhmap _hdOmrules _hdOnextVisits _hdOnt _hdOoptions _hdOparams _hdOprevVisits _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs
                        ( _tlIallvisits,_tlIchildvisit,_tlIerrors,_tlIfromToStates,_tlIintramap,_tlIlazyIntras,_tlIruleKinds,_tlIruleUsage,_tlIsem_visit,_tlIt_visits,_tlIvisitKinds,_tlIvisitdefs,_tlIvisituses) =
                            tl_ _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallchildvisit _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOchildintros _tlOcon _tlOinhmap _tlOmrules _tlOnextVisits _tlOnt _tlOoptions _tlOparams _tlOprevVisits _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs
                        ___node =
                            (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
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
                        _lhsOchildvisit :: (Map VisitIdentifier (Identifier -> VisitKind -> Either Error (PP_Doc, PP_Doc)))
                        _lhsOerrors :: (Seq Error)
                        _lhsOfromToStates :: (Map VisitIdentifier (Int,Int))
                        _lhsOintramap :: (Map StateIdentifier (Map String (Maybe NonLocalAttr)))
                        _lhsOlazyIntras :: (Set String)
                        _lhsOruleKinds :: (Map Identifier (Set VisitKind))
                        _lhsOruleUsage :: (Map Identifier Int)
                        _lhsOsem_visit :: ( [(StateIdentifier,Bool -> PP_Doc)] )
                        _lhsOt_visits :: PP_Doc
                        _lhsOvisitKinds :: (Map VisitIdentifier VisitKind)
                        _lhsOvisitdefs :: (Map VisitIdentifier (Set Identifier))
                        _lhsOvisituses :: (Map VisitIdentifier (Set Identifier))
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 336, column 29)
                        _lhsOallvisits =
                            ({-# LINE 336 "src-ag/ExecutionPlan2Hs.ag" #-}
                             []
                             {-# LINE 9601 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1187, column 37)
                        _lhsOchildvisit =
                            ({-# LINE 1187 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9607 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1629, column 132)
                        _lhsOerrors =
                            ({-# LINE 1629 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Seq.empty
                             {-# LINE 9613 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1544, column 22)
                        _lhsOfromToStates =
                            ({-# LINE 1544 "src-ag/ExecutionPlan2Hs.ag" #-}
                             mempty
                             {-# LINE 9619 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1281, column 34)
                        _lhsOintramap =
                            ({-# LINE 1281 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9625 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1355, column 57)
                        _lhsOlazyIntras =
                            ({-# LINE 1355 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Set.empty
                             {-# LINE 9631 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1255, column 56)
                        _lhsOruleKinds =
                            ({-# LINE 1255 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9637 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1243, column 56)
                        _lhsOruleUsage =
                            ({-# LINE 1243 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9643 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 706, column 29)
                        _lhsOsem_visit =
                            ({-# LINE 706 "src-ag/ExecutionPlan2Hs.ag" #-}
                             []
                             {-# LINE 9649 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 394, column 59)
                        _lhsOt_visits =
                            ({-# LINE 394 "src-ag/ExecutionPlan2Hs.ag" #-}
                             empty
                             {-# LINE 9655 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1591, column 68)
                        _lhsOvisitKinds =
                            ({-# LINE 1591 "src-ag/ExecutionPlan2Hs.ag" #-}
                             mempty
                             {-# LINE 9661 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1322, column 36)
                        _lhsOvisitdefs =
                            ({-# LINE 1322 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9667 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        -- use rule "src-ag/ExecutionPlan2Hs.ag"(line 1323, column 36)
                        _lhsOvisituses =
                            ({-# LINE 1323 "src-ag/ExecutionPlan2Hs.ag" #-}
                             Map.empty
                             {-# LINE 9673 "dist/build/ExecutionPlan2Hs" #-}
                             )
                        ___node =
                            (Syn_Visits _lhsOallvisits _lhsOchildvisit _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOsem_visit _lhsOt_visits _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
                    in  ( _lhsOallvisits,_lhsOchildvisit,_lhsOerrors,_lhsOfromToStates,_lhsOintramap,_lhsOlazyIntras,_lhsOruleKinds,_lhsOruleUsage,_lhsOsem_visit,_lhsOt_visits,_lhsOvisitKinds,_lhsOvisitdefs,_lhsOvisituses))))