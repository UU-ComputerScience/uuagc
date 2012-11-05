

-- UUAGC 0.9.42.1 (src-ag/GenerateCode.ag)
module GenerateCode where
{-# LINE 9 "./src-ag/GenerateCode.ag" #-}

import CommonTypes
import SequentialTypes
import Code hiding (Type)
import qualified Code
import Options
import CodeSyntax
import ErrorMessages
import GrammarInfo
import DeclBlocks

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import UU.Scanner.Position
import TokenDef
import HsToken
import HsTokenScanner

import Data.List(partition,intersperse,intersect,(\\))
import Data.Maybe(fromJust,isJust)

import Debug.Trace
{-# LINE 33 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 41 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 48 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/DeclBlocks.ag" #-}

import Code (Decl,Expr)
{-# LINE 53 "dist/build/GenerateCode.hs" #-}
{-# LINE 107 "./src-ag/GenerateCode.ag" #-}

-- remove possible @v references in the types of a data type.
cleanupArg :: String -> String
cleanupArg s
  = case idEvalType (SimpleType s) of
      SimpleType s' -> s'
{-# LINE 61 "dist/build/GenerateCode.hs" #-}

{-# LINE 122 "./src-ag/GenerateCode.ag" #-}

appContext :: ContextMap -> NontermIdent -> Code.Type -> Code.Type
appContext mp nt tp
  = maybe tp (\ctx -> CtxApp (map (\(n,ns) -> (getName n, ns)) ctx) tp) $ Map.lookup nt mp

appQuant :: QuantMap -> NontermIdent -> Code.Type -> Code.Type
appQuant mp nt tp
  = foldr QuantApp tp $ Map.findWithDefault [] nt mp
{-# LINE 72 "dist/build/GenerateCode.hs" #-}

{-# LINE 247 "./src-ag/GenerateCode.ag" #-}

mkDecl True  lhs rhs _ _   = Bind lhs rhs
mkDecl False lhs rhs s1 s2 = Decl lhs rhs s1 s2

unwrapSem :: Bool -> NontermIdent -> Expr -> Expr
unwrapSem False _ e = e
unwrapSem True nm e = Case e alts
  where alts  = [CaseAlt left right]
        left  = Fun (typeName nm 0) [SimpleExpr "x"]
        right = SimpleExpr "x"
{-# LINE 85 "dist/build/GenerateCode.hs" #-}

{-# LINE 537 "./src-ag/GenerateCode.ag" #-}

mkLambdaArg :: String -> Maybe Code.Type -> Expr
mkLambdaArg nm Nothing = SimpleExpr nm
mkLambdaArg nm (Just tp) = TypedExpr (SimpleExpr nm) tp

mkLambda :: Exprs -> Expr -> Expr
mkLambda [] e = e
mkLambda xs e = Lambda xs e

mkSemFun :: Identifier -> Int -> Exprs -> Expr -> Expr
mkSemFun nt nr xs e = SemFun (typeName nt nr) xs e

typeAppStrs nm params = TypeApp (SimpleType nm) (map SimpleType params)

isHigherOrder :: ChildKind -> Bool
isHigherOrder ChildAttr = True
isHigherOrder _         = False

pickOrigType :: (Identifier, Type, ChildKind) -> (Identifier, Type, ChildKind)
pickOrigType (nm, _, virt@(ChildReplace x)) = (nm, x, virt)
pickOrigType x = x
{-# LINE 109 "dist/build/GenerateCode.hs" #-}

{-# LINE 633 "./src-ag/GenerateCode.ag" #-}

mkPartitionedFunction :: String -> Bool -> [Decl] -> [String] -> DeclBlocks -> ([Decl], Expr)
mkPartitionedFunction prefix optCase nextVisitDecls lastExprVars cpsTree
  = let inh = Inh_DeclBlocksRoot { prefix_Inh_DeclBlocksRoot = prefix
                                 , optCase_Inh_DeclBlocksRoot = optCase
                                 , nextVisitDecls_Inh_DeclBlocksRoot = nextVisitDecls
                                 , lastExprVars_Inh_DeclBlocksRoot = lastExprVars
                                 }
        sem = sem_DeclBlocksRoot (DeclBlocksRoot cpsTree)
        syn = wrap_DeclBlocksRoot sem inh
    in (lambdas_Syn_DeclBlocksRoot syn, firstCall_Syn_DeclBlocksRoot syn)
{-# LINE 123 "dist/build/GenerateCode.hs" #-}

{-# LINE 683 "./src-ag/GenerateCode.ag" #-}

freevars :: [String] -> [Decl] -> [String]
freevars additional decls
  = Set.toList (allused `Set.difference` alldefined)
  where
    allused = Set.unions (Set.fromList additional : map usedvars decls)
    alldefined = Set.unions (map definedvars decls)

    usedvars (Decl _ _ _ uses) = uses
    usedvars _                 = Set.empty

    definedvars (Decl _ _ defs _) = defs
    definedvars _                 = Set.empty

mkBlockLambda :: Bool -> String -> [String] -> [Decl] -> Expr -> Decl
mkBlockLambda optCase name args decls expr
  = Decl lhs rhs Set.empty Set.empty
  where
    lhs = Fun name (map SimpleExpr args)
    rhs = mkLet optCase decls expr
{-# LINE 146 "dist/build/GenerateCode.hs" #-}

{-# LINE 761 "./src-ag/GenerateCode.ag" #-}

typeToCodeType :: Maybe NontermIdent -> [String] -> Type -> Code.Type
typeToCodeType mbNt params tp
  = case tp of
      NT nt tps defor -> NontermType (getName nt) tps defor
      Haskell t       -> SimpleType t

evalType :: (String -> String) -> Code.Type -> Code.Type
evalType replf t
  = chase t
  where
    chase t
      = case t of
          Arr l r              -> Arr (chase l) (chase r)
          TypeApp f as         -> TypeApp (chase f) (map chase as)
          TupleType tps        -> TupleType (map chase tps)
          UnboxedTupleType tps -> UnboxedTupleType (map chase tps)
          Code.List tp         -> Code.List (chase tp)
          SimpleType txt       -> let tks  = lexTokens (initPos txt) txt
                                      tks' = map replaceTok tks
                                      txt' = unlines . showTokens . tokensToStrings $ tks'
                                  in SimpleType txt'
          TMaybe m             -> TMaybe (chase m)
          TEither l r          -> TEither (chase l) (chase r)
          TMap k v             -> TMap (chase k) (chase v)
          TIntMap v            -> TIntMap (chase v)
          _                    -> t

    replaceTok t
      = case t of
          AGLocal v p _ -> HsToken (replf $ getName v) p
          _             -> t

idEvalType :: Code.Type -> Code.Type
idEvalType = evalType id
{-# LINE 184 "dist/build/GenerateCode.hs" #-}

{-# LINE 885 "./src-ag/GenerateCode.ag" #-}

-- for a virtual child that already existed as a child, returns
isFirstOrder :: ChildKind -> Type -> Maybe Type
isFirstOrder ChildSyntax       tp = Just tp
isFirstOrder ChildAttr         _  = Nothing
isFirstOrder (ChildReplace tp) _  = Just tp
{-# LINE 193 "dist/build/GenerateCode.hs" #-}

{-# LINE 906 "./src-ag/GenerateCode.ag" #-}

makeLocalComment :: Int -> String -> Identifier -> Maybe Type -> String
makeLocalComment width what  name tp = let  x = getName name
                                            y = maybe "_" (\t -> case t of (NT nt tps _) -> getName nt ++ " " ++ unwords tps; Haskell t -> '{':t++"}") tp
                                       in   ( what ++ " " ++ x ++ replicate ((width - length x) `max` 0) ' ' ++ " : " ++ y )

{-# LINE 202 "dist/build/GenerateCode.hs" #-}

{-# LINE 937 "./src-ag/GenerateCode.ag" #-}

-- Lets or nested Cases?
-- or even a do-expression?

data DeclsType = DeclsLet | DeclsCase | DeclsDo

mkDecls :: DeclsType -> Decls -> Expr -> Expr
mkDecls DeclsLet  = mkLet False
mkDecls DeclsCase = mkLet True
mkDecls DeclsDo   = \decls -> Do (map toBind decls)
  where toBind (Decl lhs rhs _ _) = BindLet lhs rhs
        toBind d                  = d

mkLet :: Bool -> Decls -> Expr -> Expr
mkLet False decls body = Let decls body
mkLet True decls body = foldr oneCase body decls

oneCase :: Decl -> Expr -> Expr
oneCase (Decl left rhs _ _)      exp = Case rhs [CaseAlt left exp]
oneCase (Resume _ nt left rhs)   exp = ResumeExpr nt rhs left exp
oneCase _                        exp = exp

-- Gives the name of the visit function
funname field 0  = show field ++ "_"
funname field nr = show field ++ "_" ++ show nr

-- Gives the name of a semantic function
seqSemname :: String -> NontermIdent -> ConstructorIdent -> Int -> String
seqSemname pre nt con  0 = semname pre nt con
seqSemname pre nt con nr = semname pre nt con ++ "_" ++ show nr

-- Gives the name of a type
typeName :: NontermIdent -> Int -> String
typeName nt 0 = "T_" ++ show nt
typeName nt n = "T_" ++ show nt ++ "_" ++ show n

ntOfVisit :: NontermIdent -> Int -> NontermIdent
ntOfVisit nt 0 = nt
ntOfVisit nt n = Ident (show nt ++ "_" ++ show n) (getPos nt)

-- Gives the name of a visit function
visitname  ::  String -> NontermIdent -> Int -> String
visitname pre nt n =  pre ++ getName nt ++ "_" ++ show n
{-# LINE 248 "dist/build/GenerateCode.hs" #-}

{-# LINE 1026 "./src-ag/GenerateCode.ag" #-}

toNamedType :: Bool -> NontermIdent -> ConstructorIdent -> Identifier -> Code.Type -> Code.NamedType
toNamedType genStrict nt con nm tp
  = Code.Named genStrict strNm tp
  where strNm = recordFieldname nt con nm
{-# LINE 256 "dist/build/GenerateCode.hs" #-}
-- CGrammar ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : Program
   alternatives:
      alternative CGrammar:
         child typeSyns       : {TypeSyns}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : CNonterminals 
         child pragmas        : {PragmaMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Set Identifier))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))}
         child multivisit     : {Bool}
         visit 0:
            local options     : _
            local aroundMap   : _
            local mergeMap    : _
            local unfoldSemDom : _
-}
-- cata
sem_CGrammar :: CGrammar ->
                T_CGrammar
sem_CGrammar (CGrammar _typeSyns _derivings _wrappers _nonts _pragmas _paramMap _contextMap _quantMap _aroundsMap _mergeMap _multivisit) =
    (sem_CGrammar_CGrammar _typeSyns _derivings _wrappers (sem_CNonterminals _nonts) _pragmas _paramMap _contextMap _quantMap _aroundsMap _mergeMap _multivisit)
-- semantic domain
newtype T_CGrammar = T_CGrammar (Options ->
                                 ( (Seq Error),Program))
data Inh_CGrammar = Inh_CGrammar {options_Inh_CGrammar :: !(Options)}
data Syn_CGrammar = Syn_CGrammar {errors_Syn_CGrammar :: !((Seq Error)),output_Syn_CGrammar :: !(Program)}
wrap_CGrammar :: T_CGrammar ->
                 Inh_CGrammar ->
                 Syn_CGrammar
wrap_CGrammar (T_CGrammar sem) (Inh_CGrammar _lhsIoptions) =
    (let ( _lhsOerrors,_lhsOoutput) = sem _lhsIoptions
     in  (Syn_CGrammar _lhsOerrors _lhsOoutput))
sem_CGrammar_CGrammar :: TypeSyns ->
                         Derivings ->
                         (Set NontermIdent) ->
                         T_CNonterminals ->
                         PragmaMap ->
                         ParamMap ->
                         ContextMap ->
                         QuantMap ->
                         (Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
                         (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                         Bool ->
                         T_CGrammar
sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ (T_CNonterminals nonts_) pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_ =
    (T_CGrammar (\ _lhsIoptions ->
                     (let _nontsOo_sig :: Bool
                          _nontsOo_cata :: Bool
                          _nontsOo_sem :: Bool
                          _nontsOo_newtypes :: Bool
                          _nontsOo_unbox :: Bool
                          _nontsOo_case :: Bool
                          _nontsOo_pretty :: Bool
                          _nontsOo_rename :: Bool
                          _nontsOo_strictwrap :: Bool
                          _nontsOo_splitsems :: Bool
                          _nontsOo_data :: (Maybe Bool)
                          _nontsOprefix :: String
                          _nontsOo_traces :: Bool
                          _nontsOo_costcentre :: Bool
                          _nontsOo_linePragmas :: Bool
                          _nontsOo_monadic :: Bool
                          _nontsOallPragmas :: PragmaMap
                          _nontsOparamMap :: ParamMap
                          _nontsOcontextMap :: ContextMap
                          _nontsOquantMap :: QuantMap
                          _nontsOallNts :: (Set NontermIdent)
                          _nontsOwith_sig :: Bool
                          _lhsOerrors :: (Seq Error)
                          _lhsOoutput :: Program
                          _nontsOtypeSyns :: TypeSyns
                          _nontsOderivings :: Derivings
                          _nontsOwrappers :: (Set NontermIdent)
                          _nontsOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Set Identifier)))
                          _nontsOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))))
                          _nontsOoptions :: Options
                          _nontsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _nontsIchunks :: Chunks
                          _nontsIgathNts :: (Set NontermIdent)
                          _nontsIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_sig =
                              ({-# LINE 53 "./src-ag/GenerateCode.ag" #-}
                               typeSigs       _lhsIoptions
                               {-# LINE 352 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_cata =
                              ({-# LINE 54 "./src-ag/GenerateCode.ag" #-}
                               folds          _lhsIoptions
                               {-# LINE 358 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_sem =
                              ({-# LINE 55 "./src-ag/GenerateCode.ag" #-}
                               semfuns        _lhsIoptions
                               {-# LINE 364 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_newtypes =
                              ({-# LINE 56 "./src-ag/GenerateCode.ag" #-}
                               newtypes       _lhsIoptions
                               {-# LINE 370 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_unbox =
                              ({-# LINE 57 "./src-ag/GenerateCode.ag" #-}
                               unbox          _lhsIoptions
                               {-# LINE 376 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_case =
                              ({-# LINE 58 "./src-ag/GenerateCode.ag" #-}
                               cases          _lhsIoptions
                               {-# LINE 382 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_pretty =
                              ({-# LINE 59 "./src-ag/GenerateCode.ag" #-}
                               attrInfo       _lhsIoptions
                               {-# LINE 388 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_rename =
                              ({-# LINE 60 "./src-ag/GenerateCode.ag" #-}
                               rename         _lhsIoptions
                               {-# LINE 394 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_strictwrap =
                              ({-# LINE 61 "./src-ag/GenerateCode.ag" #-}
                               strictWrap     _lhsIoptions
                               {-# LINE 400 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_splitsems =
                              ({-# LINE 62 "./src-ag/GenerateCode.ag" #-}
                               splitSems      _lhsIoptions
                               {-# LINE 406 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_data =
                              ({-# LINE 63 "./src-ag/GenerateCode.ag" #-}
                               if dataTypes _lhsIoptions then Just (strictData _lhsIoptions) else Nothing
                               {-# LINE 412 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOprefix =
                              ({-# LINE 64 "./src-ag/GenerateCode.ag" #-}
                               prefix         _lhsIoptions
                               {-# LINE 418 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_traces =
                              ({-# LINE 65 "./src-ag/GenerateCode.ag" #-}
                               genTraces      _lhsIoptions
                               {-# LINE 424 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_costcentre =
                              ({-# LINE 66 "./src-ag/GenerateCode.ag" #-}
                               genCostCentres _lhsIoptions
                               {-# LINE 430 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_linePragmas =
                              ({-# LINE 67 "./src-ag/GenerateCode.ag" #-}
                               genLinePragmas _lhsIoptions
                               {-# LINE 436 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 53, column 17)
                          _nontsOo_monadic =
                              ({-# LINE 68 "./src-ag/GenerateCode.ag" #-}
                               monadic        _lhsIoptions
                               {-# LINE 442 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 71, column 3)
                          _options =
                              ({-# LINE 71 "./src-ag/GenerateCode.ag" #-}
                               _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && multivisit_ }
                               {-# LINE 448 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 76, column 15)
                          _nontsOallPragmas =
                              ({-# LINE 76 "./src-ag/GenerateCode.ag" #-}
                               pragmas_
                               {-# LINE 454 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 98, column 14)
                          _nontsOparamMap =
                              ({-# LINE 98 "./src-ag/GenerateCode.ag" #-}
                               paramMap_
                               {-# LINE 460 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 119, column 7)
                          _nontsOcontextMap =
                              ({-# LINE 119 "./src-ag/GenerateCode.ag" #-}
                               contextMap_
                               {-# LINE 466 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 120, column 7)
                          _nontsOquantMap =
                              ({-# LINE 120 "./src-ag/GenerateCode.ag" #-}
                               quantMap_
                               {-# LINE 472 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 136, column 7)
                          _nontsOallNts =
                              ({-# LINE 136 "./src-ag/GenerateCode.ag" #-}
                               _nontsIgathNts
                               {-# LINE 478 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 582, column 34)
                          _aroundMap =
                              ({-# LINE 582 "./src-ag/GenerateCode.ag" #-}
                               aroundsMap_
                               {-# LINE 484 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 598, column 34)
                          _mergeMap =
                              ({-# LINE 598 "./src-ag/GenerateCode.ag" #-}
                               mergeMap_
                               {-# LINE 490 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 754, column 7)
                          _unfoldSemDom =
                              ({-# LINE 754 "./src-ag/GenerateCode.ag" #-}
                               \nt nr repl ->
                                let (params, tp) = Map.findWithDefault (error ("No such semantic domain: " ++ show nt)) (nt, nr) _nontsIsemDomUnfoldGath
                                    replMap = Map.fromList (zip params repl)
                                    replace k = Map.findWithDefault ('@':k) k replMap
                                in evalType replace tp
                               {-# LINE 500 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 854, column 14)
                          _nontsOwith_sig =
                              ({-# LINE 854 "./src-ag/GenerateCode.ag" #-}
                               typeSigs _lhsIoptions
                               {-# LINE 506 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 857, column 15)
                          _lhsOerrors =
                              ({-# LINE 857 "./src-ag/GenerateCode.ag" #-}
                               Seq.empty
                               {-# LINE 512 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 923, column 17)
                          _lhsOoutput =
                              ({-# LINE 923 "./src-ag/GenerateCode.ag" #-}
                               Program _nontsIchunks multivisit_
                               {-# LINE 518 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 990, column 14)
                          _nontsOtypeSyns =
                              ({-# LINE 990 "./src-ag/GenerateCode.ag" #-}
                               typeSyns_
                               {-# LINE 524 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 990, column 14)
                          _nontsOderivings =
                              ({-# LINE 991 "./src-ag/GenerateCode.ag" #-}
                               derivings_
                               {-# LINE 530 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 990, column 14)
                          _nontsOwrappers =
                              ({-# LINE 992 "./src-ag/GenerateCode.ag" #-}
                               wrappers_
                               {-# LINE 536 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (from local)
                          _nontsOaroundMap =
                              ({-# LINE 575 "./src-ag/GenerateCode.ag" #-}
                               _aroundMap
                               {-# LINE 542 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (from local)
                          _nontsOmergeMap =
                              ({-# LINE 591 "./src-ag/GenerateCode.ag" #-}
                               _mergeMap
                               {-# LINE 548 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (from local)
                          _nontsOoptions =
                              ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                               _options
                               {-# LINE 554 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (from local)
                          _nontsOunfoldSemDom =
                              ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                               _unfoldSemDom
                               {-# LINE 560 "dist/build/GenerateCode.hs" #-}
                               )
                          ( _nontsIchunks,_nontsIgathNts,_nontsIsemDomUnfoldGath) =
                              nonts_ _nontsOallNts _nontsOallPragmas _nontsOaroundMap _nontsOcontextMap _nontsOderivings _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_costcentre _nontsOo_data _nontsOo_linePragmas _nontsOo_monadic _nontsOo_newtypes _nontsOo_pretty _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_splitsems _nontsOo_strictwrap _nontsOo_traces _nontsOo_unbox _nontsOoptions _nontsOparamMap _nontsOprefix _nontsOquantMap _nontsOtypeSyns _nontsOunfoldSemDom _nontsOwith_sig _nontsOwrappers
                      in  ( _lhsOerrors,_lhsOoutput))))
-- CInterface --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative CInterface:
         child seg            : CSegments 
-}
-- cata
sem_CInterface :: CInterface ->
                  T_CInterface
sem_CInterface (CInterface _seg) =
    (sem_CInterface_CInterface (sem_CSegments _seg))
-- semantic domain
newtype T_CInterface = T_CInterface (Attributes ->
                                     NontermIdent ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     (Maybe Bool) ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Bool ->
                                     Options ->
                                     ParamMap ->
                                     String ->
                                     Attributes ->
                                     ( ([String]),([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CInterface = Inh_CInterface {inh_Inh_CInterface :: !(Attributes),nt_Inh_CInterface :: !(NontermIdent),o_case_Inh_CInterface :: !(Bool),o_cata_Inh_CInterface :: !(Bool),o_costcentre_Inh_CInterface :: !(Bool),o_data_Inh_CInterface :: !((Maybe Bool)),o_linePragmas_Inh_CInterface :: !(Bool),o_monadic_Inh_CInterface :: !(Bool),o_newtypes_Inh_CInterface :: !(Bool),o_pretty_Inh_CInterface :: !(Bool),o_rename_Inh_CInterface :: !(Bool),o_sem_Inh_CInterface :: !(Bool),o_sig_Inh_CInterface :: !(Bool),o_splitsems_Inh_CInterface :: !(Bool),o_strictwrap_Inh_CInterface :: !(Bool),o_traces_Inh_CInterface :: !(Bool),o_unbox_Inh_CInterface :: !(Bool),options_Inh_CInterface :: !(Options),paramMap_Inh_CInterface :: !(ParamMap),prefix_Inh_CInterface :: !(String),syn_Inh_CInterface :: !(Attributes)}
data Syn_CInterface = Syn_CInterface {comments_Syn_CInterface :: !(([String])),semDom_Syn_CInterface :: !(([Decl])),semDomUnfoldGath_Syn_CInterface :: !((Map (NontermIdent, Int) ([String], Code.Type))),wrapDecls_Syn_CInterface :: !(Decls)}
wrap_CInterface :: T_CInterface ->
                   Inh_CInterface ->
                   Syn_CInterface
wrap_CInterface (T_CInterface sem) (Inh_CInterface _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
    (let ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) = sem _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
     in  (Syn_CInterface _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls))
sem_CInterface_CInterface :: T_CSegments ->
                             T_CInterface
sem_CInterface_CInterface (T_CSegments seg_) =
    (T_CInterface (\ _lhsIinh
                     _lhsInt
                     _lhsIo_case
                     _lhsIo_cata
                     _lhsIo_costcentre
                     _lhsIo_data
                     _lhsIo_linePragmas
                     _lhsIo_monadic
                     _lhsIo_newtypes
                     _lhsIo_pretty
                     _lhsIo_rename
                     _lhsIo_sem
                     _lhsIo_sig
                     _lhsIo_splitsems
                     _lhsIo_strictwrap
                     _lhsIo_traces
                     _lhsIo_unbox
                     _lhsIoptions
                     _lhsIparamMap
                     _lhsIprefix
                     _lhsIsyn ->
                       (let _segOnr :: Int
                            _lhsOsemDom :: ([Decl])
                            _lhsOcomments :: ([String])
                            _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                            _lhsOwrapDecls :: Decls
                            _segOinh :: Attributes
                            _segOnt :: NontermIdent
                            _segOo_case :: Bool
                            _segOo_cata :: Bool
                            _segOo_costcentre :: Bool
                            _segOo_data :: (Maybe Bool)
                            _segOo_linePragmas :: Bool
                            _segOo_monadic :: Bool
                            _segOo_newtypes :: Bool
                            _segOo_pretty :: Bool
                            _segOo_rename :: Bool
                            _segOo_sem :: Bool
                            _segOo_sig :: Bool
                            _segOo_splitsems :: Bool
                            _segOo_strictwrap :: Bool
                            _segOo_traces :: Bool
                            _segOo_unbox :: Bool
                            _segOoptions :: Options
                            _segOparamMap :: ParamMap
                            _segOprefix :: String
                            _segOsyn :: Attributes
                            _segIcomments :: ([String])
                            _segIisNil :: Bool
                            _segIsemDom :: ([Decl])
                            _segIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                            _segIwrapDecls :: Decls
                            -- "./src-ag/GenerateCode.ag"(line 284, column 17)
                            _segOnr =
                                ({-# LINE 284 "./src-ag/GenerateCode.ag" #-}
                                 0
                                 {-# LINE 694 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 712, column 18)
                            _lhsOsemDom =
                                ({-# LINE 712 "./src-ag/GenerateCode.ag" #-}
                                 Comment "semantic domain" : _segIsemDom
                                 {-# LINE 700 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                            _lhsOcomments =
                                ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                                 _segIcomments
                                 {-# LINE 706 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                            _lhsOsemDomUnfoldGath =
                                ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                 _segIsemDomUnfoldGath
                                 {-# LINE 712 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- use rule "./src-ag/GenerateCode.ag"(line 829, column 52)
                            _lhsOwrapDecls =
                                ({-# LINE 829 "./src-ag/GenerateCode.ag" #-}
                                 _segIwrapDecls
                                 {-# LINE 718 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOinh =
                                ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIinh
                                 {-# LINE 724 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOnt =
                                ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                 _lhsInt
                                 {-# LINE 730 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_case =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_case
                                 {-# LINE 736 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_cata =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_cata
                                 {-# LINE 742 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_costcentre =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_costcentre
                                 {-# LINE 748 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_data =
                                ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_data
                                 {-# LINE 754 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_linePragmas =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_linePragmas
                                 {-# LINE 760 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_monadic =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_monadic
                                 {-# LINE 766 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_newtypes =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_newtypes
                                 {-# LINE 772 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_pretty =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_pretty
                                 {-# LINE 778 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_rename =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_rename
                                 {-# LINE 784 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_sem =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_sem
                                 {-# LINE 790 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_sig =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_sig
                                 {-# LINE 796 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_splitsems =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_splitsems
                                 {-# LINE 802 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_strictwrap =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_strictwrap
                                 {-# LINE 808 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_traces =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_traces
                                 {-# LINE 814 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOo_unbox =
                                ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIo_unbox
                                 {-# LINE 820 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOoptions =
                                ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIoptions
                                 {-# LINE 826 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOparamMap =
                                ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIparamMap
                                 {-# LINE 832 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOprefix =
                                ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIprefix
                                 {-# LINE 838 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _segOsyn =
                                ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIsyn
                                 {-# LINE 844 "dist/build/GenerateCode.hs" #-}
                                 )
                            ( _segIcomments,_segIisNil,_segIsemDom,_segIsemDomUnfoldGath,_segIwrapDecls) =
                                seg_ _segOinh _segOnr _segOnt _segOo_case _segOo_cata _segOo_costcentre _segOo_data _segOo_linePragmas _segOo_monadic _segOo_newtypes _segOo_pretty _segOo_rename _segOo_sem _segOo_sig _segOo_splitsems _segOo_strictwrap _segOo_traces _segOo_unbox _segOoptions _segOparamMap _segOprefix _segOsyn
                        in  ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))))
-- CNonterminal ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Set Identifier))
         contextMap           : ContextMap
         derivings            : Derivings
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         typeSyns             : TypeSyns
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      synthesized attributes:
         chunks               : Chunks
         gathNts              : Set NontermIdent
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
   alternatives:
      alternative CNonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : CProductions 
         child inter          : CInterface 
         visit 0:
            local aroundMap   : _
            local mergeMap    : _
            local semWrapper  : _
            local comment     : _
            local dataDef     : _
            local genCata     : _
            local cataFun     : _
-}
-- cata
sem_CNonterminal :: CNonterminal ->
                    T_CNonterminal
sem_CNonterminal (CNonterminal _nt _params _inh _syn _prods _inter) =
    (sem_CNonterminal_CNonterminal _nt _params _inh _syn (sem_CProductions _prods) (sem_CInterface _inter))
-- semantic domain
newtype T_CNonterminal = T_CNonterminal ((Set NontermIdent) ->
                                         PragmaMap ->
                                         (Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
                                         ContextMap ->
                                         Derivings ->
                                         (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         (Maybe Bool) ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Options ->
                                         ParamMap ->
                                         String ->
                                         QuantMap ->
                                         TypeSyns ->
                                         (NontermIdent -> Int -> [String] -> Code.Type) ->
                                         Bool ->
                                         (Set NontermIdent) ->
                                         ( Chunks,(Set NontermIdent),(Map (NontermIdent, Int) ([String], Code.Type))))
data Inh_CNonterminal = Inh_CNonterminal {allNts_Inh_CNonterminal :: !((Set NontermIdent)),allPragmas_Inh_CNonterminal :: !(PragmaMap),aroundMap_Inh_CNonterminal :: !((Map NontermIdent (Map ConstructorIdent (Set Identifier)))),contextMap_Inh_CNonterminal :: !(ContextMap),derivings_Inh_CNonterminal :: !(Derivings),mergeMap_Inh_CNonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))))),o_case_Inh_CNonterminal :: !(Bool),o_cata_Inh_CNonterminal :: !(Bool),o_costcentre_Inh_CNonterminal :: !(Bool),o_data_Inh_CNonterminal :: !((Maybe Bool)),o_linePragmas_Inh_CNonterminal :: !(Bool),o_monadic_Inh_CNonterminal :: !(Bool),o_newtypes_Inh_CNonterminal :: !(Bool),o_pretty_Inh_CNonterminal :: !(Bool),o_rename_Inh_CNonterminal :: !(Bool),o_sem_Inh_CNonterminal :: !(Bool),o_sig_Inh_CNonterminal :: !(Bool),o_splitsems_Inh_CNonterminal :: !(Bool),o_strictwrap_Inh_CNonterminal :: !(Bool),o_traces_Inh_CNonterminal :: !(Bool),o_unbox_Inh_CNonterminal :: !(Bool),options_Inh_CNonterminal :: !(Options),paramMap_Inh_CNonterminal :: !(ParamMap),prefix_Inh_CNonterminal :: !(String),quantMap_Inh_CNonterminal :: !(QuantMap),typeSyns_Inh_CNonterminal :: !(TypeSyns),unfoldSemDom_Inh_CNonterminal :: !((NontermIdent -> Int -> [String] -> Code.Type)),with_sig_Inh_CNonterminal :: !(Bool),wrappers_Inh_CNonterminal :: !((Set NontermIdent))}
data Syn_CNonterminal = Syn_CNonterminal {chunks_Syn_CNonterminal :: !(Chunks),gathNts_Syn_CNonterminal :: !((Set NontermIdent)),semDomUnfoldGath_Syn_CNonterminal :: !((Map (NontermIdent, Int) ([String], Code.Type)))}
wrap_CNonterminal :: T_CNonterminal ->
                     Inh_CNonterminal ->
                     Syn_CNonterminal
wrap_CNonterminal (T_CNonterminal sem) (Inh_CNonterminal _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOchunks,_lhsOgathNts,_lhsOsemDomUnfoldGath) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
     in  (Syn_CNonterminal _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath))
sem_CNonterminal_CNonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 Attributes ->
                                 Attributes ->
                                 T_CProductions ->
                                 T_CInterface ->
                                 T_CNonterminal
sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ (T_CProductions prods_) (T_CInterface inter_) =
    (T_CNonterminal (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIaroundMap
                       _lhsIcontextMap
                       _lhsIderivings
                       _lhsImergeMap
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_monadic
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIoptions
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsIquantMap
                       _lhsItypeSyns
                       _lhsIunfoldSemDom
                       _lhsIwith_sig
                       _lhsIwrappers ->
                         (let _interOinh :: Attributes
                              _interOsyn :: Attributes
                              _interOnt :: NontermIdent
                              _prodsOinh :: Attributes
                              _prodsOsyn :: Attributes
                              _prodsOnt :: NontermIdent
                              _lhsOgathNts :: (Set NontermIdent)
                              _lhsOchunks :: Chunks
                              _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                              _prodsOallNts :: (Set NontermIdent)
                              _prodsOallPragmas :: PragmaMap
                              _prodsOaroundMap :: (Map ConstructorIdent (Set Identifier))
                              _prodsOcontextMap :: ContextMap
                              _prodsOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))
                              _prodsOo_case :: Bool
                              _prodsOo_cata :: Bool
                              _prodsOo_costcentre :: Bool
                              _prodsOo_data :: (Maybe Bool)
                              _prodsOo_linePragmas :: Bool
                              _prodsOo_monadic :: Bool
                              _prodsOo_newtypes :: Bool
                              _prodsOo_pretty :: Bool
                              _prodsOo_rename :: Bool
                              _prodsOo_sem :: Bool
                              _prodsOo_sig :: Bool
                              _prodsOo_splitsems :: Bool
                              _prodsOo_strictwrap :: Bool
                              _prodsOo_traces :: Bool
                              _prodsOo_unbox :: Bool
                              _prodsOoptions :: Options
                              _prodsOparamMap :: ParamMap
                              _prodsOprefix :: String
                              _prodsOquantMap :: QuantMap
                              _prodsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _prodsOwith_sig :: Bool
                              _prodsOwrappers :: (Set NontermIdent)
                              _interOo_case :: Bool
                              _interOo_cata :: Bool
                              _interOo_costcentre :: Bool
                              _interOo_data :: (Maybe Bool)
                              _interOo_linePragmas :: Bool
                              _interOo_monadic :: Bool
                              _interOo_newtypes :: Bool
                              _interOo_pretty :: Bool
                              _interOo_rename :: Bool
                              _interOo_sem :: Bool
                              _interOo_sig :: Bool
                              _interOo_splitsems :: Bool
                              _interOo_strictwrap :: Bool
                              _interOo_traces :: Bool
                              _interOo_unbox :: Bool
                              _interOoptions :: Options
                              _interOparamMap :: ParamMap
                              _interOprefix :: String
                              _prodsIcataAlts :: Decls
                              _prodsIcomments :: ([String])
                              _prodsIdataAlts :: DataAlts
                              _prodsIdecls :: Decls
                              _prodsIsemNames :: ([String])
                              _interIcomments :: ([String])
                              _interIsemDom :: ([Decl])
                              _interIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                              _interIwrapDecls :: Decls
                              -- "./src-ag/GenerateCode.ag"(line 86, column 26)
                              (_interOinh,_interOsyn,_interOnt) =
                                  ({-# LINE 86 "./src-ag/GenerateCode.ag" #-}
                                   (inh_,syn_,nt_)
                                   {-# LINE 1051 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 87, column 25)
                              (_prodsOinh,_prodsOsyn,_prodsOnt) =
                                  ({-# LINE 87 "./src-ag/GenerateCode.ag" #-}
                                   (inh_,syn_,nt_)
                                   {-# LINE 1057 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 142, column 7)
                              _lhsOgathNts =
                                  ({-# LINE 142 "./src-ag/GenerateCode.ag" #-}
                                   Set.singleton nt_
                                   {-# LINE 1063 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 583, column 34)
                              _aroundMap =
                                  ({-# LINE 583 "./src-ag/GenerateCode.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                   {-# LINE 1069 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 599, column 34)
                              _mergeMap =
                                  ({-# LINE 599 "./src-ag/GenerateCode.ag" #-}
                                   Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                   {-# LINE 1075 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 803, column 18)
                              _semWrapper =
                                  ({-# LINE 803 "./src-ag/GenerateCode.ag" #-}
                                   let params' = map getName params_
                                       inhAttrs = Map.toList inh_
                                       synAttrs = Map.toList syn_
                                       inhVars = [ SimpleExpr (attrname True _LHS a) | (a,_) <- inhAttrs ]
                                       synVars = [ SimpleExpr (attrname False _LHS a) | (a,_) <- synAttrs ]
                                       var = "sem"
                                       wrapNT = "wrap" ++ "_" ++ getName nt_
                                       inhNT = "Inh" ++ "_" ++ getName nt_
                                       synNT = "Syn" ++ "_" ++ getName nt_
                                       varPat = if  _lhsIo_newtypes
                                                    then App (sdtype nt_) [SimpleExpr var]
                                                    else SimpleExpr var
                                       evalTp | null params' = id
                                              | otherwise    = idEvalType
                                       appParams nm = TypeApp (SimpleType nm) (map SimpleType params')
                                       typeSig = TSig wrapNT (evalTp $ appParams (sdtype nt_) `Arr` (appParams inhNT `Arr` appParams synNT))
                                       mkstrict = Named _lhsIo_strictwrap
                                       mkdata n attrs = Data n params' [Record n [mkstrict (getName f++"_"++n) $ evalTp $ typeToCodeType (Just nt_) params' $ removeDeforested t | (f,t) <- attrs]] False []
                                       datas = [mkdata inhNT inhAttrs, mkdata synNT synAttrs]
                                   in datas ++ [ typeSig
                                               , Decl (Fun wrapNT [varPat, App inhNT inhVars])
                                                     (Let _interIwrapDecls (App synNT synVars))
                                                     Set.empty Set.empty
                                               ]
                                   {-# LINE 1104 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 864, column 18)
                              _comment =
                                  ({-# LINE 864 "./src-ag/GenerateCode.ag" #-}
                                   Comment . unlines . map ind $ ( _interIcomments ++ ("alternatives:" : map ind _prodsIcomments) )
                                   {-# LINE 1110 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 926, column 19)
                              _lhsOchunks =
                                  ({-# LINE 926 "./src-ag/GenerateCode.ag" #-}
                                   [ Chunk (getName nt_)
                                          (Comment (getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-'))
                                          (if _lhsIo_pretty                  then [_comment    ]   else [])
                                          (if isJust _lhsIo_data             then [_dataDef    ]   else [])
                                          (if _lhsIo_cata && _genCata        then  _cataFun        else [])
                                          (if _lhsIo_sig                     then  _interIsemDom   else [])
                                          (if nt_ `Set.member` _lhsIwrappers then  _semWrapper     else [])
                                          (if _lhsIo_sem                     then  _prodsIdecls     else [])
                                          (if _lhsIo_sem                     then  _prodsIsemNames  else [])
                                   ]
                                   {-# LINE 1125 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 995, column 18)
                              _dataDef =
                                  ({-# LINE 995 "./src-ag/GenerateCode.ag" #-}
                                   let params' = map getName params_
                                       typeSyn tp = let theType =
                                                          case tp of
                                                            CommonTypes.Maybe t      -> TMaybe $ typeToCodeType (Just nt_) params' t
                                                            CommonTypes.Either t1 t2 -> TEither (typeToCodeType (Just nt_) params' t1) (typeToCodeType (Just nt_) params' t2)
                                                            CommonTypes.Map t1 t2    -> TMap (typeToCodeType (Just nt_) params' t1) (typeToCodeType (Just nt_) params' t2)
                                                            CommonTypes.IntMap t     -> TIntMap $ typeToCodeType (Just nt_) params' t
                                                            CommonTypes.List t       -> Code.List $ typeToCodeType (Just nt_) params' t
                                                            CommonTypes.Tuple ts     -> Code.TupleType [typeToCodeType (Just nt_) params' t
                                                                                                   | (_,t) <- ts
                                                                                                   ]
                                                     in Code.Type (getName nt_) params' (idEvalType theType)
                                       derivings  = maybe [] (map getName . Set.toList) (Map.lookup nt_ _lhsIderivings)
                                       dataDef    = Data (getName nt_) (map getName params_) _prodsIdataAlts (maybe False id _lhsIo_data) derivings
                                   in maybe dataDef typeSyn $ lookup nt_ _lhsItypeSyns
                                   {-# LINE 1145 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 1038, column 18)
                              _genCata =
                                  ({-# LINE 1038 "./src-ag/GenerateCode.ag" #-}
                                   not (nt_ `Set.member` nocatas _lhsIoptions)
                                   {-# LINE 1151 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 1039, column 18)
                              _cataFun =
                                  ({-# LINE 1039 "./src-ag/GenerateCode.ag" #-}
                                   let appParams nm = TypeApp (SimpleType nm) (map SimpleType (map getName params_))
                                       evalTp | null params_ = id
                                              | otherwise    = idEvalType
                                       tSig = TSig (cataname _lhsIprefix nt_)
                                                   (appQuant _lhsIquantMap nt_ $ appContext _lhsIcontextMap nt_ $ evalTp $ appParams (getName nt_) `Arr` appParams (sdtype nt_))
                                       special typ = case typ of
                                                     CommonTypes.List tp ->
                                                         let cons = SimpleExpr (semname _lhsIprefix nt_ (identifier "Cons"))
                                                             nil  = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil" ))
                                                             arg  = SimpleExpr "list"
                                                             rarg = case tp of
                                                                      NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                  in SimpleExpr ("(Prelude.map " ++ (cataname _lhsIprefix t') ++ " list)")
                                                                      _        -> arg
                                                             lhs = Fun (cataname _lhsIprefix nt_) [arg]
                                                             rhs = (App "Prelude.foldr" [cons,nil,rarg])
                                                         in  [Decl lhs rhs Set.empty Set.empty]
                                                     CommonTypes.Maybe tp ->
                                                         let just    = semname _lhsIprefix nt_ (identifier "Just")
                                                             nothing = semname _lhsIprefix nt_ (identifier "Nothing" )
                                                             arg  = SimpleExpr "x"
                                                             rarg = case tp of
                                                                      NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                  in App (cataname _lhsIprefix t') [arg]
                                                                      _        -> arg
                                                             lhs a = Fun (cataname _lhsIprefix nt_) [a]
                                                         in  [Decl (lhs (App "Prelude.Just" [arg]))     (App just [rarg])    Set.empty Set.empty
                                                             ,Decl (lhs (SimpleExpr "Prelude.Nothing")) (SimpleExpr nothing) Set.empty Set.empty
                                                             ]
                                                     CommonTypes.Either tp1 tp2 ->
                                                         let left  = semname _lhsIprefix nt_ (identifier "Left")
                                                             right = semname _lhsIprefix nt_ (identifier "Right" )
                                                             arg   = SimpleExpr "x"
                                                             rarg0 = case tp1 of
                                                                      NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                  in App (cataname _lhsIprefix t') [arg]
                                                                      _        -> arg
                                                             rarg1 = case tp2 of
                                                                      NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                  in App (cataname _lhsIprefix t') [arg]
                                                                      _        -> arg
                                                             lhs a = Fun (cataname _lhsIprefix nt_) [a]
                                                         in  [Decl (lhs (App "Prelude.Left"  [arg]))     (App left  [rarg0])    Set.empty Set.empty
                                                             ,Decl (lhs (App "Prelude.Right" [arg]))     (App right [rarg1])    Set.empty Set.empty
                                                             ]
                                                     CommonTypes.Map _ tp ->
                                                       let entry = SimpleExpr (semname _lhsIprefix nt_ (identifier "Entry"))
                                                           nil   = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil"))
                                                           arg   = SimpleExpr "m"
                                                           rarg  = case tp of
                                                                     NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                 in App "Data.Map.map" [SimpleExpr $ cataname _lhsIprefix t', arg]
                                                                     _        -> arg
                                                           lhs   = Fun (cataname _lhsIprefix nt_) [arg]
                                                           rhs   = App "Data.Map.foldrWithKey" [entry,nil,rarg]
                                                       in [Decl lhs rhs Set.empty Set.empty]
                                                     CommonTypes.IntMap tp ->
                                                       let entry = SimpleExpr (semname _lhsIprefix nt_ (identifier "Entry"))
                                                           nil   = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil"))
                                                           arg   = SimpleExpr "m"
                                                           rarg  = case tp of
                                                                     NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                 in App "Data.IntMap.map" [SimpleExpr $ cataname _lhsIprefix t', arg]
                                                                     _        -> arg
                                                           lhs   = Fun (cataname _lhsIprefix nt_) [arg]
                                                           rhs   = App "Data.IntMap.foldWithKey" [entry,nil,rarg]
                                                       in [Decl lhs rhs Set.empty Set.empty]
                                                     CommonTypes.Tuple tps ->
                                                         let con  = semname _lhsIprefix nt_ (identifier "Tuple")
                                                             tps' = [ (SimpleExpr (getName x),y) | (x,y) <- tps]
                                                             rargs = map rarg tps'
                                                             rarg (n, tp) = case tp of
                                                                      NT t _ _ -> let t' = maybe t id (deforestedNt t)
                                                                                  in App (cataname _lhsIprefix t') [n]
                                                                      _        -> n
                                                             lhs = Fun (cataname _lhsIprefix nt_) [TupleExpr (map fst tps')]
                                                             rhs = App con rargs
                                                         in  [Decl lhs rhs Set.empty Set.empty]
                                   in  Comment "cata" :
                                       (if _lhsIo_sig then [tSig] else []) ++
                                       maybe _prodsIcataAlts special (lookup nt_ _lhsItypeSyns)
                                   {-# LINE 1237 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                              _lhsOsemDomUnfoldGath =
                                  ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                   _interIsemDomUnfoldGath
                                   {-# LINE 1243 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallNts =
                                  ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallNts
                                   {-# LINE 1249 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOallPragmas =
                                  ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallPragmas
                                   {-# LINE 1255 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (from local)
                              _prodsOaroundMap =
                                  ({-# LINE 578 "./src-ag/GenerateCode.ag" #-}
                                   _aroundMap
                                   {-# LINE 1261 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOcontextMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIcontextMap
                                   {-# LINE 1267 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (from local)
                              _prodsOmergeMap =
                                  ({-# LINE 594 "./src-ag/GenerateCode.ag" #-}
                                   _mergeMap
                                   {-# LINE 1273 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_case =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 1279 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_cata =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 1285 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_costcentre =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_costcentre
                                   {-# LINE 1291 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_data =
                                  ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 1297 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_linePragmas =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_linePragmas
                                   {-# LINE 1303 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_monadic =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_monadic
                                   {-# LINE 1309 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_newtypes =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 1315 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_pretty =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_pretty
                                   {-# LINE 1321 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_rename =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1327 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_sem =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 1333 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_sig =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 1339 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_splitsems =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_splitsems
                                   {-# LINE 1345 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_strictwrap =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_strictwrap
                                   {-# LINE 1351 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_traces =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_traces
                                   {-# LINE 1357 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOo_unbox =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 1363 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOoptions =
                                  ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1369 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOparamMap =
                                  ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIparamMap
                                   {-# LINE 1375 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOprefix =
                                  ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 1381 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOquantMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIquantMap
                                   {-# LINE 1387 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOunfoldSemDom =
                                  ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIunfoldSemDom
                                   {-# LINE 1393 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOwith_sig =
                                  ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwith_sig
                                   {-# LINE 1399 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _prodsOwrappers =
                                  ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwrappers
                                   {-# LINE 1405 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_case =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 1411 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_cata =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 1417 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_costcentre =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_costcentre
                                   {-# LINE 1423 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_data =
                                  ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 1429 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_linePragmas =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_linePragmas
                                   {-# LINE 1435 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_monadic =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_monadic
                                   {-# LINE 1441 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_newtypes =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 1447 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_pretty =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_pretty
                                   {-# LINE 1453 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_rename =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1459 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_sem =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 1465 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_sig =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 1471 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_splitsems =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_splitsems
                                   {-# LINE 1477 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_strictwrap =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_strictwrap
                                   {-# LINE 1483 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_traces =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_traces
                                   {-# LINE 1489 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOo_unbox =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 1495 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOoptions =
                                  ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 1501 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOparamMap =
                                  ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIparamMap
                                   {-# LINE 1507 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _interOprefix =
                                  ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 1513 "dist/build/GenerateCode.hs" #-}
                                   )
                              ( _prodsIcataAlts,_prodsIcomments,_prodsIdataAlts,_prodsIdecls,_prodsIsemNames) =
                                  prods_ _prodsOallNts _prodsOallPragmas _prodsOaroundMap _prodsOcontextMap _prodsOinh _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_costcentre _prodsOo_data _prodsOo_linePragmas _prodsOo_monadic _prodsOo_newtypes _prodsOo_pretty _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_splitsems _prodsOo_strictwrap _prodsOo_traces _prodsOo_unbox _prodsOoptions _prodsOparamMap _prodsOprefix _prodsOquantMap _prodsOsyn _prodsOunfoldSemDom _prodsOwith_sig _prodsOwrappers
                              ( _interIcomments,_interIsemDom,_interIsemDomUnfoldGath,_interIwrapDecls) =
                                  inter_ _interOinh _interOnt _interOo_case _interOo_cata _interOo_costcentre _interOo_data _interOo_linePragmas _interOo_monadic _interOo_newtypes _interOo_pretty _interOo_rename _interOo_sem _interOo_sig _interOo_splitsems _interOo_strictwrap _interOo_traces _interOo_unbox _interOoptions _interOparamMap _interOprefix _interOsyn
                          in  ( _lhsOchunks,_lhsOgathNts,_lhsOsemDomUnfoldGath))))
-- CNonterminals -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Map NontermIdent (Map ConstructorIdent (Set Identifier))
         contextMap           : ContextMap
         derivings            : Derivings
         mergeMap             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         typeSyns             : TypeSyns
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      synthesized attributes:
         chunks               : Chunks
         gathNts              : Set NontermIdent
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
   alternatives:
      alternative Cons:
         child hd             : CNonterminal 
         child tl             : CNonterminals 
      alternative Nil:
-}
-- cata
sem_CNonterminals :: CNonterminals ->
                     T_CNonterminals
sem_CNonterminals list =
    (Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list))
-- semantic domain
newtype T_CNonterminals = T_CNonterminals ((Set NontermIdent) ->
                                           PragmaMap ->
                                           (Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
                                           ContextMap ->
                                           Derivings ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           (Maybe Bool) ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Bool ->
                                           Options ->
                                           ParamMap ->
                                           String ->
                                           QuantMap ->
                                           TypeSyns ->
                                           (NontermIdent -> Int -> [String] -> Code.Type) ->
                                           Bool ->
                                           (Set NontermIdent) ->
                                           ( Chunks,(Set NontermIdent),(Map (NontermIdent, Int) ([String], Code.Type))))
data Inh_CNonterminals = Inh_CNonterminals {allNts_Inh_CNonterminals :: !((Set NontermIdent)),allPragmas_Inh_CNonterminals :: !(PragmaMap),aroundMap_Inh_CNonterminals :: !((Map NontermIdent (Map ConstructorIdent (Set Identifier)))),contextMap_Inh_CNonterminals :: !(ContextMap),derivings_Inh_CNonterminals :: !(Derivings),mergeMap_Inh_CNonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))))),o_case_Inh_CNonterminals :: !(Bool),o_cata_Inh_CNonterminals :: !(Bool),o_costcentre_Inh_CNonterminals :: !(Bool),o_data_Inh_CNonterminals :: !((Maybe Bool)),o_linePragmas_Inh_CNonterminals :: !(Bool),o_monadic_Inh_CNonterminals :: !(Bool),o_newtypes_Inh_CNonterminals :: !(Bool),o_pretty_Inh_CNonterminals :: !(Bool),o_rename_Inh_CNonterminals :: !(Bool),o_sem_Inh_CNonterminals :: !(Bool),o_sig_Inh_CNonterminals :: !(Bool),o_splitsems_Inh_CNonterminals :: !(Bool),o_strictwrap_Inh_CNonterminals :: !(Bool),o_traces_Inh_CNonterminals :: !(Bool),o_unbox_Inh_CNonterminals :: !(Bool),options_Inh_CNonterminals :: !(Options),paramMap_Inh_CNonterminals :: !(ParamMap),prefix_Inh_CNonterminals :: !(String),quantMap_Inh_CNonterminals :: !(QuantMap),typeSyns_Inh_CNonterminals :: !(TypeSyns),unfoldSemDom_Inh_CNonterminals :: !((NontermIdent -> Int -> [String] -> Code.Type)),with_sig_Inh_CNonterminals :: !(Bool),wrappers_Inh_CNonterminals :: !((Set NontermIdent))}
data Syn_CNonterminals = Syn_CNonterminals {chunks_Syn_CNonterminals :: !(Chunks),gathNts_Syn_CNonterminals :: !((Set NontermIdent)),semDomUnfoldGath_Syn_CNonterminals :: !((Map (NontermIdent, Int) ([String], Code.Type)))}
wrap_CNonterminals :: T_CNonterminals ->
                      Inh_CNonterminals ->
                      Syn_CNonterminals
wrap_CNonterminals (T_CNonterminals sem) (Inh_CNonterminals _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOchunks,_lhsOgathNts,_lhsOsemDomUnfoldGath) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
     in  (Syn_CNonterminals _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath))
sem_CNonterminals_Cons :: T_CNonterminal ->
                          T_CNonterminals ->
                          T_CNonterminals
sem_CNonterminals_Cons (T_CNonterminal hd_) (T_CNonterminals tl_) =
    (T_CNonterminals (\ _lhsIallNts
                        _lhsIallPragmas
                        _lhsIaroundMap
                        _lhsIcontextMap
                        _lhsIderivings
                        _lhsImergeMap
                        _lhsIo_case
                        _lhsIo_cata
                        _lhsIo_costcentre
                        _lhsIo_data
                        _lhsIo_linePragmas
                        _lhsIo_monadic
                        _lhsIo_newtypes
                        _lhsIo_pretty
                        _lhsIo_rename
                        _lhsIo_sem
                        _lhsIo_sig
                        _lhsIo_splitsems
                        _lhsIo_strictwrap
                        _lhsIo_traces
                        _lhsIo_unbox
                        _lhsIoptions
                        _lhsIparamMap
                        _lhsIprefix
                        _lhsIquantMap
                        _lhsItypeSyns
                        _lhsIunfoldSemDom
                        _lhsIwith_sig
                        _lhsIwrappers ->
                          (let _lhsOchunks :: Chunks
                               _lhsOgathNts :: (Set NontermIdent)
                               _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               _hdOallNts :: (Set NontermIdent)
                               _hdOallPragmas :: PragmaMap
                               _hdOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Set Identifier)))
                               _hdOcontextMap :: ContextMap
                               _hdOderivings :: Derivings
                               _hdOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))))
                               _hdOo_case :: Bool
                               _hdOo_cata :: Bool
                               _hdOo_costcentre :: Bool
                               _hdOo_data :: (Maybe Bool)
                               _hdOo_linePragmas :: Bool
                               _hdOo_monadic :: Bool
                               _hdOo_newtypes :: Bool
                               _hdOo_pretty :: Bool
                               _hdOo_rename :: Bool
                               _hdOo_sem :: Bool
                               _hdOo_sig :: Bool
                               _hdOo_splitsems :: Bool
                               _hdOo_strictwrap :: Bool
                               _hdOo_traces :: Bool
                               _hdOo_unbox :: Bool
                               _hdOoptions :: Options
                               _hdOparamMap :: ParamMap
                               _hdOprefix :: String
                               _hdOquantMap :: QuantMap
                               _hdOtypeSyns :: TypeSyns
                               _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                               _hdOwith_sig :: Bool
                               _hdOwrappers :: (Set NontermIdent)
                               _tlOallNts :: (Set NontermIdent)
                               _tlOallPragmas :: PragmaMap
                               _tlOaroundMap :: (Map NontermIdent (Map ConstructorIdent (Set Identifier)))
                               _tlOcontextMap :: ContextMap
                               _tlOderivings :: Derivings
                               _tlOmergeMap :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))))
                               _tlOo_case :: Bool
                               _tlOo_cata :: Bool
                               _tlOo_costcentre :: Bool
                               _tlOo_data :: (Maybe Bool)
                               _tlOo_linePragmas :: Bool
                               _tlOo_monadic :: Bool
                               _tlOo_newtypes :: Bool
                               _tlOo_pretty :: Bool
                               _tlOo_rename :: Bool
                               _tlOo_sem :: Bool
                               _tlOo_sig :: Bool
                               _tlOo_splitsems :: Bool
                               _tlOo_strictwrap :: Bool
                               _tlOo_traces :: Bool
                               _tlOo_unbox :: Bool
                               _tlOoptions :: Options
                               _tlOparamMap :: ParamMap
                               _tlOprefix :: String
                               _tlOquantMap :: QuantMap
                               _tlOtypeSyns :: TypeSyns
                               _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                               _tlOwith_sig :: Bool
                               _tlOwrappers :: (Set NontermIdent)
                               _hdIchunks :: Chunks
                               _hdIgathNts :: (Set NontermIdent)
                               _hdIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               _tlIchunks :: Chunks
                               _tlIgathNts :: (Set NontermIdent)
                               _tlIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               -- use rule "./src-ag/GenerateCode.ag"(line 918, column 49)
                               _lhsOchunks =
                                   ({-# LINE 918 "./src-ag/GenerateCode.ag" #-}
                                    _hdIchunks ++ _tlIchunks
                                    {-# LINE 1711 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- use rule "./src-ag/GenerateCode.ag"(line 138, column 47)
                               _lhsOgathNts =
                                   ({-# LINE 138 "./src-ag/GenerateCode.ag" #-}
                                    _hdIgathNts `Set.union` _tlIgathNts
                                    {-# LINE 1717 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                               _lhsOsemDomUnfoldGath =
                                   ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                    _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
                                    {-# LINE 1723 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallNts =
                                   ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIallNts
                                    {-# LINE 1729 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOallPragmas =
                                   ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIallPragmas
                                    {-# LINE 1735 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOaroundMap =
                                   ({-# LINE 575 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIaroundMap
                                    {-# LINE 1741 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOcontextMap =
                                   ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIcontextMap
                                    {-# LINE 1747 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOderivings =
                                   ({-# LINE 986 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 1753 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOmergeMap =
                                   ({-# LINE 591 "./src-ag/GenerateCode.ag" #-}
                                    _lhsImergeMap
                                    {-# LINE 1759 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_case =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_case
                                    {-# LINE 1765 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_cata =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_cata
                                    {-# LINE 1771 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_costcentre =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_costcentre
                                    {-# LINE 1777 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_data =
                                   ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_data
                                    {-# LINE 1783 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_linePragmas =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_linePragmas
                                    {-# LINE 1789 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_monadic =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_monadic
                                    {-# LINE 1795 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_newtypes =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_newtypes
                                    {-# LINE 1801 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_pretty =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_pretty
                                    {-# LINE 1807 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_rename =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_rename
                                    {-# LINE 1813 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_sem =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_sem
                                    {-# LINE 1819 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_sig =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_sig
                                    {-# LINE 1825 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_splitsems =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_splitsems
                                    {-# LINE 1831 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_strictwrap =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_strictwrap
                                    {-# LINE 1837 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_traces =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_traces
                                    {-# LINE 1843 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOo_unbox =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_unbox
                                    {-# LINE 1849 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOoptions =
                                   ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 1855 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOparamMap =
                                   ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIparamMap
                                    {-# LINE 1861 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOprefix =
                                   ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIprefix
                                    {-# LINE 1867 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOquantMap =
                                   ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIquantMap
                                    {-# LINE 1873 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOtypeSyns =
                                   ({-# LINE 986 "./src-ag/GenerateCode.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 1879 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOunfoldSemDom =
                                   ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIunfoldSemDom
                                    {-# LINE 1885 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOwith_sig =
                                   ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIwith_sig
                                    {-# LINE 1891 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _hdOwrappers =
                                   ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 1897 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallNts =
                                   ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIallNts
                                    {-# LINE 1903 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOallPragmas =
                                   ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIallPragmas
                                    {-# LINE 1909 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOaroundMap =
                                   ({-# LINE 575 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIaroundMap
                                    {-# LINE 1915 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOcontextMap =
                                   ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIcontextMap
                                    {-# LINE 1921 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOderivings =
                                   ({-# LINE 986 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIderivings
                                    {-# LINE 1927 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOmergeMap =
                                   ({-# LINE 591 "./src-ag/GenerateCode.ag" #-}
                                    _lhsImergeMap
                                    {-# LINE 1933 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_case =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_case
                                    {-# LINE 1939 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_cata =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_cata
                                    {-# LINE 1945 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_costcentre =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_costcentre
                                    {-# LINE 1951 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_data =
                                   ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_data
                                    {-# LINE 1957 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_linePragmas =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_linePragmas
                                    {-# LINE 1963 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_monadic =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_monadic
                                    {-# LINE 1969 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_newtypes =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_newtypes
                                    {-# LINE 1975 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_pretty =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_pretty
                                    {-# LINE 1981 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_rename =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_rename
                                    {-# LINE 1987 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_sem =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_sem
                                    {-# LINE 1993 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_sig =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_sig
                                    {-# LINE 1999 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_splitsems =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_splitsems
                                    {-# LINE 2005 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_strictwrap =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_strictwrap
                                    {-# LINE 2011 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_traces =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_traces
                                    {-# LINE 2017 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOo_unbox =
                                   ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIo_unbox
                                    {-# LINE 2023 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOoptions =
                                   ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2029 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOparamMap =
                                   ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIparamMap
                                    {-# LINE 2035 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOprefix =
                                   ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIprefix
                                    {-# LINE 2041 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOquantMap =
                                   ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIquantMap
                                    {-# LINE 2047 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOtypeSyns =
                                   ({-# LINE 986 "./src-ag/GenerateCode.ag" #-}
                                    _lhsItypeSyns
                                    {-# LINE 2053 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOunfoldSemDom =
                                   ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIunfoldSemDom
                                    {-# LINE 2059 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOwith_sig =
                                   ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIwith_sig
                                    {-# LINE 2065 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- copy rule (down)
                               _tlOwrappers =
                                   ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                    _lhsIwrappers
                                    {-# LINE 2071 "dist/build/GenerateCode.hs" #-}
                                    )
                               ( _hdIchunks,_hdIgathNts,_hdIsemDomUnfoldGath) =
                                   hd_ _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOderivings _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOtypeSyns _hdOunfoldSemDom _hdOwith_sig _hdOwrappers
                               ( _tlIchunks,_tlIgathNts,_tlIsemDomUnfoldGath) =
                                   tl_ _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOderivings _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOtypeSyns _tlOunfoldSemDom _tlOwith_sig _tlOwrappers
                           in  ( _lhsOchunks,_lhsOgathNts,_lhsOsemDomUnfoldGath))))
sem_CNonterminals_Nil :: T_CNonterminals
sem_CNonterminals_Nil =
    (T_CNonterminals (\ _lhsIallNts
                        _lhsIallPragmas
                        _lhsIaroundMap
                        _lhsIcontextMap
                        _lhsIderivings
                        _lhsImergeMap
                        _lhsIo_case
                        _lhsIo_cata
                        _lhsIo_costcentre
                        _lhsIo_data
                        _lhsIo_linePragmas
                        _lhsIo_monadic
                        _lhsIo_newtypes
                        _lhsIo_pretty
                        _lhsIo_rename
                        _lhsIo_sem
                        _lhsIo_sig
                        _lhsIo_splitsems
                        _lhsIo_strictwrap
                        _lhsIo_traces
                        _lhsIo_unbox
                        _lhsIoptions
                        _lhsIparamMap
                        _lhsIprefix
                        _lhsIquantMap
                        _lhsItypeSyns
                        _lhsIunfoldSemDom
                        _lhsIwith_sig
                        _lhsIwrappers ->
                          (let _lhsOchunks :: Chunks
                               _lhsOgathNts :: (Set NontermIdent)
                               _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               -- use rule "./src-ag/GenerateCode.ag"(line 918, column 49)
                               _lhsOchunks =
                                   ({-# LINE 918 "./src-ag/GenerateCode.ag" #-}
                                    []
                                    {-# LINE 2116 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- use rule "./src-ag/GenerateCode.ag"(line 138, column 47)
                               _lhsOgathNts =
                                   ({-# LINE 138 "./src-ag/GenerateCode.ag" #-}
                                    Set.empty
                                    {-# LINE 2122 "dist/build/GenerateCode.hs" #-}
                                    )
                               -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                               _lhsOsemDomUnfoldGath =
                                   ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                    Map.empty
                                    {-# LINE 2128 "dist/build/GenerateCode.hs" #-}
                                    )
                           in  ( _lhsOchunks,_lhsOgathNts,_lhsOsemDomUnfoldGath))))
-- CProduction -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Map ConstructorIdent (Set Identifier)
         contextMap           : ContextMap
         inh                  : Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         syn                  : Attributes
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      synthesized attributes:
         cataAlt              : Decl
         comments             : [String]
         dataAlt              : DataAlt
         decls                : Decls
         semNames             : [String]
   alternatives:
      alternative CProduction:
         child con            : {ConstructorIdent}
         child visits         : CVisits 
         child children       : {[(Identifier,Type,ChildKind)]}
         child terminals      : {[Identifier]}
         visit 0:
            local paramInstMap : _
            local aroundMap   : _
            local mergeMap    : _
            local firstOrderChildren : _
            local params      : _
-}
-- cata
sem_CProduction :: CProduction ->
                   T_CProduction
sem_CProduction (CProduction _con _visits _children _terminals) =
    (sem_CProduction_CProduction _con (sem_CVisits _visits) _children _terminals)
-- semantic domain
newtype T_CProduction = T_CProduction ((Set NontermIdent) ->
                                       PragmaMap ->
                                       (Map ConstructorIdent (Set Identifier)) ->
                                       ContextMap ->
                                       Attributes ->
                                       (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
                                       NontermIdent ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       (Maybe Bool) ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Bool ->
                                       Options ->
                                       ParamMap ->
                                       String ->
                                       QuantMap ->
                                       Attributes ->
                                       (NontermIdent -> Int -> [String] -> Code.Type) ->
                                       Bool ->
                                       (Set NontermIdent) ->
                                       ( Decl,([String]),DataAlt,Decls,([String])))
data Inh_CProduction = Inh_CProduction {allNts_Inh_CProduction :: !((Set NontermIdent)),allPragmas_Inh_CProduction :: !(PragmaMap),aroundMap_Inh_CProduction :: !((Map ConstructorIdent (Set Identifier))),contextMap_Inh_CProduction :: !(ContextMap),inh_Inh_CProduction :: !(Attributes),mergeMap_Inh_CProduction :: !((Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))),nt_Inh_CProduction :: !(NontermIdent),o_case_Inh_CProduction :: !(Bool),o_cata_Inh_CProduction :: !(Bool),o_costcentre_Inh_CProduction :: !(Bool),o_data_Inh_CProduction :: !((Maybe Bool)),o_linePragmas_Inh_CProduction :: !(Bool),o_monadic_Inh_CProduction :: !(Bool),o_newtypes_Inh_CProduction :: !(Bool),o_pretty_Inh_CProduction :: !(Bool),o_rename_Inh_CProduction :: !(Bool),o_sem_Inh_CProduction :: !(Bool),o_sig_Inh_CProduction :: !(Bool),o_splitsems_Inh_CProduction :: !(Bool),o_strictwrap_Inh_CProduction :: !(Bool),o_traces_Inh_CProduction :: !(Bool),o_unbox_Inh_CProduction :: !(Bool),options_Inh_CProduction :: !(Options),paramMap_Inh_CProduction :: !(ParamMap),prefix_Inh_CProduction :: !(String),quantMap_Inh_CProduction :: !(QuantMap),syn_Inh_CProduction :: !(Attributes),unfoldSemDom_Inh_CProduction :: !((NontermIdent -> Int -> [String] -> Code.Type)),with_sig_Inh_CProduction :: !(Bool),wrappers_Inh_CProduction :: !((Set NontermIdent))}
data Syn_CProduction = Syn_CProduction {cataAlt_Syn_CProduction :: !(Decl),comments_Syn_CProduction :: !(([String])),dataAlt_Syn_CProduction :: !(DataAlt),decls_Syn_CProduction :: !(Decls),semNames_Syn_CProduction :: !(([String]))}
wrap_CProduction :: T_CProduction ->
                    Inh_CProduction ->
                    Syn_CProduction
wrap_CProduction (T_CProduction sem) (Inh_CProduction _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOcataAlt,_lhsOcomments,_lhsOdataAlt,_lhsOdecls,_lhsOsemNames) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
     in  (Syn_CProduction _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOsemNames))
sem_CProduction_CProduction :: ConstructorIdent ->
                               T_CVisits ->
                               ([(Identifier,Type,ChildKind)]) ->
                               ([Identifier]) ->
                               T_CProduction
sem_CProduction_CProduction con_ (T_CVisits visits_) children_ terminals_ =
    (T_CProduction (\ _lhsIallNts
                      _lhsIallPragmas
                      _lhsIaroundMap
                      _lhsIcontextMap
                      _lhsIinh
                      _lhsImergeMap
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_costcentre
                      _lhsIo_data
                      _lhsIo_linePragmas
                      _lhsIo_monadic
                      _lhsIo_newtypes
                      _lhsIo_pretty
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_splitsems
                      _lhsIo_strictwrap
                      _lhsIo_traces
                      _lhsIo_unbox
                      _lhsIoptions
                      _lhsIparamMap
                      _lhsIprefix
                      _lhsIquantMap
                      _lhsIsyn
                      _lhsIunfoldSemDom
                      _lhsIwith_sig
                      _lhsIwrappers ->
                        (let _visitsOcon :: ConstructorIdent
                             _visitsOterminals :: ([Identifier])
                             _visitsOvisitedSet :: (Set Identifier)
                             _visitsOnr :: Int
                             _visitsOchildren :: ([(Identifier,Type, ChildKind)])
                             _visitsOinstVisitNrs :: (Map Identifier Int)
                             _lhsOcomments :: ([String])
                             _lhsOdataAlt :: DataAlt
                             _lhsOcataAlt :: Decl
                             _lhsOdecls :: Decls
                             _lhsOsemNames :: ([String])
                             _visitsOallNts :: (Set NontermIdent)
                             _visitsOallPragmas :: PragmaMap
                             _visitsOaroundMap :: (Set Identifier)
                             _visitsOcontextMap :: ContextMap
                             _visitsOinh :: Attributes
                             _visitsOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                             _visitsOnt :: NontermIdent
                             _visitsOo_case :: Bool
                             _visitsOo_cata :: Bool
                             _visitsOo_costcentre :: Bool
                             _visitsOo_data :: (Maybe Bool)
                             _visitsOo_linePragmas :: Bool
                             _visitsOo_monadic :: Bool
                             _visitsOo_newtypes :: Bool
                             _visitsOo_pretty :: Bool
                             _visitsOo_rename :: Bool
                             _visitsOo_sem :: Bool
                             _visitsOo_sig :: Bool
                             _visitsOo_splitsems :: Bool
                             _visitsOo_strictwrap :: Bool
                             _visitsOo_traces :: Bool
                             _visitsOo_unbox :: Bool
                             _visitsOoptions :: Options
                             _visitsOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                             _visitsOparamMap :: ParamMap
                             _visitsOprefix :: String
                             _visitsOquantMap :: QuantMap
                             _visitsOsyn :: Attributes
                             _visitsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                             _visitsOwith_sig :: Bool
                             _visitsOwrappers :: (Set NontermIdent)
                             _visitsIcomments :: ([String])
                             _visitsIdecls :: Decls
                             _visitsIgatherInstVisitNrs :: (Map Identifier Int)
                             _visitsIintra :: Exprs
                             _visitsIintraVars :: (Set String)
                             _visitsIisNil :: Bool
                             _visitsIsemNames :: ([String])
                             _visitsIvisitedSet :: (Set Identifier)
                             -- "./src-ag/GenerateCode.ag"(line 92, column 19)
                             _visitsOcon =
                                 ({-# LINE 92 "./src-ag/GenerateCode.ag" #-}
                                  con_
                                  {-# LINE 2319 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 93, column 20)
                             _visitsOterminals =
                                 ({-# LINE 93 "./src-ag/GenerateCode.ag" #-}
                                  terminals_
                                  {-# LINE 2325 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 105, column 7)
                             _paramInstMap =
                                 ({-# LINE 105 "./src-ag/GenerateCode.ag" #-}
                                  Map.fromList [(nm, (extractNonterminal tp, tps)) | (nm,tp,_) <- children_, let tps = map cleanupArg $ nontermArgs tp, not (null tps) ]
                                  {-# LINE 2331 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 146, column 32)
                             _visitsOvisitedSet =
                                 ({-# LINE 146 "./src-ag/GenerateCode.ag" #-}
                                  Set.empty
                                  {-# LINE 2337 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 280, column 18)
                             _visitsOnr =
                                 ({-# LINE 280 "./src-ag/GenerateCode.ag" #-}
                                  0
                                  {-# LINE 2343 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 412, column 18)
                             _visitsOchildren =
                                 ({-# LINE 412 "./src-ag/GenerateCode.ag" #-}
                                  children_
                                  {-# LINE 2349 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 564, column 7)
                             _visitsOinstVisitNrs =
                                 ({-# LINE 564 "./src-ag/GenerateCode.ag" #-}
                                  _visitsIgatherInstVisitNrs
                                  {-# LINE 2355 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 584, column 34)
                             _aroundMap =
                                 ({-# LINE 584 "./src-ag/GenerateCode.ag" #-}
                                  Map.findWithDefault Set.empty con_ _lhsIaroundMap
                                  {-# LINE 2361 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 600, column 34)
                             _mergeMap =
                                 ({-# LINE 600 "./src-ag/GenerateCode.ag" #-}
                                  Map.findWithDefault Map.empty con_ _lhsImergeMap
                                  {-# LINE 2367 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 879, column 18)
                             _firstOrderChildren =
                                 ({-# LINE 879 "./src-ag/GenerateCode.ag" #-}
                                  [ (nm,fromJust mb,virt) | (nm,tp,virt) <- children_, let mb = isFirstOrder virt tp, isJust mb ]
                                  {-# LINE 2373 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 880, column 18)
                             _lhsOcomments =
                                 ({-# LINE 880 "./src-ag/GenerateCode.ag" #-}
                                  ("alternative " ++ getName con_ ++ ":")
                                  : map ind (  map (\(x,y,_) -> makeLocalComment 14 "child" x (Just y)) _firstOrderChildren
                                            ++ _visitsIcomments
                                            )
                                  {-# LINE 2382 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 1019, column 17)
                             _params =
                                 ({-# LINE 1019 "./src-ag/GenerateCode.ag" #-}
                                  map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                                  {-# LINE 2388 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 1020, column 17)
                             _lhsOdataAlt =
                                 ({-# LINE 1020 "./src-ag/GenerateCode.ag" #-}
                                  let conNm = conname _lhsIo_rename _lhsInt con_
                                      mkFields f = map (\(nm,t,_) -> f _lhsInt con_ nm (typeToCodeType (Just _lhsInt) _params     $ removeDeforested t)) _firstOrderChildren
                                  in if dataRecords _lhsIoptions
                                     then Record conNm $ mkFields $ toNamedType (strictData _lhsIoptions)
                                     else DataAlt conNm $ mkFields $ \_ _ _ t -> t
                                  {-# LINE 2398 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- "./src-ag/GenerateCode.ag"(line 1131, column 17)
                             _lhsOcataAlt =
                                 ({-# LINE 1131 "./src-ag/GenerateCode.ag" #-}
                                  let lhs = Fun (cataname _lhsIprefix _lhsInt) [lhs_pat]
                                      lhs_pat = App (conname _lhsIo_rename _lhsInt con_)
                                                     (map (\(n,_,_) -> SimpleExpr $ locname $ n) _firstOrderChildren    )
                                      rhs = App (semname _lhsIprefix _lhsInt con_)
                                                 (map argument _firstOrderChildren    )
                                      argument (nm,NT tp _ _,_) = App (cataname _lhsIprefix tp)
                                                                       [SimpleExpr (locname nm)]
                                      argument (nm, _,_)    = SimpleExpr (locname nm)
                                   in Decl lhs rhs Set.empty Set.empty
                                  {-# LINE 2412 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- use rule "./src-ag/GenerateCode.ag"(line 919, column 44)
                             _lhsOdecls =
                                 ({-# LINE 919 "./src-ag/GenerateCode.ag" #-}
                                  _visitsIdecls
                                  {-# LINE 2418 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- use rule "./src-ag/GenerateCode.ag"(line 1146, column 61)
                             _lhsOsemNames =
                                 ({-# LINE 1146 "./src-ag/GenerateCode.ag" #-}
                                  _visitsIsemNames
                                  {-# LINE 2424 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallNts =
                                 ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIallNts
                                  {-# LINE 2430 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOallPragmas =
                                 ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIallPragmas
                                  {-# LINE 2436 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (from local)
                             _visitsOaroundMap =
                                 ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                                  _aroundMap
                                  {-# LINE 2442 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOcontextMap =
                                 ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIcontextMap
                                  {-# LINE 2448 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOinh =
                                 ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2454 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (from local)
                             _visitsOmergeMap =
                                 ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                                  _mergeMap
                                  {-# LINE 2460 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOnt =
                                 ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                  _lhsInt
                                  {-# LINE 2466 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_case =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_case
                                  {-# LINE 2472 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_cata =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_cata
                                  {-# LINE 2478 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_costcentre =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_costcentre
                                  {-# LINE 2484 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_data =
                                 ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_data
                                  {-# LINE 2490 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_linePragmas =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_linePragmas
                                  {-# LINE 2496 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_monadic =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_monadic
                                  {-# LINE 2502 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_newtypes =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_newtypes
                                  {-# LINE 2508 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_pretty =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_pretty
                                  {-# LINE 2514 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_rename =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 2520 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_sem =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_sem
                                  {-# LINE 2526 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_sig =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_sig
                                  {-# LINE 2532 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_splitsems =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_splitsems
                                  {-# LINE 2538 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_strictwrap =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_strictwrap
                                  {-# LINE 2544 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_traces =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_traces
                                  {-# LINE 2550 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOo_unbox =
                                 ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIo_unbox
                                  {-# LINE 2556 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOoptions =
                                 ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIoptions
                                  {-# LINE 2562 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (from local)
                             _visitsOparamInstMap =
                                 ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                                  _paramInstMap
                                  {-# LINE 2568 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOparamMap =
                                 ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIparamMap
                                  {-# LINE 2574 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOprefix =
                                 ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIprefix
                                  {-# LINE 2580 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOquantMap =
                                 ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIquantMap
                                  {-# LINE 2586 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOsyn =
                                 ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2592 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOunfoldSemDom =
                                 ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIunfoldSemDom
                                  {-# LINE 2598 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOwith_sig =
                                 ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIwith_sig
                                  {-# LINE 2604 "dist/build/GenerateCode.hs" #-}
                                  )
                             -- copy rule (down)
                             _visitsOwrappers =
                                 ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                  _lhsIwrappers
                                  {-# LINE 2610 "dist/build/GenerateCode.hs" #-}
                                  )
                             ( _visitsIcomments,_visitsIdecls,_visitsIgatherInstVisitNrs,_visitsIintra,_visitsIintraVars,_visitsIisNil,_visitsIsemNames,_visitsIvisitedSet) =
                                 visits_ _visitsOallNts _visitsOallPragmas _visitsOaroundMap _visitsOchildren _visitsOcon _visitsOcontextMap _visitsOinh _visitsOinstVisitNrs _visitsOmergeMap _visitsOnr _visitsOnt _visitsOo_case _visitsOo_cata _visitsOo_costcentre _visitsOo_data _visitsOo_linePragmas _visitsOo_monadic _visitsOo_newtypes _visitsOo_pretty _visitsOo_rename _visitsOo_sem _visitsOo_sig _visitsOo_splitsems _visitsOo_strictwrap _visitsOo_traces _visitsOo_unbox _visitsOoptions _visitsOparamInstMap _visitsOparamMap _visitsOprefix _visitsOquantMap _visitsOsyn _visitsOterminals _visitsOunfoldSemDom _visitsOvisitedSet _visitsOwith_sig _visitsOwrappers
                         in  ( _lhsOcataAlt,_lhsOcomments,_lhsOdataAlt,_lhsOdecls,_lhsOsemNames))))
-- CProductions ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Map ConstructorIdent (Set Identifier)
         contextMap           : ContextMap
         inh                  : Attributes
         mergeMap             : Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         syn                  : Attributes
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      synthesized attributes:
         cataAlts             : Decls
         comments             : [String]
         dataAlts             : DataAlts
         decls                : Decls
         semNames             : [String]
   alternatives:
      alternative Cons:
         child hd             : CProduction 
         child tl             : CProductions 
      alternative Nil:
-}
-- cata
sem_CProductions :: CProductions ->
                    T_CProductions
sem_CProductions list =
    (Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list))
-- semantic domain
newtype T_CProductions = T_CProductions ((Set NontermIdent) ->
                                         PragmaMap ->
                                         (Map ConstructorIdent (Set Identifier)) ->
                                         ContextMap ->
                                         Attributes ->
                                         (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
                                         NontermIdent ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         (Maybe Bool) ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Bool ->
                                         Options ->
                                         ParamMap ->
                                         String ->
                                         QuantMap ->
                                         Attributes ->
                                         (NontermIdent -> Int -> [String] -> Code.Type) ->
                                         Bool ->
                                         (Set NontermIdent) ->
                                         ( Decls,([String]),DataAlts,Decls,([String])))
data Inh_CProductions = Inh_CProductions {allNts_Inh_CProductions :: !((Set NontermIdent)),allPragmas_Inh_CProductions :: !(PragmaMap),aroundMap_Inh_CProductions :: !((Map ConstructorIdent (Set Identifier))),contextMap_Inh_CProductions :: !(ContextMap),inh_Inh_CProductions :: !(Attributes),mergeMap_Inh_CProductions :: !((Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))),nt_Inh_CProductions :: !(NontermIdent),o_case_Inh_CProductions :: !(Bool),o_cata_Inh_CProductions :: !(Bool),o_costcentre_Inh_CProductions :: !(Bool),o_data_Inh_CProductions :: !((Maybe Bool)),o_linePragmas_Inh_CProductions :: !(Bool),o_monadic_Inh_CProductions :: !(Bool),o_newtypes_Inh_CProductions :: !(Bool),o_pretty_Inh_CProductions :: !(Bool),o_rename_Inh_CProductions :: !(Bool),o_sem_Inh_CProductions :: !(Bool),o_sig_Inh_CProductions :: !(Bool),o_splitsems_Inh_CProductions :: !(Bool),o_strictwrap_Inh_CProductions :: !(Bool),o_traces_Inh_CProductions :: !(Bool),o_unbox_Inh_CProductions :: !(Bool),options_Inh_CProductions :: !(Options),paramMap_Inh_CProductions :: !(ParamMap),prefix_Inh_CProductions :: !(String),quantMap_Inh_CProductions :: !(QuantMap),syn_Inh_CProductions :: !(Attributes),unfoldSemDom_Inh_CProductions :: !((NontermIdent -> Int -> [String] -> Code.Type)),with_sig_Inh_CProductions :: !(Bool),wrappers_Inh_CProductions :: !((Set NontermIdent))}
data Syn_CProductions = Syn_CProductions {cataAlts_Syn_CProductions :: !(Decls),comments_Syn_CProductions :: !(([String])),dataAlts_Syn_CProductions :: !(DataAlts),decls_Syn_CProductions :: !(Decls),semNames_Syn_CProductions :: !(([String]))}
wrap_CProductions :: T_CProductions ->
                     Inh_CProductions ->
                     Syn_CProductions
wrap_CProductions (T_CProductions sem) (Inh_CProductions _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOsemNames) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
     in  (Syn_CProductions _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames))
sem_CProductions_Cons :: T_CProduction ->
                         T_CProductions ->
                         T_CProductions
sem_CProductions_Cons (T_CProduction hd_) (T_CProductions tl_) =
    (T_CProductions (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIaroundMap
                       _lhsIcontextMap
                       _lhsIinh
                       _lhsImergeMap
                       _lhsInt
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_monadic
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIoptions
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsIquantMap
                       _lhsIsyn
                       _lhsIunfoldSemDom
                       _lhsIwith_sig
                       _lhsIwrappers ->
                         (let _lhsOdataAlts :: DataAlts
                              _lhsOcataAlts :: Decls
                              _lhsOcomments :: ([String])
                              _lhsOdecls :: Decls
                              _lhsOsemNames :: ([String])
                              _hdOallNts :: (Set NontermIdent)
                              _hdOallPragmas :: PragmaMap
                              _hdOaroundMap :: (Map ConstructorIdent (Set Identifier))
                              _hdOcontextMap :: ContextMap
                              _hdOinh :: Attributes
                              _hdOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))
                              _hdOnt :: NontermIdent
                              _hdOo_case :: Bool
                              _hdOo_cata :: Bool
                              _hdOo_costcentre :: Bool
                              _hdOo_data :: (Maybe Bool)
                              _hdOo_linePragmas :: Bool
                              _hdOo_monadic :: Bool
                              _hdOo_newtypes :: Bool
                              _hdOo_pretty :: Bool
                              _hdOo_rename :: Bool
                              _hdOo_sem :: Bool
                              _hdOo_sig :: Bool
                              _hdOo_splitsems :: Bool
                              _hdOo_strictwrap :: Bool
                              _hdOo_traces :: Bool
                              _hdOo_unbox :: Bool
                              _hdOoptions :: Options
                              _hdOparamMap :: ParamMap
                              _hdOprefix :: String
                              _hdOquantMap :: QuantMap
                              _hdOsyn :: Attributes
                              _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _hdOwith_sig :: Bool
                              _hdOwrappers :: (Set NontermIdent)
                              _tlOallNts :: (Set NontermIdent)
                              _tlOallPragmas :: PragmaMap
                              _tlOaroundMap :: (Map ConstructorIdent (Set Identifier))
                              _tlOcontextMap :: ContextMap
                              _tlOinh :: Attributes
                              _tlOmergeMap :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))
                              _tlOnt :: NontermIdent
                              _tlOo_case :: Bool
                              _tlOo_cata :: Bool
                              _tlOo_costcentre :: Bool
                              _tlOo_data :: (Maybe Bool)
                              _tlOo_linePragmas :: Bool
                              _tlOo_monadic :: Bool
                              _tlOo_newtypes :: Bool
                              _tlOo_pretty :: Bool
                              _tlOo_rename :: Bool
                              _tlOo_sem :: Bool
                              _tlOo_sig :: Bool
                              _tlOo_splitsems :: Bool
                              _tlOo_strictwrap :: Bool
                              _tlOo_traces :: Bool
                              _tlOo_unbox :: Bool
                              _tlOoptions :: Options
                              _tlOparamMap :: ParamMap
                              _tlOprefix :: String
                              _tlOquantMap :: QuantMap
                              _tlOsyn :: Attributes
                              _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _tlOwith_sig :: Bool
                              _tlOwrappers :: (Set NontermIdent)
                              _hdIcataAlt :: Decl
                              _hdIcomments :: ([String])
                              _hdIdataAlt :: DataAlt
                              _hdIdecls :: Decls
                              _hdIsemNames :: ([String])
                              _tlIcataAlts :: Decls
                              _tlIcomments :: ([String])
                              _tlIdataAlts :: DataAlts
                              _tlIdecls :: Decls
                              _tlIsemNames :: ([String])
                              -- "./src-ag/GenerateCode.ag"(line 1015, column 17)
                              _lhsOdataAlts =
                                  ({-# LINE 1015 "./src-ag/GenerateCode.ag" #-}
                                   _hdIdataAlt : _tlIdataAlts
                                   {-# LINE 2819 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 1127, column 10)
                              _lhsOcataAlts =
                                  ({-# LINE 1127 "./src-ag/GenerateCode.ag" #-}
                                   _hdIcataAlt : _tlIcataAlts
                                   {-# LINE 2825 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                              _lhsOcomments =
                                  ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                                   _hdIcomments ++ _tlIcomments
                                   {-# LINE 2831 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 919, column 44)
                              _lhsOdecls =
                                  ({-# LINE 919 "./src-ag/GenerateCode.ag" #-}
                                   _hdIdecls ++ _tlIdecls
                                   {-# LINE 2837 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 1146, column 61)
                              _lhsOsemNames =
                                  ({-# LINE 1146 "./src-ag/GenerateCode.ag" #-}
                                   _hdIsemNames ++ _tlIsemNames
                                   {-# LINE 2843 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallNts =
                                  ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallNts
                                   {-# LINE 2849 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOallPragmas =
                                  ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallPragmas
                                   {-# LINE 2855 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOaroundMap =
                                  ({-# LINE 578 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 2861 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOcontextMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIcontextMap
                                   {-# LINE 2867 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinh =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIinh
                                   {-# LINE 2873 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOmergeMap =
                                  ({-# LINE 594 "./src-ag/GenerateCode.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 2879 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnt =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsInt
                                   {-# LINE 2885 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_case =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 2891 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_cata =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 2897 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_costcentre =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_costcentre
                                   {-# LINE 2903 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_data =
                                  ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 2909 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_linePragmas =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_linePragmas
                                   {-# LINE 2915 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_monadic =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_monadic
                                   {-# LINE 2921 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_newtypes =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 2927 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_pretty =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_pretty
                                   {-# LINE 2933 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_rename =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 2939 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_sem =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 2945 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_sig =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 2951 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_splitsems =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_splitsems
                                   {-# LINE 2957 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_strictwrap =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_strictwrap
                                   {-# LINE 2963 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_traces =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_traces
                                   {-# LINE 2969 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_unbox =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 2975 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOoptions =
                                  ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 2981 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOparamMap =
                                  ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIparamMap
                                   {-# LINE 2987 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOprefix =
                                  ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 2993 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOquantMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIquantMap
                                   {-# LINE 2999 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsyn =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIsyn
                                   {-# LINE 3005 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOunfoldSemDom =
                                  ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIunfoldSemDom
                                   {-# LINE 3011 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOwith_sig =
                                  ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwith_sig
                                   {-# LINE 3017 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOwrappers =
                                  ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwrappers
                                   {-# LINE 3023 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallNts =
                                  ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallNts
                                   {-# LINE 3029 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOallPragmas =
                                  ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIallPragmas
                                   {-# LINE 3035 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOaroundMap =
                                  ({-# LINE 578 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIaroundMap
                                   {-# LINE 3041 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOcontextMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIcontextMap
                                   {-# LINE 3047 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinh =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIinh
                                   {-# LINE 3053 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOmergeMap =
                                  ({-# LINE 594 "./src-ag/GenerateCode.ag" #-}
                                   _lhsImergeMap
                                   {-# LINE 3059 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnt =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsInt
                                   {-# LINE 3065 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_case =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_case
                                   {-# LINE 3071 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_cata =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_cata
                                   {-# LINE 3077 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_costcentre =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_costcentre
                                   {-# LINE 3083 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_data =
                                  ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_data
                                   {-# LINE 3089 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_linePragmas =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_linePragmas
                                   {-# LINE 3095 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_monadic =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_monadic
                                   {-# LINE 3101 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_newtypes =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_newtypes
                                   {-# LINE 3107 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_pretty =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_pretty
                                   {-# LINE 3113 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_rename =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 3119 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_sem =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sem
                                   {-# LINE 3125 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_sig =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_sig
                                   {-# LINE 3131 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_splitsems =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_splitsems
                                   {-# LINE 3137 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_strictwrap =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_strictwrap
                                   {-# LINE 3143 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_traces =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_traces
                                   {-# LINE 3149 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_unbox =
                                  ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIo_unbox
                                   {-# LINE 3155 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOoptions =
                                  ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIoptions
                                   {-# LINE 3161 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOparamMap =
                                  ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIparamMap
                                   {-# LINE 3167 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOprefix =
                                  ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIprefix
                                   {-# LINE 3173 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOquantMap =
                                  ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIquantMap
                                   {-# LINE 3179 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsyn =
                                  ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIsyn
                                   {-# LINE 3185 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOunfoldSemDom =
                                  ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIunfoldSemDom
                                   {-# LINE 3191 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOwith_sig =
                                  ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwith_sig
                                   {-# LINE 3197 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOwrappers =
                                  ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                                   _lhsIwrappers
                                   {-# LINE 3203 "dist/build/GenerateCode.hs" #-}
                                   )
                              ( _hdIcataAlt,_hdIcomments,_hdIdataAlt,_hdIdecls,_hdIsemNames) =
                                  hd_ _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOinh _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOunfoldSemDom _hdOwith_sig _hdOwrappers
                              ( _tlIcataAlts,_tlIcomments,_tlIdataAlts,_tlIdecls,_tlIsemNames) =
                                  tl_ _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOinh _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOunfoldSemDom _tlOwith_sig _tlOwrappers
                          in  ( _lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOsemNames))))
sem_CProductions_Nil :: T_CProductions
sem_CProductions_Nil =
    (T_CProductions (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIaroundMap
                       _lhsIcontextMap
                       _lhsIinh
                       _lhsImergeMap
                       _lhsInt
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_monadic
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIoptions
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsIquantMap
                       _lhsIsyn
                       _lhsIunfoldSemDom
                       _lhsIwith_sig
                       _lhsIwrappers ->
                         (let _lhsOdataAlts :: DataAlts
                              _lhsOcataAlts :: Decls
                              _lhsOcomments :: ([String])
                              _lhsOdecls :: Decls
                              _lhsOsemNames :: ([String])
                              -- "./src-ag/GenerateCode.ag"(line 1016, column 17)
                              _lhsOdataAlts =
                                  ({-# LINE 1016 "./src-ag/GenerateCode.ag" #-}
                                   []
                                   {-# LINE 3251 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- "./src-ag/GenerateCode.ag"(line 1128, column 10)
                              _lhsOcataAlts =
                                  ({-# LINE 1128 "./src-ag/GenerateCode.ag" #-}
                                   []
                                   {-# LINE 3257 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                              _lhsOcomments =
                                  ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                                   []
                                   {-# LINE 3263 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 919, column 44)
                              _lhsOdecls =
                                  ({-# LINE 919 "./src-ag/GenerateCode.ag" #-}
                                   []
                                   {-# LINE 3269 "dist/build/GenerateCode.hs" #-}
                                   )
                              -- use rule "./src-ag/GenerateCode.ag"(line 1146, column 61)
                              _lhsOsemNames =
                                  ({-# LINE 1146 "./src-ag/GenerateCode.ag" #-}
                                   []
                                   {-# LINE 3275 "dist/build/GenerateCode.hs" #-}
                                   )
                          in  ( _lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOsemNames))))
-- CRule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         aroundMap            : Set Identifier
         children             : [(Identifier,Type,ChildKind)]
         con                  : ConstructorIdent
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         mergeMap             : Map Identifier (Identifier, [Identifier])
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         what                 : String
      chained attributes:
         declsAbove           : [Decl]
         visitedSet           : Set Identifier
      synthesized attributes:
         allTpsFound          : Bool
         bldBlocksFun         : DeclBlocks -> DeclBlocks
         comments             : [String]
         decls                : Decls
         definedInsts         : [Identifier]
         exprs                : Exprs
         tSigs                : [Decl]
         tps                  : [Type]
         usedVars             : Set String
   alternatives:
      alternative CRule:
         child name           : {Identifier}
         child isIn           : {Bool}
         child hasCode        : {Bool}
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child childnt        : {Maybe NontermIdent}
         child tp             : {Maybe Type}
         child pattern        : Pattern 
         child rhs            : {[String]}
         child defines        : {Map Int (Identifier,Identifier,Maybe Type)}
         child owrt           : {Bool}
         child origin         : {String}
         child uses           : {Set (Identifier, Identifier)}
         child explicit       : {Bool}
         child mbNamed        : {Maybe Identifier}
         visit 0:
            local instTypes   : _
            local originComment : _
            local instDecls   : _
            local patDescr    : _
            local traceDescr  : _
            local addTrace    : _
            local costCentreDescr : _
            local addCostCentre : _
            local addLinePragma : _
            local decls       : _
            local definedInsts : _
            local rulename    : _
            local mkTp        : _
            local orgParams   : _
            local evalTp      : _
      alternative CChildVisit:
         child name           : {Identifier}
         child nt             : {NontermIdent}
         child nr             : {Int}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child isLast         : {Bool}
         visit 0:
            local visitedSet  : _
            local costCentreDescr : _
            local addCostCentre : _
            local decls       : _
            local isSuperfluousHigherOrderIntra : _
            local names       : _
            local mkTp        : _
            local definedTps  : _
            local nextTp      : _
            local orgParams   : _
            local instParams  : _
            local replParamMap : _
            local replace     : _
            local evalTp      : _
-}
-- cata
sem_CRule :: CRule ->
             T_CRule
sem_CRule (CRule _name _isIn _hasCode _nt _con _field _childnt _tp _pattern _rhs _defines _owrt _origin _uses _explicit _mbNamed) =
    (sem_CRule_CRule _name _isIn _hasCode _nt _con _field _childnt _tp (sem_Pattern _pattern) _rhs _defines _owrt _origin _uses _explicit _mbNamed)
sem_CRule (CChildVisit _name _nt _nr _inh _syn _isLast) =
    (sem_CRule_CChildVisit _name _nt _nr _inh _syn _isLast)
-- semantic domain
newtype T_CRule = T_CRule ((Set NontermIdent) ->
                           (Set Identifier) ->
                           ([(Identifier,Type,ChildKind)]) ->
                           ConstructorIdent ->
                           ([Decl]) ->
                           Attributes ->
                           (Map Identifier Int) ->
                           (Map Identifier (Identifier, [Identifier])) ->
                           Int ->
                           NontermIdent ->
                           Bool ->
                           Bool ->
                           Bool ->
                           (Maybe Bool) ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Bool ->
                           Options ->
                           (Map Identifier (NontermIdent, [String])) ->
                           ParamMap ->
                           String ->
                           Attributes ->
                           ([Identifier]) ->
                           (NontermIdent -> Int -> [String] -> Code.Type) ->
                           (Set Identifier) ->
                           String ->
                           ( Bool,(DeclBlocks -> DeclBlocks),([String]),Decls,([Decl]),([Identifier]),Exprs,([Decl]),([Type]),(Set String),(Set Identifier)))
data Inh_CRule = Inh_CRule {allNts_Inh_CRule :: !((Set NontermIdent)),aroundMap_Inh_CRule :: !((Set Identifier)),children_Inh_CRule :: !(([(Identifier,Type,ChildKind)])),con_Inh_CRule :: !(ConstructorIdent),declsAbove_Inh_CRule :: !(([Decl])),inh_Inh_CRule :: !(Attributes),instVisitNrs_Inh_CRule :: !((Map Identifier Int)),mergeMap_Inh_CRule :: !((Map Identifier (Identifier, [Identifier]))),nr_Inh_CRule :: !(Int),nt_Inh_CRule :: !(NontermIdent),o_case_Inh_CRule :: !(Bool),o_cata_Inh_CRule :: !(Bool),o_costcentre_Inh_CRule :: !(Bool),o_data_Inh_CRule :: !((Maybe Bool)),o_linePragmas_Inh_CRule :: !(Bool),o_monadic_Inh_CRule :: !(Bool),o_newtypes_Inh_CRule :: !(Bool),o_pretty_Inh_CRule :: !(Bool),o_rename_Inh_CRule :: !(Bool),o_sem_Inh_CRule :: !(Bool),o_sig_Inh_CRule :: !(Bool),o_splitsems_Inh_CRule :: !(Bool),o_strictwrap_Inh_CRule :: !(Bool),o_traces_Inh_CRule :: !(Bool),o_unbox_Inh_CRule :: !(Bool),options_Inh_CRule :: !(Options),paramInstMap_Inh_CRule :: !((Map Identifier (NontermIdent, [String]))),paramMap_Inh_CRule :: !(ParamMap),prefix_Inh_CRule :: !(String),syn_Inh_CRule :: !(Attributes),terminals_Inh_CRule :: !(([Identifier])),unfoldSemDom_Inh_CRule :: !((NontermIdent -> Int -> [String] -> Code.Type)),visitedSet_Inh_CRule :: !((Set Identifier)),what_Inh_CRule :: !(String)}
data Syn_CRule = Syn_CRule {allTpsFound_Syn_CRule :: !(Bool),bldBlocksFun_Syn_CRule :: !((DeclBlocks -> DeclBlocks)),comments_Syn_CRule :: !(([String])),decls_Syn_CRule :: !(Decls),declsAbove_Syn_CRule :: !(([Decl])),definedInsts_Syn_CRule :: !(([Identifier])),exprs_Syn_CRule :: !(Exprs),tSigs_Syn_CRule :: !(([Decl])),tps_Syn_CRule :: !(([Type])),usedVars_Syn_CRule :: !((Set String)),visitedSet_Syn_CRule :: !((Set Identifier))}
wrap_CRule :: T_CRule ->
              Inh_CRule ->
              Syn_CRule
wrap_CRule (T_CRule sem) (Inh_CRule _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
    (let ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet) = sem _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
     in  (Syn_CRule _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet))
sem_CRule_CRule :: Identifier ->
                   Bool ->
                   Bool ->
                   NontermIdent ->
                   ConstructorIdent ->
                   Identifier ->
                   (Maybe NontermIdent) ->
                   (Maybe Type) ->
                   T_Pattern ->
                   ([String]) ->
                   (Map Int (Identifier,Identifier,Maybe Type)) ->
                   Bool ->
                   String ->
                   (Set (Identifier, Identifier)) ->
                   Bool ->
                   (Maybe Identifier) ->
                   T_CRule
sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ (T_Pattern pattern_) rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_ =
    (T_CRule (\ _lhsIallNts
                _lhsIaroundMap
                _lhsIchildren
                _lhsIcon
                _lhsIdeclsAbove
                _lhsIinh
                _lhsIinstVisitNrs
                _lhsImergeMap
                _lhsInr
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_costcentre
                _lhsIo_data
                _lhsIo_linePragmas
                _lhsIo_monadic
                _lhsIo_newtypes
                _lhsIo_pretty
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_splitsems
                _lhsIo_strictwrap
                _lhsIo_traces
                _lhsIo_unbox
                _lhsIoptions
                _lhsIparamInstMap
                _lhsIparamMap
                _lhsIprefix
                _lhsIsyn
                _lhsIterminals
                _lhsIunfoldSemDom
                _lhsIvisitedSet
                _lhsIwhat ->
                  (let _lhsOexprs :: Exprs
                       _lhsOusedVars :: (Set String)
                       _lhsOtSigs :: ([Decl])
                       _lhsOtps :: ([Type])
                       _lhsOallTpsFound :: Bool
                       _lhsOdeclsAbove :: ([Decl])
                       _lhsObldBlocksFun :: (DeclBlocks -> DeclBlocks)
                       _lhsOcomments :: ([String])
                       _lhsOdecls :: Decls
                       _lhsOdefinedInsts :: ([Identifier])
                       _lhsOvisitedSet :: (Set Identifier)
                       _patternIcopy :: Pattern
                       _patternIdefinedInsts :: ([Identifier])
                       _patternIpatternAttributes :: ([(Identifier, Identifier)])
                       -- "./src-ag/GenerateCode.ag"(line 157, column 12)
                       _instTypes =
                           ({-# LINE 157 "./src-ag/GenerateCode.ag" #-}
                            [ (n, (t, mb, for)) | (n, NT t _ for, mb) <- _lhsIchildren ]
                            {-# LINE 3505 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 158, column 12)
                       _originComment =
                           ({-# LINE 158 "./src-ag/GenerateCode.ag" #-}
                            if  _lhsIo_pretty
                                then (Comment origin_:)
                                else id
                            {-# LINE 3513 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 161, column 12)
                       _instDecls =
                           ({-# LINE 161 "./src-ag/GenerateCode.ag" #-}
                            [ mkDecl _lhsIo_monadic (Pattern3 (Alias _INST' inst (Underscore (getPos inst))))
                                   ( let (nm,mb,defor) = fromJust $ inst `lookup` _instTypes
                                     in unwrapSem _lhsIo_newtypes nm
                                        $ case mb of
                                            ChildReplace tp' -> App instLocFieldName [SimpleExpr $ fieldname inst]
                                            _                ->
                                               if defor
                                               then SimpleExpr instLocFieldName
                                               else App (cataname _lhsIprefix nm)
                                                              [SimpleExpr instLocFieldName]
                                   )
                                   (Set.singleton instSemFieldName)
                                   (Set.singleton instLocFieldName)
                            | inst <- _definedInsts
                            , let instLocFieldName = attrname True _INST inst
                                  instSemFieldName = attrname False _INST' inst
                            ]
                            {-# LINE 3535 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 178, column 12)
                       _patDescr =
                           ({-# LINE 178 "./src-ag/GenerateCode.ag" #-}
                            if isIn_
                            then "_"
                            else concat $ intersperse "," (map (\(f,a) -> show f ++ "." ++ show a) _patternIpatternAttributes)
                            {-# LINE 3543 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 181, column 12)
                       _traceDescr =
                           ({-# LINE 181 "./src-ag/GenerateCode.ag" #-}
                            (maybe "" (\nm -> show nm ++ ":") mbNamed_) ++ show nt_ ++ " :: " ++ show con_ ++ " :: " ++ _patDescr
                            {-# LINE 3549 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 183, column 12)
                       _addTrace =
                           ({-# LINE 183 "./src-ag/GenerateCode.ag" #-}
                            \v -> if _lhsIo_traces
                                  then Trace _traceDescr     v
                                  else v
                            {-# LINE 3557 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 186, column 12)
                       _costCentreDescr =
                           ({-# LINE 186 "./src-ag/GenerateCode.ag" #-}
                            show nt_ ++ ":" ++ show con_ ++ ":" ++ _patDescr
                            {-# LINE 3563 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 187, column 12)
                       _addCostCentre =
                           ({-# LINE 187 "./src-ag/GenerateCode.ag" #-}
                            \v -> if _lhsIo_costcentre
                                  then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                  else v
                            {-# LINE 3571 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 190, column 12)
                       _addLinePragma =
                           ({-# LINE 190 "./src-ag/GenerateCode.ag" #-}
                            \v -> let p = getPos name_
                                      hasPos = line p > 0 && column p >= 0 && not (null (file p))
                                  in if _lhsIo_linePragmas && hasPos
                                     then PragmaExpr True True ("LINE " ++ show (line p) ++ " " ++ show (file p))
                                          $ LineExpr
                                          $ v
                                     else v
                            {-# LINE 3583 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 197, column 12)
                       _decls =
                           ({-# LINE 197 "./src-ag/GenerateCode.ag" #-}
                            if hasCode_
                            then _originComment ( mkDecl (_lhsIo_monadic && explicit_) (Pattern3 _patternIcopy) (_addTrace     $ _addCostCentre     $ _addLinePragma     $ (TextExpr rhs_))
                                                       (Set.fromList [attrname False fld nm | (fld,nm,_) <- Map.elems defines_])
                                                       (Set.fromList [attrname True fld nm | (fld,nm) <- Set.toList uses_])
                                                : _instDecls    )
                            else _instDecls
                            {-# LINE 3594 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 266, column 12)
                       _definedInsts =
                           ({-# LINE 266 "./src-ag/GenerateCode.ag" #-}
                            if isIn_ then [] else _patternIdefinedInsts
                            {-# LINE 3600 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 336, column 12)
                       _rulename =
                           ({-# LINE 336 "./src-ag/GenerateCode.ag" #-}
                            if  field_ == _LOC && name_ `elem` _lhsIterminals
                            then funname name_ 0
                            else attrname isIn_ field_ name_
                            {-# LINE 3608 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 339, column 12)
                       _lhsOexprs =
                           ({-# LINE 339 "./src-ag/GenerateCode.ag" #-}
                            [SimpleExpr _rulename    ]
                            {-# LINE 3614 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 355, column 7)
                       _lhsOusedVars =
                           ({-# LINE 355 "./src-ag/GenerateCode.ag" #-}
                            Set.singleton _rulename
                            {-# LINE 3620 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 365, column 19)
                       _mkTp =
                           ({-# LINE 365 "./src-ag/GenerateCode.ag" #-}
                            typeToCodeType (Just _lhsInt) _orgParams
                            {-# LINE 3626 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 366, column 19)
                       _lhsOtSigs =
                           ({-# LINE 366 "./src-ag/GenerateCode.ag" #-}
                            [ TSig (attrname False field attr) tp'
                            |  (field,attr,tp) <- Map.elems defines_, isJust tp
                            , let tp1 = _evalTp     field $ _mkTp (fromJust tp)
                                  tp' = case findOrigType attr _lhsIchildren of
                                         Just tp' -> let tp2 = _evalTp     field $ _mkTp tp'
                                                     in Arr tp2 tp1
                                         Nothing -> tp1
                                  findOrigType nm [] = Nothing
                                  findOrigType nm ((n,_,kind) : r)
                                    | nm == n = case kind of
                                                  ChildReplace orig -> Just orig
                                                  _                 -> Nothing
                                    | otherwise = findOrigType nm r
                            ]
                            {-# LINE 3645 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 381, column 19)
                       _orgParams =
                           ({-# LINE 381 "./src-ag/GenerateCode.ag" #-}
                            map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                            {-# LINE 3651 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 382, column 19)
                       _evalTp =
                           ({-# LINE 382 "./src-ag/GenerateCode.ag" #-}
                            \field tp -> let orgFldParams = map getName $ Map.findWithDefault [] childNt _lhsIparamMap
                                             (childNt,instParams) = Map.findWithDefault (_lhsInt,[]) field _lhsIparamInstMap
                                             replMap = Map.fromList (zip orgFldParams instParams)
                                             replace k = Map.findWithDefault ('@':k) k replMap
                                         in if null instParams
                                            then if null _orgParams
                                                 then tp
                                                 else idEvalType tp
                                            else evalType replace tp
                            {-# LINE 3665 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 418, column 23)
                       (_lhsOtps,_lhsOallTpsFound) =
                           ({-# LINE 418 "./src-ag/GenerateCode.ag" #-}
                            maybe ([],False) (\tp -> ([tp],True)) tp_
                            {-# LINE 3671 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 616, column 7)
                       _lhsOdeclsAbove =
                           ({-# LINE 616 "./src-ag/GenerateCode.ag" #-}
                            _lhsIdeclsAbove ++ _decls
                            {-# LINE 3677 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 629, column 7)
                       _lhsObldBlocksFun =
                           ({-# LINE 629 "./src-ag/GenerateCode.ag" #-}
                            id
                            {-# LINE 3683 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 903, column 18)
                       _lhsOcomments =
                           ({-# LINE 903 "./src-ag/GenerateCode.ag" #-}
                            [ makeLocalComment 11 _lhsIwhat name tp | (field,name,tp) <- Map.elems defines_, field == _LOC ]
                            ++ [ makeLocalComment 11 "inst " name tp | (field,name,tp) <- Map.elems defines_, field == _INST ]
                            {-# LINE 3690 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 155, column 34)
                       _lhsOdecls =
                           ({-# LINE 155 "./src-ag/GenerateCode.ag" #-}
                            _decls
                            {-# LINE 3696 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                       _lhsOdefinedInsts =
                           ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                            _definedInsts
                            {-# LINE 3702 "dist/build/GenerateCode.hs" #-}
                            )
                       -- copy rule (chain)
                       _lhsOvisitedSet =
                           ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                            _lhsIvisitedSet
                            {-# LINE 3708 "dist/build/GenerateCode.hs" #-}
                            )
                       ( _patternIcopy,_patternIdefinedInsts,_patternIpatternAttributes) =
                           pattern_
                   in  ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet))))
sem_CRule_CChildVisit :: Identifier ->
                         NontermIdent ->
                         Int ->
                         Attributes ->
                         Attributes ->
                         Bool ->
                         T_CRule
sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_ =
    (T_CRule (\ _lhsIallNts
                _lhsIaroundMap
                _lhsIchildren
                _lhsIcon
                _lhsIdeclsAbove
                _lhsIinh
                _lhsIinstVisitNrs
                _lhsImergeMap
                _lhsInr
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_costcentre
                _lhsIo_data
                _lhsIo_linePragmas
                _lhsIo_monadic
                _lhsIo_newtypes
                _lhsIo_pretty
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_splitsems
                _lhsIo_strictwrap
                _lhsIo_traces
                _lhsIo_unbox
                _lhsIoptions
                _lhsIparamInstMap
                _lhsIparamMap
                _lhsIprefix
                _lhsIsyn
                _lhsIterminals
                _lhsIunfoldSemDom
                _lhsIvisitedSet
                _lhsIwhat ->
                  (let _lhsOexprs :: Exprs
                       _lhsOusedVars :: (Set String)
                       _lhsOtSigs :: ([Decl])
                       _lhsOtps :: ([Type])
                       _lhsOdeclsAbove :: ([Decl])
                       _lhsObldBlocksFun :: (DeclBlocks -> DeclBlocks)
                       _lhsOallTpsFound :: Bool
                       _lhsOcomments :: ([String])
                       _lhsOdecls :: Decls
                       _lhsOdefinedInsts :: ([Identifier])
                       _lhsOvisitedSet :: (Set Identifier)
                       -- "./src-ag/GenerateCode.ag"(line 147, column 26)
                       _visitedSet =
                           ({-# LINE 147 "./src-ag/GenerateCode.ag" #-}
                            Set.insert name_ _lhsIvisitedSet
                            {-# LINE 3770 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 203, column 18)
                       _costCentreDescr =
                           ({-# LINE 203 "./src-ag/GenerateCode.ag" #-}
                            show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show name_ ++ ":" ++ show nt_ ++ ":" ++ show nr_
                            {-# LINE 3776 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 204, column 18)
                       _addCostCentre =
                           ({-# LINE 204 "./src-ag/GenerateCode.ag" #-}
                            \v -> if _lhsIo_costcentre
                                  then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                  else v
                            {-# LINE 3784 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 207, column 18)
                       _decls =
                           ({-# LINE 207 "./src-ag/GenerateCode.ag" #-}
                            let  lhsVars =  map (attrname True name_) (Map.keys syn_)
                                            ++ if isLast_ then [] else [unwrap ++ funname name_ (nr_+1)]
                                 rhsVars = map (attrname False name_) (Map.keys inh_)
                                 unwrap = if _lhsIo_newtypes then typeName nt_ (nr_ + 1) ++ " " else ""
                                 tuple | isMerging = TupleLhs [locname name_ ++ "_comp"]
                                       | otherwise = mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars
                                 rhs = _addCostCentre     $ Code.InvokeExpr (typeName nt_ nr_) (SimpleExpr fun) (map SimpleExpr rhsVars)
                                 isVirtual _ [] = False
                                 isVirtual nm ((n,t,kind) : r)
                                   | nm == n   = case kind of
                                                   ChildAttr -> True
                                                   _         -> False
                                   | otherwise = isVirtual nm r
                                 isMerged = name_ `Map.member` _lhsImergeMap
                                 isMerging = name_ `elem` concatMap (\(_,cs) -> cs) (Map.elems _lhsImergeMap)
                                 merges = [ (c,cs) | (c,(_,cs)) <- Map.assocs _lhsImergeMap, all (`Set.member` _visitedSet    ) cs, name_ `elem` (c:cs) ]
                                 baseNm = if nr_ == 0 && isVirtual name_ _lhsIchildren
                                          then Ident (getName name_ ++ "_inst") (getPos name_)
                                          else name_
                                 fun | nr_ == 0 && Set.member name_ _lhsIaroundMap
                                                 = locname name_ ++ "_around " ++ funname baseNm 0
                                     | otherwise = funname baseNm nr_
                                 outDecls | isMerged  = []
                                          | otherwise =
                                                        if isMerging
                                                        then [mkDecl _lhsIo_monadic tuple rhs Set.empty Set.empty]
                                                        else [Resume _lhsIo_monadic (typeName nt_ nr_) tuple rhs]
                                 outMerged | null merges || nr_ /= 0 = []
                                           | otherwise = let (c,cs) = head merges
                                                             tuple' = mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars'
                                                             lhsVars' = map (attrname True c) (Map.keys syn_)
                                                                        ++ if isLast_ then [] else [unwrap ++ funname c (nr_+1)]
                                                             rhsVars = [ locname c ++ "_comp" | c <- cs ]
                                                             fun     = locname c ++ "_merge"
                                                             rhs' = App fun (map SimpleExpr rhsVars)
                                                         in [Resume _lhsIo_monadic (typeName nt_ nr_) tuple' rhs']
                            in
                               (outDecls ++ outMerged)
                            {-# LINE 3827 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 327, column 7)
                       _isSuperfluousHigherOrderIntra =
                           ({-# LINE 327 "./src-ag/GenerateCode.ag" #-}
                            _lhsInr <= Map.findWithDefault (-1) name_ _lhsIinstVisitNrs
                            {-# LINE 3833 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 341, column 8)
                       _names =
                           ({-# LINE 341 "./src-ag/GenerateCode.ag" #-}
                            if _isSuperfluousHigherOrderIntra
                            then []
                            else [funname name_ (nr_+1)]
                            {-# LINE 3841 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 345, column 8)
                       _lhsOexprs =
                           ({-# LINE 345 "./src-ag/GenerateCode.ag" #-}
                            let wrap = if _lhsIo_newtypes then \x -> App (typeName nt_ (nr_ + 1)) [x] else id
                                addType expr | null _instParams     = expr
                                             | otherwise            = TypedExpr expr (_lhsIunfoldSemDom nt_ (nr_+1) _instParams    )
                            in map (wrap . addType . SimpleExpr) _names
                            {-# LINE 3850 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 357, column 7)
                       _lhsOusedVars =
                           ({-# LINE 357 "./src-ag/GenerateCode.ag" #-}
                            Set.fromList _names
                            {-# LINE 3856 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 393, column 19)
                       _mkTp =
                           ({-# LINE 393 "./src-ag/GenerateCode.ag" #-}
                            _evalTp     . typeToCodeType (Just nt_) _orgParams     . removeDeforested
                            {-# LINE 3862 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 394, column 19)
                       _definedTps =
                           ({-# LINE 394 "./src-ag/GenerateCode.ag" #-}
                            [ TSig (attrname True name_ a) (_mkTp tp) |  (a,tp) <- Map.toList syn_ ]
                            {-# LINE 3868 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 395, column 19)
                       _nextTp =
                           ({-# LINE 395 "./src-ag/GenerateCode.ag" #-}
                            typeName nt_ (nr_+1)
                            {-# LINE 3874 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 396, column 19)
                       _lhsOtSigs =
                           ({-# LINE 396 "./src-ag/GenerateCode.ag" #-}
                            (if isLast_ then id else (TSig (funname name_ (nr_+1)) (TypeApp (SimpleType _nextTp) (map SimpleType _instParams    )) :)) _definedTps
                            {-# LINE 3880 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 398, column 19)
                       _orgParams =
                           ({-# LINE 398 "./src-ag/GenerateCode.ag" #-}
                            map getName $ Map.findWithDefault [] nt_ _lhsIparamMap
                            {-# LINE 3886 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 399, column 19)
                       _instParams =
                           ({-# LINE 399 "./src-ag/GenerateCode.ag" #-}
                            snd $ Map.findWithDefault (nt_,[]) name_ _lhsIparamInstMap
                            {-# LINE 3892 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 400, column 19)
                       _replParamMap =
                           ({-# LINE 400 "./src-ag/GenerateCode.ag" #-}
                            Map.fromList (zip _orgParams     _instParams    )
                            {-# LINE 3898 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 401, column 19)
                       _replace =
                           ({-# LINE 401 "./src-ag/GenerateCode.ag" #-}
                            \k -> Map.findWithDefault k k _replParamMap
                            {-# LINE 3904 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 402, column 19)
                       _evalTp =
                           ({-# LINE 402 "./src-ag/GenerateCode.ag" #-}
                            if null _orgParams     then id else evalType _replace
                            {-# LINE 3910 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 419, column 19)
                       _lhsOtps =
                           ({-# LINE 419 "./src-ag/GenerateCode.ag" #-}
                            if _isSuperfluousHigherOrderIntra
                            then []
                            else [NT (ntOfVisit nt_ (nr_+1)) _instParams     False]
                            {-# LINE 3918 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 618, column 7)
                       _lhsOdeclsAbove =
                           ({-# LINE 618 "./src-ag/GenerateCode.ag" #-}
                            []
                            {-# LINE 3924 "dist/build/GenerateCode.hs" #-}
                            )
                       -- "./src-ag/GenerateCode.ag"(line 631, column 7)
                       _lhsObldBlocksFun =
                           ({-# LINE 631 "./src-ag/GenerateCode.ag" #-}
                            DeclBlock _lhsIdeclsAbove (head _decls    )
                            {-# LINE 3930 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 416, column 39)
                       _lhsOallTpsFound =
                           ({-# LINE 416 "./src-ag/GenerateCode.ag" #-}
                            True
                            {-# LINE 3936 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                       _lhsOcomments =
                           ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                            []
                            {-# LINE 3942 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 155, column 34)
                       _lhsOdecls =
                           ({-# LINE 155 "./src-ag/GenerateCode.ag" #-}
                            _decls
                            {-# LINE 3948 "dist/build/GenerateCode.hs" #-}
                            )
                       -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                       _lhsOdefinedInsts =
                           ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                            []
                            {-# LINE 3954 "dist/build/GenerateCode.hs" #-}
                            )
                       -- copy rule (from local)
                       _lhsOvisitedSet =
                           ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                            _visitedSet
                            {-# LINE 3960 "dist/build/GenerateCode.hs" #-}
                            )
                   in  ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet))))
-- CSegment ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         isLast               : Bool
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative CSegment:
         child inh            : {Attributes}
         child syn            : {Attributes}
         visit 0:
            local altSemForm  : _
            local tp          : _
            local inhTps      : _
            local inhTup      : _
            local synTps      : _
            local curTypeName : _
            local nextTypeName : _
            local indexName   : _
            local dataIndex   : _
            local indexExpr   : _
            local indexStr    : _
            local inhInstance : _
            local synInstance : _
            local continuation : _
            local params      : _
-}
-- cata
sem_CSegment :: CSegment ->
                T_CSegment
sem_CSegment (CSegment _inh _syn) =
    (sem_CSegment_CSegment _inh _syn)
-- semantic domain
newtype T_CSegment = T_CSegment (Attributes ->
                                 Bool ->
                                 Int ->
                                 NontermIdent ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 (Maybe Bool) ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Options ->
                                 ParamMap ->
                                 String ->
                                 Attributes ->
                                 ( ([String]),([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CSegment = Inh_CSegment {inh_Inh_CSegment :: !(Attributes),isLast_Inh_CSegment :: !(Bool),nr_Inh_CSegment :: !(Int),nt_Inh_CSegment :: !(NontermIdent),o_case_Inh_CSegment :: !(Bool),o_cata_Inh_CSegment :: !(Bool),o_costcentre_Inh_CSegment :: !(Bool),o_data_Inh_CSegment :: !((Maybe Bool)),o_linePragmas_Inh_CSegment :: !(Bool),o_monadic_Inh_CSegment :: !(Bool),o_newtypes_Inh_CSegment :: !(Bool),o_pretty_Inh_CSegment :: !(Bool),o_rename_Inh_CSegment :: !(Bool),o_sem_Inh_CSegment :: !(Bool),o_sig_Inh_CSegment :: !(Bool),o_splitsems_Inh_CSegment :: !(Bool),o_strictwrap_Inh_CSegment :: !(Bool),o_traces_Inh_CSegment :: !(Bool),o_unbox_Inh_CSegment :: !(Bool),options_Inh_CSegment :: !(Options),paramMap_Inh_CSegment :: !(ParamMap),prefix_Inh_CSegment :: !(String),syn_Inh_CSegment :: !(Attributes)}
data Syn_CSegment = Syn_CSegment {comments_Syn_CSegment :: !(([String])),semDom_Syn_CSegment :: !(([Decl])),semDomUnfoldGath_Syn_CSegment :: !((Map (NontermIdent, Int) ([String], Code.Type))),wrapDecls_Syn_CSegment :: !(Decls)}
wrap_CSegment :: T_CSegment ->
                 Inh_CSegment ->
                 Syn_CSegment
wrap_CSegment (T_CSegment sem) (Inh_CSegment _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
    (let ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) = sem _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
     in  (Syn_CSegment _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls))
sem_CSegment_CSegment :: Attributes ->
                         Attributes ->
                         T_CSegment
sem_CSegment_CSegment inh_ syn_ =
    (T_CSegment (\ _lhsIinh
                   _lhsIisLast
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_monadic
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIoptions
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn ->
                     (let _lhsOsemDom :: ([Decl])
                          _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                          _lhsOwrapDecls :: Decls
                          _lhsOcomments :: ([String])
                          -- "./src-ag/GenerateCode.ag"(line 715, column 15)
                          _altSemForm =
                              ({-# LINE 715 "./src-ag/GenerateCode.ag" #-}
                               breadthFirst _lhsIoptions
                               {-# LINE 4089 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 716, column 15)
                          _tp =
                              ({-# LINE 716 "./src-ag/GenerateCode.ag" #-}
                               if _altSemForm
                               then TypeApp (SimpleType "Child") [SimpleType "EvalInfo", _indexExpr     ]
                               else foldr Arr _synTps     _inhTps
                               {-# LINE 4097 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 719, column 15)
                          _inhTps =
                              ({-# LINE 719 "./src-ag/GenerateCode.ag" #-}
                               [typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems inh_]
                               {-# LINE 4103 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 720, column 15)
                          _inhTup =
                              ({-# LINE 720 "./src-ag/GenerateCode.ag" #-}
                               mkTupleType _lhsIo_unbox (null _inhTps    ) _inhTps
                               {-# LINE 4109 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 721, column 15)
                          _synTps =
                              ({-# LINE 721 "./src-ag/GenerateCode.ag" #-}
                               mkTupleType _lhsIo_unbox (null _inhTps    ) ([typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems syn_] ++ _continuation    )
                               {-# LINE 4115 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 722, column 15)
                          _curTypeName =
                              ({-# LINE 722 "./src-ag/GenerateCode.ag" #-}
                               typeName _lhsInt _lhsInr
                               {-# LINE 4121 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 723, column 15)
                          _nextTypeName =
                              ({-# LINE 723 "./src-ag/GenerateCode.ag" #-}
                               typeName _lhsInt (_lhsInr + 1)
                               {-# LINE 4127 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 724, column 15)
                          _indexName =
                              ({-# LINE 724 "./src-ag/GenerateCode.ag" #-}
                               "I_" ++ _curTypeName
                               {-# LINE 4133 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 725, column 15)
                          _dataIndex =
                              ({-# LINE 725 "./src-ag/GenerateCode.ag" #-}
                               Code.Data _indexName     _params     [DataAlt _indexName     []] False []
                               {-# LINE 4139 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 726, column 15)
                          _indexExpr =
                              ({-# LINE 726 "./src-ag/GenerateCode.ag" #-}
                               TypeApp (SimpleType _indexName    ) (map (SimpleType . ('@':)) _params    )
                               {-# LINE 4145 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 727, column 15)
                          _indexStr =
                              ({-# LINE 727 "./src-ag/GenerateCode.ag" #-}
                               "(" ++ _indexName     ++ concatMap (\p -> " " ++ p) _params     ++ ")"
                               {-# LINE 4151 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 728, column 15)
                          _inhInstance =
                              ({-# LINE 728 "./src-ag/GenerateCode.ag" #-}
                               Code.Data "instance Inh" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Inh") [_inhTup    ] ] False []
                               {-# LINE 4157 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 729, column 15)
                          _synInstance =
                              ({-# LINE 729 "./src-ag/GenerateCode.ag" #-}
                               Code.Data "instance Syn" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Syn") [_synTps    ] ] False []
                               {-# LINE 4163 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 730, column 15)
                          _continuation =
                              ({-# LINE 730 "./src-ag/GenerateCode.ag" #-}
                               if  _lhsIisLast
                               then []
                               else [TypeApp (SimpleType _nextTypeName    ) (map (SimpleType . ('@':)) _params    )]
                               {-# LINE 4171 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 733, column 15)
                          _params =
                              ({-# LINE 733 "./src-ag/GenerateCode.ag" #-}
                               map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                               {-# LINE 4177 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 734, column 15)
                          _lhsOsemDom =
                              ({-# LINE 734 "./src-ag/GenerateCode.ag" #-}
                               let name = typeName _lhsInt _lhsInr
                                   evalTp | null _params     = id
                                          | otherwise        = idEvalType
                               in ( if _lhsIo_newtypes
                                    then [ Code.NewType name _params     name (evalTp _tp    ) ]
                                    else [ Code.Type name _params     (evalTp _tp    ) ] )
                                  ++ ( if _altSemForm
                                       then [_dataIndex    , _inhInstance    , _synInstance    ]
                                       else [] )
                               {-# LINE 4191 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 748, column 7)
                          _lhsOsemDomUnfoldGath =
                              ({-# LINE 748 "./src-ag/GenerateCode.ag" #-}
                               Map.singleton (_lhsInt, _lhsInr) (_params    , _tp    )
                               {-# LINE 4197 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 831, column 15)
                          _lhsOwrapDecls =
                              ({-# LINE 831 "./src-ag/GenerateCode.ag" #-}
                               let lhsVars = map (lhsname False) (Map.keys syn_)
                                             ++ if _lhsIisLast then [] else [unwrap ++ sem (_lhsInr+1)]
                                   rhsVars = map (lhsname True) (Map.keys inh_)
                                   rhs = map SimpleExpr rhsVars
                                   unwrap = if _lhsIo_newtypes then typeName _lhsInt (_lhsInr + 1) ++ " " else ""
                                   var   = "sem"
                                   sem 0 = var
                                   sem n = var ++ "_" ++ show n
                                   ntt   = typeName _lhsInt _lhsInr
                               in [ EvalDecl ntt (mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars) (InvokeExpr ntt (SimpleExpr $ sem _lhsInr) rhs) ]
                               {-# LINE 4212 "dist/build/GenerateCode.hs" #-}
                               )
                          -- "./src-ag/GenerateCode.ag"(line 873, column 18)
                          _lhsOcomments =
                              ({-# LINE 873 "./src-ag/GenerateCode.ag" #-}
                               let body = map ind (showsSegment (CSegment inh_ syn_))
                               in if null body
                                  then []
                                  else ("visit " ++ show _lhsInr ++ ":") : body
                               {-# LINE 4221 "dist/build/GenerateCode.hs" #-}
                               )
                      in  ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))))
-- CSegments ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         isNil                : Bool
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative Cons:
         child hd             : CSegment 
         child tl             : CSegments 
      alternative Nil:
-}
-- cata
sem_CSegments :: CSegments ->
                 T_CSegments
sem_CSegments list =
    (Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list))
-- semantic domain
newtype T_CSegments = T_CSegments (Attributes ->
                                   Int ->
                                   NontermIdent ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   (Maybe Bool) ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Bool ->
                                   Options ->
                                   ParamMap ->
                                   String ->
                                   Attributes ->
                                   ( ([String]),Bool,([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CSegments = Inh_CSegments {inh_Inh_CSegments :: !(Attributes),nr_Inh_CSegments :: !(Int),nt_Inh_CSegments :: !(NontermIdent),o_case_Inh_CSegments :: !(Bool),o_cata_Inh_CSegments :: !(Bool),o_costcentre_Inh_CSegments :: !(Bool),o_data_Inh_CSegments :: !((Maybe Bool)),o_linePragmas_Inh_CSegments :: !(Bool),o_monadic_Inh_CSegments :: !(Bool),o_newtypes_Inh_CSegments :: !(Bool),o_pretty_Inh_CSegments :: !(Bool),o_rename_Inh_CSegments :: !(Bool),o_sem_Inh_CSegments :: !(Bool),o_sig_Inh_CSegments :: !(Bool),o_splitsems_Inh_CSegments :: !(Bool),o_strictwrap_Inh_CSegments :: !(Bool),o_traces_Inh_CSegments :: !(Bool),o_unbox_Inh_CSegments :: !(Bool),options_Inh_CSegments :: !(Options),paramMap_Inh_CSegments :: !(ParamMap),prefix_Inh_CSegments :: !(String),syn_Inh_CSegments :: !(Attributes)}
data Syn_CSegments = Syn_CSegments {comments_Syn_CSegments :: !(([String])),isNil_Syn_CSegments :: !(Bool),semDom_Syn_CSegments :: !(([Decl])),semDomUnfoldGath_Syn_CSegments :: !((Map (NontermIdent, Int) ([String], Code.Type))),wrapDecls_Syn_CSegments :: !(Decls)}
wrap_CSegments :: T_CSegments ->
                  Inh_CSegments ->
                  Syn_CSegments
wrap_CSegments (T_CSegments sem) (Inh_CSegments _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
    (let ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) = sem _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
     in  (Syn_CSegments _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls))
sem_CSegments_Cons :: T_CSegment ->
                      T_CSegments ->
                      T_CSegments
sem_CSegments_Cons (T_CSegment hd_) (T_CSegments tl_) =
    (T_CSegments (\ _lhsIinh
                    _lhsInr
                    _lhsInt
                    _lhsIo_case
                    _lhsIo_cata
                    _lhsIo_costcentre
                    _lhsIo_data
                    _lhsIo_linePragmas
                    _lhsIo_monadic
                    _lhsIo_newtypes
                    _lhsIo_pretty
                    _lhsIo_rename
                    _lhsIo_sem
                    _lhsIo_sig
                    _lhsIo_splitsems
                    _lhsIo_strictwrap
                    _lhsIo_traces
                    _lhsIo_unbox
                    _lhsIoptions
                    _lhsIparamMap
                    _lhsIprefix
                    _lhsIsyn ->
                      (let _tlOnr :: Int
                           _lhsOisNil :: Bool
                           _hdOisLast :: Bool
                           _lhsOcomments :: ([String])
                           _lhsOsemDom :: ([Decl])
                           _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _lhsOwrapDecls :: Decls
                           _hdOinh :: Attributes
                           _hdOnr :: Int
                           _hdOnt :: NontermIdent
                           _hdOo_case :: Bool
                           _hdOo_cata :: Bool
                           _hdOo_costcentre :: Bool
                           _hdOo_data :: (Maybe Bool)
                           _hdOo_linePragmas :: Bool
                           _hdOo_monadic :: Bool
                           _hdOo_newtypes :: Bool
                           _hdOo_pretty :: Bool
                           _hdOo_rename :: Bool
                           _hdOo_sem :: Bool
                           _hdOo_sig :: Bool
                           _hdOo_splitsems :: Bool
                           _hdOo_strictwrap :: Bool
                           _hdOo_traces :: Bool
                           _hdOo_unbox :: Bool
                           _hdOoptions :: Options
                           _hdOparamMap :: ParamMap
                           _hdOprefix :: String
                           _hdOsyn :: Attributes
                           _tlOinh :: Attributes
                           _tlOnt :: NontermIdent
                           _tlOo_case :: Bool
                           _tlOo_cata :: Bool
                           _tlOo_costcentre :: Bool
                           _tlOo_data :: (Maybe Bool)
                           _tlOo_linePragmas :: Bool
                           _tlOo_monadic :: Bool
                           _tlOo_newtypes :: Bool
                           _tlOo_pretty :: Bool
                           _tlOo_rename :: Bool
                           _tlOo_sem :: Bool
                           _tlOo_sig :: Bool
                           _tlOo_splitsems :: Bool
                           _tlOo_strictwrap :: Bool
                           _tlOo_traces :: Bool
                           _tlOo_unbox :: Bool
                           _tlOoptions :: Options
                           _tlOparamMap :: ParamMap
                           _tlOprefix :: String
                           _tlOsyn :: Attributes
                           _hdIcomments :: ([String])
                           _hdIsemDom :: ([Decl])
                           _hdIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _hdIwrapDecls :: Decls
                           _tlIcomments :: ([String])
                           _tlIisNil :: Bool
                           _tlIsemDom :: ([Decl])
                           _tlIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _tlIwrapDecls :: Decls
                           -- "./src-ag/GenerateCode.ag"(line 286, column 11)
                           _tlOnr =
                               ({-# LINE 286 "./src-ag/GenerateCode.ag" #-}
                                _lhsInr + 1
                                {-# LINE 4388 "dist/build/GenerateCode.hs" #-}
                                )
                           -- "./src-ag/GenerateCode.ag"(line 299, column 12)
                           _lhsOisNil =
                               ({-# LINE 299 "./src-ag/GenerateCode.ag" #-}
                                False
                                {-# LINE 4394 "dist/build/GenerateCode.hs" #-}
                                )
                           -- "./src-ag/GenerateCode.ag"(line 300, column 12)
                           _hdOisLast =
                               ({-# LINE 300 "./src-ag/GenerateCode.ag" #-}
                                _tlIisNil
                                {-# LINE 4400 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                           _lhsOcomments =
                               ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                                _hdIcomments ++ _tlIcomments
                                {-# LINE 4406 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 710, column 50)
                           _lhsOsemDom =
                               ({-# LINE 710 "./src-ag/GenerateCode.ag" #-}
                                _hdIsemDom ++ _tlIsemDom
                                {-# LINE 4412 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                           _lhsOsemDomUnfoldGath =
                               ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
                                {-# LINE 4418 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 829, column 52)
                           _lhsOwrapDecls =
                               ({-# LINE 829 "./src-ag/GenerateCode.ag" #-}
                                _hdIwrapDecls ++ _tlIwrapDecls
                                {-# LINE 4424 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOinh =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsIinh
                                {-# LINE 4430 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOnr =
                               ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                                _lhsInr
                                {-# LINE 4436 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOnt =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsInt
                                {-# LINE 4442 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_case =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_case
                                {-# LINE 4448 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_cata =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_cata
                                {-# LINE 4454 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_costcentre =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_costcentre
                                {-# LINE 4460 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_data =
                               ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_data
                                {-# LINE 4466 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_linePragmas =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_linePragmas
                                {-# LINE 4472 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_monadic =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_monadic
                                {-# LINE 4478 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_newtypes =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_newtypes
                                {-# LINE 4484 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_pretty =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_pretty
                                {-# LINE 4490 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_rename =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_rename
                                {-# LINE 4496 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_sem =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_sem
                                {-# LINE 4502 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_sig =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_sig
                                {-# LINE 4508 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_splitsems =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_splitsems
                                {-# LINE 4514 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_strictwrap =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_strictwrap
                                {-# LINE 4520 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_traces =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_traces
                                {-# LINE 4526 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOo_unbox =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_unbox
                                {-# LINE 4532 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOoptions =
                               ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 4538 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOparamMap =
                               ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                _lhsIparamMap
                                {-# LINE 4544 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOprefix =
                               ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                _lhsIprefix
                                {-# LINE 4550 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _hdOsyn =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsIsyn
                                {-# LINE 4556 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOinh =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsIinh
                                {-# LINE 4562 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOnt =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsInt
                                {-# LINE 4568 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_case =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_case
                                {-# LINE 4574 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_cata =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_cata
                                {-# LINE 4580 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_costcentre =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_costcentre
                                {-# LINE 4586 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_data =
                               ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_data
                                {-# LINE 4592 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_linePragmas =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_linePragmas
                                {-# LINE 4598 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_monadic =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_monadic
                                {-# LINE 4604 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_newtypes =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_newtypes
                                {-# LINE 4610 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_pretty =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_pretty
                                {-# LINE 4616 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_rename =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_rename
                                {-# LINE 4622 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_sem =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_sem
                                {-# LINE 4628 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_sig =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_sig
                                {-# LINE 4634 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_splitsems =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_splitsems
                                {-# LINE 4640 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_strictwrap =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_strictwrap
                                {-# LINE 4646 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_traces =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_traces
                                {-# LINE 4652 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOo_unbox =
                               ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                                _lhsIo_unbox
                                {-# LINE 4658 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOoptions =
                               ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                                _lhsIoptions
                                {-# LINE 4664 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOparamMap =
                               ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                                _lhsIparamMap
                                {-# LINE 4670 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOprefix =
                               ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                                _lhsIprefix
                                {-# LINE 4676 "dist/build/GenerateCode.hs" #-}
                                )
                           -- copy rule (down)
                           _tlOsyn =
                               ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                                _lhsIsyn
                                {-# LINE 4682 "dist/build/GenerateCode.hs" #-}
                                )
                           ( _hdIcomments,_hdIsemDom,_hdIsemDomUnfoldGath,_hdIwrapDecls) =
                               hd_ _hdOinh _hdOisLast _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOsyn
                           ( _tlIcomments,_tlIisNil,_tlIsemDom,_tlIsemDomUnfoldGath,_tlIwrapDecls) =
                               tl_ _tlOinh _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOsyn
                       in  ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))))
sem_CSegments_Nil :: T_CSegments
sem_CSegments_Nil =
    (T_CSegments (\ _lhsIinh
                    _lhsInr
                    _lhsInt
                    _lhsIo_case
                    _lhsIo_cata
                    _lhsIo_costcentre
                    _lhsIo_data
                    _lhsIo_linePragmas
                    _lhsIo_monadic
                    _lhsIo_newtypes
                    _lhsIo_pretty
                    _lhsIo_rename
                    _lhsIo_sem
                    _lhsIo_sig
                    _lhsIo_splitsems
                    _lhsIo_strictwrap
                    _lhsIo_traces
                    _lhsIo_unbox
                    _lhsIoptions
                    _lhsIparamMap
                    _lhsIprefix
                    _lhsIsyn ->
                      (let _lhsOisNil :: Bool
                           _lhsOcomments :: ([String])
                           _lhsOsemDom :: ([Decl])
                           _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _lhsOwrapDecls :: Decls
                           -- "./src-ag/GenerateCode.ag"(line 301, column 10)
                           _lhsOisNil =
                               ({-# LINE 301 "./src-ag/GenerateCode.ag" #-}
                                True
                                {-# LINE 4722 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                           _lhsOcomments =
                               ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                                []
                                {-# LINE 4728 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 710, column 50)
                           _lhsOsemDom =
                               ({-# LINE 710 "./src-ag/GenerateCode.ag" #-}
                                []
                                {-# LINE 4734 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 744, column 86)
                           _lhsOsemDomUnfoldGath =
                               ({-# LINE 744 "./src-ag/GenerateCode.ag" #-}
                                Map.empty
                                {-# LINE 4740 "dist/build/GenerateCode.hs" #-}
                                )
                           -- use rule "./src-ag/GenerateCode.ag"(line 829, column 52)
                           _lhsOwrapDecls =
                               ({-# LINE 829 "./src-ag/GenerateCode.ag" #-}
                                []
                                {-# LINE 4746 "dist/build/GenerateCode.hs" #-}
                                )
                       in  ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))))
-- CVisit ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Set Identifier
         children             : [(Identifier,Type, ChildKind)]
         con                  : ConstructorIdent
         contextMap           : ContextMap
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         isLast               : Bool
         mergeMap             : Map Identifier (Identifier, [Identifier])
         nextIntra            : Exprs
         nextIntraVars        : Set String
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      chained attributes:
         decls                : Decls
         visitedSet           : Set Identifier
      synthesized attributes:
         comments             : [String]
         gatherInstVisitNrs   : Map Identifier Int
         intra                : Exprs
         intraVars            : Set String
         semNames             : [String]
   alternatives:
      alternative CVisit:
         child inh            : {Attributes}
         child syn            : {Attributes}
         child vss            : Sequence 
         child intra          : Sequence 
         child ordered        : {Bool}
         visit 0:
            local higherOrderChildren : _
            local firstOrderChildren : _
            local firstOrderOrig : _
            local funcname    : _
            local nextVisitName : _
            local nextVisitDecl : _
            local isOneVisit  : _
            local hasWrappers : _
            local refDecls    : _
            local decls       : _
            local lastExprVars : _
            local blockFunDecls : _
            local blockFirstFunCall : _
            local costCentreDescr : _
            local addCostCentre : _
            local params      : _
            local semFun      : _
            local tsig        : _
            local semType     : _
            local typeSigs    : _
            local o_do        : _
            local o_case      : _
            local declsType   : _
            local o_splitsems : _
-}
-- cata
sem_CVisit :: CVisit ->
              T_CVisit
sem_CVisit (CVisit _inh _syn _vss _intra _ordered) =
    (sem_CVisit_CVisit _inh _syn (sem_Sequence _vss) (sem_Sequence _intra) _ordered)
-- semantic domain
newtype T_CVisit = T_CVisit ((Set NontermIdent) ->
                             PragmaMap ->
                             (Set Identifier) ->
                             ([(Identifier,Type, ChildKind)]) ->
                             ConstructorIdent ->
                             ContextMap ->
                             Decls ->
                             Attributes ->
                             (Map Identifier Int) ->
                             Bool ->
                             (Map Identifier (Identifier, [Identifier])) ->
                             Exprs ->
                             (Set String) ->
                             Int ->
                             NontermIdent ->
                             Bool ->
                             Bool ->
                             Bool ->
                             (Maybe Bool) ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Bool ->
                             Options ->
                             (Map Identifier (NontermIdent, [String])) ->
                             ParamMap ->
                             String ->
                             QuantMap ->
                             Attributes ->
                             ([Identifier]) ->
                             (NontermIdent -> Int -> [String] -> Code.Type) ->
                             (Set Identifier) ->
                             Bool ->
                             (Set NontermIdent) ->
                             ( ([String]),Decls,(Map Identifier Int),Exprs,(Set String),([String]),(Set Identifier)))
data Inh_CVisit = Inh_CVisit {allNts_Inh_CVisit :: !((Set NontermIdent)),allPragmas_Inh_CVisit :: !(PragmaMap),aroundMap_Inh_CVisit :: !((Set Identifier)),children_Inh_CVisit :: !(([(Identifier,Type, ChildKind)])),con_Inh_CVisit :: !(ConstructorIdent),contextMap_Inh_CVisit :: !(ContextMap),decls_Inh_CVisit :: !(Decls),inh_Inh_CVisit :: !(Attributes),instVisitNrs_Inh_CVisit :: !((Map Identifier Int)),isLast_Inh_CVisit :: !(Bool),mergeMap_Inh_CVisit :: !((Map Identifier (Identifier, [Identifier]))),nextIntra_Inh_CVisit :: !(Exprs),nextIntraVars_Inh_CVisit :: !((Set String)),nr_Inh_CVisit :: !(Int),nt_Inh_CVisit :: !(NontermIdent),o_case_Inh_CVisit :: !(Bool),o_cata_Inh_CVisit :: !(Bool),o_costcentre_Inh_CVisit :: !(Bool),o_data_Inh_CVisit :: !((Maybe Bool)),o_linePragmas_Inh_CVisit :: !(Bool),o_monadic_Inh_CVisit :: !(Bool),o_newtypes_Inh_CVisit :: !(Bool),o_pretty_Inh_CVisit :: !(Bool),o_rename_Inh_CVisit :: !(Bool),o_sem_Inh_CVisit :: !(Bool),o_sig_Inh_CVisit :: !(Bool),o_splitsems_Inh_CVisit :: !(Bool),o_strictwrap_Inh_CVisit :: !(Bool),o_traces_Inh_CVisit :: !(Bool),o_unbox_Inh_CVisit :: !(Bool),options_Inh_CVisit :: !(Options),paramInstMap_Inh_CVisit :: !((Map Identifier (NontermIdent, [String]))),paramMap_Inh_CVisit :: !(ParamMap),prefix_Inh_CVisit :: !(String),quantMap_Inh_CVisit :: !(QuantMap),syn_Inh_CVisit :: !(Attributes),terminals_Inh_CVisit :: !(([Identifier])),unfoldSemDom_Inh_CVisit :: !((NontermIdent -> Int -> [String] -> Code.Type)),visitedSet_Inh_CVisit :: !((Set Identifier)),with_sig_Inh_CVisit :: !(Bool),wrappers_Inh_CVisit :: !((Set NontermIdent))}
data Syn_CVisit = Syn_CVisit {comments_Syn_CVisit :: !(([String])),decls_Syn_CVisit :: !(Decls),gatherInstVisitNrs_Syn_CVisit :: !((Map Identifier Int)),intra_Syn_CVisit :: !(Exprs),intraVars_Syn_CVisit :: !((Set String)),semNames_Syn_CVisit :: !(([String])),visitedSet_Syn_CVisit :: !((Set Identifier))}
wrap_CVisit :: T_CVisit ->
               Inh_CVisit ->
               Syn_CVisit
wrap_CVisit (T_CVisit sem) (Inh_CVisit _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOcomments,_lhsOdecls,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOsemNames,_lhsOvisitedSet) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
     in  (Syn_CVisit _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames _lhsOvisitedSet))
sem_CVisit_CVisit :: Attributes ->
                     Attributes ->
                     T_Sequence ->
                     T_Sequence ->
                     Bool ->
                     T_CVisit
sem_CVisit_CVisit inh_ syn_ (T_Sequence vss_) (T_Sequence intra_) ordered_ =
    (T_CVisit (\ _lhsIallNts
                 _lhsIallPragmas
                 _lhsIaroundMap
                 _lhsIchildren
                 _lhsIcon
                 _lhsIcontextMap
                 _lhsIdecls
                 _lhsIinh
                 _lhsIinstVisitNrs
                 _lhsIisLast
                 _lhsImergeMap
                 _lhsInextIntra
                 _lhsInextIntraVars
                 _lhsInr
                 _lhsInt
                 _lhsIo_case
                 _lhsIo_cata
                 _lhsIo_costcentre
                 _lhsIo_data
                 _lhsIo_linePragmas
                 _lhsIo_monadic
                 _lhsIo_newtypes
                 _lhsIo_pretty
                 _lhsIo_rename
                 _lhsIo_sem
                 _lhsIo_sig
                 _lhsIo_splitsems
                 _lhsIo_strictwrap
                 _lhsIo_traces
                 _lhsIo_unbox
                 _lhsIoptions
                 _lhsIparamInstMap
                 _lhsIparamMap
                 _lhsIprefix
                 _lhsIquantMap
                 _lhsIsyn
                 _lhsIterminals
                 _lhsIunfoldSemDom
                 _lhsIvisitedSet
                 _lhsIwith_sig
                 _lhsIwrappers ->
                   (let _lhsOintra :: Exprs
                        _lhsOintraVars :: (Set String)
                        _vssOlastExpr :: Expr
                        _intraOlastExpr :: Expr
                        _lhsOdecls :: Decls
                        _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                        _vssOdeclsAbove :: ([Decl])
                        _intraOdeclsAbove :: ([Decl])
                        _lhsOcomments :: ([String])
                        _vssOwhat :: String
                        _intraOwhat :: String
                        _lhsOsemNames :: ([String])
                        _lhsOvisitedSet :: (Set Identifier)
                        _vssOallNts :: (Set NontermIdent)
                        _vssOaroundMap :: (Set Identifier)
                        _vssOchildren :: ([(Identifier,Type,ChildKind)])
                        _vssOcon :: ConstructorIdent
                        _vssOinh :: Attributes
                        _vssOinstVisitNrs :: (Map Identifier Int)
                        _vssOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                        _vssOnr :: Int
                        _vssOnt :: NontermIdent
                        _vssOo_case :: Bool
                        _vssOo_cata :: Bool
                        _vssOo_costcentre :: Bool
                        _vssOo_data :: (Maybe Bool)
                        _vssOo_linePragmas :: Bool
                        _vssOo_monadic :: Bool
                        _vssOo_newtypes :: Bool
                        _vssOo_pretty :: Bool
                        _vssOo_rename :: Bool
                        _vssOo_sem :: Bool
                        _vssOo_sig :: Bool
                        _vssOo_splitsems :: Bool
                        _vssOo_strictwrap :: Bool
                        _vssOo_traces :: Bool
                        _vssOo_unbox :: Bool
                        _vssOoptions :: Options
                        _vssOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                        _vssOparamMap :: ParamMap
                        _vssOprefix :: String
                        _vssOsyn :: Attributes
                        _vssOterminals :: ([Identifier])
                        _vssOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                        _vssOvisitedSet :: (Set Identifier)
                        _intraOallNts :: (Set NontermIdent)
                        _intraOaroundMap :: (Set Identifier)
                        _intraOchildren :: ([(Identifier,Type,ChildKind)])
                        _intraOcon :: ConstructorIdent
                        _intraOinh :: Attributes
                        _intraOinstVisitNrs :: (Map Identifier Int)
                        _intraOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                        _intraOnr :: Int
                        _intraOnt :: NontermIdent
                        _intraOo_case :: Bool
                        _intraOo_cata :: Bool
                        _intraOo_costcentre :: Bool
                        _intraOo_data :: (Maybe Bool)
                        _intraOo_linePragmas :: Bool
                        _intraOo_monadic :: Bool
                        _intraOo_newtypes :: Bool
                        _intraOo_pretty :: Bool
                        _intraOo_rename :: Bool
                        _intraOo_sem :: Bool
                        _intraOo_sig :: Bool
                        _intraOo_splitsems :: Bool
                        _intraOo_strictwrap :: Bool
                        _intraOo_traces :: Bool
                        _intraOo_unbox :: Bool
                        _intraOoptions :: Options
                        _intraOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                        _intraOparamMap :: ParamMap
                        _intraOprefix :: String
                        _intraOsyn :: Attributes
                        _intraOterminals :: ([Identifier])
                        _intraOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                        _intraOvisitedSet :: (Set Identifier)
                        _vssIallTpsFound :: Bool
                        _vssIblockDecls :: DeclBlocks
                        _vssIcomments :: ([String])
                        _vssIdecls :: Decls
                        _vssIdeclsAbove :: ([Decl])
                        _vssIdefinedInsts :: ([Identifier])
                        _vssIexprs :: Exprs
                        _vssItSigs :: ([Decl])
                        _vssItps :: ([Type])
                        _vssIusedVars :: (Set String)
                        _vssIvisitedSet :: (Set Identifier)
                        _intraIallTpsFound :: Bool
                        _intraIblockDecls :: DeclBlocks
                        _intraIcomments :: ([String])
                        _intraIdecls :: Decls
                        _intraIdeclsAbove :: ([Decl])
                        _intraIdefinedInsts :: ([Identifier])
                        _intraIexprs :: Exprs
                        _intraItSigs :: ([Decl])
                        _intraItps :: ([Type])
                        _intraIusedVars :: (Set String)
                        _intraIvisitedSet :: (Set Identifier)
                        -- "./src-ag/GenerateCode.ag"(line 310, column 13)
                        _lhsOintra =
                            ({-# LINE 310 "./src-ag/GenerateCode.ag" #-}
                             _intraIexprs
                             {-# LINE 5041 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 311, column 13)
                        _lhsOintraVars =
                            ({-# LINE 311 "./src-ag/GenerateCode.ag" #-}
                             _intraIusedVars
                             {-# LINE 5047 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 441, column 13)
                        (_higherOrderChildren,_firstOrderChildren) =
                            ({-# LINE 441 "./src-ag/GenerateCode.ag" #-}
                             partition (\(_,_,virt) -> isHigherOrder virt) _lhsIchildren
                             {-# LINE 5053 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 442, column 13)
                        _firstOrderOrig =
                            ({-# LINE 442 "./src-ag/GenerateCode.ag" #-}
                             map pickOrigType _firstOrderChildren
                             {-# LINE 5059 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 443, column 13)
                        _funcname =
                            ({-# LINE 443 "./src-ag/GenerateCode.ag" #-}
                             seqSemname _lhsIprefix _lhsInt _lhsIcon _lhsInr
                             {-# LINE 5065 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 444, column 13)
                        _nextVisitName =
                            ({-# LINE 444 "./src-ag/GenerateCode.ag" #-}
                             if _lhsIisLast then [] else [visitname _lhsIprefix _lhsInt (_lhsInr+1)]
                             {-# LINE 5071 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 445, column 13)
                        _nextVisitDecl =
                            ({-# LINE 445 "./src-ag/GenerateCode.ag" #-}
                             let  lhs = TupleLhs _nextVisitName
                                  rhs = Let _lhsIdecls (SimpleExpr fun)
                                  fun = seqSemname _lhsIprefix _lhsInt _lhsIcon (_lhsInr+1)
                             in if _lhsIisLast
                                then []
                                else [Decl lhs rhs (Set.fromList _nextVisitName) _lhsInextIntraVars]
                             {-# LINE 5082 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 452, column 13)
                        _isOneVisit =
                            ({-# LINE 452 "./src-ag/GenerateCode.ag" #-}
                             _lhsIisLast && _lhsInr == 0
                             {-# LINE 5088 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 453, column 13)
                        _hasWrappers =
                            ({-# LINE 453 "./src-ag/GenerateCode.ag" #-}
                             _lhsInt `Set.member` _lhsIwrappers
                             {-# LINE 5094 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 454, column 13)
                        _refDecls =
                            ({-# LINE 454 "./src-ag/GenerateCode.ag" #-}
                             if _isOneVisit     && _hasWrappers     && reference _lhsIoptions
                             then let synAttrs = Map.toList syn_
                                      synNT = "Syn" ++ "_" ++ getName _lhsInt
                                      synVars = [ SimpleExpr (attrname False _LHS a) | (a,_) <- synAttrs ]
                                      rhs = App synNT synVars
                                      lhs = Fun "___node" []
                                  in [Decl lhs rhs Set.empty Set.empty]
                             else []
                             {-# LINE 5107 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 462, column 13)
                        _decls =
                            ({-# LINE 462 "./src-ag/GenerateCode.ag" #-}
                             _typeSigs ++ _vssIdecls ++ _nextVisitDecl ++ _refDecls
                             {-# LINE 5113 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 463, column 13)
                        _vssOlastExpr =
                            ({-# LINE 463 "./src-ag/GenerateCode.ag" #-}
                             mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_) $ map (SimpleExpr . lhsname False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                             {-# LINE 5119 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 464, column 13)
                        _intraOlastExpr =
                            ({-# LINE 464 "./src-ag/GenerateCode.ag" #-}
                             error "lastExpr: not used here"
                             {-# LINE 5125 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 465, column 13)
                        _lastExprVars =
                            ({-# LINE 465 "./src-ag/GenerateCode.ag" #-}
                             map (lhsname False) (Map.keys syn_) ++ _nextVisitName
                             {-# LINE 5131 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 466, column 13)
                        (_blockFunDecls,_blockFirstFunCall) =
                            ({-# LINE 466 "./src-ag/GenerateCode.ag" #-}
                             mkPartitionedFunction _funcname     _o_case     _nextVisitDecl     _lastExprVars     _vssIblockDecls
                             {-# LINE 5137 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 468, column 13)
                        _costCentreDescr =
                            ({-# LINE 468 "./src-ag/GenerateCode.ag" #-}
                             "b" ++ ":" ++ show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show _lhsInr
                             {-# LINE 5143 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 469, column 13)
                        _addCostCentre =
                            ({-# LINE 469 "./src-ag/GenerateCode.ag" #-}
                             \v -> if _lhsIo_costcentre
                                   then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                   else v
                             {-# LINE 5151 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 473, column 13)
                        _params =
                            ({-# LINE 473 "./src-ag/GenerateCode.ag" #-}
                             map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                             {-# LINE 5157 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 474, column 13)
                        _semFun =
                            ({-# LINE 474 "./src-ag/GenerateCode.ag" #-}
                             let  lhs = Fun _funcname lhs_args
                                  lhs_args = if _lhsInr == 0 then map field _firstOrderOrig     else []
                                  field (name,NT tp tps _,_) = let unwrap | _lhsIo_newtypes = \x -> App (sdtype tp) [x]
                                                                          | otherwise       = id
                                                                   addType expr | null tps  = expr
                                                                                | otherwise = TypedExpr expr (_lhsIunfoldSemDom tp 0 tps)
                                                               in unwrap $ addType $ SimpleExpr $ funname name 0
                                  field (name,tp,_)        = let expr = SimpleExpr (funname name 0)
                                                             in if null _params
                                                                then expr
                                                                else TypedExpr expr (idEvalType $ typeToCodeType (Just _lhsInt) _params     $ removeDeforested tp)
                                  mbEvalTp | null _params     = const Nothing
                                           | otherwise        = Just . idEvalType
                                  rhs = wrap
                                      . mkSemFun _lhsInt _lhsInr [mkLambdaArg (lhsname True nm) (mbEvalTp $ typeToCodeType (Just _lhsInt) _params     $ removeDeforested tp) | (nm,tp) <- Map.assocs inh_]
                                      $ _addCostCentre
                                      $ if ordered_ && _o_splitsems
                                        then _blockFirstFunCall
                                        else mkDecls _declsType     _decls
                                             . ResultExpr (typeName _lhsInt _lhsInr)
                                             . mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_)
                                             $ map (SimpleExpr . lhsname False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                                  wrap = if  _lhsIo_newtypes
                                             then \x -> App (typeName _lhsInt _lhsInr) [x]
                                             else id
                             in Decl lhs rhs Set.empty Set.empty
                             {-# LINE 5188 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 505, column 13)
                        _tsig =
                            ({-# LINE 505 "./src-ag/GenerateCode.ag" #-}
                             TSig _funcname _semType
                             {-# LINE 5194 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 506, column 13)
                        _semType =
                            ({-# LINE 506 "./src-ag/GenerateCode.ag" #-}
                             let argType (NT tp tps _)  r | tp /= _SELF = typeAppStrs (sdtype tp) tps `Arr` r
                                                          | tp == _SELF = error "GenerateCode: found an intra-type with type SELF, which should have been prevented by CRule.tps"
                                 argType (Haskell tp) r                 = SimpleType tp          `Arr` r
                                 evalTp | null _params     = id
                                        | otherwise        = idEvalType
                             in appQuant _lhsIquantMap _lhsInt $ appContext _lhsIcontextMap _lhsInt $ evalTp $
                                if  _lhsInr == 0
                                    then foldr argType (typeAppStrs (sdtype   _lhsInt        ) _params    ) (map (\(_,t,_) -> t) _firstOrderOrig    )
                                    else foldr argType (typeAppStrs (typeName _lhsInt _lhsInr) _params    ) []
                             {-# LINE 5208 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 517, column 13)
                        _lhsOdecls =
                            ({-# LINE 517 "./src-ag/GenerateCode.ag" #-}
                             ( if  _lhsIwith_sig
                               then [_tsig, _semFun]
                               else [_semFun]
                             ) ++
                             ( if ordered_ && _o_splitsems
                               then _blockFunDecls
                               else []
                             )
                             {-# LINE 5221 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 525, column 13)
                        _typeSigs =
                            ({-# LINE 525 "./src-ag/GenerateCode.ag" #-}
                             if  _lhsIo_sig && not _o_case
                                 then  _vssItSigs
                                 else  []
                             {-# LINE 5229 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 528, column 13)
                        _o_do =
                            ({-# LINE 528 "./src-ag/GenerateCode.ag" #-}
                             ordered_ && _lhsIo_monadic
                             {-# LINE 5235 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 529, column 13)
                        _o_case =
                            ({-# LINE 529 "./src-ag/GenerateCode.ag" #-}
                             not _o_do     && _lhsIo_case && ordered_ && not (hasPragma _lhsIallPragmas _lhsInt _lhsIcon _NOCASE)
                             {-# LINE 5241 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 530, column 13)
                        _declsType =
                            ({-# LINE 530 "./src-ag/GenerateCode.ag" #-}
                             if _o_do
                             then DeclsDo
                             else if _o_case
                                  then DeclsCase
                                  else DeclsLet
                             {-# LINE 5251 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 535, column 13)
                        _o_splitsems =
                            ({-# LINE 535 "./src-ag/GenerateCode.ag" #-}
                             ordered_ && _lhsIo_splitsems
                             {-# LINE 5257 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 568, column 7)
                        _lhsOgatherInstVisitNrs =
                            ({-# LINE 568 "./src-ag/GenerateCode.ag" #-}
                             Map.fromList [(i,_lhsInr) | i <- _vssIdefinedInsts]
                             {-# LINE 5263 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 611, column 7)
                        _vssOdeclsAbove =
                            ({-# LINE 611 "./src-ag/GenerateCode.ag" #-}
                             []
                             {-# LINE 5269 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 612, column 7)
                        _intraOdeclsAbove =
                            ({-# LINE 612 "./src-ag/GenerateCode.ag" #-}
                             error "declsAbove: not used here"
                             {-# LINE 5275 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 894, column 18)
                        _lhsOcomments =
                            ({-# LINE 894 "./src-ag/GenerateCode.ag" #-}
                             let body = map ind (_vssIcomments ++ _intraIcomments)
                             in if null body
                                then []
                                else ("visit " ++ show _lhsInr ++ ":") : body
                             {-# LINE 5284 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 898, column 18)
                        _vssOwhat =
                            ({-# LINE 898 "./src-ag/GenerateCode.ag" #-}
                             "local"
                             {-# LINE 5290 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 899, column 18)
                        _intraOwhat =
                            ({-# LINE 899 "./src-ag/GenerateCode.ag" #-}
                             "intra"
                             {-# LINE 5296 "dist/build/GenerateCode.hs" #-}
                             )
                        -- "./src-ag/GenerateCode.ag"(line 1156, column 7)
                        _lhsOsemNames =
                            ({-# LINE 1156 "./src-ag/GenerateCode.ag" #-}
                             [_funcname    ]
                             {-# LINE 5302 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (up)
                        _lhsOvisitedSet =
                            ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                             _intraIvisitedSet
                             {-# LINE 5308 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOallNts =
                            ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                             _lhsIallNts
                             {-# LINE 5314 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOaroundMap =
                            ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                             _lhsIaroundMap
                             {-# LINE 5320 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOchildren =
                            ({-# LINE 259 "./src-ag/GenerateCode.ag" #-}
                             _lhsIchildren
                             {-# LINE 5326 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOcon =
                            ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                             _lhsIcon
                             {-# LINE 5332 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOinh =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsIinh
                             {-# LINE 5338 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOinstVisitNrs =
                            ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                             _lhsIinstVisitNrs
                             {-# LINE 5344 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOmergeMap =
                            ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                             _lhsImergeMap
                             {-# LINE 5350 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOnr =
                            ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                             _lhsInr
                             {-# LINE 5356 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOnt =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsInt
                             {-# LINE 5362 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (from local)
                        _vssOo_case =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _o_case
                             {-# LINE 5368 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_cata =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_cata
                             {-# LINE 5374 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_costcentre =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_costcentre
                             {-# LINE 5380 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_data =
                            ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_data
                             {-# LINE 5386 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_linePragmas =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_linePragmas
                             {-# LINE 5392 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_monadic =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_monadic
                             {-# LINE 5398 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_newtypes =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_newtypes
                             {-# LINE 5404 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_pretty =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_pretty
                             {-# LINE 5410 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_rename =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_rename
                             {-# LINE 5416 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_sem =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_sem
                             {-# LINE 5422 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_sig =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_sig
                             {-# LINE 5428 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (from local)
                        _vssOo_splitsems =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _o_splitsems
                             {-# LINE 5434 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_strictwrap =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_strictwrap
                             {-# LINE 5440 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_traces =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_traces
                             {-# LINE 5446 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOo_unbox =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_unbox
                             {-# LINE 5452 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOoptions =
                            ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 5458 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOparamInstMap =
                            ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                             _lhsIparamInstMap
                             {-# LINE 5464 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOparamMap =
                            ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                             _lhsIparamMap
                             {-# LINE 5470 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOprefix =
                            ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                             _lhsIprefix
                             {-# LINE 5476 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOsyn =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsIsyn
                             {-# LINE 5482 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOterminals =
                            ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                             _lhsIterminals
                             {-# LINE 5488 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOunfoldSemDom =
                            ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                             _lhsIunfoldSemDom
                             {-# LINE 5494 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _vssOvisitedSet =
                            ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                             _lhsIvisitedSet
                             {-# LINE 5500 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOallNts =
                            ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                             _lhsIallNts
                             {-# LINE 5506 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOaroundMap =
                            ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                             _lhsIaroundMap
                             {-# LINE 5512 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOchildren =
                            ({-# LINE 259 "./src-ag/GenerateCode.ag" #-}
                             _lhsIchildren
                             {-# LINE 5518 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOcon =
                            ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                             _lhsIcon
                             {-# LINE 5524 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOinh =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsIinh
                             {-# LINE 5530 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOinstVisitNrs =
                            ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                             _lhsIinstVisitNrs
                             {-# LINE 5536 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOmergeMap =
                            ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                             _lhsImergeMap
                             {-# LINE 5542 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOnr =
                            ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                             _lhsInr
                             {-# LINE 5548 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOnt =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsInt
                             {-# LINE 5554 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (from local)
                        _intraOo_case =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _o_case
                             {-# LINE 5560 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_cata =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_cata
                             {-# LINE 5566 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_costcentre =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_costcentre
                             {-# LINE 5572 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_data =
                            ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_data
                             {-# LINE 5578 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_linePragmas =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_linePragmas
                             {-# LINE 5584 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_monadic =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_monadic
                             {-# LINE 5590 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_newtypes =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_newtypes
                             {-# LINE 5596 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_pretty =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_pretty
                             {-# LINE 5602 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_rename =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_rename
                             {-# LINE 5608 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_sem =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_sem
                             {-# LINE 5614 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_sig =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_sig
                             {-# LINE 5620 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (from local)
                        _intraOo_splitsems =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _o_splitsems
                             {-# LINE 5626 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_strictwrap =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_strictwrap
                             {-# LINE 5632 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_traces =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_traces
                             {-# LINE 5638 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOo_unbox =
                            ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                             _lhsIo_unbox
                             {-# LINE 5644 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOoptions =
                            ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                             _lhsIoptions
                             {-# LINE 5650 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOparamInstMap =
                            ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                             _lhsIparamInstMap
                             {-# LINE 5656 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOparamMap =
                            ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                             _lhsIparamMap
                             {-# LINE 5662 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOprefix =
                            ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                             _lhsIprefix
                             {-# LINE 5668 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOsyn =
                            ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                             _lhsIsyn
                             {-# LINE 5674 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOterminals =
                            ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                             _lhsIterminals
                             {-# LINE 5680 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (down)
                        _intraOunfoldSemDom =
                            ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                             _lhsIunfoldSemDom
                             {-# LINE 5686 "dist/build/GenerateCode.hs" #-}
                             )
                        -- copy rule (chain)
                        _intraOvisitedSet =
                            ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                             _vssIvisitedSet
                             {-# LINE 5692 "dist/build/GenerateCode.hs" #-}
                             )
                        ( _vssIallTpsFound,_vssIblockDecls,_vssIcomments,_vssIdecls,_vssIdeclsAbove,_vssIdefinedInsts,_vssIexprs,_vssItSigs,_vssItps,_vssIusedVars,_vssIvisitedSet) =
                            vss_ _vssOallNts _vssOaroundMap _vssOchildren _vssOcon _vssOdeclsAbove _vssOinh _vssOinstVisitNrs _vssOlastExpr _vssOmergeMap _vssOnr _vssOnt _vssOo_case _vssOo_cata _vssOo_costcentre _vssOo_data _vssOo_linePragmas _vssOo_monadic _vssOo_newtypes _vssOo_pretty _vssOo_rename _vssOo_sem _vssOo_sig _vssOo_splitsems _vssOo_strictwrap _vssOo_traces _vssOo_unbox _vssOoptions _vssOparamInstMap _vssOparamMap _vssOprefix _vssOsyn _vssOterminals _vssOunfoldSemDom _vssOvisitedSet _vssOwhat
                        ( _intraIallTpsFound,_intraIblockDecls,_intraIcomments,_intraIdecls,_intraIdeclsAbove,_intraIdefinedInsts,_intraIexprs,_intraItSigs,_intraItps,_intraIusedVars,_intraIvisitedSet) =
                            intra_ _intraOallNts _intraOaroundMap _intraOchildren _intraOcon _intraOdeclsAbove _intraOinh _intraOinstVisitNrs _intraOlastExpr _intraOmergeMap _intraOnr _intraOnt _intraOo_case _intraOo_cata _intraOo_costcentre _intraOo_data _intraOo_linePragmas _intraOo_monadic _intraOo_newtypes _intraOo_pretty _intraOo_rename _intraOo_sem _intraOo_sig _intraOo_splitsems _intraOo_strictwrap _intraOo_traces _intraOo_unbox _intraOoptions _intraOparamInstMap _intraOparamMap _intraOprefix _intraOsyn _intraOterminals _intraOunfoldSemDom _intraOvisitedSet _intraOwhat
                    in  ( _lhsOcomments,_lhsOdecls,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOsemNames,_lhsOvisitedSet))))
-- CVisits -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         aroundMap            : Set Identifier
         children             : [(Identifier,Type, ChildKind)]
         con                  : ConstructorIdent
         contextMap           : ContextMap
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         mergeMap             : Map Identifier (Identifier, [Identifier])
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         quantMap             : QuantMap
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      chained attribute:
         visitedSet           : Set Identifier
      synthesized attributes:
         comments             : [String]
         decls                : Decls
         gatherInstVisitNrs   : Map Identifier Int
         intra                : Exprs
         intraVars            : Set String
         isNil                : Bool
         semNames             : [String]
   alternatives:
      alternative Cons:
         child hd             : CVisit 
         child tl             : CVisits 
      alternative Nil:
-}
-- cata
sem_CVisits :: CVisits ->
               T_CVisits
sem_CVisits list =
    (Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list))
-- semantic domain
newtype T_CVisits = T_CVisits ((Set NontermIdent) ->
                               PragmaMap ->
                               (Set Identifier) ->
                               ([(Identifier,Type, ChildKind)]) ->
                               ConstructorIdent ->
                               ContextMap ->
                               Attributes ->
                               (Map Identifier Int) ->
                               (Map Identifier (Identifier, [Identifier])) ->
                               Int ->
                               NontermIdent ->
                               Bool ->
                               Bool ->
                               Bool ->
                               (Maybe Bool) ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Bool ->
                               Options ->
                               (Map Identifier (NontermIdent, [String])) ->
                               ParamMap ->
                               String ->
                               QuantMap ->
                               Attributes ->
                               ([Identifier]) ->
                               (NontermIdent -> Int -> [String] -> Code.Type) ->
                               (Set Identifier) ->
                               Bool ->
                               (Set NontermIdent) ->
                               ( ([String]),Decls,(Map Identifier Int),Exprs,(Set String),Bool,([String]),(Set Identifier)))
data Inh_CVisits = Inh_CVisits {allNts_Inh_CVisits :: !((Set NontermIdent)),allPragmas_Inh_CVisits :: !(PragmaMap),aroundMap_Inh_CVisits :: !((Set Identifier)),children_Inh_CVisits :: !(([(Identifier,Type, ChildKind)])),con_Inh_CVisits :: !(ConstructorIdent),contextMap_Inh_CVisits :: !(ContextMap),inh_Inh_CVisits :: !(Attributes),instVisitNrs_Inh_CVisits :: !((Map Identifier Int)),mergeMap_Inh_CVisits :: !((Map Identifier (Identifier, [Identifier]))),nr_Inh_CVisits :: !(Int),nt_Inh_CVisits :: !(NontermIdent),o_case_Inh_CVisits :: !(Bool),o_cata_Inh_CVisits :: !(Bool),o_costcentre_Inh_CVisits :: !(Bool),o_data_Inh_CVisits :: !((Maybe Bool)),o_linePragmas_Inh_CVisits :: !(Bool),o_monadic_Inh_CVisits :: !(Bool),o_newtypes_Inh_CVisits :: !(Bool),o_pretty_Inh_CVisits :: !(Bool),o_rename_Inh_CVisits :: !(Bool),o_sem_Inh_CVisits :: !(Bool),o_sig_Inh_CVisits :: !(Bool),o_splitsems_Inh_CVisits :: !(Bool),o_strictwrap_Inh_CVisits :: !(Bool),o_traces_Inh_CVisits :: !(Bool),o_unbox_Inh_CVisits :: !(Bool),options_Inh_CVisits :: !(Options),paramInstMap_Inh_CVisits :: !((Map Identifier (NontermIdent, [String]))),paramMap_Inh_CVisits :: !(ParamMap),prefix_Inh_CVisits :: !(String),quantMap_Inh_CVisits :: !(QuantMap),syn_Inh_CVisits :: !(Attributes),terminals_Inh_CVisits :: !(([Identifier])),unfoldSemDom_Inh_CVisits :: !((NontermIdent -> Int -> [String] -> Code.Type)),visitedSet_Inh_CVisits :: !((Set Identifier)),with_sig_Inh_CVisits :: !(Bool),wrappers_Inh_CVisits :: !((Set NontermIdent))}
data Syn_CVisits = Syn_CVisits {comments_Syn_CVisits :: !(([String])),decls_Syn_CVisits :: !(Decls),gatherInstVisitNrs_Syn_CVisits :: !((Map Identifier Int)),intra_Syn_CVisits :: !(Exprs),intraVars_Syn_CVisits :: !((Set String)),isNil_Syn_CVisits :: !(Bool),semNames_Syn_CVisits :: !(([String])),visitedSet_Syn_CVisits :: !((Set Identifier))}
wrap_CVisits :: T_CVisits ->
                Inh_CVisits ->
                Syn_CVisits
wrap_CVisits (T_CVisits sem) (Inh_CVisits _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
    (let ( _lhsOcomments,_lhsOdecls,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames,_lhsOvisitedSet) = sem _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
     in  (Syn_CVisits _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet))
sem_CVisits_Cons :: T_CVisit ->
                    T_CVisits ->
                    T_CVisits
sem_CVisits_Cons (T_CVisit hd_) (T_CVisits tl_) =
    (T_CVisits (\ _lhsIallNts
                  _lhsIallPragmas
                  _lhsIaroundMap
                  _lhsIchildren
                  _lhsIcon
                  _lhsIcontextMap
                  _lhsIinh
                  _lhsIinstVisitNrs
                  _lhsImergeMap
                  _lhsInr
                  _lhsInt
                  _lhsIo_case
                  _lhsIo_cata
                  _lhsIo_costcentre
                  _lhsIo_data
                  _lhsIo_linePragmas
                  _lhsIo_monadic
                  _lhsIo_newtypes
                  _lhsIo_pretty
                  _lhsIo_rename
                  _lhsIo_sem
                  _lhsIo_sig
                  _lhsIo_splitsems
                  _lhsIo_strictwrap
                  _lhsIo_traces
                  _lhsIo_unbox
                  _lhsIoptions
                  _lhsIparamInstMap
                  _lhsIparamMap
                  _lhsIprefix
                  _lhsIquantMap
                  _lhsIsyn
                  _lhsIterminals
                  _lhsIunfoldSemDom
                  _lhsIvisitedSet
                  _lhsIwith_sig
                  _lhsIwrappers ->
                    (let _tlOnr :: Int
                         _lhsOisNil :: Bool
                         _hdOisLast :: Bool
                         _hdOnextIntra :: Exprs
                         _hdOnextIntraVars :: (Set String)
                         _lhsOintra :: Exprs
                         _lhsOintraVars :: (Set String)
                         _lhsOdecls :: Decls
                         _hdOdecls :: Decls
                         _lhsOcomments :: ([String])
                         _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                         _lhsOsemNames :: ([String])
                         _lhsOvisitedSet :: (Set Identifier)
                         _hdOallNts :: (Set NontermIdent)
                         _hdOallPragmas :: PragmaMap
                         _hdOaroundMap :: (Set Identifier)
                         _hdOchildren :: ([(Identifier,Type, ChildKind)])
                         _hdOcon :: ConstructorIdent
                         _hdOcontextMap :: ContextMap
                         _hdOinh :: Attributes
                         _hdOinstVisitNrs :: (Map Identifier Int)
                         _hdOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                         _hdOnr :: Int
                         _hdOnt :: NontermIdent
                         _hdOo_case :: Bool
                         _hdOo_cata :: Bool
                         _hdOo_costcentre :: Bool
                         _hdOo_data :: (Maybe Bool)
                         _hdOo_linePragmas :: Bool
                         _hdOo_monadic :: Bool
                         _hdOo_newtypes :: Bool
                         _hdOo_pretty :: Bool
                         _hdOo_rename :: Bool
                         _hdOo_sem :: Bool
                         _hdOo_sig :: Bool
                         _hdOo_splitsems :: Bool
                         _hdOo_strictwrap :: Bool
                         _hdOo_traces :: Bool
                         _hdOo_unbox :: Bool
                         _hdOoptions :: Options
                         _hdOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                         _hdOparamMap :: ParamMap
                         _hdOprefix :: String
                         _hdOquantMap :: QuantMap
                         _hdOsyn :: Attributes
                         _hdOterminals :: ([Identifier])
                         _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                         _hdOvisitedSet :: (Set Identifier)
                         _hdOwith_sig :: Bool
                         _hdOwrappers :: (Set NontermIdent)
                         _tlOallNts :: (Set NontermIdent)
                         _tlOallPragmas :: PragmaMap
                         _tlOaroundMap :: (Set Identifier)
                         _tlOchildren :: ([(Identifier,Type, ChildKind)])
                         _tlOcon :: ConstructorIdent
                         _tlOcontextMap :: ContextMap
                         _tlOinh :: Attributes
                         _tlOinstVisitNrs :: (Map Identifier Int)
                         _tlOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                         _tlOnt :: NontermIdent
                         _tlOo_case :: Bool
                         _tlOo_cata :: Bool
                         _tlOo_costcentre :: Bool
                         _tlOo_data :: (Maybe Bool)
                         _tlOo_linePragmas :: Bool
                         _tlOo_monadic :: Bool
                         _tlOo_newtypes :: Bool
                         _tlOo_pretty :: Bool
                         _tlOo_rename :: Bool
                         _tlOo_sem :: Bool
                         _tlOo_sig :: Bool
                         _tlOo_splitsems :: Bool
                         _tlOo_strictwrap :: Bool
                         _tlOo_traces :: Bool
                         _tlOo_unbox :: Bool
                         _tlOoptions :: Options
                         _tlOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                         _tlOparamMap :: ParamMap
                         _tlOprefix :: String
                         _tlOquantMap :: QuantMap
                         _tlOsyn :: Attributes
                         _tlOterminals :: ([Identifier])
                         _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                         _tlOvisitedSet :: (Set Identifier)
                         _tlOwith_sig :: Bool
                         _tlOwrappers :: (Set NontermIdent)
                         _hdIcomments :: ([String])
                         _hdIdecls :: Decls
                         _hdIgatherInstVisitNrs :: (Map Identifier Int)
                         _hdIintra :: Exprs
                         _hdIintraVars :: (Set String)
                         _hdIsemNames :: ([String])
                         _hdIvisitedSet :: (Set Identifier)
                         _tlIcomments :: ([String])
                         _tlIdecls :: Decls
                         _tlIgatherInstVisitNrs :: (Map Identifier Int)
                         _tlIintra :: Exprs
                         _tlIintraVars :: (Set String)
                         _tlIisNil :: Bool
                         _tlIsemNames :: ([String])
                         _tlIvisitedSet :: (Set Identifier)
                         -- "./src-ag/GenerateCode.ag"(line 282, column 11)
                         _tlOnr =
                             ({-# LINE 282 "./src-ag/GenerateCode.ag" #-}
                              _lhsInr + 1
                              {-# LINE 5953 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 295, column 12)
                         _lhsOisNil =
                             ({-# LINE 295 "./src-ag/GenerateCode.ag" #-}
                              False
                              {-# LINE 5959 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 296, column 12)
                         _hdOisLast =
                             ({-# LINE 296 "./src-ag/GenerateCode.ag" #-}
                              _tlIisNil
                              {-# LINE 5965 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 313, column 12)
                         _hdOnextIntra =
                             ({-# LINE 313 "./src-ag/GenerateCode.ag" #-}
                              _tlIintra
                              {-# LINE 5971 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 314, column 12)
                         _hdOnextIntraVars =
                             ({-# LINE 314 "./src-ag/GenerateCode.ag" #-}
                              _tlIintraVars
                              {-# LINE 5977 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 315, column 12)
                         _lhsOintra =
                             ({-# LINE 315 "./src-ag/GenerateCode.ag" #-}
                              _hdIintra
                              {-# LINE 5983 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 316, column 12)
                         _lhsOintraVars =
                             ({-# LINE 316 "./src-ag/GenerateCode.ag" #-}
                              _hdIintraVars
                              {-# LINE 5989 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 431, column 11)
                         _lhsOdecls =
                             ({-# LINE 431 "./src-ag/GenerateCode.ag" #-}
                              _hdIdecls
                              {-# LINE 5995 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 432, column 11)
                         _hdOdecls =
                             ({-# LINE 432 "./src-ag/GenerateCode.ag" #-}
                              _tlIdecls
                              {-# LINE 6001 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                         _lhsOcomments =
                             ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                              _hdIcomments ++ _tlIcomments
                              {-# LINE 6007 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 561, column 44)
                         _lhsOgatherInstVisitNrs =
                             ({-# LINE 561 "./src-ag/GenerateCode.ag" #-}
                              _hdIgatherInstVisitNrs `Map.union` _tlIgatherInstVisitNrs
                              {-# LINE 6013 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 1146, column 61)
                         _lhsOsemNames =
                             ({-# LINE 1146 "./src-ag/GenerateCode.ag" #-}
                              _hdIsemNames ++ _tlIsemNames
                              {-# LINE 6019 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (up)
                         _lhsOvisitedSet =
                             ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                              _tlIvisitedSet
                              {-# LINE 6025 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOallNts =
                             ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                              _lhsIallNts
                              {-# LINE 6031 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOallPragmas =
                             ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                              _lhsIallPragmas
                              {-# LINE 6037 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOaroundMap =
                             ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                              _lhsIaroundMap
                              {-# LINE 6043 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOchildren =
                             ({-# LINE 410 "./src-ag/GenerateCode.ag" #-}
                              _lhsIchildren
                              {-# LINE 6049 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOcon =
                             ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                              _lhsIcon
                              {-# LINE 6055 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOcontextMap =
                             ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                              _lhsIcontextMap
                              {-# LINE 6061 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOinh =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsIinh
                              {-# LINE 6067 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOinstVisitNrs =
                             ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                              _lhsIinstVisitNrs
                              {-# LINE 6073 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOmergeMap =
                             ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                              _lhsImergeMap
                              {-# LINE 6079 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOnr =
                             ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                              _lhsInr
                              {-# LINE 6085 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOnt =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsInt
                              {-# LINE 6091 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_case =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_case
                              {-# LINE 6097 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_cata =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_cata
                              {-# LINE 6103 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_costcentre =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_costcentre
                              {-# LINE 6109 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_data =
                             ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_data
                              {-# LINE 6115 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_linePragmas =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_linePragmas
                              {-# LINE 6121 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_monadic =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_monadic
                              {-# LINE 6127 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_newtypes =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_newtypes
                              {-# LINE 6133 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_pretty =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_pretty
                              {-# LINE 6139 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_rename =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_rename
                              {-# LINE 6145 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_sem =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_sem
                              {-# LINE 6151 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_sig =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_sig
                              {-# LINE 6157 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_splitsems =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_splitsems
                              {-# LINE 6163 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_strictwrap =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_strictwrap
                              {-# LINE 6169 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_traces =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_traces
                              {-# LINE 6175 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOo_unbox =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_unbox
                              {-# LINE 6181 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOoptions =
                             ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 6187 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOparamInstMap =
                             ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                              _lhsIparamInstMap
                              {-# LINE 6193 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOparamMap =
                             ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                              _lhsIparamMap
                              {-# LINE 6199 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOprefix =
                             ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                              _lhsIprefix
                              {-# LINE 6205 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOquantMap =
                             ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                              _lhsIquantMap
                              {-# LINE 6211 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOsyn =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsIsyn
                              {-# LINE 6217 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOterminals =
                             ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                              _lhsIterminals
                              {-# LINE 6223 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOunfoldSemDom =
                             ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                              _lhsIunfoldSemDom
                              {-# LINE 6229 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOvisitedSet =
                             ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                              _lhsIvisitedSet
                              {-# LINE 6235 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOwith_sig =
                             ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                              _lhsIwith_sig
                              {-# LINE 6241 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _hdOwrappers =
                             ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                              _lhsIwrappers
                              {-# LINE 6247 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOallNts =
                             ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                              _lhsIallNts
                              {-# LINE 6253 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOallPragmas =
                             ({-# LINE 73 "./src-ag/GenerateCode.ag" #-}
                              _lhsIallPragmas
                              {-# LINE 6259 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOaroundMap =
                             ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                              _lhsIaroundMap
                              {-# LINE 6265 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOchildren =
                             ({-# LINE 410 "./src-ag/GenerateCode.ag" #-}
                              _lhsIchildren
                              {-# LINE 6271 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOcon =
                             ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                              _lhsIcon
                              {-# LINE 6277 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOcontextMap =
                             ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                              _lhsIcontextMap
                              {-# LINE 6283 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOinh =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsIinh
                              {-# LINE 6289 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOinstVisitNrs =
                             ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                              _lhsIinstVisitNrs
                              {-# LINE 6295 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOmergeMap =
                             ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                              _lhsImergeMap
                              {-# LINE 6301 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOnt =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsInt
                              {-# LINE 6307 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_case =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_case
                              {-# LINE 6313 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_cata =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_cata
                              {-# LINE 6319 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_costcentre =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_costcentre
                              {-# LINE 6325 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_data =
                             ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_data
                              {-# LINE 6331 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_linePragmas =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_linePragmas
                              {-# LINE 6337 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_monadic =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_monadic
                              {-# LINE 6343 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_newtypes =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_newtypes
                              {-# LINE 6349 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_pretty =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_pretty
                              {-# LINE 6355 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_rename =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_rename
                              {-# LINE 6361 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_sem =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_sem
                              {-# LINE 6367 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_sig =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_sig
                              {-# LINE 6373 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_splitsems =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_splitsems
                              {-# LINE 6379 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_strictwrap =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_strictwrap
                              {-# LINE 6385 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_traces =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_traces
                              {-# LINE 6391 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOo_unbox =
                             ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                              _lhsIo_unbox
                              {-# LINE 6397 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOoptions =
                             ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                              _lhsIoptions
                              {-# LINE 6403 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOparamInstMap =
                             ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                              _lhsIparamInstMap
                              {-# LINE 6409 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOparamMap =
                             ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                              _lhsIparamMap
                              {-# LINE 6415 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOprefix =
                             ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                              _lhsIprefix
                              {-# LINE 6421 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOquantMap =
                             ({-# LINE 115 "./src-ag/GenerateCode.ag" #-}
                              _lhsIquantMap
                              {-# LINE 6427 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOsyn =
                             ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                              _lhsIsyn
                              {-# LINE 6433 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOterminals =
                             ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                              _lhsIterminals
                              {-# LINE 6439 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOunfoldSemDom =
                             ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                              _lhsIunfoldSemDom
                              {-# LINE 6445 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (chain)
                         _tlOvisitedSet =
                             ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                              _hdIvisitedSet
                              {-# LINE 6451 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOwith_sig =
                             ({-# LINE 851 "./src-ag/GenerateCode.ag" #-}
                              _lhsIwith_sig
                              {-# LINE 6457 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (down)
                         _tlOwrappers =
                             ({-# LINE 987 "./src-ag/GenerateCode.ag" #-}
                              _lhsIwrappers
                              {-# LINE 6463 "dist/build/GenerateCode.hs" #-}
                              )
                         ( _hdIcomments,_hdIdecls,_hdIgatherInstVisitNrs,_hdIintra,_hdIintraVars,_hdIsemNames,_hdIvisitedSet) =
                             hd_ _hdOallNts _hdOallPragmas _hdOaroundMap _hdOchildren _hdOcon _hdOcontextMap _hdOdecls _hdOinh _hdOinstVisitNrs _hdOisLast _hdOmergeMap _hdOnextIntra _hdOnextIntraVars _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwith_sig _hdOwrappers
                         ( _tlIcomments,_tlIdecls,_tlIgatherInstVisitNrs,_tlIintra,_tlIintraVars,_tlIisNil,_tlIsemNames,_tlIvisitedSet) =
                             tl_ _tlOallNts _tlOallPragmas _tlOaroundMap _tlOchildren _tlOcon _tlOcontextMap _tlOinh _tlOinstVisitNrs _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwith_sig _tlOwrappers
                     in  ( _lhsOcomments,_lhsOdecls,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames,_lhsOvisitedSet))))
sem_CVisits_Nil :: T_CVisits
sem_CVisits_Nil =
    (T_CVisits (\ _lhsIallNts
                  _lhsIallPragmas
                  _lhsIaroundMap
                  _lhsIchildren
                  _lhsIcon
                  _lhsIcontextMap
                  _lhsIinh
                  _lhsIinstVisitNrs
                  _lhsImergeMap
                  _lhsInr
                  _lhsInt
                  _lhsIo_case
                  _lhsIo_cata
                  _lhsIo_costcentre
                  _lhsIo_data
                  _lhsIo_linePragmas
                  _lhsIo_monadic
                  _lhsIo_newtypes
                  _lhsIo_pretty
                  _lhsIo_rename
                  _lhsIo_sem
                  _lhsIo_sig
                  _lhsIo_splitsems
                  _lhsIo_strictwrap
                  _lhsIo_traces
                  _lhsIo_unbox
                  _lhsIoptions
                  _lhsIparamInstMap
                  _lhsIparamMap
                  _lhsIprefix
                  _lhsIquantMap
                  _lhsIsyn
                  _lhsIterminals
                  _lhsIunfoldSemDom
                  _lhsIvisitedSet
                  _lhsIwith_sig
                  _lhsIwrappers ->
                    (let _lhsOisNil :: Bool
                         _lhsOintra :: Exprs
                         _lhsOintraVars :: (Set String)
                         _lhsOdecls :: Decls
                         _lhsOcomments :: ([String])
                         _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                         _lhsOsemNames :: ([String])
                         _lhsOvisitedSet :: (Set Identifier)
                         -- "./src-ag/GenerateCode.ag"(line 297, column 10)
                         _lhsOisNil =
                             ({-# LINE 297 "./src-ag/GenerateCode.ag" #-}
                              True
                              {-# LINE 6521 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 317, column 10)
                         _lhsOintra =
                             ({-# LINE 317 "./src-ag/GenerateCode.ag" #-}
                              []
                              {-# LINE 6527 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 318, column 10)
                         _lhsOintraVars =
                             ({-# LINE 318 "./src-ag/GenerateCode.ag" #-}
                              Set.empty
                              {-# LINE 6533 "dist/build/GenerateCode.hs" #-}
                              )
                         -- "./src-ag/GenerateCode.ag"(line 430, column 11)
                         _lhsOdecls =
                             ({-# LINE 430 "./src-ag/GenerateCode.ag" #-}
                              []
                              {-# LINE 6539 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                         _lhsOcomments =
                             ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                              []
                              {-# LINE 6545 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 561, column 44)
                         _lhsOgatherInstVisitNrs =
                             ({-# LINE 561 "./src-ag/GenerateCode.ag" #-}
                              Map.empty
                              {-# LINE 6551 "dist/build/GenerateCode.hs" #-}
                              )
                         -- use rule "./src-ag/GenerateCode.ag"(line 1146, column 61)
                         _lhsOsemNames =
                             ({-# LINE 1146 "./src-ag/GenerateCode.ag" #-}
                              []
                              {-# LINE 6557 "dist/build/GenerateCode.hs" #-}
                              )
                         -- copy rule (chain)
                         _lhsOvisitedSet =
                             ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                              _lhsIvisitedSet
                              {-# LINE 6563 "dist/build/GenerateCode.hs" #-}
                              )
                     in  ( _lhsOcomments,_lhsOdecls,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames,_lhsOvisitedSet))))
-- DeclBlocks --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         blockNr              : Int
         lastExprVars         : [String]
         nextVisitDecls       : [Decl]
         optCase              : Bool
         prefix               : String
      synthesized attributes:
         callExpr             : Expr
         decls                : [Decl]
         freeVars             : [String]
   alternatives:
      alternative DeclBlock:
         child defs           : {[Decl]}
         child visit          : {Decl}
         child next           : DeclBlocks 
         visit 0:
            local lambdaName  : _
            local pragmaDecl  : _
            local freeVars    : _
            local decl        : _
      alternative DeclTerminator:
         child defs           : {[Decl]}
         child result         : {Expr}
         visit 0:
            local lambdaName  : _
            local pragmaDecl  : _
            local freeVars    : _
-}
-- cata
sem_DeclBlocks :: DeclBlocks ->
                  T_DeclBlocks
sem_DeclBlocks (DeclBlock _defs _visit _next) =
    (sem_DeclBlocks_DeclBlock _defs _visit (sem_DeclBlocks _next))
sem_DeclBlocks (DeclTerminator _defs _result) =
    (sem_DeclBlocks_DeclTerminator _defs _result)
-- semantic domain
newtype T_DeclBlocks = T_DeclBlocks (Int ->
                                     ([String]) ->
                                     ([Decl]) ->
                                     Bool ->
                                     String ->
                                     ( Expr,([Decl]),([String])))
data Inh_DeclBlocks = Inh_DeclBlocks {blockNr_Inh_DeclBlocks :: !(Int),lastExprVars_Inh_DeclBlocks :: !(([String])),nextVisitDecls_Inh_DeclBlocks :: !(([Decl])),optCase_Inh_DeclBlocks :: !(Bool),prefix_Inh_DeclBlocks :: !(String)}
data Syn_DeclBlocks = Syn_DeclBlocks {callExpr_Syn_DeclBlocks :: !(Expr),decls_Syn_DeclBlocks :: !(([Decl])),freeVars_Syn_DeclBlocks :: !(([String]))}
wrap_DeclBlocks :: T_DeclBlocks ->
                   Inh_DeclBlocks ->
                   Syn_DeclBlocks
wrap_DeclBlocks (T_DeclBlocks sem) (Inh_DeclBlocks _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) =
    (let ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars) = sem _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix
     in  (Syn_DeclBlocks _lhsOcallExpr _lhsOdecls _lhsOfreeVars))
sem_DeclBlocks_DeclBlock :: ([Decl]) ->
                            Decl ->
                            T_DeclBlocks ->
                            T_DeclBlocks
sem_DeclBlocks_DeclBlock defs_ visit_ (T_DeclBlocks next_) =
    (T_DeclBlocks (\ _lhsIblockNr
                     _lhsIlastExprVars
                     _lhsInextVisitDecls
                     _lhsIoptCase
                     _lhsIprefix ->
                       (let _nextOblockNr :: Int
                            _lhsOcallExpr :: Expr
                            _lhsOdecls :: ([Decl])
                            _lhsOfreeVars :: ([String])
                            _nextOlastExprVars :: ([String])
                            _nextOnextVisitDecls :: ([Decl])
                            _nextOoptCase :: Bool
                            _nextOprefix :: String
                            _nextIcallExpr :: Expr
                            _nextIdecls :: ([Decl])
                            _nextIfreeVars :: ([String])
                            -- "./src-ag/GenerateCode.ag"(line 662, column 7)
                            _nextOblockNr =
                                ({-# LINE 662 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIblockNr + 1
                                 {-# LINE 6644 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 667, column 7)
                            _lambdaName =
                                ({-# LINE 667 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                                 {-# LINE 6650 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 668, column 7)
                            _pragmaDecl =
                                ({-# LINE 668 "./src-ag/GenerateCode.ag" #-}
                                 PragmaDecl ("NOINLINE " ++ _lambdaName    )
                                 {-# LINE 6656 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 669, column 7)
                            _lhsOcallExpr =
                                ({-# LINE 669 "./src-ag/GenerateCode.ag" #-}
                                 App _lambdaName     (map SimpleExpr _freeVars    )
                                 {-# LINE 6662 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 673, column 7)
                            _freeVars =
                                ({-# LINE 673 "./src-ag/GenerateCode.ag" #-}
                                 freevars _nextIfreeVars (visit_ : defs_)
                                 {-# LINE 6668 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 680, column 7)
                            _decl =
                                ({-# LINE 680 "./src-ag/GenerateCode.ag" #-}
                                 mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ [visit_]) _nextIcallExpr
                                 {-# LINE 6674 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 681, column 7)
                            _lhsOdecls =
                                ({-# LINE 681 "./src-ag/GenerateCode.ag" #-}
                                 (if _lhsIblockNr > 1 then [_pragmaDecl    ] else []) ++ [_decl    ] ++ _nextIdecls
                                 {-# LINE 6680 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOfreeVars =
                                ({-# LINE 664 "./src-ag/GenerateCode.ag" #-}
                                 _freeVars
                                 {-# LINE 6686 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _nextOlastExprVars =
                                ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIlastExprVars
                                 {-# LINE 6692 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _nextOnextVisitDecls =
                                ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                 _lhsInextVisitDecls
                                 {-# LINE 6698 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _nextOoptCase =
                                ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIoptCase
                                 {-# LINE 6704 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (down)
                            _nextOprefix =
                                ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIprefix
                                 {-# LINE 6710 "dist/build/GenerateCode.hs" #-}
                                 )
                            ( _nextIcallExpr,_nextIdecls,_nextIfreeVars) =
                                next_ _nextOblockNr _nextOlastExprVars _nextOnextVisitDecls _nextOoptCase _nextOprefix
                        in  ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars))))
sem_DeclBlocks_DeclTerminator :: ([Decl]) ->
                                 Expr ->
                                 T_DeclBlocks
sem_DeclBlocks_DeclTerminator defs_ result_ =
    (T_DeclBlocks (\ _lhsIblockNr
                     _lhsIlastExprVars
                     _lhsInextVisitDecls
                     _lhsIoptCase
                     _lhsIprefix ->
                       (let _lhsOcallExpr :: Expr
                            _lhsOdecls :: ([Decl])
                            _lhsOfreeVars :: ([String])
                            -- "./src-ag/GenerateCode.ag"(line 667, column 7)
                            _lambdaName =
                                ({-# LINE 667 "./src-ag/GenerateCode.ag" #-}
                                 _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                                 {-# LINE 6731 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 668, column 7)
                            _pragmaDecl =
                                ({-# LINE 668 "./src-ag/GenerateCode.ag" #-}
                                 PragmaDecl ("NOINLINE " ++ _lambdaName    )
                                 {-# LINE 6737 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 669, column 7)
                            _lhsOcallExpr =
                                ({-# LINE 669 "./src-ag/GenerateCode.ag" #-}
                                 App _lambdaName     (map SimpleExpr _freeVars    )
                                 {-# LINE 6743 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 671, column 7)
                            _freeVars =
                                ({-# LINE 671 "./src-ag/GenerateCode.ag" #-}
                                 freevars _lhsIlastExprVars (defs_ ++ _lhsInextVisitDecls)
                                 {-# LINE 6749 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- "./src-ag/GenerateCode.ag"(line 678, column 7)
                            _lhsOdecls =
                                ({-# LINE 678 "./src-ag/GenerateCode.ag" #-}
                                 [ mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ _lhsInextVisitDecls) result_ ]
                                 {-# LINE 6755 "dist/build/GenerateCode.hs" #-}
                                 )
                            -- copy rule (from local)
                            _lhsOfreeVars =
                                ({-# LINE 664 "./src-ag/GenerateCode.ag" #-}
                                 _freeVars
                                 {-# LINE 6761 "dist/build/GenerateCode.hs" #-}
                                 )
                        in  ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars))))
-- DeclBlocksRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         lastExprVars         : [String]
         nextVisitDecls       : [Decl]
         optCase              : Bool
         prefix               : String
      synthesized attributes:
         firstCall            : Expr
         lambdas              : [Decl]
   alternatives:
      alternative DeclBlocksRoot:
         child blocks         : DeclBlocks 
-}
-- cata
sem_DeclBlocksRoot :: DeclBlocksRoot ->
                      T_DeclBlocksRoot
sem_DeclBlocksRoot (DeclBlocksRoot _blocks) =
    (sem_DeclBlocksRoot_DeclBlocksRoot (sem_DeclBlocks _blocks))
-- semantic domain
newtype T_DeclBlocksRoot = T_DeclBlocksRoot (([String]) ->
                                             ([Decl]) ->
                                             Bool ->
                                             String ->
                                             ( Expr,([Decl])))
data Inh_DeclBlocksRoot = Inh_DeclBlocksRoot {lastExprVars_Inh_DeclBlocksRoot :: !(([String])),nextVisitDecls_Inh_DeclBlocksRoot :: !(([Decl])),optCase_Inh_DeclBlocksRoot :: !(Bool),prefix_Inh_DeclBlocksRoot :: !(String)}
data Syn_DeclBlocksRoot = Syn_DeclBlocksRoot {firstCall_Syn_DeclBlocksRoot :: !(Expr),lambdas_Syn_DeclBlocksRoot :: !(([Decl]))}
wrap_DeclBlocksRoot :: T_DeclBlocksRoot ->
                       Inh_DeclBlocksRoot ->
                       Syn_DeclBlocksRoot
wrap_DeclBlocksRoot (T_DeclBlocksRoot sem) (Inh_DeclBlocksRoot _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) =
    (let ( _lhsOfirstCall,_lhsOlambdas) = sem _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix
     in  (Syn_DeclBlocksRoot _lhsOfirstCall _lhsOlambdas))
sem_DeclBlocksRoot_DeclBlocksRoot :: T_DeclBlocks ->
                                     T_DeclBlocksRoot
sem_DeclBlocksRoot_DeclBlocksRoot (T_DeclBlocks blocks_) =
    (T_DeclBlocksRoot (\ _lhsIlastExprVars
                         _lhsInextVisitDecls
                         _lhsIoptCase
                         _lhsIprefix ->
                           (let _lhsOlambdas :: ([Decl])
                                _lhsOfirstCall :: Expr
                                _blocksOblockNr :: Int
                                _blocksOlastExprVars :: ([String])
                                _blocksOnextVisitDecls :: ([Decl])
                                _blocksOoptCase :: Bool
                                _blocksOprefix :: String
                                _blocksIcallExpr :: Expr
                                _blocksIdecls :: ([Decl])
                                _blocksIfreeVars :: ([String])
                                -- "./src-ag/GenerateCode.ag"(line 653, column 7)
                                _lhsOlambdas =
                                    ({-# LINE 653 "./src-ag/GenerateCode.ag" #-}
                                     _blocksIdecls
                                     {-# LINE 6819 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- "./src-ag/GenerateCode.ag"(line 654, column 7)
                                _lhsOfirstCall =
                                    ({-# LINE 654 "./src-ag/GenerateCode.ag" #-}
                                     _blocksIcallExpr
                                     {-# LINE 6825 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- "./src-ag/GenerateCode.ag"(line 659, column 7)
                                _blocksOblockNr =
                                    ({-# LINE 659 "./src-ag/GenerateCode.ag" #-}
                                     1
                                     {-# LINE 6831 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- copy rule (down)
                                _blocksOlastExprVars =
                                    ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                     _lhsIlastExprVars
                                     {-# LINE 6837 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- copy rule (down)
                                _blocksOnextVisitDecls =
                                    ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                     _lhsInextVisitDecls
                                     {-# LINE 6843 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- copy rule (down)
                                _blocksOoptCase =
                                    ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                     _lhsIoptCase
                                     {-# LINE 6849 "dist/build/GenerateCode.hs" #-}
                                     )
                                -- copy rule (down)
                                _blocksOprefix =
                                    ({-# LINE 648 "./src-ag/GenerateCode.ag" #-}
                                     _lhsIprefix
                                     {-# LINE 6855 "dist/build/GenerateCode.hs" #-}
                                     )
                                ( _blocksIcallExpr,_blocksIdecls,_blocksIfreeVars) =
                                    blocks_ _blocksOblockNr _blocksOlastExprVars _blocksOnextVisitDecls _blocksOoptCase _blocksOprefix
                            in  ( _lhsOfirstCall,_lhsOlambdas))))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Pattern 
         definedInsts         : [Identifier]
         patternAttributes    : [(Identifier, Identifier)]
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
newtype T_Pattern = T_Pattern (( Pattern,([Identifier]),([(Identifier, Identifier)])))
data Inh_Pattern = Inh_Pattern {}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),definedInsts_Syn_Pattern :: !(([Identifier])),patternAttributes_Syn_Pattern :: !(([(Identifier, Identifier)]))}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern) =
    (let ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes) = sem
     in  (Syn_Pattern _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 6930 "dist/build/GenerateCode.hs" #-}
                         )
                    -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                    _lhsOpatternAttributes =
                        ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                         _patsIpatternAttributes
                         {-# LINE 6936 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 6942 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6948 "dist/build/GenerateCode.hs" #-}
                         )
                    ( _patsIcopy,_patsIdefinedInsts,_patsIpatternAttributes) =
                        pats_
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 6967 "dist/build/GenerateCode.hs" #-}
                         )
                    -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                    _lhsOpatternAttributes =
                        ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                         _patsIpatternAttributes
                         {-# LINE 6973 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 6979 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6985 "dist/build/GenerateCode.hs" #-}
                         )
                    ( _patsIcopy,_patsIdefinedInsts,_patsIpatternAttributes) =
                        pats_
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatternAttributes :: ([(Identifier, Identifier)])
                    -- "./src-ag/GenerateCode.ag"(line 263, column 11)
                    _lhsOdefinedInsts =
                        ({-# LINE 263 "./src-ag/GenerateCode.ag" #-}
                         (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                         {-# LINE 7005 "dist/build/GenerateCode.hs" #-}
                         )
                    -- "./src-ag/GenerateCode.ag"(line 271, column 7)
                    _lhsOpatternAttributes =
                        ({-# LINE 271 "./src-ag/GenerateCode.ag" #-}
                         (field_,attr_) : _patIpatternAttributes
                         {-# LINE 7011 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 7017 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 7023 "dist/build/GenerateCode.hs" #-}
                         )
                    ( _patIcopy,_patIdefinedInsts,_patIpatternAttributes) =
                        pat_
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                         _patIdefinedInsts
                         {-# LINE 7041 "dist/build/GenerateCode.hs" #-}
                         )
                    -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                    _lhsOpatternAttributes =
                        ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                         _patIpatternAttributes
                         {-# LINE 7047 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 7053 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 7059 "dist/build/GenerateCode.hs" #-}
                         )
                    ( _patIcopy,_patIdefinedInsts,_patIpatternAttributes) =
                        pat_
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 7074 "dist/build/GenerateCode.hs" #-}
                         )
                    -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                    _lhsOpatternAttributes =
                        ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 7080 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 7086 "dist/build/GenerateCode.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 7092 "dist/build/GenerateCode.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Patterns 
         definedInsts         : [Identifier]
         patternAttributes    : [(Identifier, Identifier)]
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
newtype T_Patterns = T_Patterns (( Patterns,([Identifier]),([(Identifier, Identifier)])))
data Inh_Patterns = Inh_Patterns {}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),definedInsts_Syn_Patterns :: !(([Identifier])),patternAttributes_Syn_Patterns :: !(([(Identifier, Identifier)]))}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns) =
    (let ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes) = sem
     in  (Syn_Patterns _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (let _lhsOdefinedInsts :: ([Identifier])
                     _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIdefinedInsts :: ([Identifier])
                     _hdIpatternAttributes :: ([(Identifier, Identifier)])
                     _tlIcopy :: Patterns
                     _tlIdefinedInsts :: ([Identifier])
                     _tlIpatternAttributes :: ([(Identifier, Identifier)])
                     -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                          _hdIdefinedInsts ++ _tlIdefinedInsts
                          {-# LINE 7144 "dist/build/GenerateCode.hs" #-}
                          )
                     -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                     _lhsOpatternAttributes =
                         ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                          _hdIpatternAttributes ++ _tlIpatternAttributes
                          {-# LINE 7150 "dist/build/GenerateCode.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 7156 "dist/build/GenerateCode.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 7162 "dist/build/GenerateCode.hs" #-}
                          )
                     ( _hdIcopy,_hdIdefinedInsts,_hdIpatternAttributes) =
                         hd_
                     ( _tlIcopy,_tlIdefinedInsts,_tlIpatternAttributes) =
                         tl_
                 in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (let _lhsOdefinedInsts :: ([Identifier])
                     _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                     _lhsOcopy :: Patterns
                     -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                          []
                          {-# LINE 7178 "dist/build/GenerateCode.hs" #-}
                          )
                     -- use rule "./src-ag/GenerateCode.ag"(line 268, column 47)
                     _lhsOpatternAttributes =
                         ({-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                          []
                          {-# LINE 7184 "dist/build/GenerateCode.hs" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 7190 "dist/build/GenerateCode.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 7196 "dist/build/GenerateCode.hs" #-}
                          )
                 in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)))
-- Sequence ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         aroundMap            : Set Identifier
         children             : [(Identifier,Type,ChildKind)]
         con                  : ConstructorIdent
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         lastExpr             : Expr
         mergeMap             : Map Identifier (Identifier, [Identifier])
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_monadic            : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         options              : Options
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         what                 : String
      chained attributes:
         declsAbove           : [Decl]
         visitedSet           : Set Identifier
      synthesized attributes:
         allTpsFound          : Bool
         blockDecls           : DeclBlocks 
         comments             : [String]
         decls                : Decls
         definedInsts         : [Identifier]
         exprs                : Exprs
         tSigs                : [Decl]
         tps                  : [Type]
         usedVars             : Set String
   alternatives:
      alternative Cons:
         child hd             : CRule 
         child tl             : Sequence 
      alternative Nil:
-}
-- cata
sem_Sequence :: Sequence ->
                T_Sequence
sem_Sequence list =
    (Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list))
-- semantic domain
newtype T_Sequence = T_Sequence ((Set NontermIdent) ->
                                 (Set Identifier) ->
                                 ([(Identifier,Type,ChildKind)]) ->
                                 ConstructorIdent ->
                                 ([Decl]) ->
                                 Attributes ->
                                 (Map Identifier Int) ->
                                 Expr ->
                                 (Map Identifier (Identifier, [Identifier])) ->
                                 Int ->
                                 NontermIdent ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 (Maybe Bool) ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Bool ->
                                 Options ->
                                 (Map Identifier (NontermIdent, [String])) ->
                                 ParamMap ->
                                 String ->
                                 Attributes ->
                                 ([Identifier]) ->
                                 (NontermIdent -> Int -> [String] -> Code.Type) ->
                                 (Set Identifier) ->
                                 String ->
                                 ( Bool,DeclBlocks,([String]),Decls,([Decl]),([Identifier]),Exprs,([Decl]),([Type]),(Set String),(Set Identifier)))
data Inh_Sequence = Inh_Sequence {allNts_Inh_Sequence :: !((Set NontermIdent)),aroundMap_Inh_Sequence :: !((Set Identifier)),children_Inh_Sequence :: !(([(Identifier,Type,ChildKind)])),con_Inh_Sequence :: !(ConstructorIdent),declsAbove_Inh_Sequence :: !(([Decl])),inh_Inh_Sequence :: !(Attributes),instVisitNrs_Inh_Sequence :: !((Map Identifier Int)),lastExpr_Inh_Sequence :: !(Expr),mergeMap_Inh_Sequence :: !((Map Identifier (Identifier, [Identifier]))),nr_Inh_Sequence :: !(Int),nt_Inh_Sequence :: !(NontermIdent),o_case_Inh_Sequence :: !(Bool),o_cata_Inh_Sequence :: !(Bool),o_costcentre_Inh_Sequence :: !(Bool),o_data_Inh_Sequence :: !((Maybe Bool)),o_linePragmas_Inh_Sequence :: !(Bool),o_monadic_Inh_Sequence :: !(Bool),o_newtypes_Inh_Sequence :: !(Bool),o_pretty_Inh_Sequence :: !(Bool),o_rename_Inh_Sequence :: !(Bool),o_sem_Inh_Sequence :: !(Bool),o_sig_Inh_Sequence :: !(Bool),o_splitsems_Inh_Sequence :: !(Bool),o_strictwrap_Inh_Sequence :: !(Bool),o_traces_Inh_Sequence :: !(Bool),o_unbox_Inh_Sequence :: !(Bool),options_Inh_Sequence :: !(Options),paramInstMap_Inh_Sequence :: !((Map Identifier (NontermIdent, [String]))),paramMap_Inh_Sequence :: !(ParamMap),prefix_Inh_Sequence :: !(String),syn_Inh_Sequence :: !(Attributes),terminals_Inh_Sequence :: !(([Identifier])),unfoldSemDom_Inh_Sequence :: !((NontermIdent -> Int -> [String] -> Code.Type)),visitedSet_Inh_Sequence :: !((Set Identifier)),what_Inh_Sequence :: !(String)}
data Syn_Sequence = Syn_Sequence {allTpsFound_Syn_Sequence :: !(Bool),blockDecls_Syn_Sequence :: !(DeclBlocks),comments_Syn_Sequence :: !(([String])),decls_Syn_Sequence :: !(Decls),declsAbove_Syn_Sequence :: !(([Decl])),definedInsts_Syn_Sequence :: !(([Identifier])),exprs_Syn_Sequence :: !(Exprs),tSigs_Syn_Sequence :: !(([Decl])),tps_Syn_Sequence :: !(([Type])),usedVars_Syn_Sequence :: !((Set String)),visitedSet_Syn_Sequence :: !((Set Identifier))}
wrap_Sequence :: T_Sequence ->
                 Inh_Sequence ->
                 Syn_Sequence
wrap_Sequence (T_Sequence sem) (Inh_Sequence _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
    (let ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet) = sem _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
     in  (Syn_Sequence _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet))
sem_Sequence_Cons :: T_CRule ->
                     T_Sequence ->
                     T_Sequence
sem_Sequence_Cons (T_CRule hd_) (T_Sequence tl_) =
    (T_Sequence (\ _lhsIallNts
                   _lhsIaroundMap
                   _lhsIchildren
                   _lhsIcon
                   _lhsIdeclsAbove
                   _lhsIinh
                   _lhsIinstVisitNrs
                   _lhsIlastExpr
                   _lhsImergeMap
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_monadic
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIoptions
                   _lhsIparamInstMap
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn
                   _lhsIterminals
                   _lhsIunfoldSemDom
                   _lhsIvisitedSet
                   _lhsIwhat ->
                     (let _lhsOblockDecls :: DeclBlocks
                          _lhsOallTpsFound :: Bool
                          _lhsOcomments :: ([String])
                          _lhsOdecls :: Decls
                          _lhsOdefinedInsts :: ([Identifier])
                          _lhsOexprs :: Exprs
                          _lhsOtSigs :: ([Decl])
                          _lhsOtps :: ([Type])
                          _lhsOusedVars :: (Set String)
                          _lhsOdeclsAbove :: ([Decl])
                          _lhsOvisitedSet :: (Set Identifier)
                          _hdOallNts :: (Set NontermIdent)
                          _hdOaroundMap :: (Set Identifier)
                          _hdOchildren :: ([(Identifier,Type,ChildKind)])
                          _hdOcon :: ConstructorIdent
                          _hdOdeclsAbove :: ([Decl])
                          _hdOinh :: Attributes
                          _hdOinstVisitNrs :: (Map Identifier Int)
                          _hdOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                          _hdOnr :: Int
                          _hdOnt :: NontermIdent
                          _hdOo_case :: Bool
                          _hdOo_cata :: Bool
                          _hdOo_costcentre :: Bool
                          _hdOo_data :: (Maybe Bool)
                          _hdOo_linePragmas :: Bool
                          _hdOo_monadic :: Bool
                          _hdOo_newtypes :: Bool
                          _hdOo_pretty :: Bool
                          _hdOo_rename :: Bool
                          _hdOo_sem :: Bool
                          _hdOo_sig :: Bool
                          _hdOo_splitsems :: Bool
                          _hdOo_strictwrap :: Bool
                          _hdOo_traces :: Bool
                          _hdOo_unbox :: Bool
                          _hdOoptions :: Options
                          _hdOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                          _hdOparamMap :: ParamMap
                          _hdOprefix :: String
                          _hdOsyn :: Attributes
                          _hdOterminals :: ([Identifier])
                          _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _hdOvisitedSet :: (Set Identifier)
                          _hdOwhat :: String
                          _tlOallNts :: (Set NontermIdent)
                          _tlOaroundMap :: (Set Identifier)
                          _tlOchildren :: ([(Identifier,Type,ChildKind)])
                          _tlOcon :: ConstructorIdent
                          _tlOdeclsAbove :: ([Decl])
                          _tlOinh :: Attributes
                          _tlOinstVisitNrs :: (Map Identifier Int)
                          _tlOlastExpr :: Expr
                          _tlOmergeMap :: (Map Identifier (Identifier, [Identifier]))
                          _tlOnr :: Int
                          _tlOnt :: NontermIdent
                          _tlOo_case :: Bool
                          _tlOo_cata :: Bool
                          _tlOo_costcentre :: Bool
                          _tlOo_data :: (Maybe Bool)
                          _tlOo_linePragmas :: Bool
                          _tlOo_monadic :: Bool
                          _tlOo_newtypes :: Bool
                          _tlOo_pretty :: Bool
                          _tlOo_rename :: Bool
                          _tlOo_sem :: Bool
                          _tlOo_sig :: Bool
                          _tlOo_splitsems :: Bool
                          _tlOo_strictwrap :: Bool
                          _tlOo_traces :: Bool
                          _tlOo_unbox :: Bool
                          _tlOoptions :: Options
                          _tlOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                          _tlOparamMap :: ParamMap
                          _tlOprefix :: String
                          _tlOsyn :: Attributes
                          _tlOterminals :: ([Identifier])
                          _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _tlOvisitedSet :: (Set Identifier)
                          _tlOwhat :: String
                          _hdIallTpsFound :: Bool
                          _hdIbldBlocksFun :: (DeclBlocks -> DeclBlocks)
                          _hdIcomments :: ([String])
                          _hdIdecls :: Decls
                          _hdIdeclsAbove :: ([Decl])
                          _hdIdefinedInsts :: ([Identifier])
                          _hdIexprs :: Exprs
                          _hdItSigs :: ([Decl])
                          _hdItps :: ([Type])
                          _hdIusedVars :: (Set String)
                          _hdIvisitedSet :: (Set Identifier)
                          _tlIallTpsFound :: Bool
                          _tlIblockDecls :: DeclBlocks
                          _tlIcomments :: ([String])
                          _tlIdecls :: Decls
                          _tlIdeclsAbove :: ([Decl])
                          _tlIdefinedInsts :: ([Identifier])
                          _tlIexprs :: Exprs
                          _tlItSigs :: ([Decl])
                          _tlItps :: ([Type])
                          _tlIusedVars :: (Set String)
                          _tlIvisitedSet :: (Set Identifier)
                          -- "./src-ag/GenerateCode.ag"(line 622, column 7)
                          _lhsOblockDecls =
                              ({-# LINE 622 "./src-ag/GenerateCode.ag" #-}
                               _hdIbldBlocksFun _tlIblockDecls
                               {-# LINE 7450 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 416, column 39)
                          _lhsOallTpsFound =
                              ({-# LINE 416 "./src-ag/GenerateCode.ag" #-}
                               _hdIallTpsFound && _tlIallTpsFound
                               {-# LINE 7456 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                          _lhsOcomments =
                              ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                               _hdIcomments ++ _tlIcomments
                               {-# LINE 7462 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 155, column 34)
                          _lhsOdecls =
                              ({-# LINE 155 "./src-ag/GenerateCode.ag" #-}
                               _hdIdecls ++ _tlIdecls
                               {-# LINE 7468 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                          _lhsOdefinedInsts =
                              ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                               _hdIdefinedInsts ++ _tlIdefinedInsts
                               {-# LINE 7474 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 334, column 34)
                          _lhsOexprs =
                              ({-# LINE 334 "./src-ag/GenerateCode.ag" #-}
                               _hdIexprs ++ _tlIexprs
                               {-# LINE 7480 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 363, column 33)
                          _lhsOtSigs =
                              ({-# LINE 363 "./src-ag/GenerateCode.ag" #-}
                               _hdItSigs ++ _tlItSigs
                               {-# LINE 7486 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 415, column 31)
                          _lhsOtps =
                              ({-# LINE 415 "./src-ag/GenerateCode.ag" #-}
                               _hdItps ++ _tlItps
                               {-# LINE 7492 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 352, column 37)
                          _lhsOusedVars =
                              ({-# LINE 352 "./src-ag/GenerateCode.ag" #-}
                               _hdIusedVars `Set.union` _tlIusedVars
                               {-# LINE 7498 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (up)
                          _lhsOdeclsAbove =
                              ({-# LINE 608 "./src-ag/GenerateCode.ag" #-}
                               _tlIdeclsAbove
                               {-# LINE 7504 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (up)
                          _lhsOvisitedSet =
                              ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                               _tlIvisitedSet
                               {-# LINE 7510 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallNts =
                              ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                               _lhsIallNts
                               {-# LINE 7516 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOaroundMap =
                              ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                               _lhsIaroundMap
                               {-# LINE 7522 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOchildren =
                              ({-# LINE 259 "./src-ag/GenerateCode.ag" #-}
                               _lhsIchildren
                               {-# LINE 7528 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                               _lhsIcon
                               {-# LINE 7534 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOdeclsAbove =
                              ({-# LINE 608 "./src-ag/GenerateCode.ag" #-}
                               _lhsIdeclsAbove
                               {-# LINE 7540 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinh =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsIinh
                               {-# LINE 7546 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinstVisitNrs =
                              ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                               _lhsIinstVisitNrs
                               {-# LINE 7552 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOmergeMap =
                              ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                               _lhsImergeMap
                               {-# LINE 7558 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnr =
                              ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                               _lhsInr
                               {-# LINE 7564 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsInt
                               {-# LINE 7570 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_case =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_case
                               {-# LINE 7576 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_cata =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_cata
                               {-# LINE 7582 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_costcentre =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_costcentre
                               {-# LINE 7588 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_data =
                              ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_data
                               {-# LINE 7594 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_linePragmas =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_linePragmas
                               {-# LINE 7600 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_monadic =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_monadic
                               {-# LINE 7606 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_newtypes =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_newtypes
                               {-# LINE 7612 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_pretty =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_pretty
                               {-# LINE 7618 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_rename =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_rename
                               {-# LINE 7624 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_sem =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_sem
                               {-# LINE 7630 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_sig =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_sig
                               {-# LINE 7636 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_splitsems =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_splitsems
                               {-# LINE 7642 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_strictwrap =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_strictwrap
                               {-# LINE 7648 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_traces =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_traces
                               {-# LINE 7654 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_unbox =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_unbox
                               {-# LINE 7660 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOoptions =
                              ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 7666 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOparamInstMap =
                              ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                               _lhsIparamInstMap
                               {-# LINE 7672 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOparamMap =
                              ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                               _lhsIparamMap
                               {-# LINE 7678 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOprefix =
                              ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                               _lhsIprefix
                               {-# LINE 7684 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsyn =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsIsyn
                               {-# LINE 7690 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOterminals =
                              ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                               _lhsIterminals
                               {-# LINE 7696 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOunfoldSemDom =
                              ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                               _lhsIunfoldSemDom
                               {-# LINE 7702 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOvisitedSet =
                              ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                               _lhsIvisitedSet
                               {-# LINE 7708 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOwhat =
                              ({-# LINE 870 "./src-ag/GenerateCode.ag" #-}
                               _lhsIwhat
                               {-# LINE 7714 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallNts =
                              ({-# LINE 132 "./src-ag/GenerateCode.ag" #-}
                               _lhsIallNts
                               {-# LINE 7720 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOaroundMap =
                              ({-# LINE 580 "./src-ag/GenerateCode.ag" #-}
                               _lhsIaroundMap
                               {-# LINE 7726 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOchildren =
                              ({-# LINE 259 "./src-ag/GenerateCode.ag" #-}
                               _lhsIchildren
                               {-# LINE 7732 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 89 "./src-ag/GenerateCode.ag" #-}
                               _lhsIcon
                               {-# LINE 7738 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (chain)
                          _tlOdeclsAbove =
                              ({-# LINE 608 "./src-ag/GenerateCode.ag" #-}
                               _hdIdeclsAbove
                               {-# LINE 7744 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinh =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsIinh
                               {-# LINE 7750 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinstVisitNrs =
                              ({-# LINE 560 "./src-ag/GenerateCode.ag" #-}
                               _lhsIinstVisitNrs
                               {-# LINE 7756 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOlastExpr =
                              ({-# LINE 606 "./src-ag/GenerateCode.ag" #-}
                               _lhsIlastExpr
                               {-# LINE 7762 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOmergeMap =
                              ({-# LINE 596 "./src-ag/GenerateCode.ag" #-}
                               _lhsImergeMap
                               {-# LINE 7768 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnr =
                              ({-# LINE 278 "./src-ag/GenerateCode.ag" #-}
                               _lhsInr
                               {-# LINE 7774 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsInt
                               {-# LINE 7780 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_case =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_case
                               {-# LINE 7786 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_cata =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_cata
                               {-# LINE 7792 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_costcentre =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_costcentre
                               {-# LINE 7798 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_data =
                              ({-# LINE 48 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_data
                               {-# LINE 7804 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_linePragmas =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_linePragmas
                               {-# LINE 7810 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_monadic =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_monadic
                               {-# LINE 7816 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_newtypes =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_newtypes
                               {-# LINE 7822 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_pretty =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_pretty
                               {-# LINE 7828 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_rename =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_rename
                               {-# LINE 7834 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_sem =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_sem
                               {-# LINE 7840 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_sig =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_sig
                               {-# LINE 7846 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_splitsems =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_splitsems
                               {-# LINE 7852 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_strictwrap =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_strictwrap
                               {-# LINE 7858 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_traces =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_traces
                               {-# LINE 7864 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_unbox =
                              ({-# LINE 47 "./src-ag/GenerateCode.ag" #-}
                               _lhsIo_unbox
                               {-# LINE 7870 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOoptions =
                              ({-# LINE 50 "./src-ag/GenerateCode.ag" #-}
                               _lhsIoptions
                               {-# LINE 7876 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOparamInstMap =
                              ({-# LINE 101 "./src-ag/GenerateCode.ag" #-}
                               _lhsIparamInstMap
                               {-# LINE 7882 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOparamMap =
                              ({-# LINE 95 "./src-ag/GenerateCode.ag" #-}
                               _lhsIparamMap
                               {-# LINE 7888 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOprefix =
                              ({-# LINE 49 "./src-ag/GenerateCode.ag" #-}
                               _lhsIprefix
                               {-# LINE 7894 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsyn =
                              ({-# LINE 84 "./src-ag/GenerateCode.ag" #-}
                               _lhsIsyn
                               {-# LINE 7900 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOterminals =
                              ({-# LINE 90 "./src-ag/GenerateCode.ag" #-}
                               _lhsIterminals
                               {-# LINE 7906 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOunfoldSemDom =
                              ({-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                               _lhsIunfoldSemDom
                               {-# LINE 7912 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (chain)
                          _tlOvisitedSet =
                              ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                               _hdIvisitedSet
                               {-# LINE 7918 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOwhat =
                              ({-# LINE 870 "./src-ag/GenerateCode.ag" #-}
                               _lhsIwhat
                               {-# LINE 7924 "dist/build/GenerateCode.hs" #-}
                               )
                          ( _hdIallTpsFound,_hdIbldBlocksFun,_hdIcomments,_hdIdecls,_hdIdeclsAbove,_hdIdefinedInsts,_hdIexprs,_hdItSigs,_hdItps,_hdIusedVars,_hdIvisitedSet) =
                              hd_ _hdOallNts _hdOaroundMap _hdOchildren _hdOcon _hdOdeclsAbove _hdOinh _hdOinstVisitNrs _hdOmergeMap _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwhat
                          ( _tlIallTpsFound,_tlIblockDecls,_tlIcomments,_tlIdecls,_tlIdeclsAbove,_tlIdefinedInsts,_tlIexprs,_tlItSigs,_tlItps,_tlIusedVars,_tlIvisitedSet) =
                              tl_ _tlOallNts _tlOaroundMap _tlOchildren _tlOcon _tlOdeclsAbove _tlOinh _tlOinstVisitNrs _tlOlastExpr _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwhat
                      in  ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet))))
sem_Sequence_Nil :: T_Sequence
sem_Sequence_Nil =
    (T_Sequence (\ _lhsIallNts
                   _lhsIaroundMap
                   _lhsIchildren
                   _lhsIcon
                   _lhsIdeclsAbove
                   _lhsIinh
                   _lhsIinstVisitNrs
                   _lhsIlastExpr
                   _lhsImergeMap
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_monadic
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIoptions
                   _lhsIparamInstMap
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn
                   _lhsIterminals
                   _lhsIunfoldSemDom
                   _lhsIvisitedSet
                   _lhsIwhat ->
                     (let _lhsOblockDecls :: DeclBlocks
                          _lhsOallTpsFound :: Bool
                          _lhsOcomments :: ([String])
                          _lhsOdecls :: Decls
                          _lhsOdefinedInsts :: ([Identifier])
                          _lhsOexprs :: Exprs
                          _lhsOtSigs :: ([Decl])
                          _lhsOtps :: ([Type])
                          _lhsOusedVars :: (Set String)
                          _lhsOdeclsAbove :: ([Decl])
                          _lhsOvisitedSet :: (Set Identifier)
                          -- "./src-ag/GenerateCode.ag"(line 624, column 7)
                          _lhsOblockDecls =
                              ({-# LINE 624 "./src-ag/GenerateCode.ag" #-}
                               DeclTerminator _lhsIdeclsAbove _lhsIlastExpr
                               {-# LINE 7983 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 416, column 39)
                          _lhsOallTpsFound =
                              ({-# LINE 416 "./src-ag/GenerateCode.ag" #-}
                               True
                               {-# LINE 7989 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 868, column 52)
                          _lhsOcomments =
                              ({-# LINE 868 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 7995 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 155, column 34)
                          _lhsOdecls =
                              ({-# LINE 155 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 8001 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 261, column 55)
                          _lhsOdefinedInsts =
                              ({-# LINE 261 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 8007 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 334, column 34)
                          _lhsOexprs =
                              ({-# LINE 334 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 8013 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 363, column 33)
                          _lhsOtSigs =
                              ({-# LINE 363 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 8019 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 415, column 31)
                          _lhsOtps =
                              ({-# LINE 415 "./src-ag/GenerateCode.ag" #-}
                               []
                               {-# LINE 8025 "dist/build/GenerateCode.hs" #-}
                               )
                          -- use rule "./src-ag/GenerateCode.ag"(line 352, column 37)
                          _lhsOusedVars =
                              ({-# LINE 352 "./src-ag/GenerateCode.ag" #-}
                               Set.empty
                               {-# LINE 8031 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (chain)
                          _lhsOdeclsAbove =
                              ({-# LINE 608 "./src-ag/GenerateCode.ag" #-}
                               _lhsIdeclsAbove
                               {-# LINE 8037 "dist/build/GenerateCode.hs" #-}
                               )
                          -- copy rule (chain)
                          _lhsOvisitedSet =
                              ({-# LINE 145 "./src-ag/GenerateCode.ag" #-}
                               _lhsIvisitedSet
                               {-# LINE 8043 "dist/build/GenerateCode.hs" #-}
                               )
                      in  ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars,_lhsOvisitedSet))))