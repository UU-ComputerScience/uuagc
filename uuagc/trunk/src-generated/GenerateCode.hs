{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenerateCode where
{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 12 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 19 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/DeclBlocks.ag" #-}

import Code (Decl,Expr)
{-# LINE 24 "dist/build/GenerateCode.hs" #-}

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

import Data.List(partition,intersperse)
import Data.Maybe(fromJust,isJust)

{-# LINE 52 "dist/build/GenerateCode.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 106 "./src-ag/GenerateCode.ag" #-}

-- remove possible @v references in the types of a data type.
cleanupArg :: String -> String
cleanupArg s
  = case idEvalType (SimpleType s) of
      SimpleType s' -> s'
      _             -> error "Only SimpleType supported"
{-# LINE 63 "dist/build/GenerateCode.hs" #-}

{-# LINE 122 "./src-ag/GenerateCode.ag" #-}

appContext :: ContextMap -> NontermIdent -> Code.Type -> Code.Type
appContext mp nt tp
  = maybe tp (\ctx -> CtxApp (map (\(n,ns) -> (getName n, ns)) ctx) tp) $ Map.lookup nt mp

appQuant :: QuantMap -> NontermIdent -> Code.Type -> Code.Type
appQuant mp nt tp
  = foldr QuantApp tp $ Map.findWithDefault [] nt mp
{-# LINE 74 "dist/build/GenerateCode.hs" #-}

{-# LINE 247 "./src-ag/GenerateCode.ag" #-}

mkDecl :: Bool -> Lhs -> Expr -> Set String -> Set String -> Decl
mkDecl True  lhs rhs _ _   = Bind lhs rhs
mkDecl False lhs rhs s1 s2 = Decl lhs rhs s1 s2

unwrapSem :: Bool -> NontermIdent -> Expr -> Expr
unwrapSem False _ e = e
unwrapSem True nm e = Case e alts
  where alts  = [CaseAlt left right]
        left  = Fun (typeName nm 0) [SimpleExpr "x"]
        right = SimpleExpr "x"
{-# LINE 88 "dist/build/GenerateCode.hs" #-}

{-# LINE 538 "./src-ag/GenerateCode.ag" #-}

mkLambdaArg :: String -> Maybe Code.Type -> Expr
mkLambdaArg nm Nothing = SimpleExpr nm
mkLambdaArg nm (Just tp) = TypedExpr (SimpleExpr nm) tp

mkLambda :: Exprs -> Expr -> Expr
mkLambda [] e = e
mkLambda xs e = Lambda xs e

mkSemFun :: Identifier -> Int -> Exprs -> Expr -> Expr
mkSemFun nt nr xs e = SemFun (typeName nt nr) xs e

typeAppStrs :: String -> [String] -> Code.Type
typeAppStrs nm params = TypeApp (SimpleType nm) (map SimpleType params)

isHigherOrder :: ChildKind -> Bool
isHigherOrder ChildAttr = True
isHigherOrder _         = False

pickOrigType :: (Identifier, Type, ChildKind) -> (Identifier, Type, ChildKind)
pickOrigType (nm, _, virt@(ChildReplace x)) = (nm, x, virt)
pickOrigType x = x
{-# LINE 113 "dist/build/GenerateCode.hs" #-}

{-# LINE 635 "./src-ag/GenerateCode.ag" #-}

mkPartitionedFunction :: String -> Bool -> [Decl] -> [String] -> DeclBlocks -> ([Decl], Expr)
mkPartitionedFunction prefix' optCase nextVisitDecls lastExprVars cpsTree
  = let inh = Inh_DeclBlocksRoot { prefix_Inh_DeclBlocksRoot = prefix'
                                 , optCase_Inh_DeclBlocksRoot = optCase
                                 , nextVisitDecls_Inh_DeclBlocksRoot = nextVisitDecls
                                 , lastExprVars_Inh_DeclBlocksRoot = lastExprVars
                                 }
        sem = sem_DeclBlocksRoot (DeclBlocksRoot cpsTree)
        syn = wrap_DeclBlocksRoot sem inh
    in (lambdas_Syn_DeclBlocksRoot syn, firstCall_Syn_DeclBlocksRoot syn)
{-# LINE 127 "dist/build/GenerateCode.hs" #-}

{-# LINE 685 "./src-ag/GenerateCode.ag" #-}

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
{-# LINE 150 "dist/build/GenerateCode.hs" #-}

{-# LINE 763 "./src-ag/GenerateCode.ag" #-}

typeToCodeType :: Maybe NontermIdent -> [String] -> Type -> Code.Type
typeToCodeType _ _ tp
  = case tp of
      NT nt tps defor -> NontermType (getName nt) tps defor
      Haskell t       -> SimpleType t
      Self            -> error "Self type not allowed here."

evalType :: (String -> String) -> Code.Type -> Code.Type
evalType replf t'
  = chase t'
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
{-# LINE 189 "dist/build/GenerateCode.hs" #-}

{-# LINE 888 "./src-ag/GenerateCode.ag" #-}

-- for a virtual child that already existed as a child, returns
isFirstOrder :: ChildKind -> Type -> Maybe Type
isFirstOrder ChildSyntax       tp = Just tp
isFirstOrder ChildAttr         _  = Nothing
isFirstOrder (ChildReplace tp) _  = Just tp
{-# LINE 198 "dist/build/GenerateCode.hs" #-}

{-# LINE 909 "./src-ag/GenerateCode.ag" #-}

makeLocalComment :: Int -> String -> Identifier -> Maybe Type -> String
makeLocalComment width what  name tp = let  x = getName name
                                            y = maybe "_" (\t -> case t of
					      	      (NT nt tps _) -> getName nt ++ " " ++ unwords tps
					      	      Haskell t' -> '{' : t' ++ "}"
						      Self -> error "Self type not allowed here.") tp
                                       in   ( what ++ " " ++ x ++ replicate ((width - length x) `max` 0) ' ' ++ " : " ++ y )

{-# LINE 210 "dist/build/GenerateCode.hs" #-}

{-# LINE 943 "./src-ag/GenerateCode.ag" #-}

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
oneCase (Decl left rhs _ _)      ex = Case rhs [CaseAlt left ex]
oneCase (Resume _ nt left rhs)   ex = ResumeExpr nt rhs left ex
oneCase _                        ex = ex

-- Gives the name of the visit function
funname :: Show a => a -> Int -> String
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
{-# LINE 257 "dist/build/GenerateCode.hs" #-}

{-# LINE 1033 "./src-ag/GenerateCode.ag" #-}

toNamedType :: Bool -> NontermIdent -> ConstructorIdent -> Identifier -> Code.Type -> Code.NamedType
toNamedType genStrict nt con nm tp
  = Code.Named genStrict strNm tp
  where strNm = recordFieldname nt con nm
{-# LINE 265 "dist/build/GenerateCode.hs" #-}
-- CGrammar ----------------------------------------------------
-- wrapper
data Inh_CGrammar  = Inh_CGrammar { options_Inh_CGrammar :: (Options) }
data Syn_CGrammar  = Syn_CGrammar { errors_Syn_CGrammar :: (Seq Error), output_Syn_CGrammar :: (Program) }
{-# INLINABLE wrap_CGrammar #-}
wrap_CGrammar :: T_CGrammar  -> Inh_CGrammar  -> (Syn_CGrammar )
wrap_CGrammar (T_CGrammar act) (Inh_CGrammar _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CGrammar_vIn1 _lhsIoptions
        (T_CGrammar_vOut1 _lhsOerrors _lhsOoutput) <- return (inv_CGrammar_s2 sem arg)
        return (Syn_CGrammar _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_CGrammar #-}
sem_CGrammar :: CGrammar  -> T_CGrammar 
sem_CGrammar ( CGrammar typeSyns_ derivings_ wrappers_ nonts_ pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_ ) = sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ ( sem_CNonterminals nonts_ ) pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_

-- semantic domain
newtype T_CGrammar  = T_CGrammar {
                                 attach_T_CGrammar :: Identity (T_CGrammar_s2 )
                                 }
newtype T_CGrammar_s2  = C_CGrammar_s2 {
                                       inv_CGrammar_s2 :: (T_CGrammar_v1 )
                                       }
data T_CGrammar_s3  = C_CGrammar_s3
type T_CGrammar_v1  = (T_CGrammar_vIn1 ) -> (T_CGrammar_vOut1 )
data T_CGrammar_vIn1  = T_CGrammar_vIn1 (Options)
data T_CGrammar_vOut1  = T_CGrammar_vOut1 (Seq Error) (Program)
{-# NOINLINE sem_CGrammar_CGrammar #-}
sem_CGrammar_CGrammar :: (TypeSyns) -> (Derivings) -> (Set NontermIdent) -> T_CNonterminals  -> (PragmaMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (Map NontermIdent (Map ConstructorIdent (Set Identifier))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) -> (Bool) -> T_CGrammar 
sem_CGrammar_CGrammar arg_typeSyns_ arg_derivings_ arg_wrappers_ arg_nonts_ arg_pragmas_ arg_paramMap_ arg_contextMap_ arg_quantMap_ arg_aroundsMap_ arg_mergeMap_ arg_multivisit_ = T_CGrammar (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_CGrammar_v1 
      v1 = \ (T_CGrammar_vIn1 _lhsIoptions) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_nonts_))
         (T_CNonterminals_vOut10 _nontsIchunks _nontsIgathNts _nontsIsemDomUnfoldGath) = inv_CNonterminals_s11 _nontsX11 (T_CNonterminals_vIn10 _nontsOallNts _nontsOallPragmas _nontsOaroundMap _nontsOcontextMap _nontsOderivings _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_costcentre _nontsOo_data _nontsOo_linePragmas _nontsOo_monadic _nontsOo_newtypes _nontsOo_pretty _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_splitsems _nontsOo_strictwrap _nontsOo_traces _nontsOo_unbox _nontsOoptions _nontsOparamMap _nontsOprefix _nontsOquantMap _nontsOtypeSyns _nontsOunfoldSemDom _nontsOwith_sig _nontsOwrappers)
         _nontsOo_sig = rule0 _lhsIoptions
         _nontsOo_cata = rule1 _lhsIoptions
         _nontsOo_sem = rule2 _lhsIoptions
         _nontsOo_newtypes = rule3 _lhsIoptions
         _nontsOo_unbox = rule4 _lhsIoptions
         _nontsOo_case = rule5 _lhsIoptions
         _nontsOo_pretty = rule6 _lhsIoptions
         _nontsOo_rename = rule7 _lhsIoptions
         _nontsOo_strictwrap = rule8 _lhsIoptions
         _nontsOo_splitsems = rule9 _lhsIoptions
         _nontsOo_data = rule10 _lhsIoptions
         _nontsOprefix = rule11 _lhsIoptions
         _nontsOo_traces = rule12 _lhsIoptions
         _nontsOo_costcentre = rule13 _lhsIoptions
         _nontsOo_linePragmas = rule14 _lhsIoptions
         _nontsOo_monadic = rule15 _lhsIoptions
         _options = rule16 _lhsIoptions arg_multivisit_
         _nontsOallPragmas = rule17 arg_pragmas_
         _nontsOparamMap = rule18 arg_paramMap_
         _nontsOcontextMap = rule19 arg_contextMap_
         _nontsOquantMap = rule20 arg_quantMap_
         _nontsOallNts = rule21 _nontsIgathNts
         _aroundMap = rule22 arg_aroundsMap_
         _mergeMap = rule23 arg_mergeMap_
         _unfoldSemDom = rule24 _nontsIsemDomUnfoldGath
         _nontsOwith_sig = rule25 _lhsIoptions
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule26  ()
         _lhsOoutput :: Program
         _lhsOoutput = rule27 _nontsIchunks arg_multivisit_
         _nontsOtypeSyns = rule28 arg_typeSyns_
         _nontsOderivings = rule29 arg_derivings_
         _nontsOwrappers = rule30 arg_wrappers_
         _nontsOaroundMap = rule31 _aroundMap
         _nontsOmergeMap = rule32 _mergeMap
         _nontsOoptions = rule33 _options
         _nontsOunfoldSemDom = rule34 _unfoldSemDom
         __result_ = T_CGrammar_vOut1 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_CGrammar_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 52 "./src-ag/GenerateCode.ag" #-}
   rule0 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 52 "./src-ag/GenerateCode.ag" #-}
                                        typeSigs       _lhsIoptions
                                        {-# LINE 350 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 53 "./src-ag/GenerateCode.ag" #-}
   rule1 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 53 "./src-ag/GenerateCode.ag" #-}
                                        folds          _lhsIoptions
                                        {-# LINE 356 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 54 "./src-ag/GenerateCode.ag" #-}
   rule2 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 54 "./src-ag/GenerateCode.ag" #-}
                                        semfuns        _lhsIoptions
                                        {-# LINE 362 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 55 "./src-ag/GenerateCode.ag" #-}
   rule3 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 55 "./src-ag/GenerateCode.ag" #-}
                                        newtypes       _lhsIoptions
                                        {-# LINE 368 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 56 "./src-ag/GenerateCode.ag" #-}
   rule4 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 56 "./src-ag/GenerateCode.ag" #-}
                                        unbox          _lhsIoptions
                                        {-# LINE 374 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 57 "./src-ag/GenerateCode.ag" #-}
   rule5 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 57 "./src-ag/GenerateCode.ag" #-}
                                        cases          _lhsIoptions
                                        {-# LINE 380 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 58 "./src-ag/GenerateCode.ag" #-}
   rule6 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 58 "./src-ag/GenerateCode.ag" #-}
                                        attrInfo       _lhsIoptions
                                        {-# LINE 386 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 59 "./src-ag/GenerateCode.ag" #-}
   rule7 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 59 "./src-ag/GenerateCode.ag" #-}
                                        rename         _lhsIoptions
                                        {-# LINE 392 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 60 "./src-ag/GenerateCode.ag" #-}
   rule8 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 60 "./src-ag/GenerateCode.ag" #-}
                                        strictWrap     _lhsIoptions
                                        {-# LINE 398 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 61 "./src-ag/GenerateCode.ag" #-}
   rule9 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 61 "./src-ag/GenerateCode.ag" #-}
                                        splitSems      _lhsIoptions
                                        {-# LINE 404 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 62 "./src-ag/GenerateCode.ag" #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 62 "./src-ag/GenerateCode.ag" #-}
                                        if dataTypes _lhsIoptions then Just (strictData _lhsIoptions) else Nothing
                                        {-# LINE 410 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 63 "./src-ag/GenerateCode.ag" #-}
   rule11 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 63 "./src-ag/GenerateCode.ag" #-}
                                        prefix         _lhsIoptions
                                        {-# LINE 416 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 64 "./src-ag/GenerateCode.ag" #-}
   rule12 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 64 "./src-ag/GenerateCode.ag" #-}
                                        genTraces      _lhsIoptions
                                        {-# LINE 422 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 65 "./src-ag/GenerateCode.ag" #-}
   rule13 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 65 "./src-ag/GenerateCode.ag" #-}
                                        genCostCentres _lhsIoptions
                                        {-# LINE 428 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 66 "./src-ag/GenerateCode.ag" #-}
   rule14 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 66 "./src-ag/GenerateCode.ag" #-}
                                        genLinePragmas _lhsIoptions
                                        {-# LINE 434 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 67 "./src-ag/GenerateCode.ag" #-}
   rule15 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 67 "./src-ag/GenerateCode.ag" #-}
                                        monadic        _lhsIoptions
                                        {-# LINE 440 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 70 "./src-ag/GenerateCode.ag" #-}
   rule16 = \ ((_lhsIoptions) :: Options) multivisit_ ->
                  {-# LINE 70 "./src-ag/GenerateCode.ag" #-}
                  _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && multivisit_ }
                  {-# LINE 446 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 75 "./src-ag/GenerateCode.ag" #-}
   rule17 = \ pragmas_ ->
                                   {-# LINE 75 "./src-ag/GenerateCode.ag" #-}
                                   pragmas_
                                   {-# LINE 452 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 97 "./src-ag/GenerateCode.ag" #-}
   rule18 = \ paramMap_ ->
                                {-# LINE 97 "./src-ag/GenerateCode.ag" #-}
                                paramMap_
                                {-# LINE 458 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 119 "./src-ag/GenerateCode.ag" #-}
   rule19 = \ contextMap_ ->
                           {-# LINE 119 "./src-ag/GenerateCode.ag" #-}
                           contextMap_
                           {-# LINE 464 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 120 "./src-ag/GenerateCode.ag" #-}
   rule20 = \ quantMap_ ->
                           {-# LINE 120 "./src-ag/GenerateCode.ag" #-}
                           quantMap_
                           {-# LINE 470 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 136 "./src-ag/GenerateCode.ag" #-}
   rule21 = \ ((_nontsIgathNts) :: Set NontermIdent) ->
                       {-# LINE 136 "./src-ag/GenerateCode.ag" #-}
                       _nontsIgathNts
                       {-# LINE 476 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 584 "./src-ag/GenerateCode.ag" #-}
   rule22 = \ aroundsMap_ ->
                                                   {-# LINE 584 "./src-ag/GenerateCode.ag" #-}
                                                   aroundsMap_
                                                   {-# LINE 482 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 600 "./src-ag/GenerateCode.ag" #-}
   rule23 = \ mergeMap_ ->
                                                  {-# LINE 600 "./src-ag/GenerateCode.ag" #-}
                                                  mergeMap_
                                                  {-# LINE 488 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 757 "./src-ag/GenerateCode.ag" #-}
   rule24 = \ ((_nontsIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
         {-# LINE 757 "./src-ag/GenerateCode.ag" #-}
         \nt nr repl ->
          let (params, tp) = Map.findWithDefault (error ("No such semantic domain: " ++ show nt)) (nt, nr) _nontsIsemDomUnfoldGath
              replMap = Map.fromList (zip params repl)
              replace k = Map.findWithDefault ('@':k) k replMap
          in evalType replace tp
         {-# LINE 498 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 857 "./src-ag/GenerateCode.ag" #-}
   rule25 = \ ((_lhsIoptions) :: Options) ->
                                {-# LINE 857 "./src-ag/GenerateCode.ag" #-}
                                typeSigs _lhsIoptions
                                {-# LINE 504 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 860 "./src-ag/GenerateCode.ag" #-}
   rule26 = \  (_ :: ()) ->
                             {-# LINE 860 "./src-ag/GenerateCode.ag" #-}
                             Seq.empty
                             {-# LINE 510 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 929 "./src-ag/GenerateCode.ag" #-}
   rule27 = \ ((_nontsIchunks) :: Chunks) multivisit_ ->
                               {-# LINE 929 "./src-ag/GenerateCode.ag" #-}
                               Program _nontsIchunks multivisit_
                               {-# LINE 516 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 997 "./src-ag/GenerateCode.ag" #-}
   rule28 = \ typeSyns_ ->
                                   {-# LINE 997 "./src-ag/GenerateCode.ag" #-}
                                   typeSyns_
                                   {-# LINE 522 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 998 "./src-ag/GenerateCode.ag" #-}
   rule29 = \ derivings_ ->
                                   {-# LINE 998 "./src-ag/GenerateCode.ag" #-}
                                   derivings_
                                   {-# LINE 528 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 999 "./src-ag/GenerateCode.ag" #-}
   rule30 = \ wrappers_ ->
                                   {-# LINE 999 "./src-ag/GenerateCode.ag" #-}
                                   wrappers_
                                   {-# LINE 534 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule31 #-}
   rule31 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule32 #-}
   rule32 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule33 #-}
   rule33 = \ _options ->
     _options
   {-# INLINE rule34 #-}
   rule34 = \ _unfoldSemDom ->
     _unfoldSemDom

-- CInterface --------------------------------------------------
-- wrapper
data Inh_CInterface  = Inh_CInterface { inh_Inh_CInterface :: (Attributes), nt_Inh_CInterface :: (NontermIdent), o_case_Inh_CInterface :: (Bool), o_cata_Inh_CInterface :: (Bool), o_costcentre_Inh_CInterface :: (Bool), o_data_Inh_CInterface :: (Maybe Bool), o_linePragmas_Inh_CInterface :: (Bool), o_monadic_Inh_CInterface :: (Bool), o_newtypes_Inh_CInterface :: (Bool), o_pretty_Inh_CInterface :: (Bool), o_rename_Inh_CInterface :: (Bool), o_sem_Inh_CInterface :: (Bool), o_sig_Inh_CInterface :: (Bool), o_splitsems_Inh_CInterface :: (Bool), o_strictwrap_Inh_CInterface :: (Bool), o_traces_Inh_CInterface :: (Bool), o_unbox_Inh_CInterface :: (Bool), options_Inh_CInterface :: (Options), paramMap_Inh_CInterface :: (ParamMap), prefix_Inh_CInterface :: (String), syn_Inh_CInterface :: (Attributes) }
data Syn_CInterface  = Syn_CInterface { comments_Syn_CInterface :: ([String]), semDom_Syn_CInterface :: ([Decl]), semDomUnfoldGath_Syn_CInterface :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CInterface :: (Decls) }
{-# INLINABLE wrap_CInterface #-}
wrap_CInterface :: T_CInterface  -> Inh_CInterface  -> (Syn_CInterface )
wrap_CInterface (T_CInterface act) (Inh_CInterface _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CInterface_vIn4 _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
        (T_CInterface_vOut4 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls) <- return (inv_CInterface_s5 sem arg)
        return (Syn_CInterface _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls)
   )

-- cata
{-# INLINE sem_CInterface #-}
sem_CInterface :: CInterface  -> T_CInterface 
sem_CInterface ( CInterface seg_ ) = sem_CInterface_CInterface ( sem_CSegments seg_ )

-- semantic domain
newtype T_CInterface  = T_CInterface {
                                     attach_T_CInterface :: Identity (T_CInterface_s5 )
                                     }
newtype T_CInterface_s5  = C_CInterface_s5 {
                                           inv_CInterface_s5 :: (T_CInterface_v4 )
                                           }
data T_CInterface_s6  = C_CInterface_s6
type T_CInterface_v4  = (T_CInterface_vIn4 ) -> (T_CInterface_vOut4 )
data T_CInterface_vIn4  = T_CInterface_vIn4 (Attributes) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CInterface_vOut4  = T_CInterface_vOut4 ([String]) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CInterface_CInterface #-}
sem_CInterface_CInterface :: T_CSegments  -> T_CInterface 
sem_CInterface_CInterface arg_seg_ = T_CInterface (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_CInterface_v4 
      v4 = \ (T_CInterface_vIn4 _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _segX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_seg_))
         (T_CSegments_vOut25 _segIcomments _segIisNil _segIsemDom _segIsemDomUnfoldGath _segIwrapDecls) = inv_CSegments_s26 _segX26 (T_CSegments_vIn25 _segOinh _segOnr _segOnt _segOo_case _segOo_cata _segOo_costcentre _segOo_data _segOo_linePragmas _segOo_monadic _segOo_newtypes _segOo_pretty _segOo_rename _segOo_sem _segOo_sig _segOo_splitsems _segOo_strictwrap _segOo_traces _segOo_unbox _segOoptions _segOparamMap _segOprefix _segOsyn)
         _segOnr = rule35  ()
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule36 _segIsemDom
         _lhsOcomments :: [String]
         _lhsOcomments = rule37 _segIcomments
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule38 _segIsemDomUnfoldGath
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule39 _segIwrapDecls
         _segOinh = rule40 _lhsIinh
         _segOnt = rule41 _lhsInt
         _segOo_case = rule42 _lhsIo_case
         _segOo_cata = rule43 _lhsIo_cata
         _segOo_costcentre = rule44 _lhsIo_costcentre
         _segOo_data = rule45 _lhsIo_data
         _segOo_linePragmas = rule46 _lhsIo_linePragmas
         _segOo_monadic = rule47 _lhsIo_monadic
         _segOo_newtypes = rule48 _lhsIo_newtypes
         _segOo_pretty = rule49 _lhsIo_pretty
         _segOo_rename = rule50 _lhsIo_rename
         _segOo_sem = rule51 _lhsIo_sem
         _segOo_sig = rule52 _lhsIo_sig
         _segOo_splitsems = rule53 _lhsIo_splitsems
         _segOo_strictwrap = rule54 _lhsIo_strictwrap
         _segOo_traces = rule55 _lhsIo_traces
         _segOo_unbox = rule56 _lhsIo_unbox
         _segOoptions = rule57 _lhsIoptions
         _segOparamMap = rule58 _lhsIparamMap
         _segOprefix = rule59 _lhsIprefix
         _segOsyn = rule60 _lhsIsyn
         __result_ = T_CInterface_vOut4 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CInterface_s5 v4
   {-# INLINE rule35 #-}
   {-# LINE 285 "./src-ag/GenerateCode.ag" #-}
   rule35 = \  (_ :: ()) ->
                           {-# LINE 285 "./src-ag/GenerateCode.ag" #-}
                           0
                           {-# LINE 625 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule36 #-}
   {-# LINE 714 "./src-ag/GenerateCode.ag" #-}
   rule36 = \ ((_segIsemDom) :: [Decl]) ->
                                {-# LINE 714 "./src-ag/GenerateCode.ag" #-}
                                Comment "semantic domain" : _segIsemDom
                                {-# LINE 631 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule37 #-}
   rule37 = \ ((_segIcomments) :: [String]) ->
     _segIcomments
   {-# INLINE rule38 #-}
   rule38 = \ ((_segIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _segIsemDomUnfoldGath
   {-# INLINE rule39 #-}
   rule39 = \ ((_segIwrapDecls) :: Decls) ->
     _segIwrapDecls
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- CNonterminal ------------------------------------------------
-- wrapper
data Inh_CNonterminal  = Inh_CNonterminal { allNts_Inh_CNonterminal :: (Set NontermIdent), allPragmas_Inh_CNonterminal :: (PragmaMap), aroundMap_Inh_CNonterminal :: (Map NontermIdent (Map ConstructorIdent (Set Identifier))), contextMap_Inh_CNonterminal :: (ContextMap), derivings_Inh_CNonterminal :: (Derivings), mergeMap_Inh_CNonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))), o_case_Inh_CNonterminal :: (Bool), o_cata_Inh_CNonterminal :: (Bool), o_costcentre_Inh_CNonterminal :: (Bool), o_data_Inh_CNonterminal :: (Maybe Bool), o_linePragmas_Inh_CNonterminal :: (Bool), o_monadic_Inh_CNonterminal :: (Bool), o_newtypes_Inh_CNonterminal :: (Bool), o_pretty_Inh_CNonterminal :: (Bool), o_rename_Inh_CNonterminal :: (Bool), o_sem_Inh_CNonterminal :: (Bool), o_sig_Inh_CNonterminal :: (Bool), o_splitsems_Inh_CNonterminal :: (Bool), o_strictwrap_Inh_CNonterminal :: (Bool), o_traces_Inh_CNonterminal :: (Bool), o_unbox_Inh_CNonterminal :: (Bool), options_Inh_CNonterminal :: (Options), paramMap_Inh_CNonterminal :: (ParamMap), prefix_Inh_CNonterminal :: (String), quantMap_Inh_CNonterminal :: (QuantMap), typeSyns_Inh_CNonterminal :: (TypeSyns), unfoldSemDom_Inh_CNonterminal :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CNonterminal :: (Bool), wrappers_Inh_CNonterminal :: (Set NontermIdent) }
data Syn_CNonterminal  = Syn_CNonterminal { chunks_Syn_CNonterminal :: (Chunks), gathNts_Syn_CNonterminal :: (Set NontermIdent), semDomUnfoldGath_Syn_CNonterminal :: (Map (NontermIdent, Int) ([String], Code.Type)) }
{-# INLINABLE wrap_CNonterminal #-}
wrap_CNonterminal :: T_CNonterminal  -> Inh_CNonterminal  -> (Syn_CNonterminal )
wrap_CNonterminal (T_CNonterminal act) (Inh_CNonterminal _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminal_vIn7 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
        (T_CNonterminal_vOut7 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath) <- return (inv_CNonterminal_s8 sem arg)
        return (Syn_CNonterminal _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath)
   )

-- cata
{-# INLINE sem_CNonterminal #-}
sem_CNonterminal :: CNonterminal  -> T_CNonterminal 
sem_CNonterminal ( CNonterminal nt_ params_ inh_ syn_ prods_ inter_ ) = sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ ( sem_CProductions prods_ ) ( sem_CInterface inter_ )

-- semantic domain
newtype T_CNonterminal  = T_CNonterminal {
                                         attach_T_CNonterminal :: Identity (T_CNonterminal_s8 )
                                         }
newtype T_CNonterminal_s8  = C_CNonterminal_s8 {
                                               inv_CNonterminal_s8 :: (T_CNonterminal_v7 )
                                               }
data T_CNonterminal_s9  = C_CNonterminal_s9
type T_CNonterminal_v7  = (T_CNonterminal_vIn7 ) -> (T_CNonterminal_vOut7 )
data T_CNonterminal_vIn7  = T_CNonterminal_vIn7 (Set NontermIdent) (PragmaMap) (Map NontermIdent (Map ConstructorIdent (Set Identifier))) (ContextMap) (Derivings) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (TypeSyns) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CNonterminal_vOut7  = T_CNonterminal_vOut7 (Chunks) (Set NontermIdent) (Map (NontermIdent, Int) ([String], Code.Type))
{-# NOINLINE sem_CNonterminal_CNonterminal #-}
sem_CNonterminal_CNonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_CProductions  -> T_CInterface  -> T_CNonterminal 
sem_CNonterminal_CNonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ arg_inter_ = T_CNonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_CNonterminal_v7 
      v7 = \ (T_CNonterminal_vIn7 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_prods_))
         _interX5 = Control.Monad.Identity.runIdentity (attach_T_CInterface (arg_inter_))
         (T_CProductions_vOut16 _prodsIcataAlts _prodsIcomments _prodsIdataAlts _prodsIdecls _prodsIsemNames) = inv_CProductions_s17 _prodsX17 (T_CProductions_vIn16 _prodsOallNts _prodsOallPragmas _prodsOaroundMap _prodsOcontextMap _prodsOinh _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_costcentre _prodsOo_data _prodsOo_linePragmas _prodsOo_monadic _prodsOo_newtypes _prodsOo_pretty _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_splitsems _prodsOo_strictwrap _prodsOo_traces _prodsOo_unbox _prodsOoptions _prodsOparamMap _prodsOprefix _prodsOquantMap _prodsOsyn _prodsOunfoldSemDom _prodsOwith_sig _prodsOwrappers)
         (T_CInterface_vOut4 _interIcomments _interIsemDom _interIsemDomUnfoldGath _interIwrapDecls) = inv_CInterface_s5 _interX5 (T_CInterface_vIn4 _interOinh _interOnt _interOo_case _interOo_cata _interOo_costcentre _interOo_data _interOo_linePragmas _interOo_monadic _interOo_newtypes _interOo_pretty _interOo_rename _interOo_sem _interOo_sig _interOo_splitsems _interOo_strictwrap _interOo_traces _interOo_unbox _interOoptions _interOparamMap _interOprefix _interOsyn)
         (_interOinh,_interOsyn,_interOnt) = rule61 arg_inh_ arg_nt_ arg_syn_
         (_prodsOinh,_prodsOsyn,_prodsOnt) = rule62 arg_inh_ arg_nt_ arg_syn_
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule63 arg_nt_
         _aroundMap = rule64 _lhsIaroundMap arg_nt_
         _mergeMap = rule65 _lhsImergeMap arg_nt_
         _semWrapper = rule66 _interIwrapDecls _lhsIo_newtypes _lhsIo_strictwrap arg_inh_ arg_nt_ arg_params_ arg_syn_
         _comment = rule67 _interIcomments _prodsIcomments
         _lhsOchunks :: Chunks
         _lhsOchunks = rule68 _cataFun _comment _dataDef _genCata _interIsemDom _lhsIo_cata _lhsIo_data _lhsIo_pretty _lhsIo_sem _lhsIo_sig _lhsIwrappers _prodsIdecls _prodsIsemNames _semWrapper arg_nt_
         _dataDef = rule69 _lhsIderivings _lhsIo_data _lhsItypeSyns _prodsIdataAlts arg_nt_ arg_params_
         _genCata = rule70 _lhsIoptions arg_nt_
         _cataFun = rule71 _lhsIcontextMap _lhsIo_sig _lhsIprefix _lhsIquantMap _lhsItypeSyns _prodsIcataAlts arg_nt_ arg_params_
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule72 _interIsemDomUnfoldGath
         _prodsOallNts = rule73 _lhsIallNts
         _prodsOallPragmas = rule74 _lhsIallPragmas
         _prodsOaroundMap = rule75 _aroundMap
         _prodsOcontextMap = rule76 _lhsIcontextMap
         _prodsOmergeMap = rule77 _mergeMap
         _prodsOo_case = rule78 _lhsIo_case
         _prodsOo_cata = rule79 _lhsIo_cata
         _prodsOo_costcentre = rule80 _lhsIo_costcentre
         _prodsOo_data = rule81 _lhsIo_data
         _prodsOo_linePragmas = rule82 _lhsIo_linePragmas
         _prodsOo_monadic = rule83 _lhsIo_monadic
         _prodsOo_newtypes = rule84 _lhsIo_newtypes
         _prodsOo_pretty = rule85 _lhsIo_pretty
         _prodsOo_rename = rule86 _lhsIo_rename
         _prodsOo_sem = rule87 _lhsIo_sem
         _prodsOo_sig = rule88 _lhsIo_sig
         _prodsOo_splitsems = rule89 _lhsIo_splitsems
         _prodsOo_strictwrap = rule90 _lhsIo_strictwrap
         _prodsOo_traces = rule91 _lhsIo_traces
         _prodsOo_unbox = rule92 _lhsIo_unbox
         _prodsOoptions = rule93 _lhsIoptions
         _prodsOparamMap = rule94 _lhsIparamMap
         _prodsOprefix = rule95 _lhsIprefix
         _prodsOquantMap = rule96 _lhsIquantMap
         _prodsOunfoldSemDom = rule97 _lhsIunfoldSemDom
         _prodsOwith_sig = rule98 _lhsIwith_sig
         _prodsOwrappers = rule99 _lhsIwrappers
         _interOo_case = rule100 _lhsIo_case
         _interOo_cata = rule101 _lhsIo_cata
         _interOo_costcentre = rule102 _lhsIo_costcentre
         _interOo_data = rule103 _lhsIo_data
         _interOo_linePragmas = rule104 _lhsIo_linePragmas
         _interOo_monadic = rule105 _lhsIo_monadic
         _interOo_newtypes = rule106 _lhsIo_newtypes
         _interOo_pretty = rule107 _lhsIo_pretty
         _interOo_rename = rule108 _lhsIo_rename
         _interOo_sem = rule109 _lhsIo_sem
         _interOo_sig = rule110 _lhsIo_sig
         _interOo_splitsems = rule111 _lhsIo_splitsems
         _interOo_strictwrap = rule112 _lhsIo_strictwrap
         _interOo_traces = rule113 _lhsIo_traces
         _interOo_unbox = rule114 _lhsIo_unbox
         _interOoptions = rule115 _lhsIoptions
         _interOparamMap = rule116 _lhsIparamMap
         _interOprefix = rule117 _lhsIprefix
         __result_ = T_CNonterminal_vOut7 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminal_s8 v7
   {-# INLINE rule61 #-}
   {-# LINE 85 "./src-ag/GenerateCode.ag" #-}
   rule61 = \ inh_ nt_ syn_ ->
                                          {-# LINE 85 "./src-ag/GenerateCode.ag" #-}
                                          (inh_,syn_,nt_)
                                          {-# LINE 814 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 86 "./src-ag/GenerateCode.ag" #-}
   rule62 = \ inh_ nt_ syn_ ->
                                         {-# LINE 86 "./src-ag/GenerateCode.ag" #-}
                                         (inh_,syn_,nt_)
                                         {-# LINE 820 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 142 "./src-ag/GenerateCode.ag" #-}
   rule63 = \ nt_ ->
                      {-# LINE 142 "./src-ag/GenerateCode.ag" #-}
                      Set.singleton nt_
                      {-# LINE 826 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 585 "./src-ag/GenerateCode.ag" #-}
   rule64 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) nt_ ->
                                                   {-# LINE 585 "./src-ag/GenerateCode.ag" #-}
                                                   Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                   {-# LINE 832 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule65 #-}
   {-# LINE 601 "./src-ag/GenerateCode.ag" #-}
   rule65 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) nt_ ->
                                                  {-# LINE 601 "./src-ag/GenerateCode.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                  {-# LINE 838 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 806 "./src-ag/GenerateCode.ag" #-}
   rule66 = \ ((_interIwrapDecls) :: Decls) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_strictwrap) :: Bool) inh_ nt_ params_ syn_ ->
                                    {-# LINE 806 "./src-ag/GenerateCode.ag" #-}
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
                                        mkdata n attrs = Data n params' [Record n [mkstrict (getName f++"_"++n) $ evalTp $ typeToCodeType (Just nt_) params' t | (f,t) <- attrs]] False []
                                        datas = [mkdata inhNT inhAttrs, mkdata synNT synAttrs]
                                    in datas ++ [ typeSig
                                                , Decl (Fun wrapNT [varPat, App inhNT inhVars])
                                                      (Let _interIwrapDecls (App synNT synVars))
                                                      Set.empty Set.empty
                                                ]
                                    {-# LINE 867 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 867 "./src-ag/GenerateCode.ag" #-}
   rule67 = \ ((_interIcomments) :: [String]) ((_prodsIcomments) :: [String]) ->
                                 {-# LINE 867 "./src-ag/GenerateCode.ag" #-}
                                 Comment . unlines . map ind $ ( _interIcomments ++ ("alternatives:" : map ind _prodsIcomments) )
                                 {-# LINE 873 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 932 "./src-ag/GenerateCode.ag" #-}
   rule68 = \ _cataFun _comment _dataDef _genCata ((_interIsemDom) :: [Decl]) ((_lhsIo_cata) :: Bool) ((_lhsIo_data) :: Maybe Bool) ((_lhsIo_pretty) :: Bool) ((_lhsIo_sem) :: Bool) ((_lhsIo_sig) :: Bool) ((_lhsIwrappers) :: Set NontermIdent) ((_prodsIdecls) :: Decls) ((_prodsIsemNames) :: [String]) _semWrapper nt_ ->
                                 {-# LINE 932 "./src-ag/GenerateCode.ag" #-}
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
                                 {-# LINE 888 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 1002 "./src-ag/GenerateCode.ag" #-}
   rule69 = \ ((_lhsIderivings) :: Derivings) ((_lhsIo_data) :: Maybe Bool) ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdataAlts) :: DataAlts) nt_ params_ ->
                                 {-# LINE 1002 "./src-ag/GenerateCode.ag" #-}
                                 let params' = map getName params_
                                     typeSyn tp = let theType =
                                                        case tp of
                                                          CommonTypes.Maybe t      -> TMaybe $ typeToCodeType (Just nt_) params' t
                                                          CommonTypes.Either t1 t2 -> TEither (typeToCodeType (Just nt_) params' t1) (typeToCodeType (Just nt_) params' t2)
                                                          CommonTypes.Map t1 t2    -> TMap (typeToCodeType (Just nt_) params' t1) (typeToCodeType (Just nt_) params' t2)
                                                          CommonTypes.IntMap t     -> TIntMap $ typeToCodeType (Just nt_) params' t
                                                          CommonTypes.List t       -> Code.List $ typeToCodeType (Just nt_) params' t
                                                          CommonTypes.Tuple ts     -> Code.TupleType [typeToCodeType (Just nt_) params' t | (_,t) <- ts ]
                                                          tp'                      -> error $ show tp' ++ " not supported"
                                                   in Code.Type (getName nt_) params' (idEvalType theType)
                                     derivings  = maybe [] (map getName . Set.toList) (Map.lookup nt_ _lhsIderivings)
                                     dataDef    = Data (getName nt_) (map getName params_) _prodsIdataAlts (maybe False id _lhsIo_data) derivings
                                 in maybe dataDef typeSyn $ lookup nt_ _lhsItypeSyns
                                 {-# LINE 907 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 1045 "./src-ag/GenerateCode.ag" #-}
   rule70 = \ ((_lhsIoptions) :: Options) nt_ ->
                                 {-# LINE 1045 "./src-ag/GenerateCode.ag" #-}
                                 not (nt_ `Set.member` nocatas _lhsIoptions)
                                 {-# LINE 913 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 1046 "./src-ag/GenerateCode.ag" #-}
   rule71 = \ ((_lhsIcontextMap) :: ContextMap) ((_lhsIo_sig) :: Bool) ((_lhsIprefix) :: String) ((_lhsIquantMap) :: QuantMap) ((_lhsItypeSyns) :: TypeSyns) ((_prodsIcataAlts) :: Decls) nt_ params_ ->
                                 {-# LINE 1046 "./src-ag/GenerateCode.ag" #-}
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
                                                   _ -> error "TODO"
                                 in  Comment "cata" :
                                     (if _lhsIo_sig then [tSig] else []) ++
                                     maybe _prodsIcataAlts special (lookup nt_ _lhsItypeSyns)
                                 {-# LINE 1000 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule72 #-}
   rule72 = \ ((_interIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _interIsemDomUnfoldGath
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule74 #-}
   rule74 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule75 #-}
   rule75 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule77 #-}
   rule77 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule79 #-}
   rule79 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule81 #-}
   rule81 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule82 #-}
   rule82 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule87 #-}
   rule87 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule89 #-}
   rule89 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix

-- CNonterminals -----------------------------------------------
-- wrapper
data Inh_CNonterminals  = Inh_CNonterminals { allNts_Inh_CNonterminals :: (Set NontermIdent), allPragmas_Inh_CNonterminals :: (PragmaMap), aroundMap_Inh_CNonterminals :: (Map NontermIdent (Map ConstructorIdent (Set Identifier))), contextMap_Inh_CNonterminals :: (ContextMap), derivings_Inh_CNonterminals :: (Derivings), mergeMap_Inh_CNonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))), o_case_Inh_CNonterminals :: (Bool), o_cata_Inh_CNonterminals :: (Bool), o_costcentre_Inh_CNonterminals :: (Bool), o_data_Inh_CNonterminals :: (Maybe Bool), o_linePragmas_Inh_CNonterminals :: (Bool), o_monadic_Inh_CNonterminals :: (Bool), o_newtypes_Inh_CNonterminals :: (Bool), o_pretty_Inh_CNonterminals :: (Bool), o_rename_Inh_CNonterminals :: (Bool), o_sem_Inh_CNonterminals :: (Bool), o_sig_Inh_CNonterminals :: (Bool), o_splitsems_Inh_CNonterminals :: (Bool), o_strictwrap_Inh_CNonterminals :: (Bool), o_traces_Inh_CNonterminals :: (Bool), o_unbox_Inh_CNonterminals :: (Bool), options_Inh_CNonterminals :: (Options), paramMap_Inh_CNonterminals :: (ParamMap), prefix_Inh_CNonterminals :: (String), quantMap_Inh_CNonterminals :: (QuantMap), typeSyns_Inh_CNonterminals :: (TypeSyns), unfoldSemDom_Inh_CNonterminals :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CNonterminals :: (Bool), wrappers_Inh_CNonterminals :: (Set NontermIdent) }
data Syn_CNonterminals  = Syn_CNonterminals { chunks_Syn_CNonterminals :: (Chunks), gathNts_Syn_CNonterminals :: (Set NontermIdent), semDomUnfoldGath_Syn_CNonterminals :: (Map (NontermIdent, Int) ([String], Code.Type)) }
{-# INLINABLE wrap_CNonterminals #-}
wrap_CNonterminals :: T_CNonterminals  -> Inh_CNonterminals  -> (Syn_CNonterminals )
wrap_CNonterminals (T_CNonterminals act) (Inh_CNonterminals _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
        (T_CNonterminals_vOut10 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath) <- return (inv_CNonterminals_s11 sem arg)
        return (Syn_CNonterminals _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath)
   )

-- cata
{-# NOINLINE sem_CNonterminals #-}
sem_CNonterminals :: CNonterminals  -> T_CNonterminals 
sem_CNonterminals list = Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list)

-- semantic domain
newtype T_CNonterminals  = T_CNonterminals {
                                           attach_T_CNonterminals :: Identity (T_CNonterminals_s11 )
                                           }
newtype T_CNonterminals_s11  = C_CNonterminals_s11 {
                                                   inv_CNonterminals_s11 :: (T_CNonterminals_v10 )
                                                   }
data T_CNonterminals_s12  = C_CNonterminals_s12
type T_CNonterminals_v10  = (T_CNonterminals_vIn10 ) -> (T_CNonterminals_vOut10 )
data T_CNonterminals_vIn10  = T_CNonterminals_vIn10 (Set NontermIdent) (PragmaMap) (Map NontermIdent (Map ConstructorIdent (Set Identifier))) (ContextMap) (Derivings) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (TypeSyns) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CNonterminals_vOut10  = T_CNonterminals_vOut10 (Chunks) (Set NontermIdent) (Map (NontermIdent, Int) ([String], Code.Type))
{-# NOINLINE sem_CNonterminals_Cons #-}
sem_CNonterminals_Cons :: T_CNonterminal  -> T_CNonterminals  -> T_CNonterminals 
sem_CNonterminals_Cons arg_hd_ arg_tl_ = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_CNonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_tl_))
         (T_CNonterminal_vOut7 _hdIchunks _hdIgathNts _hdIsemDomUnfoldGath) = inv_CNonterminal_s8 _hdX8 (T_CNonterminal_vIn7 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOderivings _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOtypeSyns _hdOunfoldSemDom _hdOwith_sig _hdOwrappers)
         (T_CNonterminals_vOut10 _tlIchunks _tlIgathNts _tlIsemDomUnfoldGath) = inv_CNonterminals_s11 _tlX11 (T_CNonterminals_vIn10 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOderivings _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOtypeSyns _tlOunfoldSemDom _tlOwith_sig _tlOwrappers)
         _lhsOchunks :: Chunks
         _lhsOchunks = rule118 _hdIchunks _tlIchunks
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule119 _hdIgathNts _tlIgathNts
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule120 _hdIsemDomUnfoldGath _tlIsemDomUnfoldGath
         _hdOallNts = rule121 _lhsIallNts
         _hdOallPragmas = rule122 _lhsIallPragmas
         _hdOaroundMap = rule123 _lhsIaroundMap
         _hdOcontextMap = rule124 _lhsIcontextMap
         _hdOderivings = rule125 _lhsIderivings
         _hdOmergeMap = rule126 _lhsImergeMap
         _hdOo_case = rule127 _lhsIo_case
         _hdOo_cata = rule128 _lhsIo_cata
         _hdOo_costcentre = rule129 _lhsIo_costcentre
         _hdOo_data = rule130 _lhsIo_data
         _hdOo_linePragmas = rule131 _lhsIo_linePragmas
         _hdOo_monadic = rule132 _lhsIo_monadic
         _hdOo_newtypes = rule133 _lhsIo_newtypes
         _hdOo_pretty = rule134 _lhsIo_pretty
         _hdOo_rename = rule135 _lhsIo_rename
         _hdOo_sem = rule136 _lhsIo_sem
         _hdOo_sig = rule137 _lhsIo_sig
         _hdOo_splitsems = rule138 _lhsIo_splitsems
         _hdOo_strictwrap = rule139 _lhsIo_strictwrap
         _hdOo_traces = rule140 _lhsIo_traces
         _hdOo_unbox = rule141 _lhsIo_unbox
         _hdOoptions = rule142 _lhsIoptions
         _hdOparamMap = rule143 _lhsIparamMap
         _hdOprefix = rule144 _lhsIprefix
         _hdOquantMap = rule145 _lhsIquantMap
         _hdOtypeSyns = rule146 _lhsItypeSyns
         _hdOunfoldSemDom = rule147 _lhsIunfoldSemDom
         _hdOwith_sig = rule148 _lhsIwith_sig
         _hdOwrappers = rule149 _lhsIwrappers
         _tlOallNts = rule150 _lhsIallNts
         _tlOallPragmas = rule151 _lhsIallPragmas
         _tlOaroundMap = rule152 _lhsIaroundMap
         _tlOcontextMap = rule153 _lhsIcontextMap
         _tlOderivings = rule154 _lhsIderivings
         _tlOmergeMap = rule155 _lhsImergeMap
         _tlOo_case = rule156 _lhsIo_case
         _tlOo_cata = rule157 _lhsIo_cata
         _tlOo_costcentre = rule158 _lhsIo_costcentre
         _tlOo_data = rule159 _lhsIo_data
         _tlOo_linePragmas = rule160 _lhsIo_linePragmas
         _tlOo_monadic = rule161 _lhsIo_monadic
         _tlOo_newtypes = rule162 _lhsIo_newtypes
         _tlOo_pretty = rule163 _lhsIo_pretty
         _tlOo_rename = rule164 _lhsIo_rename
         _tlOo_sem = rule165 _lhsIo_sem
         _tlOo_sig = rule166 _lhsIo_sig
         _tlOo_splitsems = rule167 _lhsIo_splitsems
         _tlOo_strictwrap = rule168 _lhsIo_strictwrap
         _tlOo_traces = rule169 _lhsIo_traces
         _tlOo_unbox = rule170 _lhsIo_unbox
         _tlOoptions = rule171 _lhsIoptions
         _tlOparamMap = rule172 _lhsIparamMap
         _tlOprefix = rule173 _lhsIprefix
         _tlOquantMap = rule174 _lhsIquantMap
         _tlOtypeSyns = rule175 _lhsItypeSyns
         _tlOunfoldSemDom = rule176 _lhsIunfoldSemDom
         _tlOwith_sig = rule177 _lhsIwith_sig
         _tlOwrappers = rule178 _lhsIwrappers
         __result_ = T_CNonterminals_vOut10 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule118 #-}
   rule118 = \ ((_hdIchunks) :: Chunks) ((_tlIchunks) :: Chunks) ->
     _hdIchunks ++ _tlIchunks
   {-# INLINE rule119 #-}
   rule119 = \ ((_hdIgathNts) :: Set NontermIdent) ((_tlIgathNts) :: Set NontermIdent) ->
     _hdIgathNts `Set.union` _tlIgathNts
   {-# INLINE rule120 #-}
   rule120 = \ ((_hdIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ((_tlIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
     _lhsIaroundMap
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
     _lhsIaroundMap
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CNonterminals_Nil #-}
sem_CNonterminals_Nil ::  T_CNonterminals 
sem_CNonterminals_Nil  = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOchunks :: Chunks
         _lhsOchunks = rule179  ()
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule180  ()
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule181  ()
         __result_ = T_CNonterminals_vOut10 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule179 #-}
   rule179 = \  (_ :: ()) ->
     []
   {-# INLINE rule180 #-}
   rule180 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule181 #-}
   rule181 = \  (_ :: ()) ->
     Map.empty

-- CProduction -------------------------------------------------
-- wrapper
data Inh_CProduction  = Inh_CProduction { allNts_Inh_CProduction :: (Set NontermIdent), allPragmas_Inh_CProduction :: (PragmaMap), aroundMap_Inh_CProduction :: (Map ConstructorIdent (Set Identifier)), contextMap_Inh_CProduction :: (ContextMap), inh_Inh_CProduction :: (Attributes), mergeMap_Inh_CProduction :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))), nt_Inh_CProduction :: (NontermIdent), o_case_Inh_CProduction :: (Bool), o_cata_Inh_CProduction :: (Bool), o_costcentre_Inh_CProduction :: (Bool), o_data_Inh_CProduction :: (Maybe Bool), o_linePragmas_Inh_CProduction :: (Bool), o_monadic_Inh_CProduction :: (Bool), o_newtypes_Inh_CProduction :: (Bool), o_pretty_Inh_CProduction :: (Bool), o_rename_Inh_CProduction :: (Bool), o_sem_Inh_CProduction :: (Bool), o_sig_Inh_CProduction :: (Bool), o_splitsems_Inh_CProduction :: (Bool), o_strictwrap_Inh_CProduction :: (Bool), o_traces_Inh_CProduction :: (Bool), o_unbox_Inh_CProduction :: (Bool), options_Inh_CProduction :: (Options), paramMap_Inh_CProduction :: (ParamMap), prefix_Inh_CProduction :: (String), quantMap_Inh_CProduction :: (QuantMap), syn_Inh_CProduction :: (Attributes), unfoldSemDom_Inh_CProduction :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CProduction :: (Bool), wrappers_Inh_CProduction :: (Set NontermIdent) }
data Syn_CProduction  = Syn_CProduction { cataAlt_Syn_CProduction :: (Decl), comments_Syn_CProduction :: ([String]), dataAlt_Syn_CProduction :: (DataAlt), decls_Syn_CProduction :: (Decls), semNames_Syn_CProduction :: ([String]) }
{-# INLINABLE wrap_CProduction #-}
wrap_CProduction :: T_CProduction  -> Inh_CProduction  -> (Syn_CProduction )
wrap_CProduction (T_CProduction act) (Inh_CProduction _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProduction_vIn13 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
        (T_CProduction_vOut13 _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOsemNames) <- return (inv_CProduction_s14 sem arg)
        return (Syn_CProduction _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOsemNames)
   )

-- cata
{-# INLINE sem_CProduction #-}
sem_CProduction :: CProduction  -> T_CProduction 
sem_CProduction ( CProduction con_ visits_ children_ terminals_ ) = sem_CProduction_CProduction con_ ( sem_CVisits visits_ ) children_ terminals_

-- semantic domain
newtype T_CProduction  = T_CProduction {
                                       attach_T_CProduction :: Identity (T_CProduction_s14 )
                                       }
newtype T_CProduction_s14  = C_CProduction_s14 {
                                               inv_CProduction_s14 :: (T_CProduction_v13 )
                                               }
data T_CProduction_s15  = C_CProduction_s15
type T_CProduction_v13  = (T_CProduction_vIn13 ) -> (T_CProduction_vOut13 )
data T_CProduction_vIn13  = T_CProduction_vIn13 (Set NontermIdent) (PragmaMap) (Map ConstructorIdent (Set Identifier)) (ContextMap) (Attributes) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (Attributes) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CProduction_vOut13  = T_CProduction_vOut13 (Decl) ([String]) (DataAlt) (Decls) ([String])
{-# NOINLINE sem_CProduction_CProduction #-}
sem_CProduction_CProduction :: (ConstructorIdent) -> T_CVisits  -> ([(Identifier,Type,ChildKind)]) -> ([Identifier]) -> T_CProduction 
sem_CProduction_CProduction arg_con_ arg_visits_ arg_children_ arg_terminals_ = T_CProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_CProduction_v13 
      v13 = \ (T_CProduction_vIn13 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _visitsX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_visits_))
         (T_CVisits_vOut31 _visitsIcomments _visitsIdecls _visitsIgatherInstVisitNrs _visitsIintra _visitsIintraVars _visitsIisNil _visitsIsemNames _visitsIvisitedSet) = inv_CVisits_s32 _visitsX32 (T_CVisits_vIn31 _visitsOallNts _visitsOallPragmas _visitsOaroundMap _visitsOchildren _visitsOcon _visitsOcontextMap _visitsOinh _visitsOinstVisitNrs _visitsOmergeMap _visitsOnr _visitsOnt _visitsOo_case _visitsOo_cata _visitsOo_costcentre _visitsOo_data _visitsOo_linePragmas _visitsOo_monadic _visitsOo_newtypes _visitsOo_pretty _visitsOo_rename _visitsOo_sem _visitsOo_sig _visitsOo_splitsems _visitsOo_strictwrap _visitsOo_traces _visitsOo_unbox _visitsOoptions _visitsOparamInstMap _visitsOparamMap _visitsOprefix _visitsOquantMap _visitsOsyn _visitsOterminals _visitsOunfoldSemDom _visitsOvisitedSet _visitsOwith_sig _visitsOwrappers)
         _visitsOcon = rule182 arg_con_
         _visitsOterminals = rule183 arg_terminals_
         _paramInstMap = rule184 arg_children_
         _visitsOvisitedSet = rule185  ()
         _visitsOnr = rule186  ()
         _visitsOchildren = rule187 arg_children_
         _visitsOinstVisitNrs = rule188 _visitsIgatherInstVisitNrs
         _aroundMap = rule189 _lhsIaroundMap arg_con_
         _mergeMap = rule190 _lhsImergeMap arg_con_
         _firstOrderChildren = rule191 arg_children_
         _lhsOcomments :: [String]
         _lhsOcomments = rule192 _firstOrderChildren _visitsIcomments arg_con_
         _params = rule193 _lhsInt _lhsIparamMap
         _lhsOdataAlt :: DataAlt
         _lhsOdataAlt = rule194 _firstOrderChildren _lhsInt _lhsIo_rename _lhsIoptions _params arg_con_
         _lhsOcataAlt :: Decl
         _lhsOcataAlt = rule195 _firstOrderChildren _lhsInt _lhsIo_rename _lhsIprefix arg_con_
         _lhsOdecls :: Decls
         _lhsOdecls = rule196 _visitsIdecls
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule197 _visitsIsemNames
         _visitsOallNts = rule198 _lhsIallNts
         _visitsOallPragmas = rule199 _lhsIallPragmas
         _visitsOaroundMap = rule200 _aroundMap
         _visitsOcontextMap = rule201 _lhsIcontextMap
         _visitsOinh = rule202 _lhsIinh
         _visitsOmergeMap = rule203 _mergeMap
         _visitsOnt = rule204 _lhsInt
         _visitsOo_case = rule205 _lhsIo_case
         _visitsOo_cata = rule206 _lhsIo_cata
         _visitsOo_costcentre = rule207 _lhsIo_costcentre
         _visitsOo_data = rule208 _lhsIo_data
         _visitsOo_linePragmas = rule209 _lhsIo_linePragmas
         _visitsOo_monadic = rule210 _lhsIo_monadic
         _visitsOo_newtypes = rule211 _lhsIo_newtypes
         _visitsOo_pretty = rule212 _lhsIo_pretty
         _visitsOo_rename = rule213 _lhsIo_rename
         _visitsOo_sem = rule214 _lhsIo_sem
         _visitsOo_sig = rule215 _lhsIo_sig
         _visitsOo_splitsems = rule216 _lhsIo_splitsems
         _visitsOo_strictwrap = rule217 _lhsIo_strictwrap
         _visitsOo_traces = rule218 _lhsIo_traces
         _visitsOo_unbox = rule219 _lhsIo_unbox
         _visitsOoptions = rule220 _lhsIoptions
         _visitsOparamInstMap = rule221 _paramInstMap
         _visitsOparamMap = rule222 _lhsIparamMap
         _visitsOprefix = rule223 _lhsIprefix
         _visitsOquantMap = rule224 _lhsIquantMap
         _visitsOsyn = rule225 _lhsIsyn
         _visitsOunfoldSemDom = rule226 _lhsIunfoldSemDom
         _visitsOwith_sig = rule227 _lhsIwith_sig
         _visitsOwrappers = rule228 _lhsIwrappers
         __result_ = T_CProduction_vOut13 _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProduction_s14 v13
   {-# INLINE rule182 #-}
   {-# LINE 91 "./src-ag/GenerateCode.ag" #-}
   rule182 = \ con_ ->
                                 {-# LINE 91 "./src-ag/GenerateCode.ag" #-}
                                 con_
                                 {-# LINE 1556 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule183 #-}
   {-# LINE 92 "./src-ag/GenerateCode.ag" #-}
   rule183 = \ terminals_ ->
                                        {-# LINE 92 "./src-ag/GenerateCode.ag" #-}
                                        terminals_
                                        {-# LINE 1562 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule184 #-}
   {-# LINE 104 "./src-ag/GenerateCode.ag" #-}
   rule184 = \ children_ ->
                           {-# LINE 104 "./src-ag/GenerateCode.ag" #-}
                           Map.fromList [(nm, (extractNonterminal tp, tps)) | (nm,tp,_) <- children_, let tps = map cleanupArg $ nontermArgs tp, not (null tps) ]
                           {-# LINE 1568 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule185 #-}
   {-# LINE 146 "./src-ag/GenerateCode.ag" #-}
   rule185 = \  (_ :: ()) ->
                                                     {-# LINE 146 "./src-ag/GenerateCode.ag" #-}
                                                     Set.empty
                                                     {-# LINE 1574 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule186 #-}
   {-# LINE 281 "./src-ag/GenerateCode.ag" #-}
   rule186 = \  (_ :: ()) ->
                               {-# LINE 281 "./src-ag/GenerateCode.ag" #-}
                               0
                               {-# LINE 1580 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule187 #-}
   {-# LINE 413 "./src-ag/GenerateCode.ag" #-}
   rule187 = \ children_ ->
                                     {-# LINE 413 "./src-ag/GenerateCode.ag" #-}
                                     children_
                                     {-# LINE 1586 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule188 #-}
   {-# LINE 566 "./src-ag/GenerateCode.ag" #-}
   rule188 = \ ((_visitsIgatherInstVisitNrs) :: Map Identifier Int) ->
                              {-# LINE 566 "./src-ag/GenerateCode.ag" #-}
                              _visitsIgatherInstVisitNrs
                              {-# LINE 1592 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule189 #-}
   {-# LINE 586 "./src-ag/GenerateCode.ag" #-}
   rule189 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) con_ ->
                                                   {-# LINE 586 "./src-ag/GenerateCode.ag" #-}
                                                   Map.findWithDefault Set.empty con_ _lhsIaroundMap
                                                   {-# LINE 1598 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule190 #-}
   {-# LINE 602 "./src-ag/GenerateCode.ag" #-}
   rule190 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) con_ ->
                                                  {-# LINE 602 "./src-ag/GenerateCode.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                  {-# LINE 1604 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule191 #-}
   {-# LINE 882 "./src-ag/GenerateCode.ag" #-}
   rule191 = \ children_ ->
                                            {-# LINE 882 "./src-ag/GenerateCode.ag" #-}
                                            [ (nm,fromJust mb,virt) | (nm,tp,virt) <- children_, let mb = isFirstOrder virt tp, isJust mb ]
                                            {-# LINE 1610 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule192 #-}
   {-# LINE 883 "./src-ag/GenerateCode.ag" #-}
   rule192 = \ _firstOrderChildren ((_visitsIcomments) :: [String]) con_ ->
                                   {-# LINE 883 "./src-ag/GenerateCode.ag" #-}
                                   ("alternative " ++ getName con_ ++ ":")
                                   : map ind (  map (\(x,y,_) -> makeLocalComment 14 "child" x (Just y)) _firstOrderChildren
                                             ++ _visitsIcomments
                                             )
                                   {-# LINE 1619 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule193 #-}
   {-# LINE 1025 "./src-ag/GenerateCode.ag" #-}
   rule193 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                                {-# LINE 1025 "./src-ag/GenerateCode.ag" #-}
                                map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                                {-# LINE 1625 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule194 #-}
   {-# LINE 1026 "./src-ag/GenerateCode.ag" #-}
   rule194 = \ _firstOrderChildren ((_lhsInt) :: NontermIdent) ((_lhsIo_rename) :: Bool) ((_lhsIoptions) :: Options) _params con_ ->
                                {-# LINE 1026 "./src-ag/GenerateCode.ag" #-}
                                let conNm = conname _lhsIo_rename _lhsInt con_
                                    mkFields :: (NontermIdent -> ConstructorIdent -> Identifier -> Code.Type -> a) -> [a]
                                    mkFields f = map (\(nm,t,_) -> f _lhsInt con_ nm (typeToCodeType (Just _lhsInt) _params     $ removeDeforested t)) _firstOrderChildren
                                in if dataRecords _lhsIoptions
                                   then Record conNm $ mkFields $ toNamedType (strictData _lhsIoptions)
                                   else DataAlt conNm $ mkFields $ \_ _ _ t -> t
                                {-# LINE 1636 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule195 #-}
   {-# LINE 1139 "./src-ag/GenerateCode.ag" #-}
   rule195 = \ _firstOrderChildren ((_lhsInt) :: NontermIdent) ((_lhsIo_rename) :: Bool) ((_lhsIprefix) :: String) con_ ->
                                {-# LINE 1139 "./src-ag/GenerateCode.ag" #-}
                                let lhs = Fun (cataname _lhsIprefix _lhsInt) [lhs_pat]
                                    lhs_pat = App (conname _lhsIo_rename _lhsInt con_)
                                                   (map (\(n,_,_) -> SimpleExpr $ locname $ n) _firstOrderChildren    )
                                    rhs = App (semname _lhsIprefix _lhsInt con_)
                                               (map argument _firstOrderChildren    )
                                    argument (nm,NT tp _ _,_) = App (cataname _lhsIprefix tp)
                                                                     [SimpleExpr (locname nm)]
                                    argument (nm, _,_)    = SimpleExpr (locname nm)
                                 in Decl lhs rhs Set.empty Set.empty
                                {-# LINE 1650 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule196 #-}
   rule196 = \ ((_visitsIdecls) :: Decls) ->
     _visitsIdecls
   {-# INLINE rule197 #-}
   rule197 = \ ((_visitsIsemNames) :: [String]) ->
     _visitsIsemNames
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule200 #-}
   rule200 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule203 #-}
   rule203 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule209 #-}
   rule209 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule221 #-}
   rule221 = \ _paramInstMap ->
     _paramInstMap
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule228 #-}
   rule228 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers

-- CProductions ------------------------------------------------
-- wrapper
data Inh_CProductions  = Inh_CProductions { allNts_Inh_CProductions :: (Set NontermIdent), allPragmas_Inh_CProductions :: (PragmaMap), aroundMap_Inh_CProductions :: (Map ConstructorIdent (Set Identifier)), contextMap_Inh_CProductions :: (ContextMap), inh_Inh_CProductions :: (Attributes), mergeMap_Inh_CProductions :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))), nt_Inh_CProductions :: (NontermIdent), o_case_Inh_CProductions :: (Bool), o_cata_Inh_CProductions :: (Bool), o_costcentre_Inh_CProductions :: (Bool), o_data_Inh_CProductions :: (Maybe Bool), o_linePragmas_Inh_CProductions :: (Bool), o_monadic_Inh_CProductions :: (Bool), o_newtypes_Inh_CProductions :: (Bool), o_pretty_Inh_CProductions :: (Bool), o_rename_Inh_CProductions :: (Bool), o_sem_Inh_CProductions :: (Bool), o_sig_Inh_CProductions :: (Bool), o_splitsems_Inh_CProductions :: (Bool), o_strictwrap_Inh_CProductions :: (Bool), o_traces_Inh_CProductions :: (Bool), o_unbox_Inh_CProductions :: (Bool), options_Inh_CProductions :: (Options), paramMap_Inh_CProductions :: (ParamMap), prefix_Inh_CProductions :: (String), quantMap_Inh_CProductions :: (QuantMap), syn_Inh_CProductions :: (Attributes), unfoldSemDom_Inh_CProductions :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CProductions :: (Bool), wrappers_Inh_CProductions :: (Set NontermIdent) }
data Syn_CProductions  = Syn_CProductions { cataAlts_Syn_CProductions :: (Decls), comments_Syn_CProductions :: ([String]), dataAlts_Syn_CProductions :: (DataAlts), decls_Syn_CProductions :: (Decls), semNames_Syn_CProductions :: ([String]) }
{-# INLINABLE wrap_CProductions #-}
wrap_CProductions :: T_CProductions  -> Inh_CProductions  -> (Syn_CProductions )
wrap_CProductions (T_CProductions act) (Inh_CProductions _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
        (T_CProductions_vOut16 _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames) <- return (inv_CProductions_s17 sem arg)
        return (Syn_CProductions _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames)
   )

-- cata
{-# NOINLINE sem_CProductions #-}
sem_CProductions :: CProductions  -> T_CProductions 
sem_CProductions list = Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list)

-- semantic domain
newtype T_CProductions  = T_CProductions {
                                         attach_T_CProductions :: Identity (T_CProductions_s17 )
                                         }
newtype T_CProductions_s17  = C_CProductions_s17 {
                                                 inv_CProductions_s17 :: (T_CProductions_v16 )
                                                 }
data T_CProductions_s18  = C_CProductions_s18
type T_CProductions_v16  = (T_CProductions_vIn16 ) -> (T_CProductions_vOut16 )
data T_CProductions_vIn16  = T_CProductions_vIn16 (Set NontermIdent) (PragmaMap) (Map ConstructorIdent (Set Identifier)) (ContextMap) (Attributes) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (Attributes) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CProductions_vOut16  = T_CProductions_vOut16 (Decls) ([String]) (DataAlts) (Decls) ([String])
{-# NOINLINE sem_CProductions_Cons #-}
sem_CProductions_Cons :: T_CProduction  -> T_CProductions  -> T_CProductions 
sem_CProductions_Cons arg_hd_ arg_tl_ = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_CProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_tl_))
         (T_CProduction_vOut13 _hdIcataAlt _hdIcomments _hdIdataAlt _hdIdecls _hdIsemNames) = inv_CProduction_s14 _hdX14 (T_CProduction_vIn13 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOinh _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOunfoldSemDom _hdOwith_sig _hdOwrappers)
         (T_CProductions_vOut16 _tlIcataAlts _tlIcomments _tlIdataAlts _tlIdecls _tlIsemNames) = inv_CProductions_s17 _tlX17 (T_CProductions_vIn16 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOinh _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOunfoldSemDom _tlOwith_sig _tlOwrappers)
         _lhsOdataAlts :: DataAlts
         _lhsOdataAlts = rule229 _hdIdataAlt _tlIdataAlts
         _lhsOcataAlts :: Decls
         _lhsOcataAlts = rule230 _hdIcataAlt _tlIcataAlts
         _lhsOcomments :: [String]
         _lhsOcomments = rule231 _hdIcomments _tlIcomments
         _lhsOdecls :: Decls
         _lhsOdecls = rule232 _hdIdecls _tlIdecls
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule233 _hdIsemNames _tlIsemNames
         _hdOallNts = rule234 _lhsIallNts
         _hdOallPragmas = rule235 _lhsIallPragmas
         _hdOaroundMap = rule236 _lhsIaroundMap
         _hdOcontextMap = rule237 _lhsIcontextMap
         _hdOinh = rule238 _lhsIinh
         _hdOmergeMap = rule239 _lhsImergeMap
         _hdOnt = rule240 _lhsInt
         _hdOo_case = rule241 _lhsIo_case
         _hdOo_cata = rule242 _lhsIo_cata
         _hdOo_costcentre = rule243 _lhsIo_costcentre
         _hdOo_data = rule244 _lhsIo_data
         _hdOo_linePragmas = rule245 _lhsIo_linePragmas
         _hdOo_monadic = rule246 _lhsIo_monadic
         _hdOo_newtypes = rule247 _lhsIo_newtypes
         _hdOo_pretty = rule248 _lhsIo_pretty
         _hdOo_rename = rule249 _lhsIo_rename
         _hdOo_sem = rule250 _lhsIo_sem
         _hdOo_sig = rule251 _lhsIo_sig
         _hdOo_splitsems = rule252 _lhsIo_splitsems
         _hdOo_strictwrap = rule253 _lhsIo_strictwrap
         _hdOo_traces = rule254 _lhsIo_traces
         _hdOo_unbox = rule255 _lhsIo_unbox
         _hdOoptions = rule256 _lhsIoptions
         _hdOparamMap = rule257 _lhsIparamMap
         _hdOprefix = rule258 _lhsIprefix
         _hdOquantMap = rule259 _lhsIquantMap
         _hdOsyn = rule260 _lhsIsyn
         _hdOunfoldSemDom = rule261 _lhsIunfoldSemDom
         _hdOwith_sig = rule262 _lhsIwith_sig
         _hdOwrappers = rule263 _lhsIwrappers
         _tlOallNts = rule264 _lhsIallNts
         _tlOallPragmas = rule265 _lhsIallPragmas
         _tlOaroundMap = rule266 _lhsIaroundMap
         _tlOcontextMap = rule267 _lhsIcontextMap
         _tlOinh = rule268 _lhsIinh
         _tlOmergeMap = rule269 _lhsImergeMap
         _tlOnt = rule270 _lhsInt
         _tlOo_case = rule271 _lhsIo_case
         _tlOo_cata = rule272 _lhsIo_cata
         _tlOo_costcentre = rule273 _lhsIo_costcentre
         _tlOo_data = rule274 _lhsIo_data
         _tlOo_linePragmas = rule275 _lhsIo_linePragmas
         _tlOo_monadic = rule276 _lhsIo_monadic
         _tlOo_newtypes = rule277 _lhsIo_newtypes
         _tlOo_pretty = rule278 _lhsIo_pretty
         _tlOo_rename = rule279 _lhsIo_rename
         _tlOo_sem = rule280 _lhsIo_sem
         _tlOo_sig = rule281 _lhsIo_sig
         _tlOo_splitsems = rule282 _lhsIo_splitsems
         _tlOo_strictwrap = rule283 _lhsIo_strictwrap
         _tlOo_traces = rule284 _lhsIo_traces
         _tlOo_unbox = rule285 _lhsIo_unbox
         _tlOoptions = rule286 _lhsIoptions
         _tlOparamMap = rule287 _lhsIparamMap
         _tlOprefix = rule288 _lhsIprefix
         _tlOquantMap = rule289 _lhsIquantMap
         _tlOsyn = rule290 _lhsIsyn
         _tlOunfoldSemDom = rule291 _lhsIunfoldSemDom
         _tlOwith_sig = rule292 _lhsIwith_sig
         _tlOwrappers = rule293 _lhsIwrappers
         __result_ = T_CProductions_vOut16 _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule229 #-}
   {-# LINE 1021 "./src-ag/GenerateCode.ag" #-}
   rule229 = \ ((_hdIdataAlt) :: DataAlt) ((_tlIdataAlts) :: DataAlts) ->
                                  {-# LINE 1021 "./src-ag/GenerateCode.ag" #-}
                                  _hdIdataAlt : _tlIdataAlts
                                  {-# LINE 1870 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule230 #-}
   {-# LINE 1135 "./src-ag/GenerateCode.ag" #-}
   rule230 = \ ((_hdIcataAlt) :: Decl) ((_tlIcataAlts) :: Decls) ->
                          {-# LINE 1135 "./src-ag/GenerateCode.ag" #-}
                          _hdIcataAlt : _tlIcataAlts
                          {-# LINE 1876 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule231 #-}
   rule231 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule232 #-}
   rule232 = \ ((_hdIdecls) :: Decls) ((_tlIdecls) :: Decls) ->
     _hdIdecls ++ _tlIdecls
   {-# INLINE rule233 #-}
   rule233 = \ ((_hdIsemNames) :: [String]) ((_tlIsemNames) :: [String]) ->
     _hdIsemNames ++ _tlIsemNames
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) ->
     _lhsIaroundMap
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) ->
     _lhsIaroundMap
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule271 #-}
   rule271 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule274 #-}
   rule274 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule275 #-}
   rule275 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CProductions_Nil #-}
sem_CProductions_Nil ::  T_CProductions 
sem_CProductions_Nil  = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOdataAlts :: DataAlts
         _lhsOdataAlts = rule294  ()
         _lhsOcataAlts :: Decls
         _lhsOcataAlts = rule295  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule296  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule297  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule298  ()
         __result_ = T_CProductions_vOut16 _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule294 #-}
   {-# LINE 1022 "./src-ag/GenerateCode.ag" #-}
   rule294 = \  (_ :: ()) ->
                                  {-# LINE 1022 "./src-ag/GenerateCode.ag" #-}
                                  []
                                  {-# LINE 2091 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule295 #-}
   {-# LINE 1136 "./src-ag/GenerateCode.ag" #-}
   rule295 = \  (_ :: ()) ->
                          {-# LINE 1136 "./src-ag/GenerateCode.ag" #-}
                          []
                          {-# LINE 2097 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule296 #-}
   rule296 = \  (_ :: ()) ->
     []
   {-# INLINE rule297 #-}
   rule297 = \  (_ :: ()) ->
     []
   {-# INLINE rule298 #-}
   rule298 = \  (_ :: ()) ->
     []

-- CRule -------------------------------------------------------
-- wrapper
data Inh_CRule  = Inh_CRule { allNts_Inh_CRule :: (Set NontermIdent), aroundMap_Inh_CRule :: (Set Identifier), children_Inh_CRule :: ([(Identifier,Type,ChildKind)]), con_Inh_CRule :: (ConstructorIdent), declsAbove_Inh_CRule :: ([Decl]), inh_Inh_CRule :: (Attributes), instVisitNrs_Inh_CRule :: (Map Identifier Int), mergeMap_Inh_CRule :: (Map Identifier (Identifier, [Identifier])), nr_Inh_CRule :: (Int), nt_Inh_CRule :: (NontermIdent), o_case_Inh_CRule :: (Bool), o_cata_Inh_CRule :: (Bool), o_costcentre_Inh_CRule :: (Bool), o_data_Inh_CRule :: (Maybe Bool), o_linePragmas_Inh_CRule :: (Bool), o_monadic_Inh_CRule :: (Bool), o_newtypes_Inh_CRule :: (Bool), o_pretty_Inh_CRule :: (Bool), o_rename_Inh_CRule :: (Bool), o_sem_Inh_CRule :: (Bool), o_sig_Inh_CRule :: (Bool), o_splitsems_Inh_CRule :: (Bool), o_strictwrap_Inh_CRule :: (Bool), o_traces_Inh_CRule :: (Bool), o_unbox_Inh_CRule :: (Bool), options_Inh_CRule :: (Options), paramInstMap_Inh_CRule :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CRule :: (ParamMap), prefix_Inh_CRule :: (String), syn_Inh_CRule :: (Attributes), terminals_Inh_CRule :: ([Identifier]), unfoldSemDom_Inh_CRule :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CRule :: (Set Identifier), what_Inh_CRule :: (String) }
data Syn_CRule  = Syn_CRule { allTpsFound_Syn_CRule :: (Bool), bldBlocksFun_Syn_CRule :: (DeclBlocks -> DeclBlocks), comments_Syn_CRule :: ([String]), decls_Syn_CRule :: (Decls), declsAbove_Syn_CRule :: ([Decl]), definedInsts_Syn_CRule :: ([Identifier]), exprs_Syn_CRule :: (Exprs), tSigs_Syn_CRule :: ([Decl]), tps_Syn_CRule :: ([Type]), usedVars_Syn_CRule :: (Set String), visitedSet_Syn_CRule :: (Set Identifier) }
{-# INLINABLE wrap_CRule #-}
wrap_CRule :: T_CRule  -> Inh_CRule  -> (Syn_CRule )
wrap_CRule (T_CRule act) (Inh_CRule _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
        (T_CRule_vOut19 _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet) <- return (inv_CRule_s20 sem arg)
        return (Syn_CRule _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet)
   )

-- cata
{-# NOINLINE sem_CRule #-}
sem_CRule :: CRule  -> T_CRule 
sem_CRule ( CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ pattern_ rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_ ) = sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ ( sem_Pattern pattern_ ) rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_
sem_CRule ( CChildVisit name_ nt_ nr_ inh_ syn_ isLast_ ) = sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_

-- semantic domain
newtype T_CRule  = T_CRule {
                           attach_T_CRule :: Identity (T_CRule_s20 )
                           }
newtype T_CRule_s20  = C_CRule_s20 {
                                   inv_CRule_s20 :: (T_CRule_v19 )
                                   }
data T_CRule_s21  = C_CRule_s21
type T_CRule_v19  = (T_CRule_vIn19 ) -> (T_CRule_vOut19 )
data T_CRule_vIn19  = T_CRule_vIn19 (Set NontermIdent) (Set Identifier) ([(Identifier,Type,ChildKind)]) (ConstructorIdent) ([Decl]) (Attributes) (Map Identifier Int) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (String)
data T_CRule_vOut19  = T_CRule_vOut19 (Bool) (DeclBlocks -> DeclBlocks) ([String]) (Decls) ([Decl]) ([Identifier]) (Exprs) ([Decl]) ([Type]) (Set String) (Set Identifier)
{-# NOINLINE sem_CRule_CRule #-}
sem_CRule_CRule :: (Identifier) -> (Bool) -> (Bool) -> (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Maybe NontermIdent) -> (Maybe Type) -> T_Pattern  -> ([String]) -> (Map Int (Identifier,Identifier,Maybe Type)) -> (Bool) -> (String) -> (Set (Identifier, Identifier)) -> (Bool) -> (Maybe Identifier) -> T_CRule 
sem_CRule_CRule arg_name_ arg_isIn_ arg_hasCode_ arg_nt_ arg_con_ arg_field_ _ arg_tp_ arg_pattern_ arg_rhs_ arg_defines_ _ arg_origin_ arg_uses_ arg_explicit_ arg_mbNamed_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut40 _patternIcopy _patternIdefinedInsts _patternIpatternAttributes) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 )
         _instTypes = rule299 _lhsIchildren
         _originComment = rule300 _lhsIo_pretty arg_origin_
         _instDecls = rule301 _definedInsts _instTypes _lhsIo_monadic _lhsIo_newtypes _lhsIprefix
         _patDescr = rule302 _patternIpatternAttributes arg_isIn_
         _traceDescr = rule303 _patDescr arg_con_ arg_mbNamed_ arg_nt_
         _addTrace = rule304 _lhsIo_traces _traceDescr
         _costCentreDescr = rule305 _patDescr arg_con_ arg_nt_
         _addCostCentre = rule306 _costCentreDescr _lhsIo_costcentre
         _addLinePragma = rule307 _lhsIo_linePragmas arg_name_
         _decls = rule308 _addCostCentre _addLinePragma _addTrace _instDecls _lhsIo_monadic _originComment _patternIcopy arg_defines_ arg_explicit_ arg_hasCode_ arg_rhs_ arg_uses_
         _definedInsts = rule309 _patternIdefinedInsts arg_isIn_
         _rulename = rule310 _lhsIterminals arg_field_ arg_isIn_ arg_name_
         _lhsOexprs :: Exprs
         _lhsOexprs = rule311 _rulename
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule312 _rulename
         _mkTp = rule313 _lhsInt _orgParams
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule314 _evalTp _lhsIchildren _mkTp arg_defines_
         _orgParams = rule315 _lhsInt _lhsIparamMap
         _evalTp = rule316 _lhsInt _lhsIparamInstMap _lhsIparamMap _orgParams
         _lhsOtps :: [Type]
         _lhsOallTpsFound :: Bool
         (_lhsOtps,_lhsOallTpsFound) = rule317 arg_tp_
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule318 _decls _lhsIdeclsAbove
         _lhsObldBlocksFun :: DeclBlocks -> DeclBlocks
         _lhsObldBlocksFun = rule319  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule320 _lhsIwhat arg_defines_
         _lhsOdecls :: Decls
         _lhsOdecls = rule321 _decls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule322 _definedInsts
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule323 _lhsIvisitedSet
         __result_ = T_CRule_vOut19 _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule299 #-}
   {-# LINE 157 "./src-ag/GenerateCode.ag" #-}
   rule299 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
                             {-# LINE 157 "./src-ag/GenerateCode.ag" #-}
                             [ (n, (t, mb, for)) | (n, NT t _ for, mb) <- _lhsIchildren ]
                             {-# LINE 2192 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule300 #-}
   {-# LINE 158 "./src-ag/GenerateCode.ag" #-}
   rule300 = \ ((_lhsIo_pretty) :: Bool) origin_ ->
                                 {-# LINE 158 "./src-ag/GenerateCode.ag" #-}
                                 if  _lhsIo_pretty
                                     then (Comment origin_:)
                                     else id
                                 {-# LINE 2200 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule301 #-}
   {-# LINE 161 "./src-ag/GenerateCode.ag" #-}
   rule301 = \ _definedInsts _instTypes ((_lhsIo_monadic) :: Bool) ((_lhsIo_newtypes) :: Bool) ((_lhsIprefix) :: String) ->
                             {-# LINE 161 "./src-ag/GenerateCode.ag" #-}
                             [ mkDecl _lhsIo_monadic (Pattern3 (Alias _INST' inst (Underscore (getPos inst))))
                                    ( let (nm,mb,defor) = fromJust $ inst `lookup` _instTypes
                                      in unwrapSem _lhsIo_newtypes nm
                                         $ case mb of
                                             ChildReplace _ -> App instLocFieldName [SimpleExpr $ fieldname inst]
                                             _              ->
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
                             {-# LINE 2222 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule302 #-}
   {-# LINE 178 "./src-ag/GenerateCode.ag" #-}
   rule302 = \ ((_patternIpatternAttributes) :: [(Identifier, Identifier)]) isIn_ ->
                            {-# LINE 178 "./src-ag/GenerateCode.ag" #-}
                            if isIn_
                            then "_"
                            else concat $ intersperse "," (map (\(f,a) -> show f ++ "." ++ show a) _patternIpatternAttributes)
                            {-# LINE 2230 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule303 #-}
   {-# LINE 181 "./src-ag/GenerateCode.ag" #-}
   rule303 = \ _patDescr con_ mbNamed_ nt_ ->
                              {-# LINE 181 "./src-ag/GenerateCode.ag" #-}
                              (maybe "" (\nm -> show nm ++ ":") mbNamed_) ++ show nt_ ++ " :: " ++ show con_ ++ " :: " ++ _patDescr
                              {-# LINE 2236 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule304 #-}
   {-# LINE 183 "./src-ag/GenerateCode.ag" #-}
   rule304 = \ ((_lhsIo_traces) :: Bool) _traceDescr ->
                            {-# LINE 183 "./src-ag/GenerateCode.ag" #-}
                            \v -> if _lhsIo_traces
                                  then Trace _traceDescr     v
                                  else v
                            {-# LINE 2244 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule305 #-}
   {-# LINE 186 "./src-ag/GenerateCode.ag" #-}
   rule305 = \ _patDescr con_ nt_ ->
                                   {-# LINE 186 "./src-ag/GenerateCode.ag" #-}
                                   show nt_ ++ ":" ++ show con_ ++ ":" ++ _patDescr
                                   {-# LINE 2250 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule306 #-}
   {-# LINE 187 "./src-ag/GenerateCode.ag" #-}
   rule306 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                 {-# LINE 187 "./src-ag/GenerateCode.ag" #-}
                                 \v -> if _lhsIo_costcentre
                                       then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                       else v
                                 {-# LINE 2258 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule307 #-}
   {-# LINE 190 "./src-ag/GenerateCode.ag" #-}
   rule307 = \ ((_lhsIo_linePragmas) :: Bool) name_ ->
                                 {-# LINE 190 "./src-ag/GenerateCode.ag" #-}
                                 \v -> let p = getPos name_
                                           hasPos = line p > 0 && column p >= 0 && not (null (file p))
                                       in if _lhsIo_linePragmas && hasPos
                                          then PragmaExpr True True ("LINE " ++ show (line p) ++ " " ++ show (file p))
                                               $ LineExpr
                                               $ v
                                          else v
                                 {-# LINE 2270 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule308 #-}
   {-# LINE 197 "./src-ag/GenerateCode.ag" #-}
   rule308 = \ _addCostCentre _addLinePragma _addTrace _instDecls ((_lhsIo_monadic) :: Bool) _originComment ((_patternIcopy) :: Pattern) defines_ explicit_ hasCode_ rhs_ uses_ ->
                         {-# LINE 197 "./src-ag/GenerateCode.ag" #-}
                         if hasCode_
                         then _originComment ( mkDecl (_lhsIo_monadic && explicit_) (Pattern3 _patternIcopy) (_addTrace     $ _addCostCentre     $ _addLinePragma     $ (TextExpr rhs_))
                                                    (Set.fromList [attrname False fld nm | (fld,nm,_) <- Map.elems defines_])
                                                    (Set.fromList [attrname True fld nm | (fld,nm) <- Set.toList uses_])
                                             : _instDecls    )
                         else _instDecls
                         {-# LINE 2281 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule309 #-}
   {-# LINE 267 "./src-ag/GenerateCode.ag" #-}
   rule309 = \ ((_patternIdefinedInsts) :: [Identifier]) isIn_ ->
                                {-# LINE 267 "./src-ag/GenerateCode.ag" #-}
                                if isIn_ then [] else _patternIdefinedInsts
                                {-# LINE 2287 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule310 #-}
   {-# LINE 337 "./src-ag/GenerateCode.ag" #-}
   rule310 = \ ((_lhsIterminals) :: [Identifier]) field_ isIn_ name_ ->
                            {-# LINE 337 "./src-ag/GenerateCode.ag" #-}
                            if  field_ == _LOC && name_ `elem` _lhsIterminals
                            then funname name_ 0
                            else attrname isIn_ field_ name_
                            {-# LINE 2295 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule311 #-}
   {-# LINE 340 "./src-ag/GenerateCode.ag" #-}
   rule311 = \ _rulename ->
                         {-# LINE 340 "./src-ag/GenerateCode.ag" #-}
                         [SimpleExpr _rulename    ]
                         {-# LINE 2301 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule312 #-}
   {-# LINE 356 "./src-ag/GenerateCode.ag" #-}
   rule312 = \ _rulename ->
                       {-# LINE 356 "./src-ag/GenerateCode.ag" #-}
                       Set.singleton _rulename
                       {-# LINE 2307 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule313 #-}
   {-# LINE 366 "./src-ag/GenerateCode.ag" #-}
   rule313 = \ ((_lhsInt) :: NontermIdent) _orgParams ->
                               {-# LINE 366 "./src-ag/GenerateCode.ag" #-}
                               typeToCodeType (Just _lhsInt) _orgParams
                               {-# LINE 2313 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule314 #-}
   {-# LINE 367 "./src-ag/GenerateCode.ag" #-}
   rule314 = \ _evalTp ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) _mkTp defines_ ->
                                {-# LINE 367 "./src-ag/GenerateCode.ag" #-}
                                [ TSig (attrname False field attr) tp'
                                |  (field,attr,tp) <- Map.elems defines_, isJust tp
                                , let tp1 = _evalTp     field $ _mkTp (fromJust tp)
                                      tp' = case findOrigType attr _lhsIchildren of
                                             Just tp'' -> let tp2 = _evalTp     field $ _mkTp tp''
                                                          in Arr tp2 tp1
                                             Nothing -> tp1
                                      findOrigType _ [] = Nothing
                                      findOrigType nm ((n,_,kind) : r)
                                        | nm == n = case kind of
                                                      ChildReplace orig -> Just orig
                                                      _                 -> Nothing
                                        | otherwise = findOrigType nm r
                                ]
                                {-# LINE 2332 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule315 #-}
   {-# LINE 382 "./src-ag/GenerateCode.ag" #-}
   rule315 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                                    {-# LINE 382 "./src-ag/GenerateCode.ag" #-}
                                    map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                                    {-# LINE 2338 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule316 #-}
   {-# LINE 384 "./src-ag/GenerateCode.ag" #-}
   rule316 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ((_lhsIparamMap) :: ParamMap) _orgParams ->
                      {-# LINE 384 "./src-ag/GenerateCode.ag" #-}
                      \field tp -> let orgFldParams = map getName $ Map.findWithDefault [] childNt _lhsIparamMap
                                       (childNt,instParams) = Map.findWithDefault (_lhsInt,[]) field _lhsIparamInstMap
                                       replMap = Map.fromList (zip orgFldParams instParams)
                                       replace k = Map.findWithDefault ('@':k) k replMap
                                   in if null instParams
                                      then if null _orgParams
                                           then tp
                                           else idEvalType tp
                                      else evalType replace tp
                      {-# LINE 2352 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule317 #-}
   {-# LINE 419 "./src-ag/GenerateCode.ag" #-}
   rule317 = \ tp_ ->
                                            {-# LINE 419 "./src-ag/GenerateCode.ag" #-}
                                            maybe ([],False) (\tp -> ([tp],True)) tp_
                                            {-# LINE 2358 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 618 "./src-ag/GenerateCode.ag" #-}
   rule318 = \ _decls ((_lhsIdeclsAbove) :: [Decl]) ->
                         {-# LINE 618 "./src-ag/GenerateCode.ag" #-}
                         _lhsIdeclsAbove ++ _decls
                         {-# LINE 2364 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule319 #-}
   {-# LINE 631 "./src-ag/GenerateCode.ag" #-}
   rule319 = \  (_ :: ()) ->
                           {-# LINE 631 "./src-ag/GenerateCode.ag" #-}
                           id
                           {-# LINE 2370 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule320 #-}
   {-# LINE 906 "./src-ag/GenerateCode.ag" #-}
   rule320 = \ ((_lhsIwhat) :: String) defines_ ->
                                   {-# LINE 906 "./src-ag/GenerateCode.ag" #-}
                                   [ makeLocalComment 11 _lhsIwhat name tp | (field,name,tp) <- Map.elems defines_, field == _LOC ]
                                   ++ [ makeLocalComment 11 "inst " name tp | (field,name,tp) <- Map.elems defines_, field == _INST ]
                                   {-# LINE 2377 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule321 #-}
   rule321 = \ _decls ->
     _decls
   {-# INLINE rule322 #-}
   rule322 = \ _definedInsts ->
     _definedInsts
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
{-# NOINLINE sem_CRule_CChildVisit #-}
sem_CRule_CChildVisit :: (Identifier) -> (NontermIdent) -> (Int) -> (Attributes) -> (Attributes) -> (Bool) -> T_CRule 
sem_CRule_CChildVisit arg_name_ arg_nt_ arg_nr_ arg_inh_ arg_syn_ arg_isLast_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _visitedSet = rule324 _lhsIvisitedSet arg_name_
         _costCentreDescr = rule325 _lhsIcon _lhsInt arg_name_ arg_nr_ arg_nt_
         _addCostCentre = rule326 _costCentreDescr _lhsIo_costcentre
         _decls = rule327 _addCostCentre _lhsIaroundMap _lhsIchildren _lhsImergeMap _lhsIo_monadic _lhsIo_newtypes _lhsIo_unbox _visitedSet arg_inh_ arg_isLast_ arg_name_ arg_nr_ arg_nt_ arg_syn_
         _isSuperfluousHigherOrderIntra = rule328 _lhsIinstVisitNrs _lhsInr arg_name_
         _names = rule329 _isSuperfluousHigherOrderIntra arg_name_ arg_nr_
         _lhsOexprs :: Exprs
         _lhsOexprs = rule330 _instParams _lhsIo_newtypes _lhsIunfoldSemDom _names arg_nr_ arg_nt_
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule331 _names
         _mkTp = rule332 _evalTp _orgParams arg_nt_
         _definedTps = rule333 _mkTp arg_name_ arg_syn_
         _nextTp = rule334 arg_nr_ arg_nt_
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule335 _definedTps _instParams _nextTp arg_isLast_ arg_name_ arg_nr_
         _orgParams = rule336 _lhsIparamMap arg_nt_
         _instParams = rule337 _lhsIparamInstMap arg_name_ arg_nt_
         _replParamMap = rule338 _instParams _orgParams
         _replace = rule339 _replParamMap
         _evalTp = rule340 _orgParams _replace
         _lhsOtps :: [Type]
         _lhsOtps = rule341 _instParams _isSuperfluousHigherOrderIntra arg_nr_ arg_nt_
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule342  ()
         _lhsObldBlocksFun :: DeclBlocks -> DeclBlocks
         _lhsObldBlocksFun = rule343 _decls _lhsIdeclsAbove
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule344  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule345  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule346 _decls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule347  ()
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule348 _visitedSet
         __result_ = T_CRule_vOut19 _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule324 #-}
   {-# LINE 147 "./src-ag/GenerateCode.ag" #-}
   rule324 = \ ((_lhsIvisitedSet) :: Set Identifier) name_ ->
                                            {-# LINE 147 "./src-ag/GenerateCode.ag" #-}
                                            Set.insert name_ _lhsIvisitedSet
                                            {-# LINE 2438 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule325 #-}
   {-# LINE 203 "./src-ag/GenerateCode.ag" #-}
   rule325 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ nr_ nt_ ->
                                         {-# LINE 203 "./src-ag/GenerateCode.ag" #-}
                                         show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show name_ ++ ":" ++ show nt_ ++ ":" ++ show nr_
                                         {-# LINE 2444 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 204 "./src-ag/GenerateCode.ag" #-}
   rule326 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                       {-# LINE 204 "./src-ag/GenerateCode.ag" #-}
                                       \v -> if _lhsIo_costcentre
                                             then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                             else v
                                       {-# LINE 2452 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule327 #-}
   {-# LINE 207 "./src-ag/GenerateCode.ag" #-}
   rule327 = \ _addCostCentre ((_lhsIaroundMap) :: Set Identifier) ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ((_lhsIo_monadic) :: Bool) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) _visitedSet inh_ isLast_ name_ nr_ nt_ syn_ ->
                               {-# LINE 207 "./src-ag/GenerateCode.ag" #-}
                               let  lhsVars =  map (attrname True name_) (Map.keys syn_)
                                               ++ if isLast_ then [] else [unwrap ++ funname name_ (nr_+1)]
                                    rhsVars = map (attrname False name_) (Map.keys inh_)
                                    unwrap = if _lhsIo_newtypes then typeName nt_ (nr_ + 1) ++ " " else ""
                                    tuple | isMerging = TupleLhs [locname name_ ++ "_comp"]
                                          | otherwise = mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars
                                    rhs = _addCostCentre     $ Code.InvokeExpr (typeName nt_ nr_) (SimpleExpr fun) (map SimpleExpr rhsVars)
                                    isVirtual _ [] = False
                                    isVirtual nm ((n,_,kind) : r)
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
                                                                rhsVars' = [ locname c' ++ "_comp" | c' <- cs ]
                                                                fun'    = locname c ++ "_merge"
                                                                rhs' = App fun' (map SimpleExpr rhsVars')
                                                            in [Resume _lhsIo_monadic (typeName nt_ nr_) tuple' rhs']
                               in
                                  (outDecls ++ outMerged)
                               {-# LINE 2495 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule328 #-}
   {-# LINE 329 "./src-ag/GenerateCode.ag" #-}
   rule328 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ((_lhsInr) :: Int) name_ ->
            {-# LINE 329 "./src-ag/GenerateCode.ag" #-}
            _lhsInr <= Map.findWithDefault (-1) name_ _lhsIinstVisitNrs
            {-# LINE 2501 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule329 #-}
   {-# LINE 342 "./src-ag/GenerateCode.ag" #-}
   rule329 = \ _isSuperfluousHigherOrderIntra name_ nr_ ->
                     {-# LINE 342 "./src-ag/GenerateCode.ag" #-}
                     if _isSuperfluousHigherOrderIntra
                     then []
                     else [funname name_ (nr_+1)]
                     {-# LINE 2509 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule330 #-}
   {-# LINE 346 "./src-ag/GenerateCode.ag" #-}
   rule330 = \ _instParams ((_lhsIo_newtypes) :: Bool) ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) _names nr_ nt_ ->
                     {-# LINE 346 "./src-ag/GenerateCode.ag" #-}
                     let wrap = if _lhsIo_newtypes then \x -> App (typeName nt_ (nr_ + 1)) [x] else id
                         addType expr | null _instParams     = expr
                                      | otherwise            = TypedExpr expr (_lhsIunfoldSemDom nt_ (nr_+1) _instParams    )
                     in map (wrap . addType . SimpleExpr) _names
                     {-# LINE 2518 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule331 #-}
   {-# LINE 358 "./src-ag/GenerateCode.ag" #-}
   rule331 = \ _names ->
                       {-# LINE 358 "./src-ag/GenerateCode.ag" #-}
                       Set.fromList _names
                       {-# LINE 2524 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule332 #-}
   {-# LINE 394 "./src-ag/GenerateCode.ag" #-}
   rule332 = \ _evalTp _orgParams nt_ ->
                               {-# LINE 394 "./src-ag/GenerateCode.ag" #-}
                               _evalTp     . typeToCodeType (Just nt_) _orgParams
                               {-# LINE 2530 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule333 #-}
   {-# LINE 395 "./src-ag/GenerateCode.ag" #-}
   rule333 = \ _mkTp name_ syn_ ->
                                     {-# LINE 395 "./src-ag/GenerateCode.ag" #-}
                                     [ TSig (attrname True name_ a) (_mkTp tp) |  (a,tp) <- Map.toList syn_ ]
                                     {-# LINE 2536 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule334 #-}
   {-# LINE 396 "./src-ag/GenerateCode.ag" #-}
   rule334 = \ nr_ nt_ ->
                                 {-# LINE 396 "./src-ag/GenerateCode.ag" #-}
                                 typeName nt_ (nr_+1)
                                 {-# LINE 2542 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule335 #-}
   {-# LINE 397 "./src-ag/GenerateCode.ag" #-}
   rule335 = \ _definedTps _instParams _nextTp isLast_ name_ nr_ ->
                                {-# LINE 397 "./src-ag/GenerateCode.ag" #-}
                                (if isLast_ then id else (TSig (funname name_ (nr_+1)) (TypeApp (SimpleType _nextTp) (map SimpleType _instParams    )) :)) _definedTps
                                {-# LINE 2548 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule336 #-}
   {-# LINE 399 "./src-ag/GenerateCode.ag" #-}
   rule336 = \ ((_lhsIparamMap) :: ParamMap) nt_ ->
                                    {-# LINE 399 "./src-ag/GenerateCode.ag" #-}
                                    map getName $ Map.findWithDefault [] nt_ _lhsIparamMap
                                    {-# LINE 2554 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule337 #-}
   {-# LINE 400 "./src-ag/GenerateCode.ag" #-}
   rule337 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) name_ nt_ ->
                                     {-# LINE 400 "./src-ag/GenerateCode.ag" #-}
                                     snd $ Map.findWithDefault (nt_,[]) name_ _lhsIparamInstMap
                                     {-# LINE 2560 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule338 #-}
   {-# LINE 401 "./src-ag/GenerateCode.ag" #-}
   rule338 = \ _instParams _orgParams ->
                                       {-# LINE 401 "./src-ag/GenerateCode.ag" #-}
                                       Map.fromList (zip _orgParams     _instParams    )
                                       {-# LINE 2566 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule339 #-}
   {-# LINE 402 "./src-ag/GenerateCode.ag" #-}
   rule339 = \ _replParamMap ->
                                  {-# LINE 402 "./src-ag/GenerateCode.ag" #-}
                                  \k -> Map.findWithDefault k k _replParamMap
                                  {-# LINE 2572 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule340 #-}
   {-# LINE 403 "./src-ag/GenerateCode.ag" #-}
   rule340 = \ _orgParams _replace ->
                                 {-# LINE 403 "./src-ag/GenerateCode.ag" #-}
                                 if null _orgParams     then id else evalType _replace
                                 {-# LINE 2578 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 420 "./src-ag/GenerateCode.ag" #-}
   rule341 = \ _instParams _isSuperfluousHigherOrderIntra nr_ nt_ ->
                              {-# LINE 420 "./src-ag/GenerateCode.ag" #-}
                              if _isSuperfluousHigherOrderIntra
                              then []
                              else [NT (ntOfVisit nt_ (nr_+1)) _instParams     False]
                              {-# LINE 2586 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 620 "./src-ag/GenerateCode.ag" #-}
   rule342 = \  (_ :: ()) ->
                         {-# LINE 620 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 2592 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule343 #-}
   {-# LINE 633 "./src-ag/GenerateCode.ag" #-}
   rule343 = \ _decls ((_lhsIdeclsAbove) :: [Decl]) ->
                           {-# LINE 633 "./src-ag/GenerateCode.ag" #-}
                           DeclBlock _lhsIdeclsAbove (head _decls    )
                           {-# LINE 2598 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule344 #-}
   rule344 = \  (_ :: ()) ->
     True
   {-# INLINE rule345 #-}
   rule345 = \  (_ :: ()) ->
     []
   {-# INLINE rule346 #-}
   rule346 = \ _decls ->
     _decls
   {-# INLINE rule347 #-}
   rule347 = \  (_ :: ()) ->
     []
   {-# INLINE rule348 #-}
   rule348 = \ _visitedSet ->
     _visitedSet

-- CSegment ----------------------------------------------------
-- wrapper
data Inh_CSegment  = Inh_CSegment { inh_Inh_CSegment :: (Attributes), isLast_Inh_CSegment :: (Bool), nr_Inh_CSegment :: (Int), nt_Inh_CSegment :: (NontermIdent), o_case_Inh_CSegment :: (Bool), o_cata_Inh_CSegment :: (Bool), o_costcentre_Inh_CSegment :: (Bool), o_data_Inh_CSegment :: (Maybe Bool), o_linePragmas_Inh_CSegment :: (Bool), o_monadic_Inh_CSegment :: (Bool), o_newtypes_Inh_CSegment :: (Bool), o_pretty_Inh_CSegment :: (Bool), o_rename_Inh_CSegment :: (Bool), o_sem_Inh_CSegment :: (Bool), o_sig_Inh_CSegment :: (Bool), o_splitsems_Inh_CSegment :: (Bool), o_strictwrap_Inh_CSegment :: (Bool), o_traces_Inh_CSegment :: (Bool), o_unbox_Inh_CSegment :: (Bool), options_Inh_CSegment :: (Options), paramMap_Inh_CSegment :: (ParamMap), prefix_Inh_CSegment :: (String), syn_Inh_CSegment :: (Attributes) }
data Syn_CSegment  = Syn_CSegment { comments_Syn_CSegment :: ([String]), semDom_Syn_CSegment :: ([Decl]), semDomUnfoldGath_Syn_CSegment :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CSegment :: (Decls) }
{-# INLINABLE wrap_CSegment #-}
wrap_CSegment :: T_CSegment  -> Inh_CSegment  -> (Syn_CSegment )
wrap_CSegment (T_CSegment act) (Inh_CSegment _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegment_vIn22 _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
        (T_CSegment_vOut22 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls) <- return (inv_CSegment_s23 sem arg)
        return (Syn_CSegment _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls)
   )

-- cata
{-# INLINE sem_CSegment #-}
sem_CSegment :: CSegment  -> T_CSegment 
sem_CSegment ( CSegment inh_ syn_ ) = sem_CSegment_CSegment inh_ syn_

-- semantic domain
newtype T_CSegment  = T_CSegment {
                                 attach_T_CSegment :: Identity (T_CSegment_s23 )
                                 }
newtype T_CSegment_s23  = C_CSegment_s23 {
                                         inv_CSegment_s23 :: (T_CSegment_v22 )
                                         }
data T_CSegment_s24  = C_CSegment_s24
type T_CSegment_v22  = (T_CSegment_vIn22 ) -> (T_CSegment_vOut22 )
data T_CSegment_vIn22  = T_CSegment_vIn22 (Attributes) (Bool) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CSegment_vOut22  = T_CSegment_vOut22 ([String]) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CSegment_CSegment #-}
sem_CSegment_CSegment :: (Attributes) -> (Attributes) -> T_CSegment 
sem_CSegment_CSegment arg_inh_ arg_syn_ = T_CSegment (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_CSegment_v22 
      v22 = \ (T_CSegment_vIn22 _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _altSemForm = rule349 _lhsIoptions
         _tp = rule350 _altSemForm _indexExpr _inhTps _synTps
         _inhTps = rule351 _lhsInt _params arg_inh_
         _inhTup = rule352 _inhTps _lhsIo_unbox
         _synTps = rule353 _continuation _inhTps _lhsInt _lhsIo_unbox _params arg_syn_
         _curTypeName = rule354 _lhsInr _lhsInt
         _nextTypeName = rule355 _lhsInr _lhsInt
         _indexName = rule356 _curTypeName
         _dataIndex = rule357 _indexName _params
         _indexExpr = rule358 _indexName _params
         _indexStr = rule359 _indexName _params
         _inhInstance = rule360 _indexStr _inhTup _lhsInr _lhsInt
         _synInstance = rule361 _indexStr _lhsInr _lhsInt _synTps
         _continuation = rule362 _lhsIisLast _nextTypeName _params
         _params = rule363 _lhsInt _lhsIparamMap
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule364 _altSemForm _dataIndex _inhInstance _lhsInr _lhsInt _lhsIo_newtypes _params _synInstance _tp
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule365 _lhsInr _lhsInt _params _tp
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule366 _lhsIisLast _lhsInr _lhsInt _lhsIo_newtypes _lhsIo_unbox arg_inh_ arg_syn_
         _lhsOcomments :: [String]
         _lhsOcomments = rule367 _lhsInr arg_inh_ arg_syn_
         __result_ = T_CSegment_vOut22 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegment_s23 v22
   {-# INLINE rule349 #-}
   {-# LINE 717 "./src-ag/GenerateCode.ag" #-}
   rule349 = \ ((_lhsIoptions) :: Options) ->
                                 {-# LINE 717 "./src-ag/GenerateCode.ag" #-}
                                 breadthFirst _lhsIoptions
                                 {-# LINE 2683 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 718 "./src-ag/GenerateCode.ag" #-}
   rule350 = \ _altSemForm _indexExpr _inhTps _synTps ->
                         {-# LINE 718 "./src-ag/GenerateCode.ag" #-}
                         if _altSemForm
                         then TypeApp (SimpleType "Child") [SimpleType "EvalInfo", _indexExpr     ]
                         else foldr Arr _synTps     _inhTps
                         {-# LINE 2691 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 721 "./src-ag/GenerateCode.ag" #-}
   rule351 = \ ((_lhsInt) :: NontermIdent) _params inh_ ->
                             {-# LINE 721 "./src-ag/GenerateCode.ag" #-}
                             [typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems inh_]
                             {-# LINE 2697 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule352 #-}
   {-# LINE 722 "./src-ag/GenerateCode.ag" #-}
   rule352 = \ _inhTps ((_lhsIo_unbox) :: Bool) ->
                             {-# LINE 722 "./src-ag/GenerateCode.ag" #-}
                             mkTupleType _lhsIo_unbox (null _inhTps    ) _inhTps
                             {-# LINE 2703 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule353 #-}
   {-# LINE 723 "./src-ag/GenerateCode.ag" #-}
   rule353 = \ _continuation _inhTps ((_lhsInt) :: NontermIdent) ((_lhsIo_unbox) :: Bool) _params syn_ ->
                             {-# LINE 723 "./src-ag/GenerateCode.ag" #-}
                             mkTupleType _lhsIo_unbox (null _inhTps    ) ([typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems syn_] ++ _continuation    )
                             {-# LINE 2709 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule354 #-}
   {-# LINE 724 "./src-ag/GenerateCode.ag" #-}
   rule354 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                   {-# LINE 724 "./src-ag/GenerateCode.ag" #-}
                                   typeName _lhsInt _lhsInr
                                   {-# LINE 2715 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule355 #-}
   {-# LINE 725 "./src-ag/GenerateCode.ag" #-}
   rule355 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                   {-# LINE 725 "./src-ag/GenerateCode.ag" #-}
                                   typeName _lhsInt (_lhsInr + 1)
                                   {-# LINE 2721 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule356 #-}
   {-# LINE 726 "./src-ag/GenerateCode.ag" #-}
   rule356 = \ _curTypeName ->
                                   {-# LINE 726 "./src-ag/GenerateCode.ag" #-}
                                   "I_" ++ _curTypeName
                                   {-# LINE 2727 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule357 #-}
   {-# LINE 727 "./src-ag/GenerateCode.ag" #-}
   rule357 = \ _indexName _params ->
                                {-# LINE 727 "./src-ag/GenerateCode.ag" #-}
                                Code.Data _indexName     _params     [DataAlt _indexName     []] False []
                                {-# LINE 2733 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule358 #-}
   {-# LINE 728 "./src-ag/GenerateCode.ag" #-}
   rule358 = \ _indexName _params ->
                                {-# LINE 728 "./src-ag/GenerateCode.ag" #-}
                                TypeApp (SimpleType _indexName    ) (map (SimpleType . ('@':)) _params    )
                                {-# LINE 2739 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule359 #-}
   {-# LINE 729 "./src-ag/GenerateCode.ag" #-}
   rule359 = \ _indexName _params ->
                                {-# LINE 729 "./src-ag/GenerateCode.ag" #-}
                                "(" ++ _indexName     ++ concatMap (\p -> " " ++ p) _params     ++ ")"
                                {-# LINE 2745 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule360 #-}
   {-# LINE 730 "./src-ag/GenerateCode.ag" #-}
   rule360 = \ _indexStr _inhTup ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                  {-# LINE 730 "./src-ag/GenerateCode.ag" #-}
                                  Code.Data "instance Inh" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Inh") [_inhTup    ] ] False []
                                  {-# LINE 2751 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule361 #-}
   {-# LINE 731 "./src-ag/GenerateCode.ag" #-}
   rule361 = \ _indexStr ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) _synTps ->
                                  {-# LINE 731 "./src-ag/GenerateCode.ag" #-}
                                  Code.Data "instance Syn" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Syn") [_synTps    ] ] False []
                                  {-# LINE 2757 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule362 #-}
   {-# LINE 732 "./src-ag/GenerateCode.ag" #-}
   rule362 = \ ((_lhsIisLast) :: Bool) _nextTypeName _params ->
                                   {-# LINE 732 "./src-ag/GenerateCode.ag" #-}
                                   if  _lhsIisLast
                                   then []
                                   else [TypeApp (SimpleType _nextTypeName    ) (map (SimpleType . ('@':)) _params    )]
                                   {-# LINE 2765 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule363 #-}
   {-# LINE 735 "./src-ag/GenerateCode.ag" #-}
   rule363 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                             {-# LINE 735 "./src-ag/GenerateCode.ag" #-}
                             map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                             {-# LINE 2771 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule364 #-}
   {-# LINE 736 "./src-ag/GenerateCode.ag" #-}
   rule364 = \ _altSemForm _dataIndex _inhInstance ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) _params _synInstance _tp ->
                             {-# LINE 736 "./src-ag/GenerateCode.ag" #-}
                             let name = typeName _lhsInt _lhsInr
                                 evalTp | null _params     = id
                                        | otherwise        = idEvalType
                             in ( if _lhsIo_newtypes
                                  then [ Code.NewType name _params     name (evalTp _tp    ) ]
                                  else [ Code.Type name _params     (evalTp _tp    ) ] )
                                ++ ( if _altSemForm
                                     then [_dataIndex    , _inhInstance    , _synInstance    ]
                                     else [] )
                             {-# LINE 2785 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule365 #-}
   {-# LINE 750 "./src-ag/GenerateCode.ag" #-}
   rule365 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) _params _tp ->
                               {-# LINE 750 "./src-ag/GenerateCode.ag" #-}
                               Map.singleton (_lhsInt, _lhsInr) (_params    , _tp    )
                               {-# LINE 2791 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule366 #-}
   {-# LINE 834 "./src-ag/GenerateCode.ag" #-}
   rule366 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) inh_ syn_ ->
                                 {-# LINE 834 "./src-ag/GenerateCode.ag" #-}
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
                                 {-# LINE 2806 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule367 #-}
   {-# LINE 876 "./src-ag/GenerateCode.ag" #-}
   rule367 = \ ((_lhsInr) :: Int) inh_ syn_ ->
                                   {-# LINE 876 "./src-ag/GenerateCode.ag" #-}
                                   let body = map ind (showsSegment (CSegment inh_ syn_))
                                   in if null body
                                      then []
                                      else ("visit " ++ show _lhsInr ++ ":") : body
                                   {-# LINE 2815 "dist/build/GenerateCode.hs"#-}

-- CSegments ---------------------------------------------------
-- wrapper
data Inh_CSegments  = Inh_CSegments { inh_Inh_CSegments :: (Attributes), nr_Inh_CSegments :: (Int), nt_Inh_CSegments :: (NontermIdent), o_case_Inh_CSegments :: (Bool), o_cata_Inh_CSegments :: (Bool), o_costcentre_Inh_CSegments :: (Bool), o_data_Inh_CSegments :: (Maybe Bool), o_linePragmas_Inh_CSegments :: (Bool), o_monadic_Inh_CSegments :: (Bool), o_newtypes_Inh_CSegments :: (Bool), o_pretty_Inh_CSegments :: (Bool), o_rename_Inh_CSegments :: (Bool), o_sem_Inh_CSegments :: (Bool), o_sig_Inh_CSegments :: (Bool), o_splitsems_Inh_CSegments :: (Bool), o_strictwrap_Inh_CSegments :: (Bool), o_traces_Inh_CSegments :: (Bool), o_unbox_Inh_CSegments :: (Bool), options_Inh_CSegments :: (Options), paramMap_Inh_CSegments :: (ParamMap), prefix_Inh_CSegments :: (String), syn_Inh_CSegments :: (Attributes) }
data Syn_CSegments  = Syn_CSegments { comments_Syn_CSegments :: ([String]), isNil_Syn_CSegments :: (Bool), semDom_Syn_CSegments :: ([Decl]), semDomUnfoldGath_Syn_CSegments :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CSegments :: (Decls) }
{-# INLINABLE wrap_CSegments #-}
wrap_CSegments :: T_CSegments  -> Inh_CSegments  -> (Syn_CSegments )
wrap_CSegments (T_CSegments act) (Inh_CSegments _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
        (T_CSegments_vOut25 _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls) <- return (inv_CSegments_s26 sem arg)
        return (Syn_CSegments _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls)
   )

-- cata
{-# NOINLINE sem_CSegments #-}
sem_CSegments :: CSegments  -> T_CSegments 
sem_CSegments list = Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list)

-- semantic domain
newtype T_CSegments  = T_CSegments {
                                   attach_T_CSegments :: Identity (T_CSegments_s26 )
                                   }
newtype T_CSegments_s26  = C_CSegments_s26 {
                                           inv_CSegments_s26 :: (T_CSegments_v25 )
                                           }
data T_CSegments_s27  = C_CSegments_s27
type T_CSegments_v25  = (T_CSegments_vIn25 ) -> (T_CSegments_vOut25 )
data T_CSegments_vIn25  = T_CSegments_vIn25 (Attributes) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CSegments_vOut25  = T_CSegments_vOut25 ([String]) (Bool) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CSegments_Cons #-}
sem_CSegments_Cons :: T_CSegment  -> T_CSegments  -> T_CSegments 
sem_CSegments_Cons arg_hd_ arg_tl_ = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_CSegment (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_tl_))
         (T_CSegment_vOut22 _hdIcomments _hdIsemDom _hdIsemDomUnfoldGath _hdIwrapDecls) = inv_CSegment_s23 _hdX23 (T_CSegment_vIn22 _hdOinh _hdOisLast _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOsyn)
         (T_CSegments_vOut25 _tlIcomments _tlIisNil _tlIsemDom _tlIsemDomUnfoldGath _tlIwrapDecls) = inv_CSegments_s26 _tlX26 (T_CSegments_vIn25 _tlOinh _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOsyn)
         _tlOnr = rule368 _lhsInr
         _lhsOisNil :: Bool
         _lhsOisNil = rule369  ()
         _hdOisLast = rule370 _tlIisNil
         _lhsOcomments :: [String]
         _lhsOcomments = rule371 _hdIcomments _tlIcomments
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule372 _hdIsemDom _tlIsemDom
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule373 _hdIsemDomUnfoldGath _tlIsemDomUnfoldGath
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule374 _hdIwrapDecls _tlIwrapDecls
         _hdOinh = rule375 _lhsIinh
         _hdOnr = rule376 _lhsInr
         _hdOnt = rule377 _lhsInt
         _hdOo_case = rule378 _lhsIo_case
         _hdOo_cata = rule379 _lhsIo_cata
         _hdOo_costcentre = rule380 _lhsIo_costcentre
         _hdOo_data = rule381 _lhsIo_data
         _hdOo_linePragmas = rule382 _lhsIo_linePragmas
         _hdOo_monadic = rule383 _lhsIo_monadic
         _hdOo_newtypes = rule384 _lhsIo_newtypes
         _hdOo_pretty = rule385 _lhsIo_pretty
         _hdOo_rename = rule386 _lhsIo_rename
         _hdOo_sem = rule387 _lhsIo_sem
         _hdOo_sig = rule388 _lhsIo_sig
         _hdOo_splitsems = rule389 _lhsIo_splitsems
         _hdOo_strictwrap = rule390 _lhsIo_strictwrap
         _hdOo_traces = rule391 _lhsIo_traces
         _hdOo_unbox = rule392 _lhsIo_unbox
         _hdOoptions = rule393 _lhsIoptions
         _hdOparamMap = rule394 _lhsIparamMap
         _hdOprefix = rule395 _lhsIprefix
         _hdOsyn = rule396 _lhsIsyn
         _tlOinh = rule397 _lhsIinh
         _tlOnt = rule398 _lhsInt
         _tlOo_case = rule399 _lhsIo_case
         _tlOo_cata = rule400 _lhsIo_cata
         _tlOo_costcentre = rule401 _lhsIo_costcentre
         _tlOo_data = rule402 _lhsIo_data
         _tlOo_linePragmas = rule403 _lhsIo_linePragmas
         _tlOo_monadic = rule404 _lhsIo_monadic
         _tlOo_newtypes = rule405 _lhsIo_newtypes
         _tlOo_pretty = rule406 _lhsIo_pretty
         _tlOo_rename = rule407 _lhsIo_rename
         _tlOo_sem = rule408 _lhsIo_sem
         _tlOo_sig = rule409 _lhsIo_sig
         _tlOo_splitsems = rule410 _lhsIo_splitsems
         _tlOo_strictwrap = rule411 _lhsIo_strictwrap
         _tlOo_traces = rule412 _lhsIo_traces
         _tlOo_unbox = rule413 _lhsIo_unbox
         _tlOoptions = rule414 _lhsIoptions
         _tlOparamMap = rule415 _lhsIparamMap
         _tlOprefix = rule416 _lhsIprefix
         _tlOsyn = rule417 _lhsIsyn
         __result_ = T_CSegments_vOut25 _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule368 #-}
   {-# LINE 287 "./src-ag/GenerateCode.ag" #-}
   rule368 = \ ((_lhsInr) :: Int) ->
                    {-# LINE 287 "./src-ag/GenerateCode.ag" #-}
                    _lhsInr + 1
                    {-# LINE 2921 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule369 #-}
   {-# LINE 300 "./src-ag/GenerateCode.ag" #-}
   rule369 = \  (_ :: ()) ->
                         {-# LINE 300 "./src-ag/GenerateCode.ag" #-}
                         False
                         {-# LINE 2927 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule370 #-}
   {-# LINE 301 "./src-ag/GenerateCode.ag" #-}
   rule370 = \ ((_tlIisNil) :: Bool) ->
                         {-# LINE 301 "./src-ag/GenerateCode.ag" #-}
                         _tlIisNil
                         {-# LINE 2933 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule371 #-}
   rule371 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule372 #-}
   rule372 = \ ((_hdIsemDom) :: [Decl]) ((_tlIsemDom) :: [Decl]) ->
     _hdIsemDom ++ _tlIsemDom
   {-# INLINE rule373 #-}
   rule373 = \ ((_hdIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ((_tlIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
   {-# INLINE rule374 #-}
   rule374 = \ ((_hdIwrapDecls) :: Decls) ((_tlIwrapDecls) :: Decls) ->
     _hdIwrapDecls ++ _tlIwrapDecls
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule411 #-}
   rule411 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_CSegments_Nil #-}
sem_CSegments_Nil ::  T_CSegments 
sem_CSegments_Nil  = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _lhsOisNil :: Bool
         _lhsOisNil = rule418  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule419  ()
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule420  ()
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule421  ()
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule422  ()
         __result_ = T_CSegments_vOut25 _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule418 #-}
   {-# LINE 302 "./src-ag/GenerateCode.ag" #-}
   rule418 = \  (_ :: ()) ->
                       {-# LINE 302 "./src-ag/GenerateCode.ag" #-}
                       True
                       {-# LINE 3100 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule419 #-}
   rule419 = \  (_ :: ()) ->
     []
   {-# INLINE rule420 #-}
   rule420 = \  (_ :: ()) ->
     []
   {-# INLINE rule421 #-}
   rule421 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule422 #-}
   rule422 = \  (_ :: ()) ->
     []

-- CVisit ------------------------------------------------------
-- wrapper
data Inh_CVisit  = Inh_CVisit { allNts_Inh_CVisit :: (Set NontermIdent), allPragmas_Inh_CVisit :: (PragmaMap), aroundMap_Inh_CVisit :: (Set Identifier), children_Inh_CVisit :: ([(Identifier,Type, ChildKind)]), con_Inh_CVisit :: (ConstructorIdent), contextMap_Inh_CVisit :: (ContextMap), decls_Inh_CVisit :: (Decls), inh_Inh_CVisit :: (Attributes), instVisitNrs_Inh_CVisit :: (Map Identifier Int), isLast_Inh_CVisit :: (Bool), mergeMap_Inh_CVisit :: (Map Identifier (Identifier, [Identifier])), nextIntra_Inh_CVisit :: (Exprs), nextIntraVars_Inh_CVisit :: (Set String), nr_Inh_CVisit :: (Int), nt_Inh_CVisit :: (NontermIdent), o_case_Inh_CVisit :: (Bool), o_cata_Inh_CVisit :: (Bool), o_costcentre_Inh_CVisit :: (Bool), o_data_Inh_CVisit :: (Maybe Bool), o_linePragmas_Inh_CVisit :: (Bool), o_monadic_Inh_CVisit :: (Bool), o_newtypes_Inh_CVisit :: (Bool), o_pretty_Inh_CVisit :: (Bool), o_rename_Inh_CVisit :: (Bool), o_sem_Inh_CVisit :: (Bool), o_sig_Inh_CVisit :: (Bool), o_splitsems_Inh_CVisit :: (Bool), o_strictwrap_Inh_CVisit :: (Bool), o_traces_Inh_CVisit :: (Bool), o_unbox_Inh_CVisit :: (Bool), options_Inh_CVisit :: (Options), paramInstMap_Inh_CVisit :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CVisit :: (ParamMap), prefix_Inh_CVisit :: (String), quantMap_Inh_CVisit :: (QuantMap), syn_Inh_CVisit :: (Attributes), terminals_Inh_CVisit :: ([Identifier]), unfoldSemDom_Inh_CVisit :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CVisit :: (Set Identifier), with_sig_Inh_CVisit :: (Bool), wrappers_Inh_CVisit :: (Set NontermIdent) }
data Syn_CVisit  = Syn_CVisit { comments_Syn_CVisit :: ([String]), decls_Syn_CVisit :: (Decls), gatherInstVisitNrs_Syn_CVisit :: (Map Identifier Int), intra_Syn_CVisit :: (Exprs), intraVars_Syn_CVisit :: (Set String), semNames_Syn_CVisit :: ([String]), visitedSet_Syn_CVisit :: (Set Identifier) }
{-# INLINABLE wrap_CVisit #-}
wrap_CVisit :: T_CVisit  -> Inh_CVisit  -> (Syn_CVisit )
wrap_CVisit (T_CVisit act) (Inh_CVisit _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisit_vIn28 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
        (T_CVisit_vOut28 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames _lhsOvisitedSet) <- return (inv_CVisit_s29 sem arg)
        return (Syn_CVisit _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames _lhsOvisitedSet)
   )

-- cata
{-# INLINE sem_CVisit #-}
sem_CVisit :: CVisit  -> T_CVisit 
sem_CVisit ( CVisit inh_ syn_ vss_ intra_ ordered_ ) = sem_CVisit_CVisit inh_ syn_ ( sem_Sequence vss_ ) ( sem_Sequence intra_ ) ordered_

-- semantic domain
newtype T_CVisit  = T_CVisit {
                             attach_T_CVisit :: Identity (T_CVisit_s29 )
                             }
newtype T_CVisit_s29  = C_CVisit_s29 {
                                     inv_CVisit_s29 :: (T_CVisit_v28 )
                                     }
data T_CVisit_s30  = C_CVisit_s30
type T_CVisit_v28  = (T_CVisit_vIn28 ) -> (T_CVisit_vOut28 )
data T_CVisit_vIn28  = T_CVisit_vIn28 (Set NontermIdent) (PragmaMap) (Set Identifier) ([(Identifier,Type, ChildKind)]) (ConstructorIdent) (ContextMap) (Decls) (Attributes) (Map Identifier Int) (Bool) (Map Identifier (Identifier, [Identifier])) (Exprs) (Set String) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (QuantMap) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (Bool) (Set NontermIdent)
data T_CVisit_vOut28  = T_CVisit_vOut28 ([String]) (Decls) (Map Identifier Int) (Exprs) (Set String) ([String]) (Set Identifier)
{-# NOINLINE sem_CVisit_CVisit #-}
sem_CVisit_CVisit :: (Attributes) -> (Attributes) -> T_Sequence  -> T_Sequence  -> (Bool) -> T_CVisit 
sem_CVisit_CVisit arg_inh_ arg_syn_ arg_vss_ arg_intra_ arg_ordered_ = T_CVisit (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_CVisit_v28 
      v28 = \ (T_CVisit_vIn28 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _vssX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_vss_))
         _intraX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_intra_))
         (T_Sequence_vOut46 _vssIallTpsFound _vssIblockDecls _vssIcomments _vssIdecls _vssIdeclsAbove _vssIdefinedInsts _vssIexprs _vssItSigs _vssItps _vssIusedVars _vssIvisitedSet) = inv_Sequence_s47 _vssX47 (T_Sequence_vIn46 _vssOallNts _vssOaroundMap _vssOchildren _vssOcon _vssOdeclsAbove _vssOinh _vssOinstVisitNrs _vssOlastExpr _vssOmergeMap _vssOnr _vssOnt _vssOo_case _vssOo_cata _vssOo_costcentre _vssOo_data _vssOo_linePragmas _vssOo_monadic _vssOo_newtypes _vssOo_pretty _vssOo_rename _vssOo_sem _vssOo_sig _vssOo_splitsems _vssOo_strictwrap _vssOo_traces _vssOo_unbox _vssOoptions _vssOparamInstMap _vssOparamMap _vssOprefix _vssOsyn _vssOterminals _vssOunfoldSemDom _vssOvisitedSet _vssOwhat)
         (T_Sequence_vOut46 _intraIallTpsFound _intraIblockDecls _intraIcomments _intraIdecls _intraIdeclsAbove _intraIdefinedInsts _intraIexprs _intraItSigs _intraItps _intraIusedVars _intraIvisitedSet) = inv_Sequence_s47 _intraX47 (T_Sequence_vIn46 _intraOallNts _intraOaroundMap _intraOchildren _intraOcon _intraOdeclsAbove _intraOinh _intraOinstVisitNrs _intraOlastExpr _intraOmergeMap _intraOnr _intraOnt _intraOo_case _intraOo_cata _intraOo_costcentre _intraOo_data _intraOo_linePragmas _intraOo_monadic _intraOo_newtypes _intraOo_pretty _intraOo_rename _intraOo_sem _intraOo_sig _intraOo_splitsems _intraOo_strictwrap _intraOo_traces _intraOo_unbox _intraOoptions _intraOparamInstMap _intraOparamMap _intraOprefix _intraOsyn _intraOterminals _intraOunfoldSemDom _intraOvisitedSet _intraOwhat)
         _lhsOintra :: Exprs
         _lhsOintra = rule423 _intraIexprs
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule424 _intraIusedVars
         (_higherOrderChildren,_firstOrderChildren) = rule425 _lhsIchildren
         _firstOrderOrig = rule426 _firstOrderChildren
         _funcname = rule427 _lhsIcon _lhsInr _lhsInt _lhsIprefix
         _nextVisitName = rule428 _lhsIisLast _lhsInr _lhsInt _lhsIprefix
         _nextVisitDecl = rule429 _lhsIcon _lhsIdecls _lhsIisLast _lhsInextIntraVars _lhsInr _lhsInt _lhsIprefix _nextVisitName
         _isOneVisit = rule430 _lhsIisLast _lhsInr
         _hasWrappers = rule431 _lhsInt _lhsIwrappers
         _refDecls = rule432 _hasWrappers _isOneVisit _lhsInt _lhsIoptions arg_syn_
         _decls = rule433 _nextVisitDecl _refDecls _typeSigs _vssIdecls
         _vssOlastExpr = rule434 _lhsIo_unbox _nextVisitName arg_inh_ arg_syn_
         _intraOlastExpr = rule435  ()
         _lastExprVars = rule436 _nextVisitName arg_syn_
         (_blockFunDecls,_blockFirstFunCall) = rule437 _funcname _lastExprVars _nextVisitDecl _o_case _vssIblockDecls
         _costCentreDescr = rule438 _lhsIcon _lhsInr _lhsInt
         _addCostCentre = rule439 _costCentreDescr _lhsIo_costcentre
         _params = rule440 _lhsInt _lhsIparamMap
         _semFun = rule441 _addCostCentre _blockFirstFunCall _decls _declsType _firstOrderOrig _funcname _lhsInr _lhsInt _lhsIo_newtypes _lhsIo_unbox _lhsIunfoldSemDom _nextVisitName _o_splitsems _params arg_inh_ arg_ordered_ arg_syn_
         _tsig = rule442 _funcname _semType
         _semType = rule443 _firstOrderOrig _lhsIcontextMap _lhsInr _lhsInt _lhsIquantMap _params
         _lhsOdecls :: Decls
         _lhsOdecls = rule444 _blockFunDecls _lhsIwith_sig _o_splitsems _semFun _tsig arg_ordered_
         _typeSigs = rule445 _lhsIo_sig _o_case _vssItSigs
         _o_do = rule446 _lhsIo_monadic arg_ordered_
         _o_case = rule447 _lhsIallPragmas _lhsIcon _lhsInt _lhsIo_case _o_do arg_ordered_
         _declsType = rule448 _o_case _o_do
         _o_splitsems = rule449 _lhsIo_splitsems arg_ordered_
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule450 _lhsInr _vssIdefinedInsts
         _vssOdeclsAbove = rule451  ()
         _intraOdeclsAbove = rule452  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule453 _intraIcomments _lhsInr _vssIcomments
         _vssOwhat = rule454  ()
         _intraOwhat = rule455  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule456 _funcname
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule457 _intraIvisitedSet
         _vssOallNts = rule458 _lhsIallNts
         _vssOaroundMap = rule459 _lhsIaroundMap
         _vssOchildren = rule460 _lhsIchildren
         _vssOcon = rule461 _lhsIcon
         _vssOinh = rule462 _lhsIinh
         _vssOinstVisitNrs = rule463 _lhsIinstVisitNrs
         _vssOmergeMap = rule464 _lhsImergeMap
         _vssOnr = rule465 _lhsInr
         _vssOnt = rule466 _lhsInt
         _vssOo_case = rule467 _o_case
         _vssOo_cata = rule468 _lhsIo_cata
         _vssOo_costcentre = rule469 _lhsIo_costcentre
         _vssOo_data = rule470 _lhsIo_data
         _vssOo_linePragmas = rule471 _lhsIo_linePragmas
         _vssOo_monadic = rule472 _lhsIo_monadic
         _vssOo_newtypes = rule473 _lhsIo_newtypes
         _vssOo_pretty = rule474 _lhsIo_pretty
         _vssOo_rename = rule475 _lhsIo_rename
         _vssOo_sem = rule476 _lhsIo_sem
         _vssOo_sig = rule477 _lhsIo_sig
         _vssOo_splitsems = rule478 _o_splitsems
         _vssOo_strictwrap = rule479 _lhsIo_strictwrap
         _vssOo_traces = rule480 _lhsIo_traces
         _vssOo_unbox = rule481 _lhsIo_unbox
         _vssOoptions = rule482 _lhsIoptions
         _vssOparamInstMap = rule483 _lhsIparamInstMap
         _vssOparamMap = rule484 _lhsIparamMap
         _vssOprefix = rule485 _lhsIprefix
         _vssOsyn = rule486 _lhsIsyn
         _vssOterminals = rule487 _lhsIterminals
         _vssOunfoldSemDom = rule488 _lhsIunfoldSemDom
         _vssOvisitedSet = rule489 _lhsIvisitedSet
         _intraOallNts = rule490 _lhsIallNts
         _intraOaroundMap = rule491 _lhsIaroundMap
         _intraOchildren = rule492 _lhsIchildren
         _intraOcon = rule493 _lhsIcon
         _intraOinh = rule494 _lhsIinh
         _intraOinstVisitNrs = rule495 _lhsIinstVisitNrs
         _intraOmergeMap = rule496 _lhsImergeMap
         _intraOnr = rule497 _lhsInr
         _intraOnt = rule498 _lhsInt
         _intraOo_case = rule499 _o_case
         _intraOo_cata = rule500 _lhsIo_cata
         _intraOo_costcentre = rule501 _lhsIo_costcentre
         _intraOo_data = rule502 _lhsIo_data
         _intraOo_linePragmas = rule503 _lhsIo_linePragmas
         _intraOo_monadic = rule504 _lhsIo_monadic
         _intraOo_newtypes = rule505 _lhsIo_newtypes
         _intraOo_pretty = rule506 _lhsIo_pretty
         _intraOo_rename = rule507 _lhsIo_rename
         _intraOo_sem = rule508 _lhsIo_sem
         _intraOo_sig = rule509 _lhsIo_sig
         _intraOo_splitsems = rule510 _o_splitsems
         _intraOo_strictwrap = rule511 _lhsIo_strictwrap
         _intraOo_traces = rule512 _lhsIo_traces
         _intraOo_unbox = rule513 _lhsIo_unbox
         _intraOoptions = rule514 _lhsIoptions
         _intraOparamInstMap = rule515 _lhsIparamInstMap
         _intraOparamMap = rule516 _lhsIparamMap
         _intraOprefix = rule517 _lhsIprefix
         _intraOsyn = rule518 _lhsIsyn
         _intraOterminals = rule519 _lhsIterminals
         _intraOunfoldSemDom = rule520 _lhsIunfoldSemDom
         _intraOvisitedSet = rule521 _vssIvisitedSet
         __result_ = T_CVisit_vOut28 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisit_s29 v28
   {-# INLINE rule423 #-}
   {-# LINE 311 "./src-ag/GenerateCode.ag" #-}
   rule423 = \ ((_intraIexprs) :: Exprs) ->
                          {-# LINE 311 "./src-ag/GenerateCode.ag" #-}
                          _intraIexprs
                          {-# LINE 3269 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule424 #-}
   {-# LINE 312 "./src-ag/GenerateCode.ag" #-}
   rule424 = \ ((_intraIusedVars) :: Set String) ->
                              {-# LINE 312 "./src-ag/GenerateCode.ag" #-}
                              _intraIusedVars
                              {-# LINE 3275 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule425 #-}
   {-# LINE 442 "./src-ag/GenerateCode.ag" #-}
   rule425 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
                                                                 {-# LINE 442 "./src-ag/GenerateCode.ag" #-}
                                                                 partition (\(_,_,virt) -> isHigherOrder virt) _lhsIchildren
                                                                 {-# LINE 3281 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule426 #-}
   {-# LINE 443 "./src-ag/GenerateCode.ag" #-}
   rule426 = \ _firstOrderChildren ->
                                   {-# LINE 443 "./src-ag/GenerateCode.ag" #-}
                                   map pickOrigType _firstOrderChildren
                                   {-# LINE 3287 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule427 #-}
   {-# LINE 444 "./src-ag/GenerateCode.ag" #-}
   rule427 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) ->
                             {-# LINE 444 "./src-ag/GenerateCode.ag" #-}
                             seqSemname _lhsIprefix _lhsInt _lhsIcon _lhsInr
                             {-# LINE 3293 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule428 #-}
   {-# LINE 445 "./src-ag/GenerateCode.ag" #-}
   rule428 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) ->
                                  {-# LINE 445 "./src-ag/GenerateCode.ag" #-}
                                  if _lhsIisLast then [] else [visitname _lhsIprefix _lhsInt (_lhsInr+1)]
                                  {-# LINE 3299 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule429 #-}
   {-# LINE 446 "./src-ag/GenerateCode.ag" #-}
   rule429 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsIdecls) :: Decls) ((_lhsIisLast) :: Bool) ((_lhsInextIntraVars) :: Set String) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) _nextVisitName ->
                                  {-# LINE 446 "./src-ag/GenerateCode.ag" #-}
                                  let  lhs = TupleLhs _nextVisitName
                                       rhs = Let _lhsIdecls (SimpleExpr fun)
                                       fun = seqSemname _lhsIprefix _lhsInt _lhsIcon (_lhsInr+1)
                                  in if _lhsIisLast
                                     then []
                                     else [Decl lhs rhs (Set.fromList _nextVisitName) _lhsInextIntraVars]
                                  {-# LINE 3310 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule430 #-}
   {-# LINE 453 "./src-ag/GenerateCode.ag" #-}
   rule430 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ->
                                {-# LINE 453 "./src-ag/GenerateCode.ag" #-}
                                _lhsIisLast && _lhsInr == 0
                                {-# LINE 3316 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule431 #-}
   {-# LINE 454 "./src-ag/GenerateCode.ag" #-}
   rule431 = \ ((_lhsInt) :: NontermIdent) ((_lhsIwrappers) :: Set NontermIdent) ->
                                {-# LINE 454 "./src-ag/GenerateCode.ag" #-}
                                _lhsInt `Set.member` _lhsIwrappers
                                {-# LINE 3322 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule432 #-}
   {-# LINE 455 "./src-ag/GenerateCode.ag" #-}
   rule432 = \ _hasWrappers _isOneVisit ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) syn_ ->
                             {-# LINE 455 "./src-ag/GenerateCode.ag" #-}
                             if _isOneVisit     && _hasWrappers     && reference _lhsIoptions
                             then let synAttrs = Map.toList syn_
                                      synNT = "Syn" ++ "_" ++ getName _lhsInt
                                      synVars = [ SimpleExpr (attrname False _LHS a) | (a,_) <- synAttrs ]
                                      rhs = App synNT synVars
                                      lhs = Fun "___node" []
                                  in [Decl lhs rhs Set.empty Set.empty]
                             else []
                             {-# LINE 3335 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule433 #-}
   {-# LINE 463 "./src-ag/GenerateCode.ag" #-}
   rule433 = \ _nextVisitDecl _refDecls _typeSigs ((_vssIdecls) :: Decls) ->
                          {-# LINE 463 "./src-ag/GenerateCode.ag" #-}
                          _typeSigs ++ _vssIdecls ++ _nextVisitDecl ++ _refDecls
                          {-# LINE 3341 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule434 #-}
   {-# LINE 464 "./src-ag/GenerateCode.ag" #-}
   rule434 = \ ((_lhsIo_unbox) :: Bool) _nextVisitName inh_ syn_ ->
                             {-# LINE 464 "./src-ag/GenerateCode.ag" #-}
                             mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_) $ map (SimpleExpr . lhsname False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                             {-# LINE 3347 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule435 #-}
   {-# LINE 465 "./src-ag/GenerateCode.ag" #-}
   rule435 = \  (_ :: ()) ->
                               {-# LINE 465 "./src-ag/GenerateCode.ag" #-}
                               error "lastExpr: not used here"
                               {-# LINE 3353 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule436 #-}
   {-# LINE 466 "./src-ag/GenerateCode.ag" #-}
   rule436 = \ _nextVisitName syn_ ->
                                 {-# LINE 466 "./src-ag/GenerateCode.ag" #-}
                                 map (lhsname False) (Map.keys syn_) ++ _nextVisitName
                                 {-# LINE 3359 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule437 #-}
   {-# LINE 467 "./src-ag/GenerateCode.ag" #-}
   rule437 = \ _funcname _lastExprVars _nextVisitDecl _o_case ((_vssIblockDecls) :: DeclBlocks) ->
                                                           {-# LINE 467 "./src-ag/GenerateCode.ag" #-}
                                                           mkPartitionedFunction _funcname     _o_case     _nextVisitDecl     _lastExprVars     _vssIblockDecls
                                                           {-# LINE 3365 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule438 #-}
   {-# LINE 469 "./src-ag/GenerateCode.ag" #-}
   rule438 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                    {-# LINE 469 "./src-ag/GenerateCode.ag" #-}
                                    "b" ++ ":" ++ show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show _lhsInr
                                    {-# LINE 3371 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule439 #-}
   {-# LINE 470 "./src-ag/GenerateCode.ag" #-}
   rule439 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                  {-# LINE 470 "./src-ag/GenerateCode.ag" #-}
                                  \v -> if _lhsIo_costcentre
                                        then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                        else v
                                  {-# LINE 3379 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule440 #-}
   {-# LINE 474 "./src-ag/GenerateCode.ag" #-}
   rule440 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                           {-# LINE 474 "./src-ag/GenerateCode.ag" #-}
                           map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                           {-# LINE 3385 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule441 #-}
   {-# LINE 475 "./src-ag/GenerateCode.ag" #-}
   rule441 = \ _addCostCentre _blockFirstFunCall _decls _declsType _firstOrderOrig _funcname ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) _nextVisitName _o_splitsems _params inh_ ordered_ syn_ ->
                           {-# LINE 475 "./src-ag/GenerateCode.ag" #-}
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
                           {-# LINE 3416 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule442 #-}
   {-# LINE 506 "./src-ag/GenerateCode.ag" #-}
   rule442 = \ _funcname _semType ->
                         {-# LINE 506 "./src-ag/GenerateCode.ag" #-}
                         TSig _funcname _semType
                         {-# LINE 3422 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule443 #-}
   {-# LINE 507 "./src-ag/GenerateCode.ag" #-}
   rule443 = \ _firstOrderOrig ((_lhsIcontextMap) :: ContextMap) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIquantMap) :: QuantMap) _params ->
                            {-# LINE 507 "./src-ag/GenerateCode.ag" #-}
                            let argType (NT tp tps _)  r | tp /= _SELF = typeAppStrs (sdtype tp) tps `Arr` r
                                                         | tp == _SELF = error "GenerateCode: found an intra-type with type SELF, which should have been prevented by CRule.tps"
                                argType (Haskell tp) r                 = SimpleType tp          `Arr` r
                                argType _ _ = error "Self type not allowed here"
                                evalTp | null _params     = id
                                       | otherwise        = idEvalType
                            in appQuant _lhsIquantMap _lhsInt $ appContext _lhsIcontextMap _lhsInt $ evalTp $
                               if  _lhsInr == 0
                                   then foldr argType (typeAppStrs (sdtype   _lhsInt        ) _params    ) (map (\(_,t,_) -> t) _firstOrderOrig    )
                                   else foldr argType (typeAppStrs (typeName _lhsInt _lhsInr) _params    ) []
                            {-# LINE 3437 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule444 #-}
   {-# LINE 518 "./src-ag/GenerateCode.ag" #-}
   rule444 = \ _blockFunDecls ((_lhsIwith_sig) :: Bool) _o_splitsems _semFun _tsig ordered_ ->
                           {-# LINE 518 "./src-ag/GenerateCode.ag" #-}
                           ( if  _lhsIwith_sig
                             then [_tsig, _semFun]
                             else [_semFun]
                           ) ++
                           ( if ordered_ && _o_splitsems
                             then _blockFunDecls
                             else []
                           )
                           {-# LINE 3450 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule445 #-}
   {-# LINE 526 "./src-ag/GenerateCode.ag" #-}
   rule445 = \ ((_lhsIo_sig) :: Bool) _o_case ((_vssItSigs) :: [Decl]) ->
                              {-# LINE 526 "./src-ag/GenerateCode.ag" #-}
                              if  _lhsIo_sig && not _o_case
                                  then  _vssItSigs
                                  else  []
                              {-# LINE 3458 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule446 #-}
   {-# LINE 529 "./src-ag/GenerateCode.ag" #-}
   rule446 = \ ((_lhsIo_monadic) :: Bool) ordered_ ->
                           {-# LINE 529 "./src-ag/GenerateCode.ag" #-}
                           ordered_ && _lhsIo_monadic
                           {-# LINE 3464 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule447 #-}
   {-# LINE 530 "./src-ag/GenerateCode.ag" #-}
   rule447 = \ ((_lhsIallPragmas) :: PragmaMap) ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_lhsIo_case) :: Bool) _o_do ordered_ ->
                           {-# LINE 530 "./src-ag/GenerateCode.ag" #-}
                           not _o_do     && _lhsIo_case && ordered_ && not (hasPragma _lhsIallPragmas _lhsInt _lhsIcon _NOCASE)
                           {-# LINE 3470 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule448 #-}
   {-# LINE 531 "./src-ag/GenerateCode.ag" #-}
   rule448 = \ _o_case _o_do ->
                              {-# LINE 531 "./src-ag/GenerateCode.ag" #-}
                              if _o_do
                              then DeclsDo
                              else if _o_case
                                   then DeclsCase
                                   else DeclsLet
                              {-# LINE 3480 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule449 #-}
   {-# LINE 536 "./src-ag/GenerateCode.ag" #-}
   rule449 = \ ((_lhsIo_splitsems) :: Bool) ordered_ ->
                                {-# LINE 536 "./src-ag/GenerateCode.ag" #-}
                                ordered_ && _lhsIo_splitsems
                                {-# LINE 3486 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule450 #-}
   {-# LINE 570 "./src-ag/GenerateCode.ag" #-}
   rule450 = \ ((_lhsInr) :: Int) ((_vssIdefinedInsts) :: [Identifier]) ->
                                 {-# LINE 570 "./src-ag/GenerateCode.ag" #-}
                                 Map.fromList [(i,_lhsInr) | i <- _vssIdefinedInsts]
                                 {-# LINE 3492 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule451 #-}
   {-# LINE 613 "./src-ag/GenerateCode.ag" #-}
   rule451 = \  (_ :: ()) ->
                         {-# LINE 613 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 3498 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 614 "./src-ag/GenerateCode.ag" #-}
   rule452 = \  (_ :: ()) ->
                           {-# LINE 614 "./src-ag/GenerateCode.ag" #-}
                           error "declsAbove: not used here"
                           {-# LINE 3504 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule453 #-}
   {-# LINE 897 "./src-ag/GenerateCode.ag" #-}
   rule453 = \ ((_intraIcomments) :: [String]) ((_lhsInr) :: Int) ((_vssIcomments) :: [String]) ->
                                   {-# LINE 897 "./src-ag/GenerateCode.ag" #-}
                                   let body = map ind (_vssIcomments ++ _intraIcomments)
                                   in if null body
                                      then []
                                      else ("visit " ++ show _lhsInr ++ ":") : body
                                   {-# LINE 3513 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule454 #-}
   {-# LINE 901 "./src-ag/GenerateCode.ag" #-}
   rule454 = \  (_ :: ()) ->
                                  {-# LINE 901 "./src-ag/GenerateCode.ag" #-}
                                  "local"
                                  {-# LINE 3519 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule455 #-}
   {-# LINE 902 "./src-ag/GenerateCode.ag" #-}
   rule455 = \  (_ :: ()) ->
                                  {-# LINE 902 "./src-ag/GenerateCode.ag" #-}
                                  "intra"
                                  {-# LINE 3525 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule456 #-}
   {-# LINE 1164 "./src-ag/GenerateCode.ag" #-}
   rule456 = \ _funcname ->
                       {-# LINE 1164 "./src-ag/GenerateCode.ag" #-}
                       [_funcname    ]
                       {-# LINE 3531 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule457 #-}
   rule457 = \ ((_intraIvisitedSet) :: Set Identifier) ->
     _intraIvisitedSet
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule459 #-}
   rule459 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule460 #-}
   rule460 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule461 #-}
   rule461 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule467 #-}
   rule467 = \ _o_case ->
     _o_case
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule473 #-}
   rule473 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule478 #-}
   rule478 = \ _o_splitsems ->
     _o_splitsems
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule499 #-}
   rule499 = \ _o_case ->
     _o_case
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule510 #-}
   rule510 = \ _o_splitsems ->
     _o_splitsems
   {-# INLINE rule511 #-}
   rule511 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule521 #-}
   rule521 = \ ((_vssIvisitedSet) :: Set Identifier) ->
     _vssIvisitedSet

-- CVisits -----------------------------------------------------
-- wrapper
data Inh_CVisits  = Inh_CVisits { allNts_Inh_CVisits :: (Set NontermIdent), allPragmas_Inh_CVisits :: (PragmaMap), aroundMap_Inh_CVisits :: (Set Identifier), children_Inh_CVisits :: ([(Identifier,Type, ChildKind)]), con_Inh_CVisits :: (ConstructorIdent), contextMap_Inh_CVisits :: (ContextMap), inh_Inh_CVisits :: (Attributes), instVisitNrs_Inh_CVisits :: (Map Identifier Int), mergeMap_Inh_CVisits :: (Map Identifier (Identifier, [Identifier])), nr_Inh_CVisits :: (Int), nt_Inh_CVisits :: (NontermIdent), o_case_Inh_CVisits :: (Bool), o_cata_Inh_CVisits :: (Bool), o_costcentre_Inh_CVisits :: (Bool), o_data_Inh_CVisits :: (Maybe Bool), o_linePragmas_Inh_CVisits :: (Bool), o_monadic_Inh_CVisits :: (Bool), o_newtypes_Inh_CVisits :: (Bool), o_pretty_Inh_CVisits :: (Bool), o_rename_Inh_CVisits :: (Bool), o_sem_Inh_CVisits :: (Bool), o_sig_Inh_CVisits :: (Bool), o_splitsems_Inh_CVisits :: (Bool), o_strictwrap_Inh_CVisits :: (Bool), o_traces_Inh_CVisits :: (Bool), o_unbox_Inh_CVisits :: (Bool), options_Inh_CVisits :: (Options), paramInstMap_Inh_CVisits :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CVisits :: (ParamMap), prefix_Inh_CVisits :: (String), quantMap_Inh_CVisits :: (QuantMap), syn_Inh_CVisits :: (Attributes), terminals_Inh_CVisits :: ([Identifier]), unfoldSemDom_Inh_CVisits :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CVisits :: (Set Identifier), with_sig_Inh_CVisits :: (Bool), wrappers_Inh_CVisits :: (Set NontermIdent) }
data Syn_CVisits  = Syn_CVisits { comments_Syn_CVisits :: ([String]), decls_Syn_CVisits :: (Decls), gatherInstVisitNrs_Syn_CVisits :: (Map Identifier Int), intra_Syn_CVisits :: (Exprs), intraVars_Syn_CVisits :: (Set String), isNil_Syn_CVisits :: (Bool), semNames_Syn_CVisits :: ([String]), visitedSet_Syn_CVisits :: (Set Identifier) }
{-# INLINABLE wrap_CVisits #-}
wrap_CVisits :: T_CVisits  -> Inh_CVisits  -> (Syn_CVisits )
wrap_CVisits (T_CVisits act) (Inh_CVisits _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
        (T_CVisits_vOut31 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet) <- return (inv_CVisits_s32 sem arg)
        return (Syn_CVisits _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet)
   )

-- cata
{-# NOINLINE sem_CVisits #-}
sem_CVisits :: CVisits  -> T_CVisits 
sem_CVisits list = Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list)

-- semantic domain
newtype T_CVisits  = T_CVisits {
                               attach_T_CVisits :: Identity (T_CVisits_s32 )
                               }
newtype T_CVisits_s32  = C_CVisits_s32 {
                                       inv_CVisits_s32 :: (T_CVisits_v31 )
                                       }
data T_CVisits_s33  = C_CVisits_s33
type T_CVisits_v31  = (T_CVisits_vIn31 ) -> (T_CVisits_vOut31 )
data T_CVisits_vIn31  = T_CVisits_vIn31 (Set NontermIdent) (PragmaMap) (Set Identifier) ([(Identifier,Type, ChildKind)]) (ConstructorIdent) (ContextMap) (Attributes) (Map Identifier Int) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (QuantMap) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (Bool) (Set NontermIdent)
data T_CVisits_vOut31  = T_CVisits_vOut31 ([String]) (Decls) (Map Identifier Int) (Exprs) (Set String) (Bool) ([String]) (Set Identifier)
{-# NOINLINE sem_CVisits_Cons #-}
sem_CVisits_Cons :: T_CVisit  -> T_CVisits  -> T_CVisits 
sem_CVisits_Cons arg_hd_ arg_tl_ = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_CVisit (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_tl_))
         (T_CVisit_vOut28 _hdIcomments _hdIdecls _hdIgatherInstVisitNrs _hdIintra _hdIintraVars _hdIsemNames _hdIvisitedSet) = inv_CVisit_s29 _hdX29 (T_CVisit_vIn28 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOchildren _hdOcon _hdOcontextMap _hdOdecls _hdOinh _hdOinstVisitNrs _hdOisLast _hdOmergeMap _hdOnextIntra _hdOnextIntraVars _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwith_sig _hdOwrappers)
         (T_CVisits_vOut31 _tlIcomments _tlIdecls _tlIgatherInstVisitNrs _tlIintra _tlIintraVars _tlIisNil _tlIsemNames _tlIvisitedSet) = inv_CVisits_s32 _tlX32 (T_CVisits_vIn31 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOchildren _tlOcon _tlOcontextMap _tlOinh _tlOinstVisitNrs _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwith_sig _tlOwrappers)
         _tlOnr = rule522 _lhsInr
         _lhsOisNil :: Bool
         _lhsOisNil = rule523  ()
         _hdOisLast = rule524 _tlIisNil
         _hdOnextIntra = rule525 _tlIintra
         _hdOnextIntraVars = rule526 _tlIintraVars
         _lhsOintra :: Exprs
         _lhsOintra = rule527 _hdIintra
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule528 _hdIintraVars
         _lhsOdecls :: Decls
         _lhsOdecls = rule529 _hdIdecls
         _hdOdecls = rule530 _tlIdecls
         _lhsOcomments :: [String]
         _lhsOcomments = rule531 _hdIcomments _tlIcomments
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule532 _hdIgatherInstVisitNrs _tlIgatherInstVisitNrs
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule533 _hdIsemNames _tlIsemNames
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule534 _tlIvisitedSet
         _hdOallNts = rule535 _lhsIallNts
         _hdOallPragmas = rule536 _lhsIallPragmas
         _hdOaroundMap = rule537 _lhsIaroundMap
         _hdOchildren = rule538 _lhsIchildren
         _hdOcon = rule539 _lhsIcon
         _hdOcontextMap = rule540 _lhsIcontextMap
         _hdOinh = rule541 _lhsIinh
         _hdOinstVisitNrs = rule542 _lhsIinstVisitNrs
         _hdOmergeMap = rule543 _lhsImergeMap
         _hdOnr = rule544 _lhsInr
         _hdOnt = rule545 _lhsInt
         _hdOo_case = rule546 _lhsIo_case
         _hdOo_cata = rule547 _lhsIo_cata
         _hdOo_costcentre = rule548 _lhsIo_costcentre
         _hdOo_data = rule549 _lhsIo_data
         _hdOo_linePragmas = rule550 _lhsIo_linePragmas
         _hdOo_monadic = rule551 _lhsIo_monadic
         _hdOo_newtypes = rule552 _lhsIo_newtypes
         _hdOo_pretty = rule553 _lhsIo_pretty
         _hdOo_rename = rule554 _lhsIo_rename
         _hdOo_sem = rule555 _lhsIo_sem
         _hdOo_sig = rule556 _lhsIo_sig
         _hdOo_splitsems = rule557 _lhsIo_splitsems
         _hdOo_strictwrap = rule558 _lhsIo_strictwrap
         _hdOo_traces = rule559 _lhsIo_traces
         _hdOo_unbox = rule560 _lhsIo_unbox
         _hdOoptions = rule561 _lhsIoptions
         _hdOparamInstMap = rule562 _lhsIparamInstMap
         _hdOparamMap = rule563 _lhsIparamMap
         _hdOprefix = rule564 _lhsIprefix
         _hdOquantMap = rule565 _lhsIquantMap
         _hdOsyn = rule566 _lhsIsyn
         _hdOterminals = rule567 _lhsIterminals
         _hdOunfoldSemDom = rule568 _lhsIunfoldSemDom
         _hdOvisitedSet = rule569 _lhsIvisitedSet
         _hdOwith_sig = rule570 _lhsIwith_sig
         _hdOwrappers = rule571 _lhsIwrappers
         _tlOallNts = rule572 _lhsIallNts
         _tlOallPragmas = rule573 _lhsIallPragmas
         _tlOaroundMap = rule574 _lhsIaroundMap
         _tlOchildren = rule575 _lhsIchildren
         _tlOcon = rule576 _lhsIcon
         _tlOcontextMap = rule577 _lhsIcontextMap
         _tlOinh = rule578 _lhsIinh
         _tlOinstVisitNrs = rule579 _lhsIinstVisitNrs
         _tlOmergeMap = rule580 _lhsImergeMap
         _tlOnt = rule581 _lhsInt
         _tlOo_case = rule582 _lhsIo_case
         _tlOo_cata = rule583 _lhsIo_cata
         _tlOo_costcentre = rule584 _lhsIo_costcentre
         _tlOo_data = rule585 _lhsIo_data
         _tlOo_linePragmas = rule586 _lhsIo_linePragmas
         _tlOo_monadic = rule587 _lhsIo_monadic
         _tlOo_newtypes = rule588 _lhsIo_newtypes
         _tlOo_pretty = rule589 _lhsIo_pretty
         _tlOo_rename = rule590 _lhsIo_rename
         _tlOo_sem = rule591 _lhsIo_sem
         _tlOo_sig = rule592 _lhsIo_sig
         _tlOo_splitsems = rule593 _lhsIo_splitsems
         _tlOo_strictwrap = rule594 _lhsIo_strictwrap
         _tlOo_traces = rule595 _lhsIo_traces
         _tlOo_unbox = rule596 _lhsIo_unbox
         _tlOoptions = rule597 _lhsIoptions
         _tlOparamInstMap = rule598 _lhsIparamInstMap
         _tlOparamMap = rule599 _lhsIparamMap
         _tlOprefix = rule600 _lhsIprefix
         _tlOquantMap = rule601 _lhsIquantMap
         _tlOsyn = rule602 _lhsIsyn
         _tlOterminals = rule603 _lhsIterminals
         _tlOunfoldSemDom = rule604 _lhsIunfoldSemDom
         _tlOvisitedSet = rule605 _hdIvisitedSet
         _tlOwith_sig = rule606 _lhsIwith_sig
         _tlOwrappers = rule607 _lhsIwrappers
         __result_ = T_CVisits_vOut31 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule522 #-}
   {-# LINE 283 "./src-ag/GenerateCode.ag" #-}
   rule522 = \ ((_lhsInr) :: Int) ->
                    {-# LINE 283 "./src-ag/GenerateCode.ag" #-}
                    _lhsInr + 1
                    {-# LINE 3871 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule523 #-}
   {-# LINE 296 "./src-ag/GenerateCode.ag" #-}
   rule523 = \  (_ :: ()) ->
                         {-# LINE 296 "./src-ag/GenerateCode.ag" #-}
                         False
                         {-# LINE 3877 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule524 #-}
   {-# LINE 297 "./src-ag/GenerateCode.ag" #-}
   rule524 = \ ((_tlIisNil) :: Bool) ->
                         {-# LINE 297 "./src-ag/GenerateCode.ag" #-}
                         _tlIisNil
                         {-# LINE 3883 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule525 #-}
   {-# LINE 314 "./src-ag/GenerateCode.ag" #-}
   rule525 = \ ((_tlIintra) :: Exprs) ->
                            {-# LINE 314 "./src-ag/GenerateCode.ag" #-}
                            _tlIintra
                            {-# LINE 3889 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule526 #-}
   {-# LINE 315 "./src-ag/GenerateCode.ag" #-}
   rule526 = \ ((_tlIintraVars) :: Set String) ->
                                {-# LINE 315 "./src-ag/GenerateCode.ag" #-}
                                _tlIintraVars
                                {-# LINE 3895 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule527 #-}
   {-# LINE 316 "./src-ag/GenerateCode.ag" #-}
   rule527 = \ ((_hdIintra) :: Exprs) ->
                         {-# LINE 316 "./src-ag/GenerateCode.ag" #-}
                         _hdIintra
                         {-# LINE 3901 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule528 #-}
   {-# LINE 317 "./src-ag/GenerateCode.ag" #-}
   rule528 = \ ((_hdIintraVars) :: Set String) ->
                             {-# LINE 317 "./src-ag/GenerateCode.ag" #-}
                             _hdIintraVars
                             {-# LINE 3907 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule529 #-}
   {-# LINE 432 "./src-ag/GenerateCode.ag" #-}
   rule529 = \ ((_hdIdecls) :: Decls) ->
                        {-# LINE 432 "./src-ag/GenerateCode.ag" #-}
                        _hdIdecls
                        {-# LINE 3913 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule530 #-}
   {-# LINE 433 "./src-ag/GenerateCode.ag" #-}
   rule530 = \ ((_tlIdecls) :: Decls) ->
                        {-# LINE 433 "./src-ag/GenerateCode.ag" #-}
                        _tlIdecls
                        {-# LINE 3919 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule531 #-}
   rule531 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule532 #-}
   rule532 = \ ((_hdIgatherInstVisitNrs) :: Map Identifier Int) ((_tlIgatherInstVisitNrs) :: Map Identifier Int) ->
     _hdIgatherInstVisitNrs `Map.union` _tlIgatherInstVisitNrs
   {-# INLINE rule533 #-}
   rule533 = \ ((_hdIsemNames) :: [String]) ((_tlIsemNames) :: [String]) ->
     _hdIsemNames ++ _tlIsemNames
   {-# INLINE rule534 #-}
   rule534 = \ ((_tlIvisitedSet) :: Set Identifier) ->
     _tlIvisitedSet
   {-# INLINE rule535 #-}
   rule535 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule536 #-}
   rule536 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule537 #-}
   rule537 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule538 #-}
   rule538 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule539 #-}
   rule539 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule540 #-}
   rule540 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule541 #-}
   rule541 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule542 #-}
   rule542 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule543 #-}
   rule543 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule544 #-}
   rule544 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule545 #-}
   rule545 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule546 #-}
   rule546 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule547 #-}
   rule547 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule548 #-}
   rule548 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule549 #-}
   rule549 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule552 #-}
   rule552 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule553 #-}
   rule553 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule554 #-}
   rule554 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule555 #-}
   rule555 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule557 #-}
   rule557 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule558 #-}
   rule558 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule559 #-}
   rule559 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule560 #-}
   rule560 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule561 #-}
   rule561 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule562 #-}
   rule562 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule563 #-}
   rule563 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule564 #-}
   rule564 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule565 #-}
   rule565 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule566 #-}
   rule566 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule567 #-}
   rule567 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule568 #-}
   rule568 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule569 #-}
   rule569 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule570 #-}
   rule570 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule571 #-}
   rule571 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule572 #-}
   rule572 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule573 #-}
   rule573 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule574 #-}
   rule574 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule575 #-}
   rule575 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule577 #-}
   rule577 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule578 #-}
   rule578 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule579 #-}
   rule579 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule580 #-}
   rule580 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule581 #-}
   rule581 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule582 #-}
   rule582 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule583 #-}
   rule583 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule584 #-}
   rule584 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule585 #-}
   rule585 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule586 #-}
   rule586 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule587 #-}
   rule587 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule589 #-}
   rule589 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule590 #-}
   rule590 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule591 #-}
   rule591 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule592 #-}
   rule592 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule593 #-}
   rule593 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule594 #-}
   rule594 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule595 #-}
   rule595 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule600 #-}
   rule600 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule605 #-}
   rule605 = \ ((_hdIvisitedSet) :: Set Identifier) ->
     _hdIvisitedSet
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule607 #-}
   rule607 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CVisits_Nil #-}
sem_CVisits_Nil ::  T_CVisits 
sem_CVisits_Nil  = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOisNil :: Bool
         _lhsOisNil = rule608  ()
         _lhsOintra :: Exprs
         _lhsOintra = rule609  ()
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule610  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule611  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule612  ()
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule613  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule614  ()
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule615 _lhsIvisitedSet
         __result_ = T_CVisits_vOut31 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule608 #-}
   {-# LINE 298 "./src-ag/GenerateCode.ag" #-}
   rule608 = \  (_ :: ()) ->
                       {-# LINE 298 "./src-ag/GenerateCode.ag" #-}
                       True
                       {-# LINE 4182 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule609 #-}
   {-# LINE 318 "./src-ag/GenerateCode.ag" #-}
   rule609 = \  (_ :: ()) ->
                       {-# LINE 318 "./src-ag/GenerateCode.ag" #-}
                       []
                       {-# LINE 4188 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule610 #-}
   {-# LINE 319 "./src-ag/GenerateCode.ag" #-}
   rule610 = \  (_ :: ()) ->
                           {-# LINE 319 "./src-ag/GenerateCode.ag" #-}
                           Set.empty
                           {-# LINE 4194 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule611 #-}
   {-# LINE 431 "./src-ag/GenerateCode.ag" #-}
   rule611 = \  (_ :: ()) ->
                        {-# LINE 431 "./src-ag/GenerateCode.ag" #-}
                        []
                        {-# LINE 4200 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule612 #-}
   rule612 = \  (_ :: ()) ->
     []
   {-# INLINE rule613 #-}
   rule613 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule614 #-}
   rule614 = \  (_ :: ()) ->
     []
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet

-- DeclBlocks --------------------------------------------------
-- wrapper
data Inh_DeclBlocks  = Inh_DeclBlocks { blockNr_Inh_DeclBlocks :: (Int), lastExprVars_Inh_DeclBlocks :: ([String]), nextVisitDecls_Inh_DeclBlocks :: ([Decl]), optCase_Inh_DeclBlocks :: (Bool), prefix_Inh_DeclBlocks :: (String) }
data Syn_DeclBlocks  = Syn_DeclBlocks { callExpr_Syn_DeclBlocks :: (Expr), decls_Syn_DeclBlocks :: ([Decl]), freeVars_Syn_DeclBlocks :: ([String]) }
{-# INLINABLE wrap_DeclBlocks #-}
wrap_DeclBlocks :: T_DeclBlocks  -> Inh_DeclBlocks  -> (Syn_DeclBlocks )
wrap_DeclBlocks (T_DeclBlocks act) (Inh_DeclBlocks _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_DeclBlocks_vIn34 _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix
        (T_DeclBlocks_vOut34 _lhsOcallExpr _lhsOdecls _lhsOfreeVars) <- return (inv_DeclBlocks_s35 sem arg)
        return (Syn_DeclBlocks _lhsOcallExpr _lhsOdecls _lhsOfreeVars)
   )

-- cata
{-# NOINLINE sem_DeclBlocks #-}
sem_DeclBlocks :: DeclBlocks  -> T_DeclBlocks 
sem_DeclBlocks ( DeclBlock defs_ visit_ next_ ) = sem_DeclBlocks_DeclBlock defs_ visit_ ( sem_DeclBlocks next_ )
sem_DeclBlocks ( DeclTerminator defs_ result_ ) = sem_DeclBlocks_DeclTerminator defs_ result_

-- semantic domain
newtype T_DeclBlocks  = T_DeclBlocks {
                                     attach_T_DeclBlocks :: Identity (T_DeclBlocks_s35 )
                                     }
newtype T_DeclBlocks_s35  = C_DeclBlocks_s35 {
                                             inv_DeclBlocks_s35 :: (T_DeclBlocks_v34 )
                                             }
data T_DeclBlocks_s36  = C_DeclBlocks_s36
type T_DeclBlocks_v34  = (T_DeclBlocks_vIn34 ) -> (T_DeclBlocks_vOut34 )
data T_DeclBlocks_vIn34  = T_DeclBlocks_vIn34 (Int) ([String]) ([Decl]) (Bool) (String)
data T_DeclBlocks_vOut34  = T_DeclBlocks_vOut34 (Expr) ([Decl]) ([String])
{-# NOINLINE sem_DeclBlocks_DeclBlock #-}
sem_DeclBlocks_DeclBlock :: ([Decl]) -> (Decl) -> T_DeclBlocks  -> T_DeclBlocks 
sem_DeclBlocks_DeclBlock arg_defs_ arg_visit_ arg_next_ = T_DeclBlocks (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_DeclBlocks_v34 
      v34 = \ (T_DeclBlocks_vIn34 _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) -> ( let
         _nextX35 = Control.Monad.Identity.runIdentity (attach_T_DeclBlocks (arg_next_))
         (T_DeclBlocks_vOut34 _nextIcallExpr _nextIdecls _nextIfreeVars) = inv_DeclBlocks_s35 _nextX35 (T_DeclBlocks_vIn34 _nextOblockNr _nextOlastExprVars _nextOnextVisitDecls _nextOoptCase _nextOprefix)
         _nextOblockNr = rule616 _lhsIblockNr
         _lambdaName = rule617 _lhsIblockNr _lhsIprefix
         _pragmaDecl = rule618 _lambdaName
         _lhsOcallExpr :: Expr
         _lhsOcallExpr = rule619 _freeVars _lambdaName
         _freeVars = rule620 _nextIfreeVars arg_defs_ arg_visit_
         _decl = rule621 _freeVars _lambdaName _lhsIoptCase _nextIcallExpr arg_defs_ arg_visit_
         _lhsOdecls :: [Decl]
         _lhsOdecls = rule622 _decl _lhsIblockNr _nextIdecls _pragmaDecl
         _lhsOfreeVars :: [String]
         _lhsOfreeVars = rule623 _freeVars
         _nextOlastExprVars = rule624 _lhsIlastExprVars
         _nextOnextVisitDecls = rule625 _lhsInextVisitDecls
         _nextOoptCase = rule626 _lhsIoptCase
         _nextOprefix = rule627 _lhsIprefix
         __result_ = T_DeclBlocks_vOut34 _lhsOcallExpr _lhsOdecls _lhsOfreeVars
         in __result_ )
     in C_DeclBlocks_s35 v34
   {-# INLINE rule616 #-}
   {-# LINE 664 "./src-ag/GenerateCode.ag" #-}
   rule616 = \ ((_lhsIblockNr) :: Int) ->
                       {-# LINE 664 "./src-ag/GenerateCode.ag" #-}
                       _lhsIblockNr + 1
                       {-# LINE 4277 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule617 #-}
   {-# LINE 669 "./src-ag/GenerateCode.ag" #-}
   rule617 = \ ((_lhsIblockNr) :: Int) ((_lhsIprefix) :: String) ->
                         {-# LINE 669 "./src-ag/GenerateCode.ag" #-}
                         _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                         {-# LINE 4283 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule618 #-}
   {-# LINE 670 "./src-ag/GenerateCode.ag" #-}
   rule618 = \ _lambdaName ->
                         {-# LINE 670 "./src-ag/GenerateCode.ag" #-}
                         PragmaDecl ("NOINLINE " ++ _lambdaName    )
                         {-# LINE 4289 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule619 #-}
   {-# LINE 671 "./src-ag/GenerateCode.ag" #-}
   rule619 = \ _freeVars _lambdaName ->
                       {-# LINE 671 "./src-ag/GenerateCode.ag" #-}
                       App _lambdaName     (map SimpleExpr _freeVars    )
                       {-# LINE 4295 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule620 #-}
   {-# LINE 675 "./src-ag/GenerateCode.ag" #-}
   rule620 = \ ((_nextIfreeVars) :: [String]) defs_ visit_ ->
                       {-# LINE 675 "./src-ag/GenerateCode.ag" #-}
                       freevars _nextIfreeVars (visit_ : defs_)
                       {-# LINE 4301 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule621 #-}
   {-# LINE 682 "./src-ag/GenerateCode.ag" #-}
   rule621 = \ _freeVars _lambdaName ((_lhsIoptCase) :: Bool) ((_nextIcallExpr) :: Expr) defs_ visit_ ->
                   {-# LINE 682 "./src-ag/GenerateCode.ag" #-}
                   mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ [visit_]) _nextIcallExpr
                   {-# LINE 4307 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule622 #-}
   {-# LINE 683 "./src-ag/GenerateCode.ag" #-}
   rule622 = \ _decl ((_lhsIblockNr) :: Int) ((_nextIdecls) :: [Decl]) _pragmaDecl ->
                    {-# LINE 683 "./src-ag/GenerateCode.ag" #-}
                    (if _lhsIblockNr > 1 then [_pragmaDecl    ] else []) ++ [_decl    ] ++ _nextIdecls
                    {-# LINE 4313 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule623 #-}
   rule623 = \ _freeVars ->
     _freeVars
   {-# INLINE rule624 #-}
   rule624 = \ ((_lhsIlastExprVars) :: [String]) ->
     _lhsIlastExprVars
   {-# INLINE rule625 #-}
   rule625 = \ ((_lhsInextVisitDecls) :: [Decl]) ->
     _lhsInextVisitDecls
   {-# INLINE rule626 #-}
   rule626 = \ ((_lhsIoptCase) :: Bool) ->
     _lhsIoptCase
   {-# INLINE rule627 #-}
   rule627 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
{-# NOINLINE sem_DeclBlocks_DeclTerminator #-}
sem_DeclBlocks_DeclTerminator :: ([Decl]) -> (Expr) -> T_DeclBlocks 
sem_DeclBlocks_DeclTerminator arg_defs_ arg_result_ = T_DeclBlocks (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_DeclBlocks_v34 
      v34 = \ (T_DeclBlocks_vIn34 _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) -> ( let
         _lambdaName = rule628 _lhsIblockNr _lhsIprefix
         _pragmaDecl = rule629 _lambdaName
         _lhsOcallExpr :: Expr
         _lhsOcallExpr = rule630 _freeVars _lambdaName
         _freeVars = rule631 _lhsIlastExprVars _lhsInextVisitDecls arg_defs_
         _lhsOdecls :: [Decl]
         _lhsOdecls = rule632 _freeVars _lambdaName _lhsInextVisitDecls _lhsIoptCase arg_defs_ arg_result_
         _lhsOfreeVars :: [String]
         _lhsOfreeVars = rule633 _freeVars
         __result_ = T_DeclBlocks_vOut34 _lhsOcallExpr _lhsOdecls _lhsOfreeVars
         in __result_ )
     in C_DeclBlocks_s35 v34
   {-# INLINE rule628 #-}
   {-# LINE 669 "./src-ag/GenerateCode.ag" #-}
   rule628 = \ ((_lhsIblockNr) :: Int) ((_lhsIprefix) :: String) ->
                         {-# LINE 669 "./src-ag/GenerateCode.ag" #-}
                         _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                         {-# LINE 4353 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule629 #-}
   {-# LINE 670 "./src-ag/GenerateCode.ag" #-}
   rule629 = \ _lambdaName ->
                         {-# LINE 670 "./src-ag/GenerateCode.ag" #-}
                         PragmaDecl ("NOINLINE " ++ _lambdaName    )
                         {-# LINE 4359 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule630 #-}
   {-# LINE 671 "./src-ag/GenerateCode.ag" #-}
   rule630 = \ _freeVars _lambdaName ->
                       {-# LINE 671 "./src-ag/GenerateCode.ag" #-}
                       App _lambdaName     (map SimpleExpr _freeVars    )
                       {-# LINE 4365 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule631 #-}
   {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
   rule631 = \ ((_lhsIlastExprVars) :: [String]) ((_lhsInextVisitDecls) :: [Decl]) defs_ ->
                       {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
                       freevars _lhsIlastExprVars (defs_ ++ _lhsInextVisitDecls)
                       {-# LINE 4371 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule632 #-}
   {-# LINE 680 "./src-ag/GenerateCode.ag" #-}
   rule632 = \ _freeVars _lambdaName ((_lhsInextVisitDecls) :: [Decl]) ((_lhsIoptCase) :: Bool) defs_ result_ ->
                    {-# LINE 680 "./src-ag/GenerateCode.ag" #-}
                    [ mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ _lhsInextVisitDecls) result_ ]
                    {-# LINE 4377 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule633 #-}
   rule633 = \ _freeVars ->
     _freeVars

-- DeclBlocksRoot ----------------------------------------------
-- wrapper
data Inh_DeclBlocksRoot  = Inh_DeclBlocksRoot { lastExprVars_Inh_DeclBlocksRoot :: ([String]), nextVisitDecls_Inh_DeclBlocksRoot :: ([Decl]), optCase_Inh_DeclBlocksRoot :: (Bool), prefix_Inh_DeclBlocksRoot :: (String) }
data Syn_DeclBlocksRoot  = Syn_DeclBlocksRoot { firstCall_Syn_DeclBlocksRoot :: (Expr), lambdas_Syn_DeclBlocksRoot :: ([Decl]) }
{-# INLINABLE wrap_DeclBlocksRoot #-}
wrap_DeclBlocksRoot :: T_DeclBlocksRoot  -> Inh_DeclBlocksRoot  -> (Syn_DeclBlocksRoot )
wrap_DeclBlocksRoot (T_DeclBlocksRoot act) (Inh_DeclBlocksRoot _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_DeclBlocksRoot_vIn37 _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix
        (T_DeclBlocksRoot_vOut37 _lhsOfirstCall _lhsOlambdas) <- return (inv_DeclBlocksRoot_s38 sem arg)
        return (Syn_DeclBlocksRoot _lhsOfirstCall _lhsOlambdas)
   )

-- cata
{-# INLINE sem_DeclBlocksRoot #-}
sem_DeclBlocksRoot :: DeclBlocksRoot  -> T_DeclBlocksRoot 
sem_DeclBlocksRoot ( DeclBlocksRoot blocks_ ) = sem_DeclBlocksRoot_DeclBlocksRoot ( sem_DeclBlocks blocks_ )

-- semantic domain
newtype T_DeclBlocksRoot  = T_DeclBlocksRoot {
                                             attach_T_DeclBlocksRoot :: Identity (T_DeclBlocksRoot_s38 )
                                             }
newtype T_DeclBlocksRoot_s38  = C_DeclBlocksRoot_s38 {
                                                     inv_DeclBlocksRoot_s38 :: (T_DeclBlocksRoot_v37 )
                                                     }
data T_DeclBlocksRoot_s39  = C_DeclBlocksRoot_s39
type T_DeclBlocksRoot_v37  = (T_DeclBlocksRoot_vIn37 ) -> (T_DeclBlocksRoot_vOut37 )
data T_DeclBlocksRoot_vIn37  = T_DeclBlocksRoot_vIn37 ([String]) ([Decl]) (Bool) (String)
data T_DeclBlocksRoot_vOut37  = T_DeclBlocksRoot_vOut37 (Expr) ([Decl])
{-# NOINLINE sem_DeclBlocksRoot_DeclBlocksRoot #-}
sem_DeclBlocksRoot_DeclBlocksRoot :: T_DeclBlocks  -> T_DeclBlocksRoot 
sem_DeclBlocksRoot_DeclBlocksRoot arg_blocks_ = T_DeclBlocksRoot (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_DeclBlocksRoot_v37 
      v37 = \ (T_DeclBlocksRoot_vIn37 _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) -> ( let
         _blocksX35 = Control.Monad.Identity.runIdentity (attach_T_DeclBlocks (arg_blocks_))
         (T_DeclBlocks_vOut34 _blocksIcallExpr _blocksIdecls _blocksIfreeVars) = inv_DeclBlocks_s35 _blocksX35 (T_DeclBlocks_vIn34 _blocksOblockNr _blocksOlastExprVars _blocksOnextVisitDecls _blocksOoptCase _blocksOprefix)
         _lhsOlambdas :: [Decl]
         _lhsOlambdas = rule634 _blocksIdecls
         _lhsOfirstCall :: Expr
         _lhsOfirstCall = rule635 _blocksIcallExpr
         _blocksOblockNr = rule636  ()
         _blocksOlastExprVars = rule637 _lhsIlastExprVars
         _blocksOnextVisitDecls = rule638 _lhsInextVisitDecls
         _blocksOoptCase = rule639 _lhsIoptCase
         _blocksOprefix = rule640 _lhsIprefix
         __result_ = T_DeclBlocksRoot_vOut37 _lhsOfirstCall _lhsOlambdas
         in __result_ )
     in C_DeclBlocksRoot_s38 v37
   {-# INLINE rule634 #-}
   {-# LINE 655 "./src-ag/GenerateCode.ag" #-}
   rule634 = \ ((_blocksIdecls) :: [Decl]) ->
                       {-# LINE 655 "./src-ag/GenerateCode.ag" #-}
                       _blocksIdecls
                       {-# LINE 4438 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule635 #-}
   {-# LINE 656 "./src-ag/GenerateCode.ag" #-}
   rule635 = \ ((_blocksIcallExpr) :: Expr) ->
                        {-# LINE 656 "./src-ag/GenerateCode.ag" #-}
                        _blocksIcallExpr
                        {-# LINE 4444 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule636 #-}
   {-# LINE 661 "./src-ag/GenerateCode.ag" #-}
   rule636 = \  (_ :: ()) ->
                         {-# LINE 661 "./src-ag/GenerateCode.ag" #-}
                         1
                         {-# LINE 4450 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule637 #-}
   rule637 = \ ((_lhsIlastExprVars) :: [String]) ->
     _lhsIlastExprVars
   {-# INLINE rule638 #-}
   rule638 = \ ((_lhsInextVisitDecls) :: [Decl]) ->
     _lhsInextVisitDecls
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIoptCase) :: Bool) ->
     _lhsIoptCase
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), definedInsts_Syn_Pattern :: ([Identifier]), patternAttributes_Syn_Pattern :: ([(Identifier, Identifier)]) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn40 
        (T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes) <- return (inv_Pattern_s41 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes)
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
data T_Pattern_vIn40  = T_Pattern_vIn40 
data T_Pattern_vOut40  = T_Pattern_vOut40 (Pattern) ([Identifier]) ([(Identifier, Identifier)])
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIdefinedInsts _patsIpatternAttributes) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 )
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule641 _patsIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule642 _patsIpatternAttributes
         _copy = rule643 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule644 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule641 #-}
   rule641 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule642 #-}
   rule642 = \ ((_patsIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patsIpatternAttributes
   {-# INLINE rule643 #-}
   rule643 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule644 #-}
   rule644 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIdefinedInsts _patsIpatternAttributes) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 )
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule645 _patsIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule646 _patsIpatternAttributes
         _copy = rule647 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule648 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule645 #-}
   rule645 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule646 #-}
   rule646 = \ ((_patsIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patsIpatternAttributes
   {-# INLINE rule647 #-}
   rule647 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule648 #-}
   rule648 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIdefinedInsts _patIpatternAttributes) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 )
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule649 _patIdefinedInsts arg_attr_ arg_field_
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule650 _patIpatternAttributes arg_attr_ arg_field_
         _copy = rule651 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule652 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule649 #-}
   {-# LINE 264 "./src-ag/GenerateCode.ag" #-}
   rule649 = \ ((_patIdefinedInsts) :: [Identifier]) attr_ field_ ->
                               {-# LINE 264 "./src-ag/GenerateCode.ag" #-}
                               (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                               {-# LINE 4584 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule650 #-}
   {-# LINE 272 "./src-ag/GenerateCode.ag" #-}
   rule650 = \ ((_patIpatternAttributes) :: [(Identifier, Identifier)]) attr_ field_ ->
                                {-# LINE 272 "./src-ag/GenerateCode.ag" #-}
                                (field_,attr_) : _patIpatternAttributes
                                {-# LINE 4590 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule651 #-}
   rule651 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule652 #-}
   rule652 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIdefinedInsts _patIpatternAttributes) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 )
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule653 _patIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule654 _patIpatternAttributes
         _copy = rule655 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule656 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule653 #-}
   rule653 = \ ((_patIdefinedInsts) :: [Identifier]) ->
     _patIdefinedInsts
   {-# INLINE rule654 #-}
   rule654 = \ ((_patIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patIpatternAttributes
   {-# INLINE rule655 #-}
   rule655 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule656 #-}
   rule656 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule657  ()
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule658  ()
         _copy = rule659 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule660 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule657 #-}
   rule657 = \  (_ :: ()) ->
     []
   {-# INLINE rule658 #-}
   rule658 = \  (_ :: ()) ->
     []
   {-# INLINE rule659 #-}
   rule659 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule660 #-}
   rule660 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns), definedInsts_Syn_Patterns :: ([Identifier]), patternAttributes_Syn_Patterns :: ([(Identifier, Identifier)]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn43 
        (T_Patterns_vOut43 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes) <- return (inv_Patterns_s44 sem arg)
        return (Syn_Patterns _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes)
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
data T_Patterns_vIn43  = T_Patterns_vIn43 
data T_Patterns_vOut43  = T_Patterns_vOut43 (Patterns) ([Identifier]) ([(Identifier, Identifier)])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 ) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIcopy _hdIdefinedInsts _hdIpatternAttributes) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 )
         (T_Patterns_vOut43 _tlIcopy _tlIdefinedInsts _tlIpatternAttributes) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 )
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule661 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule662 _hdIpatternAttributes _tlIpatternAttributes
         _copy = rule663 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule664 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule661 #-}
   rule661 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule662 #-}
   rule662 = \ ((_hdIpatternAttributes) :: [(Identifier, Identifier)]) ((_tlIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _hdIpatternAttributes ++ _tlIpatternAttributes
   {-# INLINE rule663 #-}
   rule663 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule664 #-}
   rule664 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 ) -> ( let
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule665  ()
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule666  ()
         _copy = rule667  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule668 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule665 #-}
   rule665 = \  (_ :: ()) ->
     []
   {-# INLINE rule666 #-}
   rule666 = \  (_ :: ()) ->
     []
   {-# INLINE rule667 #-}
   rule667 = \  (_ :: ()) ->
     []
   {-# INLINE rule668 #-}
   rule668 = \ _copy ->
     _copy

-- Sequence ----------------------------------------------------
-- wrapper
data Inh_Sequence  = Inh_Sequence { allNts_Inh_Sequence :: (Set NontermIdent), aroundMap_Inh_Sequence :: (Set Identifier), children_Inh_Sequence :: ([(Identifier,Type,ChildKind)]), con_Inh_Sequence :: (ConstructorIdent), declsAbove_Inh_Sequence :: ([Decl]), inh_Inh_Sequence :: (Attributes), instVisitNrs_Inh_Sequence :: (Map Identifier Int), lastExpr_Inh_Sequence :: (Expr), mergeMap_Inh_Sequence :: (Map Identifier (Identifier, [Identifier])), nr_Inh_Sequence :: (Int), nt_Inh_Sequence :: (NontermIdent), o_case_Inh_Sequence :: (Bool), o_cata_Inh_Sequence :: (Bool), o_costcentre_Inh_Sequence :: (Bool), o_data_Inh_Sequence :: (Maybe Bool), o_linePragmas_Inh_Sequence :: (Bool), o_monadic_Inh_Sequence :: (Bool), o_newtypes_Inh_Sequence :: (Bool), o_pretty_Inh_Sequence :: (Bool), o_rename_Inh_Sequence :: (Bool), o_sem_Inh_Sequence :: (Bool), o_sig_Inh_Sequence :: (Bool), o_splitsems_Inh_Sequence :: (Bool), o_strictwrap_Inh_Sequence :: (Bool), o_traces_Inh_Sequence :: (Bool), o_unbox_Inh_Sequence :: (Bool), options_Inh_Sequence :: (Options), paramInstMap_Inh_Sequence :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_Sequence :: (ParamMap), prefix_Inh_Sequence :: (String), syn_Inh_Sequence :: (Attributes), terminals_Inh_Sequence :: ([Identifier]), unfoldSemDom_Inh_Sequence :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_Sequence :: (Set Identifier), what_Inh_Sequence :: (String) }
data Syn_Sequence  = Syn_Sequence { allTpsFound_Syn_Sequence :: (Bool), blockDecls_Syn_Sequence :: (DeclBlocks), comments_Syn_Sequence :: ([String]), decls_Syn_Sequence :: (Decls), declsAbove_Syn_Sequence :: ([Decl]), definedInsts_Syn_Sequence :: ([Identifier]), exprs_Syn_Sequence :: (Exprs), tSigs_Syn_Sequence :: ([Decl]), tps_Syn_Sequence :: ([Type]), usedVars_Syn_Sequence :: (Set String), visitedSet_Syn_Sequence :: (Set Identifier) }
{-# INLINABLE wrap_Sequence #-}
wrap_Sequence :: T_Sequence  -> Inh_Sequence  -> (Syn_Sequence )
wrap_Sequence (T_Sequence act) (Inh_Sequence _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
        (T_Sequence_vOut46 _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet) <- return (inv_Sequence_s47 sem arg)
        return (Syn_Sequence _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet)
   )

-- cata
{-# NOINLINE sem_Sequence #-}
sem_Sequence :: Sequence  -> T_Sequence 
sem_Sequence list = Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list)

-- semantic domain
newtype T_Sequence  = T_Sequence {
                                 attach_T_Sequence :: Identity (T_Sequence_s47 )
                                 }
newtype T_Sequence_s47  = C_Sequence_s47 {
                                         inv_Sequence_s47 :: (T_Sequence_v46 )
                                         }
data T_Sequence_s48  = C_Sequence_s48
type T_Sequence_v46  = (T_Sequence_vIn46 ) -> (T_Sequence_vOut46 )
data T_Sequence_vIn46  = T_Sequence_vIn46 (Set NontermIdent) (Set Identifier) ([(Identifier,Type,ChildKind)]) (ConstructorIdent) ([Decl]) (Attributes) (Map Identifier Int) (Expr) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (String)
data T_Sequence_vOut46  = T_Sequence_vOut46 (Bool) (DeclBlocks) ([String]) (Decls) ([Decl]) ([Identifier]) (Exprs) ([Decl]) ([Type]) (Set String) (Set Identifier)
{-# NOINLINE sem_Sequence_Cons #-}
sem_Sequence_Cons :: T_CRule  -> T_Sequence  -> T_Sequence 
sem_Sequence_Cons arg_hd_ arg_tl_ = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_CRule (arg_hd_))
         _tlX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_tl_))
         (T_CRule_vOut19 _hdIallTpsFound _hdIbldBlocksFun _hdIcomments _hdIdecls _hdIdeclsAbove _hdIdefinedInsts _hdIexprs _hdItSigs _hdItps _hdIusedVars _hdIvisitedSet) = inv_CRule_s20 _hdX20 (T_CRule_vIn19 _hdOallNts _hdOaroundMap _hdOchildren _hdOcon _hdOdeclsAbove _hdOinh _hdOinstVisitNrs _hdOmergeMap _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwhat)
         (T_Sequence_vOut46 _tlIallTpsFound _tlIblockDecls _tlIcomments _tlIdecls _tlIdeclsAbove _tlIdefinedInsts _tlIexprs _tlItSigs _tlItps _tlIusedVars _tlIvisitedSet) = inv_Sequence_s47 _tlX47 (T_Sequence_vIn46 _tlOallNts _tlOaroundMap _tlOchildren _tlOcon _tlOdeclsAbove _tlOinh _tlOinstVisitNrs _tlOlastExpr _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwhat)
         _lhsOblockDecls :: DeclBlocks
         _lhsOblockDecls = rule669 _hdIbldBlocksFun _tlIblockDecls
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule670 _hdIallTpsFound _tlIallTpsFound
         _lhsOcomments :: [String]
         _lhsOcomments = rule671 _hdIcomments _tlIcomments
         _lhsOdecls :: Decls
         _lhsOdecls = rule672 _hdIdecls _tlIdecls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule673 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOexprs :: Exprs
         _lhsOexprs = rule674 _hdIexprs _tlIexprs
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule675 _hdItSigs _tlItSigs
         _lhsOtps :: [Type]
         _lhsOtps = rule676 _hdItps _tlItps
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule677 _hdIusedVars _tlIusedVars
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule678 _tlIdeclsAbove
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule679 _tlIvisitedSet
         _hdOallNts = rule680 _lhsIallNts
         _hdOaroundMap = rule681 _lhsIaroundMap
         _hdOchildren = rule682 _lhsIchildren
         _hdOcon = rule683 _lhsIcon
         _hdOdeclsAbove = rule684 _lhsIdeclsAbove
         _hdOinh = rule685 _lhsIinh
         _hdOinstVisitNrs = rule686 _lhsIinstVisitNrs
         _hdOmergeMap = rule687 _lhsImergeMap
         _hdOnr = rule688 _lhsInr
         _hdOnt = rule689 _lhsInt
         _hdOo_case = rule690 _lhsIo_case
         _hdOo_cata = rule691 _lhsIo_cata
         _hdOo_costcentre = rule692 _lhsIo_costcentre
         _hdOo_data = rule693 _lhsIo_data
         _hdOo_linePragmas = rule694 _lhsIo_linePragmas
         _hdOo_monadic = rule695 _lhsIo_monadic
         _hdOo_newtypes = rule696 _lhsIo_newtypes
         _hdOo_pretty = rule697 _lhsIo_pretty
         _hdOo_rename = rule698 _lhsIo_rename
         _hdOo_sem = rule699 _lhsIo_sem
         _hdOo_sig = rule700 _lhsIo_sig
         _hdOo_splitsems = rule701 _lhsIo_splitsems
         _hdOo_strictwrap = rule702 _lhsIo_strictwrap
         _hdOo_traces = rule703 _lhsIo_traces
         _hdOo_unbox = rule704 _lhsIo_unbox
         _hdOoptions = rule705 _lhsIoptions
         _hdOparamInstMap = rule706 _lhsIparamInstMap
         _hdOparamMap = rule707 _lhsIparamMap
         _hdOprefix = rule708 _lhsIprefix
         _hdOsyn = rule709 _lhsIsyn
         _hdOterminals = rule710 _lhsIterminals
         _hdOunfoldSemDom = rule711 _lhsIunfoldSemDom
         _hdOvisitedSet = rule712 _lhsIvisitedSet
         _hdOwhat = rule713 _lhsIwhat
         _tlOallNts = rule714 _lhsIallNts
         _tlOaroundMap = rule715 _lhsIaroundMap
         _tlOchildren = rule716 _lhsIchildren
         _tlOcon = rule717 _lhsIcon
         _tlOdeclsAbove = rule718 _hdIdeclsAbove
         _tlOinh = rule719 _lhsIinh
         _tlOinstVisitNrs = rule720 _lhsIinstVisitNrs
         _tlOlastExpr = rule721 _lhsIlastExpr
         _tlOmergeMap = rule722 _lhsImergeMap
         _tlOnr = rule723 _lhsInr
         _tlOnt = rule724 _lhsInt
         _tlOo_case = rule725 _lhsIo_case
         _tlOo_cata = rule726 _lhsIo_cata
         _tlOo_costcentre = rule727 _lhsIo_costcentre
         _tlOo_data = rule728 _lhsIo_data
         _tlOo_linePragmas = rule729 _lhsIo_linePragmas
         _tlOo_monadic = rule730 _lhsIo_monadic
         _tlOo_newtypes = rule731 _lhsIo_newtypes
         _tlOo_pretty = rule732 _lhsIo_pretty
         _tlOo_rename = rule733 _lhsIo_rename
         _tlOo_sem = rule734 _lhsIo_sem
         _tlOo_sig = rule735 _lhsIo_sig
         _tlOo_splitsems = rule736 _lhsIo_splitsems
         _tlOo_strictwrap = rule737 _lhsIo_strictwrap
         _tlOo_traces = rule738 _lhsIo_traces
         _tlOo_unbox = rule739 _lhsIo_unbox
         _tlOoptions = rule740 _lhsIoptions
         _tlOparamInstMap = rule741 _lhsIparamInstMap
         _tlOparamMap = rule742 _lhsIparamMap
         _tlOprefix = rule743 _lhsIprefix
         _tlOsyn = rule744 _lhsIsyn
         _tlOterminals = rule745 _lhsIterminals
         _tlOunfoldSemDom = rule746 _lhsIunfoldSemDom
         _tlOvisitedSet = rule747 _hdIvisitedSet
         _tlOwhat = rule748 _lhsIwhat
         __result_ = T_Sequence_vOut46 _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_Sequence_s47 v46
   {-# INLINE rule669 #-}
   {-# LINE 624 "./src-ag/GenerateCode.ag" #-}
   rule669 = \ ((_hdIbldBlocksFun) :: DeclBlocks -> DeclBlocks) ((_tlIblockDecls) :: DeclBlocks) ->
                         {-# LINE 624 "./src-ag/GenerateCode.ag" #-}
                         _hdIbldBlocksFun _tlIblockDecls
                         {-# LINE 4891 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule670 #-}
   rule670 = \ ((_hdIallTpsFound) :: Bool) ((_tlIallTpsFound) :: Bool) ->
     _hdIallTpsFound && _tlIallTpsFound
   {-# INLINE rule671 #-}
   rule671 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule672 #-}
   rule672 = \ ((_hdIdecls) :: Decls) ((_tlIdecls) :: Decls) ->
     _hdIdecls ++ _tlIdecls
   {-# INLINE rule673 #-}
   rule673 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule674 #-}
   rule674 = \ ((_hdIexprs) :: Exprs) ((_tlIexprs) :: Exprs) ->
     _hdIexprs ++ _tlIexprs
   {-# INLINE rule675 #-}
   rule675 = \ ((_hdItSigs) :: [Decl]) ((_tlItSigs) :: [Decl]) ->
     _hdItSigs ++ _tlItSigs
   {-# INLINE rule676 #-}
   rule676 = \ ((_hdItps) :: [Type]) ((_tlItps) :: [Type]) ->
     _hdItps ++ _tlItps
   {-# INLINE rule677 #-}
   rule677 = \ ((_hdIusedVars) :: Set String) ((_tlIusedVars) :: Set String) ->
     _hdIusedVars `Set.union` _tlIusedVars
   {-# INLINE rule678 #-}
   rule678 = \ ((_tlIdeclsAbove) :: [Decl]) ->
     _tlIdeclsAbove
   {-# INLINE rule679 #-}
   rule679 = \ ((_tlIvisitedSet) :: Set Identifier) ->
     _tlIvisitedSet
   {-# INLINE rule680 #-}
   rule680 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule681 #-}
   rule681 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule682 #-}
   rule682 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule683 #-}
   rule683 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule684 #-}
   rule684 = \ ((_lhsIdeclsAbove) :: [Decl]) ->
     _lhsIdeclsAbove
   {-# INLINE rule685 #-}
   rule685 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule686 #-}
   rule686 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule687 #-}
   rule687 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule692 #-}
   rule692 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule693 #-}
   rule693 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule694 #-}
   rule694 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule696 #-}
   rule696 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule697 #-}
   rule697 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule698 #-}
   rule698 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule699 #-}
   rule699 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule700 #-}
   rule700 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule701 #-}
   rule701 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule702 #-}
   rule702 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule703 #-}
   rule703 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule704 #-}
   rule704 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule705 #-}
   rule705 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule706 #-}
   rule706 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule707 #-}
   rule707 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsIwhat) :: String) ->
     _lhsIwhat
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule716 #-}
   rule716 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule718 #-}
   rule718 = \ ((_hdIdeclsAbove) :: [Decl]) ->
     _hdIdeclsAbove
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule721 #-}
   rule721 = \ ((_lhsIlastExpr) :: Expr) ->
     _lhsIlastExpr
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule724 #-}
   rule724 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule725 #-}
   rule725 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule729 #-}
   rule729 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule730 #-}
   rule730 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule731 #-}
   rule731 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule732 #-}
   rule732 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule733 #-}
   rule733 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule734 #-}
   rule734 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule735 #-}
   rule735 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule736 #-}
   rule736 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule737 #-}
   rule737 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule738 #-}
   rule738 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule739 #-}
   rule739 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule740 #-}
   rule740 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule741 #-}
   rule741 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule742 #-}
   rule742 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule743 #-}
   rule743 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule744 #-}
   rule744 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule745 #-}
   rule745 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule746 #-}
   rule746 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule747 #-}
   rule747 = \ ((_hdIvisitedSet) :: Set Identifier) ->
     _hdIvisitedSet
   {-# INLINE rule748 #-}
   rule748 = \ ((_lhsIwhat) :: String) ->
     _lhsIwhat
{-# NOINLINE sem_Sequence_Nil #-}
sem_Sequence_Nil ::  T_Sequence 
sem_Sequence_Nil  = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _lhsOblockDecls :: DeclBlocks
         _lhsOblockDecls = rule749 _lhsIdeclsAbove _lhsIlastExpr
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule750  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule751  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule752  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule753  ()
         _lhsOexprs :: Exprs
         _lhsOexprs = rule754  ()
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule755  ()
         _lhsOtps :: [Type]
         _lhsOtps = rule756  ()
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule757  ()
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule758 _lhsIdeclsAbove
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule759 _lhsIvisitedSet
         __result_ = T_Sequence_vOut46 _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_Sequence_s47 v46
   {-# INLINE rule749 #-}
   {-# LINE 626 "./src-ag/GenerateCode.ag" #-}
   rule749 = \ ((_lhsIdeclsAbove) :: [Decl]) ((_lhsIlastExpr) :: Expr) ->
                         {-# LINE 626 "./src-ag/GenerateCode.ag" #-}
                         DeclTerminator _lhsIdeclsAbove _lhsIlastExpr
                         {-# LINE 5166 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule750 #-}
   rule750 = \  (_ :: ()) ->
     True
   {-# INLINE rule751 #-}
   rule751 = \  (_ :: ()) ->
     []
   {-# INLINE rule752 #-}
   rule752 = \  (_ :: ()) ->
     []
   {-# INLINE rule753 #-}
   rule753 = \  (_ :: ()) ->
     []
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
     Set.empty
   {-# INLINE rule758 #-}
   rule758 = \ ((_lhsIdeclsAbove) :: [Decl]) ->
     _lhsIdeclsAbove
   {-# INLINE rule759 #-}
   rule759 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
