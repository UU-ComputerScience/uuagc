{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenerateCode where
{-# LINE 2 "./src-ag/DeclBlocks.ag" #-}

import Code (Decl,Expr)
{-# LINE 9 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 16 "dist/build/GenerateCode.hs" #-}

{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
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
{-# LINE 107 "./src-ag/GenerateCode.ag" #-}

-- remove possible @v references in the types of a data type.
cleanupArg :: Options -> String -> String
cleanupArg opts s
  = case idEvalType opts (SimpleType s) of
      SimpleType s' -> s'
      _             -> error "Only SimpleType supported"
{-# LINE 63 "dist/build/GenerateCode.hs" #-}

{-# LINE 123 "./src-ag/GenerateCode.ag" #-}

appContext :: ContextMap -> NontermIdent -> Code.Type -> Code.Type
appContext mp nt tp
  = maybe tp (\ctx -> CtxApp (map (\(n,ns) -> (getName n, ns)) ctx) tp) $ Map.lookup nt mp

appQuant :: QuantMap -> NontermIdent -> Code.Type -> Code.Type
appQuant mp nt tp
  = foldr QuantApp tp $ Map.findWithDefault [] nt mp
{-# LINE 74 "dist/build/GenerateCode.hs" #-}

{-# LINE 248 "./src-ag/GenerateCode.ag" #-}

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

{-# LINE 541 "./src-ag/GenerateCode.ag" #-}

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

{-# LINE 638 "./src-ag/GenerateCode.ag" #-}

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

{-# LINE 688 "./src-ag/GenerateCode.ag" #-}

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

{-# LINE 766 "./src-ag/GenerateCode.ag" #-}

typeToCodeType :: Maybe NontermIdent -> [String] -> Type -> Code.Type
typeToCodeType _ _ tp
  = case tp of
      NT nt tps defor -> NontermType (getName nt) tps defor
      Haskell t       -> SimpleType t
      Self            -> error "Self type not allowed here."

evalType :: Options -> (String -> String) -> Code.Type -> Code.Type
evalType opts replf t'
  = chase t'
  where
    chase t
      = case t of
          Arr l r              -> Arr (chase l) (chase r)
          TypeApp f as         -> TypeApp (chase f) (map chase as)
          TupleType tps        -> TupleType (map chase tps)
          UnboxedTupleType tps -> UnboxedTupleType (map chase tps)
          Code.List tp         -> Code.List (chase tp)
          SimpleType txt       -> let tks  = lexTokens opts (initPos txt) txt
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

idEvalType :: Options -> Code.Type -> Code.Type
idEvalType options = evalType options id
{-# LINE 189 "dist/build/GenerateCode.hs" #-}

{-# LINE 891 "./src-ag/GenerateCode.ag" #-}

-- for a virtual child that already existed as a child, returns
isFirstOrder :: ChildKind -> Type -> Maybe Type
isFirstOrder ChildSyntax       tp = Just tp
isFirstOrder ChildAttr         _  = Nothing
isFirstOrder (ChildReplace tp) _  = Just tp
{-# LINE 198 "dist/build/GenerateCode.hs" #-}

{-# LINE 912 "./src-ag/GenerateCode.ag" #-}

makeLocalComment :: Int -> String -> Identifier -> Maybe Type -> String
makeLocalComment width what  name tp = let  x = getName name
                                            y = maybe "_" (\t -> case t of
					      	      (NT nt tps _) -> getName nt ++ " " ++ unwords tps
					      	      Haskell t' -> '{' : t' ++ "}"
						      Self -> error "Self type not allowed here.") tp
                                       in   ( what ++ " " ++ x ++ replicate ((width - length x) `max` 0) ' ' ++ " : " ++ y )

{-# LINE 210 "dist/build/GenerateCode.hs" #-}

{-# LINE 946 "./src-ag/GenerateCode.ag" #-}

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

{-# LINE 1036 "./src-ag/GenerateCode.ag" #-}

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
         (T_CNonterminals_vOut10 _nontsIchunks _nontsIgathNts _nontsIsemDomUnfoldGath) = inv_CNonterminals_s11 _nontsX11 (T_CNonterminals_vIn10 _nontsOallNts _nontsOallPragmas _nontsOaroundMap _nontsOcontextMap _nontsOderivings _nontsOmergeMap _nontsOo_case _nontsOo_cata _nontsOo_clean _nontsOo_costcentre _nontsOo_data _nontsOo_linePragmas _nontsOo_monadic _nontsOo_newtypes _nontsOo_pretty _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_splitsems _nontsOo_strictwrap _nontsOo_traces _nontsOo_unbox _nontsOoptions _nontsOparamMap _nontsOprefix _nontsOquantMap _nontsOtypeSyns _nontsOunfoldSemDom _nontsOwith_sig _nontsOwrappers)
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
         _nontsOo_clean = rule16 _lhsIoptions
         _options = rule17 _lhsIoptions arg_multivisit_
         _nontsOallPragmas = rule18 arg_pragmas_
         _nontsOparamMap = rule19 arg_paramMap_
         _nontsOcontextMap = rule20 arg_contextMap_
         _nontsOquantMap = rule21 arg_quantMap_
         _nontsOallNts = rule22 _nontsIgathNts
         _aroundMap = rule23 arg_aroundsMap_
         _mergeMap = rule24 arg_mergeMap_
         _unfoldSemDom = rule25 _lhsIoptions _nontsIsemDomUnfoldGath
         _nontsOwith_sig = rule26 _lhsIoptions
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule27  ()
         _lhsOoutput :: Program
         _lhsOoutput = rule28 _nontsIchunks arg_multivisit_
         _nontsOtypeSyns = rule29 arg_typeSyns_
         _nontsOderivings = rule30 arg_derivings_
         _nontsOwrappers = rule31 arg_wrappers_
         _nontsOaroundMap = rule32 _aroundMap
         _nontsOmergeMap = rule33 _mergeMap
         _nontsOoptions = rule34 _options
         _nontsOunfoldSemDom = rule35 _unfoldSemDom
         __result_ = T_CGrammar_vOut1 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_CGrammar_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 52 "./src-ag/GenerateCode.ag" #-}
   rule0 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 52 "./src-ag/GenerateCode.ag" #-}
                                        typeSigs       _lhsIoptions
                                        {-# LINE 351 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 53 "./src-ag/GenerateCode.ag" #-}
   rule1 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 53 "./src-ag/GenerateCode.ag" #-}
                                        folds          _lhsIoptions
                                        {-# LINE 357 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 54 "./src-ag/GenerateCode.ag" #-}
   rule2 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 54 "./src-ag/GenerateCode.ag" #-}
                                        semfuns        _lhsIoptions
                                        {-# LINE 363 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 55 "./src-ag/GenerateCode.ag" #-}
   rule3 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 55 "./src-ag/GenerateCode.ag" #-}
                                        newtypes       _lhsIoptions
                                        {-# LINE 369 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 56 "./src-ag/GenerateCode.ag" #-}
   rule4 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 56 "./src-ag/GenerateCode.ag" #-}
                                        unbox          _lhsIoptions
                                        {-# LINE 375 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 57 "./src-ag/GenerateCode.ag" #-}
   rule5 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 57 "./src-ag/GenerateCode.ag" #-}
                                        cases          _lhsIoptions
                                        {-# LINE 381 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 58 "./src-ag/GenerateCode.ag" #-}
   rule6 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 58 "./src-ag/GenerateCode.ag" #-}
                                        attrInfo       _lhsIoptions
                                        {-# LINE 387 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 59 "./src-ag/GenerateCode.ag" #-}
   rule7 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 59 "./src-ag/GenerateCode.ag" #-}
                                        rename         _lhsIoptions
                                        {-# LINE 393 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 60 "./src-ag/GenerateCode.ag" #-}
   rule8 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 60 "./src-ag/GenerateCode.ag" #-}
                                        strictWrap     _lhsIoptions
                                        {-# LINE 399 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 61 "./src-ag/GenerateCode.ag" #-}
   rule9 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 61 "./src-ag/GenerateCode.ag" #-}
                                        splitSems      _lhsIoptions
                                        {-# LINE 405 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 62 "./src-ag/GenerateCode.ag" #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 62 "./src-ag/GenerateCode.ag" #-}
                                        if dataTypes _lhsIoptions then Just (strictData _lhsIoptions) else Nothing
                                        {-# LINE 411 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 63 "./src-ag/GenerateCode.ag" #-}
   rule11 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 63 "./src-ag/GenerateCode.ag" #-}
                                        prefix         _lhsIoptions
                                        {-# LINE 417 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 64 "./src-ag/GenerateCode.ag" #-}
   rule12 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 64 "./src-ag/GenerateCode.ag" #-}
                                        genTraces      _lhsIoptions
                                        {-# LINE 423 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 65 "./src-ag/GenerateCode.ag" #-}
   rule13 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 65 "./src-ag/GenerateCode.ag" #-}
                                        genCostCentres _lhsIoptions
                                        {-# LINE 429 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 66 "./src-ag/GenerateCode.ag" #-}
   rule14 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 66 "./src-ag/GenerateCode.ag" #-}
                                        genLinePragmas _lhsIoptions
                                        {-# LINE 435 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule15 #-}
   {-# LINE 67 "./src-ag/GenerateCode.ag" #-}
   rule15 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 67 "./src-ag/GenerateCode.ag" #-}
                                        monadic        _lhsIoptions
                                        {-# LINE 441 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule16 #-}
   {-# LINE 68 "./src-ag/GenerateCode.ag" #-}
   rule16 = \ ((_lhsIoptions) :: Options) ->
                                        {-# LINE 68 "./src-ag/GenerateCode.ag" #-}
                                        clean          _lhsIoptions
                                        {-# LINE 447 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule17 #-}
   {-# LINE 71 "./src-ag/GenerateCode.ag" #-}
   rule17 = \ ((_lhsIoptions) :: Options) multivisit_ ->
                  {-# LINE 71 "./src-ag/GenerateCode.ag" #-}
                  _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && multivisit_ }
                  {-# LINE 453 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule18 #-}
   {-# LINE 76 "./src-ag/GenerateCode.ag" #-}
   rule18 = \ pragmas_ ->
                                   {-# LINE 76 "./src-ag/GenerateCode.ag" #-}
                                   pragmas_
                                   {-# LINE 459 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule19 #-}
   {-# LINE 98 "./src-ag/GenerateCode.ag" #-}
   rule19 = \ paramMap_ ->
                                {-# LINE 98 "./src-ag/GenerateCode.ag" #-}
                                paramMap_
                                {-# LINE 465 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule20 #-}
   {-# LINE 120 "./src-ag/GenerateCode.ag" #-}
   rule20 = \ contextMap_ ->
                           {-# LINE 120 "./src-ag/GenerateCode.ag" #-}
                           contextMap_
                           {-# LINE 471 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule21 #-}
   {-# LINE 121 "./src-ag/GenerateCode.ag" #-}
   rule21 = \ quantMap_ ->
                           {-# LINE 121 "./src-ag/GenerateCode.ag" #-}
                           quantMap_
                           {-# LINE 477 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule22 #-}
   {-# LINE 137 "./src-ag/GenerateCode.ag" #-}
   rule22 = \ ((_nontsIgathNts) :: Set NontermIdent) ->
                       {-# LINE 137 "./src-ag/GenerateCode.ag" #-}
                       _nontsIgathNts
                       {-# LINE 483 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule23 #-}
   {-# LINE 587 "./src-ag/GenerateCode.ag" #-}
   rule23 = \ aroundsMap_ ->
                                                   {-# LINE 587 "./src-ag/GenerateCode.ag" #-}
                                                   aroundsMap_
                                                   {-# LINE 489 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule24 #-}
   {-# LINE 603 "./src-ag/GenerateCode.ag" #-}
   rule24 = \ mergeMap_ ->
                                                  {-# LINE 603 "./src-ag/GenerateCode.ag" #-}
                                                  mergeMap_
                                                  {-# LINE 495 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule25 #-}
   {-# LINE 760 "./src-ag/GenerateCode.ag" #-}
   rule25 = \ ((_lhsIoptions) :: Options) ((_nontsIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
         {-# LINE 760 "./src-ag/GenerateCode.ag" #-}
         \nt nr repl ->
          let (params, tp) = Map.findWithDefault (error ("No such semantic domain: " ++ show nt)) (nt, nr) _nontsIsemDomUnfoldGath
              replMap = Map.fromList (zip params repl)
              replace k = Map.findWithDefault ('@':k) k replMap
          in evalType _lhsIoptions replace tp
         {-# LINE 505 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule26 #-}
   {-# LINE 860 "./src-ag/GenerateCode.ag" #-}
   rule26 = \ ((_lhsIoptions) :: Options) ->
                                {-# LINE 860 "./src-ag/GenerateCode.ag" #-}
                                typeSigs _lhsIoptions
                                {-# LINE 511 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule27 #-}
   {-# LINE 863 "./src-ag/GenerateCode.ag" #-}
   rule27 = \  (_ :: ()) ->
                             {-# LINE 863 "./src-ag/GenerateCode.ag" #-}
                             Seq.empty
                             {-# LINE 517 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule28 #-}
   {-# LINE 932 "./src-ag/GenerateCode.ag" #-}
   rule28 = \ ((_nontsIchunks) :: Chunks) multivisit_ ->
                               {-# LINE 932 "./src-ag/GenerateCode.ag" #-}
                               Program _nontsIchunks multivisit_
                               {-# LINE 523 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule29 #-}
   {-# LINE 1000 "./src-ag/GenerateCode.ag" #-}
   rule29 = \ typeSyns_ ->
                                   {-# LINE 1000 "./src-ag/GenerateCode.ag" #-}
                                   typeSyns_
                                   {-# LINE 529 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule30 #-}
   {-# LINE 1001 "./src-ag/GenerateCode.ag" #-}
   rule30 = \ derivings_ ->
                                   {-# LINE 1001 "./src-ag/GenerateCode.ag" #-}
                                   derivings_
                                   {-# LINE 535 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule31 #-}
   {-# LINE 1002 "./src-ag/GenerateCode.ag" #-}
   rule31 = \ wrappers_ ->
                                   {-# LINE 1002 "./src-ag/GenerateCode.ag" #-}
                                   wrappers_
                                   {-# LINE 541 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule32 #-}
   rule32 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule33 #-}
   rule33 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule34 #-}
   rule34 = \ _options ->
     _options
   {-# INLINE rule35 #-}
   rule35 = \ _unfoldSemDom ->
     _unfoldSemDom

-- CInterface --------------------------------------------------
-- wrapper
data Inh_CInterface  = Inh_CInterface { inh_Inh_CInterface :: (Attributes), nt_Inh_CInterface :: (NontermIdent), o_case_Inh_CInterface :: (Bool), o_cata_Inh_CInterface :: (Bool), o_clean_Inh_CInterface :: (Bool), o_costcentre_Inh_CInterface :: (Bool), o_data_Inh_CInterface :: (Maybe Bool), o_linePragmas_Inh_CInterface :: (Bool), o_monadic_Inh_CInterface :: (Bool), o_newtypes_Inh_CInterface :: (Bool), o_pretty_Inh_CInterface :: (Bool), o_rename_Inh_CInterface :: (Bool), o_sem_Inh_CInterface :: (Bool), o_sig_Inh_CInterface :: (Bool), o_splitsems_Inh_CInterface :: (Bool), o_strictwrap_Inh_CInterface :: (Bool), o_traces_Inh_CInterface :: (Bool), o_unbox_Inh_CInterface :: (Bool), options_Inh_CInterface :: (Options), paramMap_Inh_CInterface :: (ParamMap), prefix_Inh_CInterface :: (String), syn_Inh_CInterface :: (Attributes) }
data Syn_CInterface  = Syn_CInterface { comments_Syn_CInterface :: ([String]), semDom_Syn_CInterface :: ([Decl]), semDomUnfoldGath_Syn_CInterface :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CInterface :: (Decls) }
{-# INLINABLE wrap_CInterface #-}
wrap_CInterface :: T_CInterface  -> Inh_CInterface  -> (Syn_CInterface )
wrap_CInterface (T_CInterface act) (Inh_CInterface _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CInterface_vIn4 _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
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
data T_CInterface_vIn4  = T_CInterface_vIn4 (Attributes) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CInterface_vOut4  = T_CInterface_vOut4 ([String]) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CInterface_CInterface #-}
sem_CInterface_CInterface :: T_CSegments  -> T_CInterface 
sem_CInterface_CInterface arg_seg_ = T_CInterface (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_CInterface_v4 
      v4 = \ (T_CInterface_vIn4 _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _segX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_seg_))
         (T_CSegments_vOut25 _segIcomments _segIisNil _segIsemDom _segIsemDomUnfoldGath _segIwrapDecls) = inv_CSegments_s26 _segX26 (T_CSegments_vIn25 _segOinh _segOnr _segOnt _segOo_case _segOo_cata _segOo_clean _segOo_costcentre _segOo_data _segOo_linePragmas _segOo_monadic _segOo_newtypes _segOo_pretty _segOo_rename _segOo_sem _segOo_sig _segOo_splitsems _segOo_strictwrap _segOo_traces _segOo_unbox _segOoptions _segOparamMap _segOprefix _segOsyn)
         _segOnr = rule36  ()
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule37 _segIsemDom
         _lhsOcomments :: [String]
         _lhsOcomments = rule38 _segIcomments
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule39 _segIsemDomUnfoldGath
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule40 _segIwrapDecls
         _segOinh = rule41 _lhsIinh
         _segOnt = rule42 _lhsInt
         _segOo_case = rule43 _lhsIo_case
         _segOo_cata = rule44 _lhsIo_cata
         _segOo_clean = rule45 _lhsIo_clean
         _segOo_costcentre = rule46 _lhsIo_costcentre
         _segOo_data = rule47 _lhsIo_data
         _segOo_linePragmas = rule48 _lhsIo_linePragmas
         _segOo_monadic = rule49 _lhsIo_monadic
         _segOo_newtypes = rule50 _lhsIo_newtypes
         _segOo_pretty = rule51 _lhsIo_pretty
         _segOo_rename = rule52 _lhsIo_rename
         _segOo_sem = rule53 _lhsIo_sem
         _segOo_sig = rule54 _lhsIo_sig
         _segOo_splitsems = rule55 _lhsIo_splitsems
         _segOo_strictwrap = rule56 _lhsIo_strictwrap
         _segOo_traces = rule57 _lhsIo_traces
         _segOo_unbox = rule58 _lhsIo_unbox
         _segOoptions = rule59 _lhsIoptions
         _segOparamMap = rule60 _lhsIparamMap
         _segOprefix = rule61 _lhsIprefix
         _segOsyn = rule62 _lhsIsyn
         __result_ = T_CInterface_vOut4 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CInterface_s5 v4
   {-# INLINE rule36 #-}
   {-# LINE 286 "./src-ag/GenerateCode.ag" #-}
   rule36 = \  (_ :: ()) ->
                           {-# LINE 286 "./src-ag/GenerateCode.ag" #-}
                           0
                           {-# LINE 633 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule37 #-}
   {-# LINE 717 "./src-ag/GenerateCode.ag" #-}
   rule37 = \ ((_segIsemDom) :: [Decl]) ->
                                {-# LINE 717 "./src-ag/GenerateCode.ag" #-}
                                Comment "semantic domain" : _segIsemDom
                                {-# LINE 639 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule38 #-}
   rule38 = \ ((_segIcomments) :: [String]) ->
     _segIcomments
   {-# INLINE rule39 #-}
   rule39 = \ ((_segIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _segIsemDomUnfoldGath
   {-# INLINE rule40 #-}
   rule40 = \ ((_segIwrapDecls) :: Decls) ->
     _segIwrapDecls
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn

-- CNonterminal ------------------------------------------------
-- wrapper
data Inh_CNonterminal  = Inh_CNonterminal { allNts_Inh_CNonterminal :: (Set NontermIdent), allPragmas_Inh_CNonterminal :: (PragmaMap), aroundMap_Inh_CNonterminal :: (Map NontermIdent (Map ConstructorIdent (Set Identifier))), contextMap_Inh_CNonterminal :: (ContextMap), derivings_Inh_CNonterminal :: (Derivings), mergeMap_Inh_CNonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))), o_case_Inh_CNonterminal :: (Bool), o_cata_Inh_CNonterminal :: (Bool), o_clean_Inh_CNonterminal :: (Bool), o_costcentre_Inh_CNonterminal :: (Bool), o_data_Inh_CNonterminal :: (Maybe Bool), o_linePragmas_Inh_CNonterminal :: (Bool), o_monadic_Inh_CNonterminal :: (Bool), o_newtypes_Inh_CNonterminal :: (Bool), o_pretty_Inh_CNonterminal :: (Bool), o_rename_Inh_CNonterminal :: (Bool), o_sem_Inh_CNonterminal :: (Bool), o_sig_Inh_CNonterminal :: (Bool), o_splitsems_Inh_CNonterminal :: (Bool), o_strictwrap_Inh_CNonterminal :: (Bool), o_traces_Inh_CNonterminal :: (Bool), o_unbox_Inh_CNonterminal :: (Bool), options_Inh_CNonterminal :: (Options), paramMap_Inh_CNonterminal :: (ParamMap), prefix_Inh_CNonterminal :: (String), quantMap_Inh_CNonterminal :: (QuantMap), typeSyns_Inh_CNonterminal :: (TypeSyns), unfoldSemDom_Inh_CNonterminal :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CNonterminal :: (Bool), wrappers_Inh_CNonterminal :: (Set NontermIdent) }
data Syn_CNonterminal  = Syn_CNonterminal { chunks_Syn_CNonterminal :: (Chunks), gathNts_Syn_CNonterminal :: (Set NontermIdent), semDomUnfoldGath_Syn_CNonterminal :: (Map (NontermIdent, Int) ([String], Code.Type)) }
{-# INLINABLE wrap_CNonterminal #-}
wrap_CNonterminal :: T_CNonterminal  -> Inh_CNonterminal  -> (Syn_CNonterminal )
wrap_CNonterminal (T_CNonterminal act) (Inh_CNonterminal _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminal_vIn7 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
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
data T_CNonterminal_vIn7  = T_CNonterminal_vIn7 (Set NontermIdent) (PragmaMap) (Map NontermIdent (Map ConstructorIdent (Set Identifier))) (ContextMap) (Derivings) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (TypeSyns) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CNonterminal_vOut7  = T_CNonterminal_vOut7 (Chunks) (Set NontermIdent) (Map (NontermIdent, Int) ([String], Code.Type))
{-# NOINLINE sem_CNonterminal_CNonterminal #-}
sem_CNonterminal_CNonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_CProductions  -> T_CInterface  -> T_CNonterminal 
sem_CNonterminal_CNonterminal arg_nt_ arg_params_ arg_inh_ arg_syn_ arg_prods_ arg_inter_ = T_CNonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_CNonterminal_v7 
      v7 = \ (T_CNonterminal_vIn7 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_prods_))
         _interX5 = Control.Monad.Identity.runIdentity (attach_T_CInterface (arg_inter_))
         (T_CProductions_vOut16 _prodsIcataAlts _prodsIcomments _prodsIdataAlts _prodsIdecls _prodsIsemNames) = inv_CProductions_s17 _prodsX17 (T_CProductions_vIn16 _prodsOallNts _prodsOallPragmas _prodsOaroundMap _prodsOcontextMap _prodsOinh _prodsOmergeMap _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_clean _prodsOo_costcentre _prodsOo_data _prodsOo_linePragmas _prodsOo_monadic _prodsOo_newtypes _prodsOo_pretty _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_splitsems _prodsOo_strictwrap _prodsOo_traces _prodsOo_unbox _prodsOoptions _prodsOparamMap _prodsOprefix _prodsOquantMap _prodsOsyn _prodsOunfoldSemDom _prodsOwith_sig _prodsOwrappers)
         (T_CInterface_vOut4 _interIcomments _interIsemDom _interIsemDomUnfoldGath _interIwrapDecls) = inv_CInterface_s5 _interX5 (T_CInterface_vIn4 _interOinh _interOnt _interOo_case _interOo_cata _interOo_clean _interOo_costcentre _interOo_data _interOo_linePragmas _interOo_monadic _interOo_newtypes _interOo_pretty _interOo_rename _interOo_sem _interOo_sig _interOo_splitsems _interOo_strictwrap _interOo_traces _interOo_unbox _interOoptions _interOparamMap _interOprefix _interOsyn)
         (_interOinh,_interOsyn,_interOnt) = rule63 arg_inh_ arg_nt_ arg_syn_
         (_prodsOinh,_prodsOsyn,_prodsOnt) = rule64 arg_inh_ arg_nt_ arg_syn_
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule65 arg_nt_
         _aroundMap = rule66 _lhsIaroundMap arg_nt_
         _mergeMap = rule67 _lhsImergeMap arg_nt_
         _semWrapper = rule68 _interIwrapDecls _lhsIo_newtypes _lhsIo_strictwrap _lhsIoptions arg_inh_ arg_nt_ arg_params_ arg_syn_
         _comment = rule69 _interIcomments _prodsIcomments
         _lhsOchunks :: Chunks
         _lhsOchunks = rule70 _cataFun _comment _dataDef _genCata _interIsemDom _lhsIo_cata _lhsIo_data _lhsIo_pretty _lhsIo_sem _lhsIo_sig _lhsIwrappers _prodsIdecls _prodsIsemNames _semWrapper arg_nt_
         _dataDef = rule71 _lhsIderivings _lhsIo_data _lhsIoptions _lhsItypeSyns _prodsIdataAlts arg_nt_ arg_params_
         _genCata = rule72 _lhsIoptions arg_nt_
         _cataFun = rule73 _lhsIcontextMap _lhsIo_sig _lhsIoptions _lhsIprefix _lhsIquantMap _lhsItypeSyns _prodsIcataAlts arg_nt_ arg_params_
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule74 _interIsemDomUnfoldGath
         _prodsOallNts = rule75 _lhsIallNts
         _prodsOallPragmas = rule76 _lhsIallPragmas
         _prodsOaroundMap = rule77 _aroundMap
         _prodsOcontextMap = rule78 _lhsIcontextMap
         _prodsOmergeMap = rule79 _mergeMap
         _prodsOo_case = rule80 _lhsIo_case
         _prodsOo_cata = rule81 _lhsIo_cata
         _prodsOo_clean = rule82 _lhsIo_clean
         _prodsOo_costcentre = rule83 _lhsIo_costcentre
         _prodsOo_data = rule84 _lhsIo_data
         _prodsOo_linePragmas = rule85 _lhsIo_linePragmas
         _prodsOo_monadic = rule86 _lhsIo_monadic
         _prodsOo_newtypes = rule87 _lhsIo_newtypes
         _prodsOo_pretty = rule88 _lhsIo_pretty
         _prodsOo_rename = rule89 _lhsIo_rename
         _prodsOo_sem = rule90 _lhsIo_sem
         _prodsOo_sig = rule91 _lhsIo_sig
         _prodsOo_splitsems = rule92 _lhsIo_splitsems
         _prodsOo_strictwrap = rule93 _lhsIo_strictwrap
         _prodsOo_traces = rule94 _lhsIo_traces
         _prodsOo_unbox = rule95 _lhsIo_unbox
         _prodsOoptions = rule96 _lhsIoptions
         _prodsOparamMap = rule97 _lhsIparamMap
         _prodsOprefix = rule98 _lhsIprefix
         _prodsOquantMap = rule99 _lhsIquantMap
         _prodsOunfoldSemDom = rule100 _lhsIunfoldSemDom
         _prodsOwith_sig = rule101 _lhsIwith_sig
         _prodsOwrappers = rule102 _lhsIwrappers
         _interOo_case = rule103 _lhsIo_case
         _interOo_cata = rule104 _lhsIo_cata
         _interOo_clean = rule105 _lhsIo_clean
         _interOo_costcentre = rule106 _lhsIo_costcentre
         _interOo_data = rule107 _lhsIo_data
         _interOo_linePragmas = rule108 _lhsIo_linePragmas
         _interOo_monadic = rule109 _lhsIo_monadic
         _interOo_newtypes = rule110 _lhsIo_newtypes
         _interOo_pretty = rule111 _lhsIo_pretty
         _interOo_rename = rule112 _lhsIo_rename
         _interOo_sem = rule113 _lhsIo_sem
         _interOo_sig = rule114 _lhsIo_sig
         _interOo_splitsems = rule115 _lhsIo_splitsems
         _interOo_strictwrap = rule116 _lhsIo_strictwrap
         _interOo_traces = rule117 _lhsIo_traces
         _interOo_unbox = rule118 _lhsIo_unbox
         _interOoptions = rule119 _lhsIoptions
         _interOparamMap = rule120 _lhsIparamMap
         _interOprefix = rule121 _lhsIprefix
         __result_ = T_CNonterminal_vOut7 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminal_s8 v7
   {-# INLINE rule63 #-}
   {-# LINE 86 "./src-ag/GenerateCode.ag" #-}
   rule63 = \ inh_ nt_ syn_ ->
                                          {-# LINE 86 "./src-ag/GenerateCode.ag" #-}
                                          (inh_,syn_,nt_)
                                          {-# LINE 827 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 87 "./src-ag/GenerateCode.ag" #-}
   rule64 = \ inh_ nt_ syn_ ->
                                         {-# LINE 87 "./src-ag/GenerateCode.ag" #-}
                                         (inh_,syn_,nt_)
                                         {-# LINE 833 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule65 #-}
   {-# LINE 143 "./src-ag/GenerateCode.ag" #-}
   rule65 = \ nt_ ->
                      {-# LINE 143 "./src-ag/GenerateCode.ag" #-}
                      Set.singleton nt_
                      {-# LINE 839 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 588 "./src-ag/GenerateCode.ag" #-}
   rule66 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) nt_ ->
                                                   {-# LINE 588 "./src-ag/GenerateCode.ag" #-}
                                                   Map.findWithDefault Map.empty nt_ _lhsIaroundMap
                                                   {-# LINE 845 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 604 "./src-ag/GenerateCode.ag" #-}
   rule67 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) nt_ ->
                                                  {-# LINE 604 "./src-ag/GenerateCode.ag" #-}
                                                  Map.findWithDefault Map.empty nt_ _lhsImergeMap
                                                  {-# LINE 851 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 809 "./src-ag/GenerateCode.ag" #-}
   rule68 = \ ((_interIwrapDecls) :: Decls) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_strictwrap) :: Bool) ((_lhsIoptions) :: Options) inh_ nt_ params_ syn_ ->
                                    {-# LINE 809 "./src-ag/GenerateCode.ag" #-}
                                    let params' = map getName params_
                                        inhAttrs = Map.toList inh_
                                        synAttrs = Map.toList syn_
                                        inhVars = [ SimpleExpr (attrname _lhsIoptions True _LHS a) | (a,_) <- inhAttrs ]
                                        synVars = [ SimpleExpr (attrname _lhsIoptions False _LHS a) | (a,_) <- synAttrs ]
                                        var = "sem"
                                        wrapNT = "wrap" ++ "_" ++ getName nt_
                                        inhNT = "Inh" ++ "_" ++ getName nt_
                                        synNT = "Syn" ++ "_" ++ getName nt_
                                        varPat = if  _lhsIo_newtypes
                                                     then App (sdtype nt_) [SimpleExpr var]
                                                     else SimpleExpr var
                                        evalTp | null params' = id
                                               | otherwise    = idEvalType _lhsIoptions
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
                                    {-# LINE 880 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule69 #-}
   {-# LINE 870 "./src-ag/GenerateCode.ag" #-}
   rule69 = \ ((_interIcomments) :: [String]) ((_prodsIcomments) :: [String]) ->
                                 {-# LINE 870 "./src-ag/GenerateCode.ag" #-}
                                 Comment . unlines . map ind $ ( _interIcomments ++ ("alternatives:" : map ind _prodsIcomments) )
                                 {-# LINE 886 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule70 #-}
   {-# LINE 935 "./src-ag/GenerateCode.ag" #-}
   rule70 = \ _cataFun _comment _dataDef _genCata ((_interIsemDom) :: [Decl]) ((_lhsIo_cata) :: Bool) ((_lhsIo_data) :: Maybe Bool) ((_lhsIo_pretty) :: Bool) ((_lhsIo_sem) :: Bool) ((_lhsIo_sig) :: Bool) ((_lhsIwrappers) :: Set NontermIdent) ((_prodsIdecls) :: Decls) ((_prodsIsemNames) :: [String]) _semWrapper nt_ ->
                                 {-# LINE 935 "./src-ag/GenerateCode.ag" #-}
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
                                 {-# LINE 901 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 1005 "./src-ag/GenerateCode.ag" #-}
   rule71 = \ ((_lhsIderivings) :: Derivings) ((_lhsIo_data) :: Maybe Bool) ((_lhsIoptions) :: Options) ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdataAlts) :: DataAlts) nt_ params_ ->
                                 {-# LINE 1005 "./src-ag/GenerateCode.ag" #-}
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
                                                   in Code.Type (getName nt_) params' (idEvalType _lhsIoptions theType)
                                     derivings  = maybe [] (map getName . Set.toList) (Map.lookup nt_ _lhsIderivings)
                                     dataDef    = Data (getName nt_) (map getName params_) _prodsIdataAlts (maybe False id _lhsIo_data) derivings
                                 in maybe dataDef typeSyn $ lookup nt_ _lhsItypeSyns
                                 {-# LINE 920 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 1048 "./src-ag/GenerateCode.ag" #-}
   rule72 = \ ((_lhsIoptions) :: Options) nt_ ->
                                 {-# LINE 1048 "./src-ag/GenerateCode.ag" #-}
                                 not (nt_ `Set.member` nocatas _lhsIoptions)
                                 {-# LINE 926 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 1049 "./src-ag/GenerateCode.ag" #-}
   rule73 = \ ((_lhsIcontextMap) :: ContextMap) ((_lhsIo_sig) :: Bool) ((_lhsIoptions) :: Options) ((_lhsIprefix) :: String) ((_lhsIquantMap) :: QuantMap) ((_lhsItypeSyns) :: TypeSyns) ((_prodsIcataAlts) :: Decls) nt_ params_ ->
                                 {-# LINE 1049 "./src-ag/GenerateCode.ag" #-}
                                 let appParams nm = TypeApp (SimpleType nm) (map SimpleType (map getName params_))
                                     evalTp | null params_ = id
                                            | otherwise    = idEvalType _lhsIoptions
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
                                 {-# LINE 1013 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule74 #-}
   rule74 = \ ((_interIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _interIsemDomUnfoldGath
   {-# INLINE rule75 #-}
   rule75 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule76 #-}
   rule76 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule77 #-}
   rule77 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule79 #-}
   rule79 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule81 #-}
   rule81 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule82 #-}
   rule82 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule84 #-}
   rule84 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule85 #-}
   rule85 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule87 #-}
   rule87 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule88 #-}
   rule88 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule89 #-}
   rule89 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule90 #-}
   rule90 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule91 #-}
   rule91 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule105 #-}
   rule105 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule106 #-}
   rule106 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule113 #-}
   rule113 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix

-- CNonterminals -----------------------------------------------
-- wrapper
data Inh_CNonterminals  = Inh_CNonterminals { allNts_Inh_CNonterminals :: (Set NontermIdent), allPragmas_Inh_CNonterminals :: (PragmaMap), aroundMap_Inh_CNonterminals :: (Map NontermIdent (Map ConstructorIdent (Set Identifier))), contextMap_Inh_CNonterminals :: (ContextMap), derivings_Inh_CNonterminals :: (Derivings), mergeMap_Inh_CNonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))), o_case_Inh_CNonterminals :: (Bool), o_cata_Inh_CNonterminals :: (Bool), o_clean_Inh_CNonterminals :: (Bool), o_costcentre_Inh_CNonterminals :: (Bool), o_data_Inh_CNonterminals :: (Maybe Bool), o_linePragmas_Inh_CNonterminals :: (Bool), o_monadic_Inh_CNonterminals :: (Bool), o_newtypes_Inh_CNonterminals :: (Bool), o_pretty_Inh_CNonterminals :: (Bool), o_rename_Inh_CNonterminals :: (Bool), o_sem_Inh_CNonterminals :: (Bool), o_sig_Inh_CNonterminals :: (Bool), o_splitsems_Inh_CNonterminals :: (Bool), o_strictwrap_Inh_CNonterminals :: (Bool), o_traces_Inh_CNonterminals :: (Bool), o_unbox_Inh_CNonterminals :: (Bool), options_Inh_CNonterminals :: (Options), paramMap_Inh_CNonterminals :: (ParamMap), prefix_Inh_CNonterminals :: (String), quantMap_Inh_CNonterminals :: (QuantMap), typeSyns_Inh_CNonterminals :: (TypeSyns), unfoldSemDom_Inh_CNonterminals :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CNonterminals :: (Bool), wrappers_Inh_CNonterminals :: (Set NontermIdent) }
data Syn_CNonterminals  = Syn_CNonterminals { chunks_Syn_CNonterminals :: (Chunks), gathNts_Syn_CNonterminals :: (Set NontermIdent), semDomUnfoldGath_Syn_CNonterminals :: (Map (NontermIdent, Int) ([String], Code.Type)) }
{-# INLINABLE wrap_CNonterminals #-}
wrap_CNonterminals :: T_CNonterminals  -> Inh_CNonterminals  -> (Syn_CNonterminals )
wrap_CNonterminals (T_CNonterminals act) (Inh_CNonterminals _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
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
data T_CNonterminals_vIn10  = T_CNonterminals_vIn10 (Set NontermIdent) (PragmaMap) (Map NontermIdent (Map ConstructorIdent (Set Identifier))) (ContextMap) (Derivings) (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (TypeSyns) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CNonterminals_vOut10  = T_CNonterminals_vOut10 (Chunks) (Set NontermIdent) (Map (NontermIdent, Int) ([String], Code.Type))
{-# NOINLINE sem_CNonterminals_Cons #-}
sem_CNonterminals_Cons :: T_CNonterminal  -> T_CNonterminals  -> T_CNonterminals 
sem_CNonterminals_Cons arg_hd_ arg_tl_ = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_CNonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_CNonterminals (arg_tl_))
         (T_CNonterminal_vOut7 _hdIchunks _hdIgathNts _hdIsemDomUnfoldGath) = inv_CNonterminal_s8 _hdX8 (T_CNonterminal_vIn7 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOderivings _hdOmergeMap _hdOo_case _hdOo_cata _hdOo_clean _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOtypeSyns _hdOunfoldSemDom _hdOwith_sig _hdOwrappers)
         (T_CNonterminals_vOut10 _tlIchunks _tlIgathNts _tlIsemDomUnfoldGath) = inv_CNonterminals_s11 _tlX11 (T_CNonterminals_vIn10 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOderivings _tlOmergeMap _tlOo_case _tlOo_cata _tlOo_clean _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOtypeSyns _tlOunfoldSemDom _tlOwith_sig _tlOwrappers)
         _lhsOchunks :: Chunks
         _lhsOchunks = rule122 _hdIchunks _tlIchunks
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule123 _hdIgathNts _tlIgathNts
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule124 _hdIsemDomUnfoldGath _tlIsemDomUnfoldGath
         _hdOallNts = rule125 _lhsIallNts
         _hdOallPragmas = rule126 _lhsIallPragmas
         _hdOaroundMap = rule127 _lhsIaroundMap
         _hdOcontextMap = rule128 _lhsIcontextMap
         _hdOderivings = rule129 _lhsIderivings
         _hdOmergeMap = rule130 _lhsImergeMap
         _hdOo_case = rule131 _lhsIo_case
         _hdOo_cata = rule132 _lhsIo_cata
         _hdOo_clean = rule133 _lhsIo_clean
         _hdOo_costcentre = rule134 _lhsIo_costcentre
         _hdOo_data = rule135 _lhsIo_data
         _hdOo_linePragmas = rule136 _lhsIo_linePragmas
         _hdOo_monadic = rule137 _lhsIo_monadic
         _hdOo_newtypes = rule138 _lhsIo_newtypes
         _hdOo_pretty = rule139 _lhsIo_pretty
         _hdOo_rename = rule140 _lhsIo_rename
         _hdOo_sem = rule141 _lhsIo_sem
         _hdOo_sig = rule142 _lhsIo_sig
         _hdOo_splitsems = rule143 _lhsIo_splitsems
         _hdOo_strictwrap = rule144 _lhsIo_strictwrap
         _hdOo_traces = rule145 _lhsIo_traces
         _hdOo_unbox = rule146 _lhsIo_unbox
         _hdOoptions = rule147 _lhsIoptions
         _hdOparamMap = rule148 _lhsIparamMap
         _hdOprefix = rule149 _lhsIprefix
         _hdOquantMap = rule150 _lhsIquantMap
         _hdOtypeSyns = rule151 _lhsItypeSyns
         _hdOunfoldSemDom = rule152 _lhsIunfoldSemDom
         _hdOwith_sig = rule153 _lhsIwith_sig
         _hdOwrappers = rule154 _lhsIwrappers
         _tlOallNts = rule155 _lhsIallNts
         _tlOallPragmas = rule156 _lhsIallPragmas
         _tlOaroundMap = rule157 _lhsIaroundMap
         _tlOcontextMap = rule158 _lhsIcontextMap
         _tlOderivings = rule159 _lhsIderivings
         _tlOmergeMap = rule160 _lhsImergeMap
         _tlOo_case = rule161 _lhsIo_case
         _tlOo_cata = rule162 _lhsIo_cata
         _tlOo_clean = rule163 _lhsIo_clean
         _tlOo_costcentre = rule164 _lhsIo_costcentre
         _tlOo_data = rule165 _lhsIo_data
         _tlOo_linePragmas = rule166 _lhsIo_linePragmas
         _tlOo_monadic = rule167 _lhsIo_monadic
         _tlOo_newtypes = rule168 _lhsIo_newtypes
         _tlOo_pretty = rule169 _lhsIo_pretty
         _tlOo_rename = rule170 _lhsIo_rename
         _tlOo_sem = rule171 _lhsIo_sem
         _tlOo_sig = rule172 _lhsIo_sig
         _tlOo_splitsems = rule173 _lhsIo_splitsems
         _tlOo_strictwrap = rule174 _lhsIo_strictwrap
         _tlOo_traces = rule175 _lhsIo_traces
         _tlOo_unbox = rule176 _lhsIo_unbox
         _tlOoptions = rule177 _lhsIoptions
         _tlOparamMap = rule178 _lhsIparamMap
         _tlOprefix = rule179 _lhsIprefix
         _tlOquantMap = rule180 _lhsIquantMap
         _tlOtypeSyns = rule181 _lhsItypeSyns
         _tlOunfoldSemDom = rule182 _lhsIunfoldSemDom
         _tlOwith_sig = rule183 _lhsIwith_sig
         _tlOwrappers = rule184 _lhsIwrappers
         __result_ = T_CNonterminals_vOut10 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule122 #-}
   rule122 = \ ((_hdIchunks) :: Chunks) ((_tlIchunks) :: Chunks) ->
     _hdIchunks ++ _tlIchunks
   {-# INLINE rule123 #-}
   rule123 = \ ((_hdIgathNts) :: Set NontermIdent) ((_tlIgathNts) :: Set NontermIdent) ->
     _hdIgathNts `Set.union` _tlIgathNts
   {-# INLINE rule124 #-}
   rule124 = \ ((_hdIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ((_tlIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
     _lhsIaroundMap
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIaroundMap) :: Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
     _lhsIaroundMap
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsImergeMap) :: Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier])))) ->
     _lhsImergeMap
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CNonterminals_Nil #-}
sem_CNonterminals_Nil ::  T_CNonterminals 
sem_CNonterminals_Nil  = T_CNonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_CNonterminals_v10 
      v10 = \ (T_CNonterminals_vIn10 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIderivings _lhsImergeMap _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOchunks :: Chunks
         _lhsOchunks = rule185  ()
         _lhsOgathNts :: Set NontermIdent
         _lhsOgathNts = rule186  ()
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule187  ()
         __result_ = T_CNonterminals_vOut10 _lhsOchunks _lhsOgathNts _lhsOsemDomUnfoldGath
         in __result_ )
     in C_CNonterminals_s11 v10
   {-# INLINE rule185 #-}
   rule185 = \  (_ :: ()) ->
     []
   {-# INLINE rule186 #-}
   rule186 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule187 #-}
   rule187 = \  (_ :: ()) ->
     Map.empty

-- CProduction -------------------------------------------------
-- wrapper
data Inh_CProduction  = Inh_CProduction { allNts_Inh_CProduction :: (Set NontermIdent), allPragmas_Inh_CProduction :: (PragmaMap), aroundMap_Inh_CProduction :: (Map ConstructorIdent (Set Identifier)), contextMap_Inh_CProduction :: (ContextMap), inh_Inh_CProduction :: (Attributes), mergeMap_Inh_CProduction :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))), nt_Inh_CProduction :: (NontermIdent), o_case_Inh_CProduction :: (Bool), o_cata_Inh_CProduction :: (Bool), o_clean_Inh_CProduction :: (Bool), o_costcentre_Inh_CProduction :: (Bool), o_data_Inh_CProduction :: (Maybe Bool), o_linePragmas_Inh_CProduction :: (Bool), o_monadic_Inh_CProduction :: (Bool), o_newtypes_Inh_CProduction :: (Bool), o_pretty_Inh_CProduction :: (Bool), o_rename_Inh_CProduction :: (Bool), o_sem_Inh_CProduction :: (Bool), o_sig_Inh_CProduction :: (Bool), o_splitsems_Inh_CProduction :: (Bool), o_strictwrap_Inh_CProduction :: (Bool), o_traces_Inh_CProduction :: (Bool), o_unbox_Inh_CProduction :: (Bool), options_Inh_CProduction :: (Options), paramMap_Inh_CProduction :: (ParamMap), prefix_Inh_CProduction :: (String), quantMap_Inh_CProduction :: (QuantMap), syn_Inh_CProduction :: (Attributes), unfoldSemDom_Inh_CProduction :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CProduction :: (Bool), wrappers_Inh_CProduction :: (Set NontermIdent) }
data Syn_CProduction  = Syn_CProduction { cataAlt_Syn_CProduction :: (Decl), comments_Syn_CProduction :: ([String]), dataAlt_Syn_CProduction :: (DataAlt), decls_Syn_CProduction :: (Decls), semNames_Syn_CProduction :: ([String]) }
{-# INLINABLE wrap_CProduction #-}
wrap_CProduction :: T_CProduction  -> Inh_CProduction  -> (Syn_CProduction )
wrap_CProduction (T_CProduction act) (Inh_CProduction _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProduction_vIn13 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
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
data T_CProduction_vIn13  = T_CProduction_vIn13 (Set NontermIdent) (PragmaMap) (Map ConstructorIdent (Set Identifier)) (ContextMap) (Attributes) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (Attributes) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CProduction_vOut13  = T_CProduction_vOut13 (Decl) ([String]) (DataAlt) (Decls) ([String])
{-# NOINLINE sem_CProduction_CProduction #-}
sem_CProduction_CProduction :: (ConstructorIdent) -> T_CVisits  -> ([(Identifier,Type,ChildKind)]) -> ([Identifier]) -> T_CProduction 
sem_CProduction_CProduction arg_con_ arg_visits_ arg_children_ arg_terminals_ = T_CProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_CProduction_v13 
      v13 = \ (T_CProduction_vIn13 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _visitsX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_visits_))
         (T_CVisits_vOut31 _visitsIcomments _visitsIdecls _visitsIgatherInstVisitNrs _visitsIintra _visitsIintraVars _visitsIisNil _visitsIsemNames _visitsIvisitedSet) = inv_CVisits_s32 _visitsX32 (T_CVisits_vIn31 _visitsOallNts _visitsOallPragmas _visitsOaroundMap _visitsOchildren _visitsOcon _visitsOcontextMap _visitsOinh _visitsOinstVisitNrs _visitsOmergeMap _visitsOnr _visitsOnt _visitsOo_case _visitsOo_cata _visitsOo_clean _visitsOo_costcentre _visitsOo_data _visitsOo_linePragmas _visitsOo_monadic _visitsOo_newtypes _visitsOo_pretty _visitsOo_rename _visitsOo_sem _visitsOo_sig _visitsOo_splitsems _visitsOo_strictwrap _visitsOo_traces _visitsOo_unbox _visitsOoptions _visitsOparamInstMap _visitsOparamMap _visitsOprefix _visitsOquantMap _visitsOsyn _visitsOterminals _visitsOunfoldSemDom _visitsOvisitedSet _visitsOwith_sig _visitsOwrappers)
         _visitsOcon = rule188 arg_con_
         _visitsOterminals = rule189 arg_terminals_
         _paramInstMap = rule190 _lhsIoptions arg_children_
         _visitsOvisitedSet = rule191  ()
         _visitsOnr = rule192  ()
         _visitsOchildren = rule193 arg_children_
         _visitsOinstVisitNrs = rule194 _visitsIgatherInstVisitNrs
         _aroundMap = rule195 _lhsIaroundMap arg_con_
         _mergeMap = rule196 _lhsImergeMap arg_con_
         _firstOrderChildren = rule197 arg_children_
         _lhsOcomments :: [String]
         _lhsOcomments = rule198 _firstOrderChildren _visitsIcomments arg_con_
         _params = rule199 _lhsInt _lhsIparamMap
         _lhsOdataAlt :: DataAlt
         _lhsOdataAlt = rule200 _firstOrderChildren _lhsInt _lhsIo_rename _lhsIoptions _params arg_con_
         _lhsOcataAlt :: Decl
         _lhsOcataAlt = rule201 _firstOrderChildren _lhsInt _lhsIo_rename _lhsIoptions _lhsIprefix arg_con_
         _lhsOdecls :: Decls
         _lhsOdecls = rule202 _visitsIdecls
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule203 _visitsIsemNames
         _visitsOallNts = rule204 _lhsIallNts
         _visitsOallPragmas = rule205 _lhsIallPragmas
         _visitsOaroundMap = rule206 _aroundMap
         _visitsOcontextMap = rule207 _lhsIcontextMap
         _visitsOinh = rule208 _lhsIinh
         _visitsOmergeMap = rule209 _mergeMap
         _visitsOnt = rule210 _lhsInt
         _visitsOo_case = rule211 _lhsIo_case
         _visitsOo_cata = rule212 _lhsIo_cata
         _visitsOo_clean = rule213 _lhsIo_clean
         _visitsOo_costcentre = rule214 _lhsIo_costcentre
         _visitsOo_data = rule215 _lhsIo_data
         _visitsOo_linePragmas = rule216 _lhsIo_linePragmas
         _visitsOo_monadic = rule217 _lhsIo_monadic
         _visitsOo_newtypes = rule218 _lhsIo_newtypes
         _visitsOo_pretty = rule219 _lhsIo_pretty
         _visitsOo_rename = rule220 _lhsIo_rename
         _visitsOo_sem = rule221 _lhsIo_sem
         _visitsOo_sig = rule222 _lhsIo_sig
         _visitsOo_splitsems = rule223 _lhsIo_splitsems
         _visitsOo_strictwrap = rule224 _lhsIo_strictwrap
         _visitsOo_traces = rule225 _lhsIo_traces
         _visitsOo_unbox = rule226 _lhsIo_unbox
         _visitsOoptions = rule227 _lhsIoptions
         _visitsOparamInstMap = rule228 _paramInstMap
         _visitsOparamMap = rule229 _lhsIparamMap
         _visitsOprefix = rule230 _lhsIprefix
         _visitsOquantMap = rule231 _lhsIquantMap
         _visitsOsyn = rule232 _lhsIsyn
         _visitsOunfoldSemDom = rule233 _lhsIunfoldSemDom
         _visitsOwith_sig = rule234 _lhsIwith_sig
         _visitsOwrappers = rule235 _lhsIwrappers
         __result_ = T_CProduction_vOut13 _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProduction_s14 v13
   {-# INLINE rule188 #-}
   {-# LINE 92 "./src-ag/GenerateCode.ag" #-}
   rule188 = \ con_ ->
                                 {-# LINE 92 "./src-ag/GenerateCode.ag" #-}
                                 con_
                                 {-# LINE 1584 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule189 #-}
   {-# LINE 93 "./src-ag/GenerateCode.ag" #-}
   rule189 = \ terminals_ ->
                                        {-# LINE 93 "./src-ag/GenerateCode.ag" #-}
                                        terminals_
                                        {-# LINE 1590 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule190 #-}
   {-# LINE 105 "./src-ag/GenerateCode.ag" #-}
   rule190 = \ ((_lhsIoptions) :: Options) children_ ->
                           {-# LINE 105 "./src-ag/GenerateCode.ag" #-}
                           Map.fromList [(nm, (extractNonterminal tp, tps)) | (nm,tp,_) <- children_, let tps = map (cleanupArg _lhsIoptions) $ nontermArgs tp, not (null tps) ]
                           {-# LINE 1596 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule191 #-}
   {-# LINE 147 "./src-ag/GenerateCode.ag" #-}
   rule191 = \  (_ :: ()) ->
                                                     {-# LINE 147 "./src-ag/GenerateCode.ag" #-}
                                                     Set.empty
                                                     {-# LINE 1602 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule192 #-}
   {-# LINE 282 "./src-ag/GenerateCode.ag" #-}
   rule192 = \  (_ :: ()) ->
                               {-# LINE 282 "./src-ag/GenerateCode.ag" #-}
                               0
                               {-# LINE 1608 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule193 #-}
   {-# LINE 414 "./src-ag/GenerateCode.ag" #-}
   rule193 = \ children_ ->
                                     {-# LINE 414 "./src-ag/GenerateCode.ag" #-}
                                     children_
                                     {-# LINE 1614 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule194 #-}
   {-# LINE 569 "./src-ag/GenerateCode.ag" #-}
   rule194 = \ ((_visitsIgatherInstVisitNrs) :: Map Identifier Int) ->
                              {-# LINE 569 "./src-ag/GenerateCode.ag" #-}
                              _visitsIgatherInstVisitNrs
                              {-# LINE 1620 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule195 #-}
   {-# LINE 589 "./src-ag/GenerateCode.ag" #-}
   rule195 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) con_ ->
                                                   {-# LINE 589 "./src-ag/GenerateCode.ag" #-}
                                                   Map.findWithDefault Set.empty con_ _lhsIaroundMap
                                                   {-# LINE 1626 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule196 #-}
   {-# LINE 605 "./src-ag/GenerateCode.ag" #-}
   rule196 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) con_ ->
                                                  {-# LINE 605 "./src-ag/GenerateCode.ag" #-}
                                                  Map.findWithDefault Map.empty con_ _lhsImergeMap
                                                  {-# LINE 1632 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule197 #-}
   {-# LINE 885 "./src-ag/GenerateCode.ag" #-}
   rule197 = \ children_ ->
                                            {-# LINE 885 "./src-ag/GenerateCode.ag" #-}
                                            [ (nm,fromJust mb,virt) | (nm,tp,virt) <- children_, let mb = isFirstOrder virt tp, isJust mb ]
                                            {-# LINE 1638 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule198 #-}
   {-# LINE 886 "./src-ag/GenerateCode.ag" #-}
   rule198 = \ _firstOrderChildren ((_visitsIcomments) :: [String]) con_ ->
                                   {-# LINE 886 "./src-ag/GenerateCode.ag" #-}
                                   ("alternative " ++ getName con_ ++ ":")
                                   : map ind (  map (\(x,y,_) -> makeLocalComment 14 "child" x (Just y)) _firstOrderChildren
                                             ++ _visitsIcomments
                                             )
                                   {-# LINE 1647 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule199 #-}
   {-# LINE 1028 "./src-ag/GenerateCode.ag" #-}
   rule199 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                                {-# LINE 1028 "./src-ag/GenerateCode.ag" #-}
                                map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                                {-# LINE 1653 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule200 #-}
   {-# LINE 1029 "./src-ag/GenerateCode.ag" #-}
   rule200 = \ _firstOrderChildren ((_lhsInt) :: NontermIdent) ((_lhsIo_rename) :: Bool) ((_lhsIoptions) :: Options) _params con_ ->
                                {-# LINE 1029 "./src-ag/GenerateCode.ag" #-}
                                let conNm = conname _lhsIo_rename _lhsInt con_
                                    mkFields :: (NontermIdent -> ConstructorIdent -> Identifier -> Code.Type -> a) -> [a]
                                    mkFields f = map (\(nm,t,_) -> f _lhsInt con_ nm (typeToCodeType (Just _lhsInt) _params     $ removeDeforested t)) _firstOrderChildren
                                in if dataRecords _lhsIoptions
                                   then Record conNm $ mkFields $ toNamedType (strictData _lhsIoptions)
                                   else DataAlt conNm $ mkFields $ \_ _ _ t -> t
                                {-# LINE 1664 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule201 #-}
   {-# LINE 1142 "./src-ag/GenerateCode.ag" #-}
   rule201 = \ _firstOrderChildren ((_lhsInt) :: NontermIdent) ((_lhsIo_rename) :: Bool) ((_lhsIoptions) :: Options) ((_lhsIprefix) :: String) con_ ->
                                {-# LINE 1142 "./src-ag/GenerateCode.ag" #-}
                                let lhs = Fun (cataname _lhsIprefix _lhsInt) [lhs_pat]
                                    lhs_pat = App (conname _lhsIo_rename _lhsInt con_)
                                                   (map (\(n,_,_) -> SimpleExpr $ locname _lhsIoptions $ n) _firstOrderChildren    )
                                    rhs = App (semname _lhsIprefix _lhsInt con_)
                                               (map argument _firstOrderChildren    )
                                    argument (nm,NT tp _ _,_) = App (cataname _lhsIprefix tp)
                                                                     [SimpleExpr (locname _lhsIoptions nm)]
                                    argument (nm, _,_)    = SimpleExpr (locname _lhsIoptions nm)
                                 in Decl lhs rhs Set.empty Set.empty
                                {-# LINE 1678 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule202 #-}
   rule202 = \ ((_visitsIdecls) :: Decls) ->
     _visitsIdecls
   {-# INLINE rule203 #-}
   rule203 = \ ((_visitsIsemNames) :: [String]) ->
     _visitsIsemNames
   {-# INLINE rule204 #-}
   rule204 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule206 #-}
   rule206 = \ _aroundMap ->
     _aroundMap
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule209 #-}
   rule209 = \ _mergeMap ->
     _mergeMap
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule212 #-}
   rule212 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule228 #-}
   rule228 = \ _paramInstMap ->
     _paramInstMap
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers

-- CProductions ------------------------------------------------
-- wrapper
data Inh_CProductions  = Inh_CProductions { allNts_Inh_CProductions :: (Set NontermIdent), allPragmas_Inh_CProductions :: (PragmaMap), aroundMap_Inh_CProductions :: (Map ConstructorIdent (Set Identifier)), contextMap_Inh_CProductions :: (ContextMap), inh_Inh_CProductions :: (Attributes), mergeMap_Inh_CProductions :: (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))), nt_Inh_CProductions :: (NontermIdent), o_case_Inh_CProductions :: (Bool), o_cata_Inh_CProductions :: (Bool), o_clean_Inh_CProductions :: (Bool), o_costcentre_Inh_CProductions :: (Bool), o_data_Inh_CProductions :: (Maybe Bool), o_linePragmas_Inh_CProductions :: (Bool), o_monadic_Inh_CProductions :: (Bool), o_newtypes_Inh_CProductions :: (Bool), o_pretty_Inh_CProductions :: (Bool), o_rename_Inh_CProductions :: (Bool), o_sem_Inh_CProductions :: (Bool), o_sig_Inh_CProductions :: (Bool), o_splitsems_Inh_CProductions :: (Bool), o_strictwrap_Inh_CProductions :: (Bool), o_traces_Inh_CProductions :: (Bool), o_unbox_Inh_CProductions :: (Bool), options_Inh_CProductions :: (Options), paramMap_Inh_CProductions :: (ParamMap), prefix_Inh_CProductions :: (String), quantMap_Inh_CProductions :: (QuantMap), syn_Inh_CProductions :: (Attributes), unfoldSemDom_Inh_CProductions :: (NontermIdent -> Int -> [String] -> Code.Type), with_sig_Inh_CProductions :: (Bool), wrappers_Inh_CProductions :: (Set NontermIdent) }
data Syn_CProductions  = Syn_CProductions { cataAlts_Syn_CProductions :: (Decls), comments_Syn_CProductions :: ([String]), dataAlts_Syn_CProductions :: (DataAlts), decls_Syn_CProductions :: (Decls), semNames_Syn_CProductions :: ([String]) }
{-# INLINABLE wrap_CProductions #-}
wrap_CProductions :: T_CProductions  -> Inh_CProductions  -> (Syn_CProductions )
wrap_CProductions (T_CProductions act) (Inh_CProductions _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers
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
data T_CProductions_vIn16  = T_CProductions_vIn16 (Set NontermIdent) (PragmaMap) (Map ConstructorIdent (Set Identifier)) (ContextMap) (Attributes) (Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (QuantMap) (Attributes) (NontermIdent -> Int -> [String] -> Code.Type) (Bool) (Set NontermIdent)
data T_CProductions_vOut16  = T_CProductions_vOut16 (Decls) ([String]) (DataAlts) (Decls) ([String])
{-# NOINLINE sem_CProductions_Cons #-}
sem_CProductions_Cons :: T_CProduction  -> T_CProductions  -> T_CProductions 
sem_CProductions_Cons arg_hd_ arg_tl_ = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_CProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_CProductions (arg_tl_))
         (T_CProduction_vOut13 _hdIcataAlt _hdIcomments _hdIdataAlt _hdIdecls _hdIsemNames) = inv_CProduction_s14 _hdX14 (T_CProduction_vIn13 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOcontextMap _hdOinh _hdOmergeMap _hdOnt _hdOo_case _hdOo_cata _hdOo_clean _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOunfoldSemDom _hdOwith_sig _hdOwrappers)
         (T_CProductions_vOut16 _tlIcataAlts _tlIcomments _tlIdataAlts _tlIdecls _tlIsemNames) = inv_CProductions_s17 _tlX17 (T_CProductions_vIn16 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOcontextMap _tlOinh _tlOmergeMap _tlOnt _tlOo_case _tlOo_cata _tlOo_clean _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOunfoldSemDom _tlOwith_sig _tlOwrappers)
         _lhsOdataAlts :: DataAlts
         _lhsOdataAlts = rule236 _hdIdataAlt _tlIdataAlts
         _lhsOcataAlts :: Decls
         _lhsOcataAlts = rule237 _hdIcataAlt _tlIcataAlts
         _lhsOcomments :: [String]
         _lhsOcomments = rule238 _hdIcomments _tlIcomments
         _lhsOdecls :: Decls
         _lhsOdecls = rule239 _hdIdecls _tlIdecls
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule240 _hdIsemNames _tlIsemNames
         _hdOallNts = rule241 _lhsIallNts
         _hdOallPragmas = rule242 _lhsIallPragmas
         _hdOaroundMap = rule243 _lhsIaroundMap
         _hdOcontextMap = rule244 _lhsIcontextMap
         _hdOinh = rule245 _lhsIinh
         _hdOmergeMap = rule246 _lhsImergeMap
         _hdOnt = rule247 _lhsInt
         _hdOo_case = rule248 _lhsIo_case
         _hdOo_cata = rule249 _lhsIo_cata
         _hdOo_clean = rule250 _lhsIo_clean
         _hdOo_costcentre = rule251 _lhsIo_costcentre
         _hdOo_data = rule252 _lhsIo_data
         _hdOo_linePragmas = rule253 _lhsIo_linePragmas
         _hdOo_monadic = rule254 _lhsIo_monadic
         _hdOo_newtypes = rule255 _lhsIo_newtypes
         _hdOo_pretty = rule256 _lhsIo_pretty
         _hdOo_rename = rule257 _lhsIo_rename
         _hdOo_sem = rule258 _lhsIo_sem
         _hdOo_sig = rule259 _lhsIo_sig
         _hdOo_splitsems = rule260 _lhsIo_splitsems
         _hdOo_strictwrap = rule261 _lhsIo_strictwrap
         _hdOo_traces = rule262 _lhsIo_traces
         _hdOo_unbox = rule263 _lhsIo_unbox
         _hdOoptions = rule264 _lhsIoptions
         _hdOparamMap = rule265 _lhsIparamMap
         _hdOprefix = rule266 _lhsIprefix
         _hdOquantMap = rule267 _lhsIquantMap
         _hdOsyn = rule268 _lhsIsyn
         _hdOunfoldSemDom = rule269 _lhsIunfoldSemDom
         _hdOwith_sig = rule270 _lhsIwith_sig
         _hdOwrappers = rule271 _lhsIwrappers
         _tlOallNts = rule272 _lhsIallNts
         _tlOallPragmas = rule273 _lhsIallPragmas
         _tlOaroundMap = rule274 _lhsIaroundMap
         _tlOcontextMap = rule275 _lhsIcontextMap
         _tlOinh = rule276 _lhsIinh
         _tlOmergeMap = rule277 _lhsImergeMap
         _tlOnt = rule278 _lhsInt
         _tlOo_case = rule279 _lhsIo_case
         _tlOo_cata = rule280 _lhsIo_cata
         _tlOo_clean = rule281 _lhsIo_clean
         _tlOo_costcentre = rule282 _lhsIo_costcentre
         _tlOo_data = rule283 _lhsIo_data
         _tlOo_linePragmas = rule284 _lhsIo_linePragmas
         _tlOo_monadic = rule285 _lhsIo_monadic
         _tlOo_newtypes = rule286 _lhsIo_newtypes
         _tlOo_pretty = rule287 _lhsIo_pretty
         _tlOo_rename = rule288 _lhsIo_rename
         _tlOo_sem = rule289 _lhsIo_sem
         _tlOo_sig = rule290 _lhsIo_sig
         _tlOo_splitsems = rule291 _lhsIo_splitsems
         _tlOo_strictwrap = rule292 _lhsIo_strictwrap
         _tlOo_traces = rule293 _lhsIo_traces
         _tlOo_unbox = rule294 _lhsIo_unbox
         _tlOoptions = rule295 _lhsIoptions
         _tlOparamMap = rule296 _lhsIparamMap
         _tlOprefix = rule297 _lhsIprefix
         _tlOquantMap = rule298 _lhsIquantMap
         _tlOsyn = rule299 _lhsIsyn
         _tlOunfoldSemDom = rule300 _lhsIunfoldSemDom
         _tlOwith_sig = rule301 _lhsIwith_sig
         _tlOwrappers = rule302 _lhsIwrappers
         __result_ = T_CProductions_vOut16 _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule236 #-}
   {-# LINE 1024 "./src-ag/GenerateCode.ag" #-}
   rule236 = \ ((_hdIdataAlt) :: DataAlt) ((_tlIdataAlts) :: DataAlts) ->
                                  {-# LINE 1024 "./src-ag/GenerateCode.ag" #-}
                                  _hdIdataAlt : _tlIdataAlts
                                  {-# LINE 1903 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule237 #-}
   {-# LINE 1138 "./src-ag/GenerateCode.ag" #-}
   rule237 = \ ((_hdIcataAlt) :: Decl) ((_tlIcataAlts) :: Decls) ->
                          {-# LINE 1138 "./src-ag/GenerateCode.ag" #-}
                          _hdIcataAlt : _tlIcataAlts
                          {-# LINE 1909 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule238 #-}
   rule238 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule239 #-}
   rule239 = \ ((_hdIdecls) :: Decls) ((_tlIdecls) :: Decls) ->
     _hdIdecls ++ _tlIdecls
   {-# INLINE rule240 #-}
   rule240 = \ ((_hdIsemNames) :: [String]) ((_tlIsemNames) :: [String]) ->
     _hdIsemNames ++ _tlIsemNames
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) ->
     _lhsIaroundMap
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule271 #-}
   rule271 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule274 #-}
   rule274 = \ ((_lhsIaroundMap) :: Map ConstructorIdent (Set Identifier)) ->
     _lhsIaroundMap
   {-# INLINE rule275 #-}
   rule275 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsImergeMap) :: Map ConstructorIdent (Map Identifier (Identifier, [Identifier]))) ->
     _lhsImergeMap
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CProductions_Nil #-}
sem_CProductions_Nil ::  T_CProductions 
sem_CProductions_Nil  = T_CProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_CProductions_v16 
      v16 = \ (T_CProductions_vIn16 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIcontextMap _lhsIinh _lhsImergeMap _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOdataAlts :: DataAlts
         _lhsOdataAlts = rule303  ()
         _lhsOcataAlts :: Decls
         _lhsOcataAlts = rule304  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule305  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule306  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule307  ()
         __result_ = T_CProductions_vOut16 _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOsemNames
         in __result_ )
     in C_CProductions_s17 v16
   {-# INLINE rule303 #-}
   {-# LINE 1025 "./src-ag/GenerateCode.ag" #-}
   rule303 = \  (_ :: ()) ->
                                  {-# LINE 1025 "./src-ag/GenerateCode.ag" #-}
                                  []
                                  {-# LINE 2130 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule304 #-}
   {-# LINE 1139 "./src-ag/GenerateCode.ag" #-}
   rule304 = \  (_ :: ()) ->
                          {-# LINE 1139 "./src-ag/GenerateCode.ag" #-}
                          []
                          {-# LINE 2136 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule305 #-}
   rule305 = \  (_ :: ()) ->
     []
   {-# INLINE rule306 #-}
   rule306 = \  (_ :: ()) ->
     []
   {-# INLINE rule307 #-}
   rule307 = \  (_ :: ()) ->
     []

-- CRule -------------------------------------------------------
-- wrapper
data Inh_CRule  = Inh_CRule { allNts_Inh_CRule :: (Set NontermIdent), aroundMap_Inh_CRule :: (Set Identifier), children_Inh_CRule :: ([(Identifier,Type,ChildKind)]), con_Inh_CRule :: (ConstructorIdent), declsAbove_Inh_CRule :: ([Decl]), inh_Inh_CRule :: (Attributes), instVisitNrs_Inh_CRule :: (Map Identifier Int), mergeMap_Inh_CRule :: (Map Identifier (Identifier, [Identifier])), nr_Inh_CRule :: (Int), nt_Inh_CRule :: (NontermIdent), o_case_Inh_CRule :: (Bool), o_cata_Inh_CRule :: (Bool), o_clean_Inh_CRule :: (Bool), o_costcentre_Inh_CRule :: (Bool), o_data_Inh_CRule :: (Maybe Bool), o_linePragmas_Inh_CRule :: (Bool), o_monadic_Inh_CRule :: (Bool), o_newtypes_Inh_CRule :: (Bool), o_pretty_Inh_CRule :: (Bool), o_rename_Inh_CRule :: (Bool), o_sem_Inh_CRule :: (Bool), o_sig_Inh_CRule :: (Bool), o_splitsems_Inh_CRule :: (Bool), o_strictwrap_Inh_CRule :: (Bool), o_traces_Inh_CRule :: (Bool), o_unbox_Inh_CRule :: (Bool), options_Inh_CRule :: (Options), paramInstMap_Inh_CRule :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CRule :: (ParamMap), prefix_Inh_CRule :: (String), syn_Inh_CRule :: (Attributes), terminals_Inh_CRule :: ([Identifier]), unfoldSemDom_Inh_CRule :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CRule :: (Set Identifier), what_Inh_CRule :: (String) }
data Syn_CRule  = Syn_CRule { allTpsFound_Syn_CRule :: (Bool), bldBlocksFun_Syn_CRule :: (DeclBlocks -> DeclBlocks), comments_Syn_CRule :: ([String]), decls_Syn_CRule :: (Decls), declsAbove_Syn_CRule :: ([Decl]), definedInsts_Syn_CRule :: ([Identifier]), exprs_Syn_CRule :: (Exprs), tSigs_Syn_CRule :: ([Decl]), tps_Syn_CRule :: ([Type]), usedVars_Syn_CRule :: (Set String), visitedSet_Syn_CRule :: (Set Identifier) }
{-# INLINABLE wrap_CRule #-}
wrap_CRule :: T_CRule  -> Inh_CRule  -> (Syn_CRule )
wrap_CRule (T_CRule act) (Inh_CRule _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
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
data T_CRule_vIn19  = T_CRule_vIn19 (Set NontermIdent) (Set Identifier) ([(Identifier,Type,ChildKind)]) (ConstructorIdent) ([Decl]) (Attributes) (Map Identifier Int) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (String)
data T_CRule_vOut19  = T_CRule_vOut19 (Bool) (DeclBlocks -> DeclBlocks) ([String]) (Decls) ([Decl]) ([Identifier]) (Exprs) ([Decl]) ([Type]) (Set String) (Set Identifier)
{-# NOINLINE sem_CRule_CRule #-}
sem_CRule_CRule :: (Identifier) -> (Bool) -> (Bool) -> (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Maybe NontermIdent) -> (Maybe Type) -> T_Pattern  -> ([String]) -> (Map Int (Identifier,Identifier,Maybe Type)) -> (Bool) -> (String) -> (Set (Identifier, Identifier)) -> (Bool) -> (Maybe Identifier) -> T_CRule 
sem_CRule_CRule arg_name_ arg_isIn_ arg_hasCode_ arg_nt_ arg_con_ arg_field_ _ arg_tp_ arg_pattern_ arg_rhs_ arg_defines_ _ arg_origin_ arg_uses_ arg_explicit_ arg_mbNamed_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         (T_Pattern_vOut40 _patternIcopy _patternIdefinedInsts _patternIpatternAttributes) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 )
         _instTypes = rule308 _lhsIchildren
         _originComment = rule309 _lhsIo_pretty arg_origin_
         _instDecls = rule310 _definedInsts _instTypes _lhsIo_monadic _lhsIo_newtypes _lhsIoptions _lhsIprefix
         _patDescr = rule311 _patternIpatternAttributes arg_isIn_
         _traceDescr = rule312 _patDescr arg_con_ arg_mbNamed_ arg_nt_
         _addTrace = rule313 _lhsIo_traces _traceDescr
         _costCentreDescr = rule314 _patDescr arg_con_ arg_nt_
         _addCostCentre = rule315 _costCentreDescr _lhsIo_costcentre
         _addLinePragma = rule316 _lhsIo_linePragmas arg_name_
         _decls = rule317 _addCostCentre _addLinePragma _addTrace _instDecls _lhsIo_monadic _lhsIoptions _originComment _patternIcopy arg_defines_ arg_explicit_ arg_hasCode_ arg_rhs_ arg_uses_
         _definedInsts = rule318 _patternIdefinedInsts arg_isIn_
         _rulename = rule319 _lhsIoptions _lhsIterminals arg_field_ arg_isIn_ arg_name_
         _lhsOexprs :: Exprs
         _lhsOexprs = rule320 _rulename
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule321 _rulename
         _mkTp = rule322 _lhsInt _orgParams
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule323 _evalTp _lhsIchildren _lhsIoptions _mkTp arg_defines_
         _orgParams = rule324 _lhsInt _lhsIparamMap
         _evalTp = rule325 _lhsInt _lhsIoptions _lhsIparamInstMap _lhsIparamMap _orgParams
         _lhsOtps :: [Type]
         _lhsOallTpsFound :: Bool
         (_lhsOtps,_lhsOallTpsFound) = rule326 arg_tp_
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule327 _decls _lhsIdeclsAbove
         _lhsObldBlocksFun :: DeclBlocks -> DeclBlocks
         _lhsObldBlocksFun = rule328  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule329 _lhsIwhat arg_defines_
         _lhsOdecls :: Decls
         _lhsOdecls = rule330 _decls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule331 _definedInsts
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule332 _lhsIvisitedSet
         __result_ = T_CRule_vOut19 _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule308 #-}
   {-# LINE 158 "./src-ag/GenerateCode.ag" #-}
   rule308 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
                             {-# LINE 158 "./src-ag/GenerateCode.ag" #-}
                             [ (n, (t, mb, for)) | (n, NT t _ for, mb) <- _lhsIchildren ]
                             {-# LINE 2231 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule309 #-}
   {-# LINE 159 "./src-ag/GenerateCode.ag" #-}
   rule309 = \ ((_lhsIo_pretty) :: Bool) origin_ ->
                                 {-# LINE 159 "./src-ag/GenerateCode.ag" #-}
                                 if  _lhsIo_pretty
                                     then (Comment origin_:)
                                     else id
                                 {-# LINE 2239 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule310 #-}
   {-# LINE 162 "./src-ag/GenerateCode.ag" #-}
   rule310 = \ _definedInsts _instTypes ((_lhsIo_monadic) :: Bool) ((_lhsIo_newtypes) :: Bool) ((_lhsIoptions) :: Options) ((_lhsIprefix) :: String) ->
                             {-# LINE 162 "./src-ag/GenerateCode.ag" #-}
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
                             , let instLocFieldName = attrname _lhsIoptions True _INST inst
                                   instSemFieldName = attrname _lhsIoptions False _INST' inst
                             ]
                             {-# LINE 2261 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule311 #-}
   {-# LINE 179 "./src-ag/GenerateCode.ag" #-}
   rule311 = \ ((_patternIpatternAttributes) :: [(Identifier, Identifier)]) isIn_ ->
                            {-# LINE 179 "./src-ag/GenerateCode.ag" #-}
                            if isIn_
                            then "_"
                            else concat $ intersperse "," (map (\(f,a) -> show f ++ "." ++ show a) _patternIpatternAttributes)
                            {-# LINE 2269 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule312 #-}
   {-# LINE 182 "./src-ag/GenerateCode.ag" #-}
   rule312 = \ _patDescr con_ mbNamed_ nt_ ->
                              {-# LINE 182 "./src-ag/GenerateCode.ag" #-}
                              (maybe "" (\nm -> show nm ++ ":") mbNamed_) ++ show nt_ ++ " :: " ++ show con_ ++ " :: " ++ _patDescr
                              {-# LINE 2275 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule313 #-}
   {-# LINE 184 "./src-ag/GenerateCode.ag" #-}
   rule313 = \ ((_lhsIo_traces) :: Bool) _traceDescr ->
                            {-# LINE 184 "./src-ag/GenerateCode.ag" #-}
                            \v -> if _lhsIo_traces
                                  then Trace _traceDescr     v
                                  else v
                            {-# LINE 2283 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule314 #-}
   {-# LINE 187 "./src-ag/GenerateCode.ag" #-}
   rule314 = \ _patDescr con_ nt_ ->
                                   {-# LINE 187 "./src-ag/GenerateCode.ag" #-}
                                   show nt_ ++ ":" ++ show con_ ++ ":" ++ _patDescr
                                   {-# LINE 2289 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule315 #-}
   {-# LINE 188 "./src-ag/GenerateCode.ag" #-}
   rule315 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                 {-# LINE 188 "./src-ag/GenerateCode.ag" #-}
                                 \v -> if _lhsIo_costcentre
                                       then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                       else v
                                 {-# LINE 2297 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule316 #-}
   {-# LINE 191 "./src-ag/GenerateCode.ag" #-}
   rule316 = \ ((_lhsIo_linePragmas) :: Bool) name_ ->
                                 {-# LINE 191 "./src-ag/GenerateCode.ag" #-}
                                 \v -> let p = getPos name_
                                           hasPos = line p > 0 && column p >= 0 && not (null (file p))
                                       in if _lhsIo_linePragmas && hasPos
                                          then PragmaExpr True True ("LINE " ++ show (line p) ++ " " ++ show (file p))
                                               $ LineExpr
                                               $ v
                                          else v
                                 {-# LINE 2309 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule317 #-}
   {-# LINE 198 "./src-ag/GenerateCode.ag" #-}
   rule317 = \ _addCostCentre _addLinePragma _addTrace _instDecls ((_lhsIo_monadic) :: Bool) ((_lhsIoptions) :: Options) _originComment ((_patternIcopy) :: Pattern) defines_ explicit_ hasCode_ rhs_ uses_ ->
                         {-# LINE 198 "./src-ag/GenerateCode.ag" #-}
                         if hasCode_
                         then _originComment ( mkDecl (_lhsIo_monadic && explicit_) (Pattern3 _patternIcopy) (_addTrace     $ _addCostCentre     $ _addLinePragma     $ (TextExpr rhs_))
                                                    (Set.fromList [attrname _lhsIoptions False fld nm | (fld,nm,_) <- Map.elems defines_])
                                                    (Set.fromList [attrname _lhsIoptions True fld nm | (fld,nm) <- Set.toList uses_])
                                             : _instDecls    )
                         else _instDecls
                         {-# LINE 2320 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule318 #-}
   {-# LINE 268 "./src-ag/GenerateCode.ag" #-}
   rule318 = \ ((_patternIdefinedInsts) :: [Identifier]) isIn_ ->
                                {-# LINE 268 "./src-ag/GenerateCode.ag" #-}
                                if isIn_ then [] else _patternIdefinedInsts
                                {-# LINE 2326 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule319 #-}
   {-# LINE 338 "./src-ag/GenerateCode.ag" #-}
   rule319 = \ ((_lhsIoptions) :: Options) ((_lhsIterminals) :: [Identifier]) field_ isIn_ name_ ->
                            {-# LINE 338 "./src-ag/GenerateCode.ag" #-}
                            if  field_ == _LOC && name_ `elem` _lhsIterminals
                            then funname name_ 0
                            else attrname _lhsIoptions isIn_ field_ name_
                            {-# LINE 2334 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule320 #-}
   {-# LINE 341 "./src-ag/GenerateCode.ag" #-}
   rule320 = \ _rulename ->
                         {-# LINE 341 "./src-ag/GenerateCode.ag" #-}
                         [SimpleExpr _rulename    ]
                         {-# LINE 2340 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule321 #-}
   {-# LINE 357 "./src-ag/GenerateCode.ag" #-}
   rule321 = \ _rulename ->
                       {-# LINE 357 "./src-ag/GenerateCode.ag" #-}
                       Set.singleton _rulename
                       {-# LINE 2346 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule322 #-}
   {-# LINE 367 "./src-ag/GenerateCode.ag" #-}
   rule322 = \ ((_lhsInt) :: NontermIdent) _orgParams ->
                               {-# LINE 367 "./src-ag/GenerateCode.ag" #-}
                               typeToCodeType (Just _lhsInt) _orgParams
                               {-# LINE 2352 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule323 #-}
   {-# LINE 368 "./src-ag/GenerateCode.ag" #-}
   rule323 = \ _evalTp ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ((_lhsIoptions) :: Options) _mkTp defines_ ->
                                {-# LINE 368 "./src-ag/GenerateCode.ag" #-}
                                [ TSig (attrname _lhsIoptions False field attr) tp'
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
                                {-# LINE 2371 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule324 #-}
   {-# LINE 383 "./src-ag/GenerateCode.ag" #-}
   rule324 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                                    {-# LINE 383 "./src-ag/GenerateCode.ag" #-}
                                    map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                                    {-# LINE 2377 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule325 #-}
   {-# LINE 385 "./src-ag/GenerateCode.ag" #-}
   rule325 = \ ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ((_lhsIparamMap) :: ParamMap) _orgParams ->
                      {-# LINE 385 "./src-ag/GenerateCode.ag" #-}
                      \field tp -> let orgFldParams = map getName $ Map.findWithDefault [] childNt _lhsIparamMap
                                       (childNt,instParams) = Map.findWithDefault (_lhsInt,[]) field _lhsIparamInstMap
                                       replMap = Map.fromList (zip orgFldParams instParams)
                                       replace k = Map.findWithDefault ('@':k) k replMap
                                   in if null instParams
                                      then if null _orgParams
                                           then tp
                                           else idEvalType _lhsIoptions tp
                                      else evalType _lhsIoptions replace tp
                      {-# LINE 2391 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule326 #-}
   {-# LINE 420 "./src-ag/GenerateCode.ag" #-}
   rule326 = \ tp_ ->
                                            {-# LINE 420 "./src-ag/GenerateCode.ag" #-}
                                            maybe ([],False) (\tp -> ([tp],True)) tp_
                                            {-# LINE 2397 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule327 #-}
   {-# LINE 621 "./src-ag/GenerateCode.ag" #-}
   rule327 = \ _decls ((_lhsIdeclsAbove) :: [Decl]) ->
                         {-# LINE 621 "./src-ag/GenerateCode.ag" #-}
                         _lhsIdeclsAbove ++ _decls
                         {-# LINE 2403 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule328 #-}
   {-# LINE 634 "./src-ag/GenerateCode.ag" #-}
   rule328 = \  (_ :: ()) ->
                           {-# LINE 634 "./src-ag/GenerateCode.ag" #-}
                           id
                           {-# LINE 2409 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule329 #-}
   {-# LINE 909 "./src-ag/GenerateCode.ag" #-}
   rule329 = \ ((_lhsIwhat) :: String) defines_ ->
                                   {-# LINE 909 "./src-ag/GenerateCode.ag" #-}
                                   [ makeLocalComment 11 _lhsIwhat name tp | (field,name,tp) <- Map.elems defines_, field == _LOC ]
                                   ++ [ makeLocalComment 11 "inst " name tp | (field,name,tp) <- Map.elems defines_, field == _INST ]
                                   {-# LINE 2416 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule330 #-}
   rule330 = \ _decls ->
     _decls
   {-# INLINE rule331 #-}
   rule331 = \ _definedInsts ->
     _definedInsts
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
{-# NOINLINE sem_CRule_CChildVisit #-}
sem_CRule_CChildVisit :: (Identifier) -> (NontermIdent) -> (Int) -> (Attributes) -> (Attributes) -> (Bool) -> T_CRule 
sem_CRule_CChildVisit arg_name_ arg_nt_ arg_nr_ arg_inh_ arg_syn_ arg_isLast_ = T_CRule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_CRule_v19 
      v19 = \ (T_CRule_vIn19 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _visitedSet = rule333 _lhsIvisitedSet arg_name_
         _costCentreDescr = rule334 _lhsIcon _lhsInt arg_name_ arg_nr_ arg_nt_
         _addCostCentre = rule335 _costCentreDescr _lhsIo_costcentre
         _decls = rule336 _addCostCentre _lhsIaroundMap _lhsIchildren _lhsImergeMap _lhsIo_monadic _lhsIo_newtypes _lhsIo_unbox _lhsIoptions _visitedSet arg_inh_ arg_isLast_ arg_name_ arg_nr_ arg_nt_ arg_syn_
         _isSuperfluousHigherOrderIntra = rule337 _lhsIinstVisitNrs _lhsInr arg_name_
         _names = rule338 _isSuperfluousHigherOrderIntra arg_name_ arg_nr_
         _lhsOexprs :: Exprs
         _lhsOexprs = rule339 _instParams _lhsIo_newtypes _lhsIunfoldSemDom _names arg_nr_ arg_nt_
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule340 _names
         _mkTp = rule341 _evalTp _orgParams arg_nt_
         _definedTps = rule342 _lhsIoptions _mkTp arg_name_ arg_syn_
         _nextTp = rule343 arg_nr_ arg_nt_
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule344 _definedTps _instParams _nextTp arg_isLast_ arg_name_ arg_nr_
         _orgParams = rule345 _lhsIparamMap arg_nt_
         _instParams = rule346 _lhsIparamInstMap arg_name_ arg_nt_
         _replParamMap = rule347 _instParams _orgParams
         _replace = rule348 _replParamMap
         _evalTp = rule349 _lhsIoptions _orgParams _replace
         _lhsOtps :: [Type]
         _lhsOtps = rule350 _instParams _isSuperfluousHigherOrderIntra arg_nr_ arg_nt_
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule351  ()
         _lhsObldBlocksFun :: DeclBlocks -> DeclBlocks
         _lhsObldBlocksFun = rule352 _decls _lhsIdeclsAbove
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule353  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule354  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule355 _decls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule356  ()
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule357 _visitedSet
         __result_ = T_CRule_vOut19 _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_CRule_s20 v19
   {-# INLINE rule333 #-}
   {-# LINE 148 "./src-ag/GenerateCode.ag" #-}
   rule333 = \ ((_lhsIvisitedSet) :: Set Identifier) name_ ->
                                            {-# LINE 148 "./src-ag/GenerateCode.ag" #-}
                                            Set.insert name_ _lhsIvisitedSet
                                            {-# LINE 2477 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule334 #-}
   {-# LINE 204 "./src-ag/GenerateCode.ag" #-}
   rule334 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ nr_ nt_ ->
                                         {-# LINE 204 "./src-ag/GenerateCode.ag" #-}
                                         show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show name_ ++ ":" ++ show nt_ ++ ":" ++ show nr_
                                         {-# LINE 2483 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule335 #-}
   {-# LINE 205 "./src-ag/GenerateCode.ag" #-}
   rule335 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                       {-# LINE 205 "./src-ag/GenerateCode.ag" #-}
                                       \v -> if _lhsIo_costcentre
                                             then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                             else v
                                       {-# LINE 2491 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule336 #-}
   {-# LINE 208 "./src-ag/GenerateCode.ag" #-}
   rule336 = \ _addCostCentre ((_lhsIaroundMap) :: Set Identifier) ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ((_lhsIo_monadic) :: Bool) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) ((_lhsIoptions) :: Options) _visitedSet inh_ isLast_ name_ nr_ nt_ syn_ ->
                               {-# LINE 208 "./src-ag/GenerateCode.ag" #-}
                               let  lhsVars =  map (attrname _lhsIoptions True name_) (Map.keys syn_)
                                               ++ if isLast_ then [] else [unwrap ++ funname name_ (nr_+1)]
                                    rhsVars = map (attrname _lhsIoptions False name_) (Map.keys inh_)
                                    unwrap = if _lhsIo_newtypes then typeName nt_ (nr_ + 1) ++ " " else ""
                                    tuple | isMerging = TupleLhs [locname _lhsIoptions name_ ++ "_comp"]
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
                                                    = locname _lhsIoptions name_ ++ "_around " ++ funname baseNm 0
                                        | otherwise = funname baseNm nr_
                                    outDecls | isMerged  = []
                                             | otherwise =
                                                           if isMerging
                                                           then [mkDecl _lhsIo_monadic tuple rhs Set.empty Set.empty]
                                                           else [Resume _lhsIo_monadic (typeName nt_ nr_) tuple rhs]
                                    outMerged | null merges || nr_ /= 0 = []
                                              | otherwise = let (c,cs) = head merges
                                                                tuple' = mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars'
                                                                lhsVars' = map (attrname _lhsIoptions True c) (Map.keys syn_)
                                                                           ++ if isLast_ then [] else [unwrap ++ funname c (nr_+1)]
                                                                rhsVars' = [ locname _lhsIoptions c' ++ "_comp" | c' <- cs ]
                                                                fun'    = locname _lhsIoptions c ++ "_merge"
                                                                rhs' = App fun' (map SimpleExpr rhsVars')
                                                            in [Resume _lhsIo_monadic (typeName nt_ nr_) tuple' rhs']
                               in
                                  (outDecls ++ outMerged)
                               {-# LINE 2534 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule337 #-}
   {-# LINE 330 "./src-ag/GenerateCode.ag" #-}
   rule337 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ((_lhsInr) :: Int) name_ ->
            {-# LINE 330 "./src-ag/GenerateCode.ag" #-}
            _lhsInr <= Map.findWithDefault (-1) name_ _lhsIinstVisitNrs
            {-# LINE 2540 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule338 #-}
   {-# LINE 343 "./src-ag/GenerateCode.ag" #-}
   rule338 = \ _isSuperfluousHigherOrderIntra name_ nr_ ->
                     {-# LINE 343 "./src-ag/GenerateCode.ag" #-}
                     if _isSuperfluousHigherOrderIntra
                     then []
                     else [funname name_ (nr_+1)]
                     {-# LINE 2548 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule339 #-}
   {-# LINE 347 "./src-ag/GenerateCode.ag" #-}
   rule339 = \ _instParams ((_lhsIo_newtypes) :: Bool) ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) _names nr_ nt_ ->
                     {-# LINE 347 "./src-ag/GenerateCode.ag" #-}
                     let wrap = if _lhsIo_newtypes then \x -> App (typeName nt_ (nr_ + 1)) [x] else id
                         addType expr | null _instParams     = expr
                                      | otherwise            = TypedExpr expr (_lhsIunfoldSemDom nt_ (nr_+1) _instParams    )
                     in map (wrap . addType . SimpleExpr) _names
                     {-# LINE 2557 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule340 #-}
   {-# LINE 359 "./src-ag/GenerateCode.ag" #-}
   rule340 = \ _names ->
                       {-# LINE 359 "./src-ag/GenerateCode.ag" #-}
                       Set.fromList _names
                       {-# LINE 2563 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule341 #-}
   {-# LINE 395 "./src-ag/GenerateCode.ag" #-}
   rule341 = \ _evalTp _orgParams nt_ ->
                               {-# LINE 395 "./src-ag/GenerateCode.ag" #-}
                               _evalTp     . typeToCodeType (Just nt_) _orgParams
                               {-# LINE 2569 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule342 #-}
   {-# LINE 396 "./src-ag/GenerateCode.ag" #-}
   rule342 = \ ((_lhsIoptions) :: Options) _mkTp name_ syn_ ->
                                     {-# LINE 396 "./src-ag/GenerateCode.ag" #-}
                                     [ TSig (attrname _lhsIoptions True name_ a) (_mkTp tp) |  (a,tp) <- Map.toList syn_ ]
                                     {-# LINE 2575 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule343 #-}
   {-# LINE 397 "./src-ag/GenerateCode.ag" #-}
   rule343 = \ nr_ nt_ ->
                                 {-# LINE 397 "./src-ag/GenerateCode.ag" #-}
                                 typeName nt_ (nr_+1)
                                 {-# LINE 2581 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule344 #-}
   {-# LINE 398 "./src-ag/GenerateCode.ag" #-}
   rule344 = \ _definedTps _instParams _nextTp isLast_ name_ nr_ ->
                                {-# LINE 398 "./src-ag/GenerateCode.ag" #-}
                                (if isLast_ then id else (TSig (funname name_ (nr_+1)) (TypeApp (SimpleType _nextTp) (map SimpleType _instParams    )) :)) _definedTps
                                {-# LINE 2587 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule345 #-}
   {-# LINE 400 "./src-ag/GenerateCode.ag" #-}
   rule345 = \ ((_lhsIparamMap) :: ParamMap) nt_ ->
                                    {-# LINE 400 "./src-ag/GenerateCode.ag" #-}
                                    map getName $ Map.findWithDefault [] nt_ _lhsIparamMap
                                    {-# LINE 2593 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule346 #-}
   {-# LINE 401 "./src-ag/GenerateCode.ag" #-}
   rule346 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) name_ nt_ ->
                                     {-# LINE 401 "./src-ag/GenerateCode.ag" #-}
                                     snd $ Map.findWithDefault (nt_,[]) name_ _lhsIparamInstMap
                                     {-# LINE 2599 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule347 #-}
   {-# LINE 402 "./src-ag/GenerateCode.ag" #-}
   rule347 = \ _instParams _orgParams ->
                                       {-# LINE 402 "./src-ag/GenerateCode.ag" #-}
                                       Map.fromList (zip _orgParams     _instParams    )
                                       {-# LINE 2605 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule348 #-}
   {-# LINE 403 "./src-ag/GenerateCode.ag" #-}
   rule348 = \ _replParamMap ->
                                  {-# LINE 403 "./src-ag/GenerateCode.ag" #-}
                                  \k -> Map.findWithDefault k k _replParamMap
                                  {-# LINE 2611 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule349 #-}
   {-# LINE 404 "./src-ag/GenerateCode.ag" #-}
   rule349 = \ ((_lhsIoptions) :: Options) _orgParams _replace ->
                                 {-# LINE 404 "./src-ag/GenerateCode.ag" #-}
                                 if null _orgParams     then id else evalType _lhsIoptions _replace
                                 {-# LINE 2617 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule350 #-}
   {-# LINE 421 "./src-ag/GenerateCode.ag" #-}
   rule350 = \ _instParams _isSuperfluousHigherOrderIntra nr_ nt_ ->
                              {-# LINE 421 "./src-ag/GenerateCode.ag" #-}
                              if _isSuperfluousHigherOrderIntra
                              then []
                              else [NT (ntOfVisit nt_ (nr_+1)) _instParams     False]
                              {-# LINE 2625 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule351 #-}
   {-# LINE 623 "./src-ag/GenerateCode.ag" #-}
   rule351 = \  (_ :: ()) ->
                         {-# LINE 623 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 2631 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule352 #-}
   {-# LINE 636 "./src-ag/GenerateCode.ag" #-}
   rule352 = \ _decls ((_lhsIdeclsAbove) :: [Decl]) ->
                           {-# LINE 636 "./src-ag/GenerateCode.ag" #-}
                           DeclBlock _lhsIdeclsAbove (head _decls    )
                           {-# LINE 2637 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule353 #-}
   rule353 = \  (_ :: ()) ->
     True
   {-# INLINE rule354 #-}
   rule354 = \  (_ :: ()) ->
     []
   {-# INLINE rule355 #-}
   rule355 = \ _decls ->
     _decls
   {-# INLINE rule356 #-}
   rule356 = \  (_ :: ()) ->
     []
   {-# INLINE rule357 #-}
   rule357 = \ _visitedSet ->
     _visitedSet

-- CSegment ----------------------------------------------------
-- wrapper
data Inh_CSegment  = Inh_CSegment { inh_Inh_CSegment :: (Attributes), isLast_Inh_CSegment :: (Bool), nr_Inh_CSegment :: (Int), nt_Inh_CSegment :: (NontermIdent), o_case_Inh_CSegment :: (Bool), o_cata_Inh_CSegment :: (Bool), o_clean_Inh_CSegment :: (Bool), o_costcentre_Inh_CSegment :: (Bool), o_data_Inh_CSegment :: (Maybe Bool), o_linePragmas_Inh_CSegment :: (Bool), o_monadic_Inh_CSegment :: (Bool), o_newtypes_Inh_CSegment :: (Bool), o_pretty_Inh_CSegment :: (Bool), o_rename_Inh_CSegment :: (Bool), o_sem_Inh_CSegment :: (Bool), o_sig_Inh_CSegment :: (Bool), o_splitsems_Inh_CSegment :: (Bool), o_strictwrap_Inh_CSegment :: (Bool), o_traces_Inh_CSegment :: (Bool), o_unbox_Inh_CSegment :: (Bool), options_Inh_CSegment :: (Options), paramMap_Inh_CSegment :: (ParamMap), prefix_Inh_CSegment :: (String), syn_Inh_CSegment :: (Attributes) }
data Syn_CSegment  = Syn_CSegment { comments_Syn_CSegment :: ([String]), semDom_Syn_CSegment :: ([Decl]), semDomUnfoldGath_Syn_CSegment :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CSegment :: (Decls) }
{-# INLINABLE wrap_CSegment #-}
wrap_CSegment :: T_CSegment  -> Inh_CSegment  -> (Syn_CSegment )
wrap_CSegment (T_CSegment act) (Inh_CSegment _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegment_vIn22 _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
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
data T_CSegment_vIn22  = T_CSegment_vIn22 (Attributes) (Bool) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CSegment_vOut22  = T_CSegment_vOut22 ([String]) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CSegment_CSegment #-}
sem_CSegment_CSegment :: (Attributes) -> (Attributes) -> T_CSegment 
sem_CSegment_CSegment arg_inh_ arg_syn_ = T_CSegment (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_CSegment_v22 
      v22 = \ (T_CSegment_vIn22 _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _altSemForm = rule358 _lhsIoptions
         _tp = rule359 _altSemForm _indexExpr _inhTps _synTps
         _inhTps = rule360 _lhsInt _params arg_inh_
         _inhTup = rule361 _inhTps _lhsIo_unbox
         _synTps = rule362 _continuation _inhTps _lhsInt _lhsIo_unbox _params arg_syn_
         _curTypeName = rule363 _lhsInr _lhsInt
         _nextTypeName = rule364 _lhsInr _lhsInt
         _indexName = rule365 _curTypeName
         _dataIndex = rule366 _indexName _params
         _indexExpr = rule367 _indexName _params
         _indexStr = rule368 _indexName _params
         _inhInstance = rule369 _indexStr _inhTup _lhsInr _lhsInt
         _synInstance = rule370 _indexStr _lhsInr _lhsInt _synTps
         _continuation = rule371 _lhsIisLast _nextTypeName _params
         _params = rule372 _lhsInt _lhsIparamMap
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule373 _altSemForm _dataIndex _inhInstance _lhsInr _lhsInt _lhsIo_newtypes _lhsIoptions _params _synInstance _tp
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule374 _lhsInr _lhsInt _params _tp
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule375 _lhsIisLast _lhsInr _lhsInt _lhsIo_newtypes _lhsIo_unbox _lhsIoptions arg_inh_ arg_syn_
         _lhsOcomments :: [String]
         _lhsOcomments = rule376 _lhsInr arg_inh_ arg_syn_
         __result_ = T_CSegment_vOut22 _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegment_s23 v22
   {-# INLINE rule358 #-}
   {-# LINE 720 "./src-ag/GenerateCode.ag" #-}
   rule358 = \ ((_lhsIoptions) :: Options) ->
                                 {-# LINE 720 "./src-ag/GenerateCode.ag" #-}
                                 breadthFirst _lhsIoptions
                                 {-# LINE 2722 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule359 #-}
   {-# LINE 721 "./src-ag/GenerateCode.ag" #-}
   rule359 = \ _altSemForm _indexExpr _inhTps _synTps ->
                         {-# LINE 721 "./src-ag/GenerateCode.ag" #-}
                         if _altSemForm
                         then TypeApp (SimpleType "Child") [SimpleType "EvalInfo", _indexExpr     ]
                         else foldr Arr _synTps     _inhTps
                         {-# LINE 2730 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule360 #-}
   {-# LINE 724 "./src-ag/GenerateCode.ag" #-}
   rule360 = \ ((_lhsInt) :: NontermIdent) _params inh_ ->
                             {-# LINE 724 "./src-ag/GenerateCode.ag" #-}
                             [typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems inh_]
                             {-# LINE 2736 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule361 #-}
   {-# LINE 725 "./src-ag/GenerateCode.ag" #-}
   rule361 = \ _inhTps ((_lhsIo_unbox) :: Bool) ->
                             {-# LINE 725 "./src-ag/GenerateCode.ag" #-}
                             mkTupleType _lhsIo_unbox (null _inhTps    ) _inhTps
                             {-# LINE 2742 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule362 #-}
   {-# LINE 726 "./src-ag/GenerateCode.ag" #-}
   rule362 = \ _continuation _inhTps ((_lhsInt) :: NontermIdent) ((_lhsIo_unbox) :: Bool) _params syn_ ->
                             {-# LINE 726 "./src-ag/GenerateCode.ag" #-}
                             mkTupleType _lhsIo_unbox (null _inhTps    ) ([typeToCodeType (Just _lhsInt) _params     tp |  tp <- Map.elems syn_] ++ _continuation    )
                             {-# LINE 2748 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule363 #-}
   {-# LINE 727 "./src-ag/GenerateCode.ag" #-}
   rule363 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                   {-# LINE 727 "./src-ag/GenerateCode.ag" #-}
                                   typeName _lhsInt _lhsInr
                                   {-# LINE 2754 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule364 #-}
   {-# LINE 728 "./src-ag/GenerateCode.ag" #-}
   rule364 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                   {-# LINE 728 "./src-ag/GenerateCode.ag" #-}
                                   typeName _lhsInt (_lhsInr + 1)
                                   {-# LINE 2760 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule365 #-}
   {-# LINE 729 "./src-ag/GenerateCode.ag" #-}
   rule365 = \ _curTypeName ->
                                   {-# LINE 729 "./src-ag/GenerateCode.ag" #-}
                                   "I_" ++ _curTypeName
                                   {-# LINE 2766 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule366 #-}
   {-# LINE 730 "./src-ag/GenerateCode.ag" #-}
   rule366 = \ _indexName _params ->
                                {-# LINE 730 "./src-ag/GenerateCode.ag" #-}
                                Code.Data _indexName     _params     [DataAlt _indexName     []] False []
                                {-# LINE 2772 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule367 #-}
   {-# LINE 731 "./src-ag/GenerateCode.ag" #-}
   rule367 = \ _indexName _params ->
                                {-# LINE 731 "./src-ag/GenerateCode.ag" #-}
                                TypeApp (SimpleType _indexName    ) (map (SimpleType . ('@':)) _params    )
                                {-# LINE 2778 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule368 #-}
   {-# LINE 732 "./src-ag/GenerateCode.ag" #-}
   rule368 = \ _indexName _params ->
                                {-# LINE 732 "./src-ag/GenerateCode.ag" #-}
                                "(" ++ _indexName     ++ concatMap (\p -> " " ++ p) _params     ++ ")"
                                {-# LINE 2784 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule369 #-}
   {-# LINE 733 "./src-ag/GenerateCode.ag" #-}
   rule369 = \ _indexStr _inhTup ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                  {-# LINE 733 "./src-ag/GenerateCode.ag" #-}
                                  Code.Data "instance Inh" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Inh") [_inhTup    ] ] False []
                                  {-# LINE 2790 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule370 #-}
   {-# LINE 734 "./src-ag/GenerateCode.ag" #-}
   rule370 = \ _indexStr ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) _synTps ->
                                  {-# LINE 734 "./src-ag/GenerateCode.ag" #-}
                                  Code.Data "instance Syn" [_indexStr    ] [DataAlt (typeName _lhsInt _lhsInr ++ "_Syn") [_synTps    ] ] False []
                                  {-# LINE 2796 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule371 #-}
   {-# LINE 735 "./src-ag/GenerateCode.ag" #-}
   rule371 = \ ((_lhsIisLast) :: Bool) _nextTypeName _params ->
                                   {-# LINE 735 "./src-ag/GenerateCode.ag" #-}
                                   if  _lhsIisLast
                                   then []
                                   else [TypeApp (SimpleType _nextTypeName    ) (map (SimpleType . ('@':)) _params    )]
                                   {-# LINE 2804 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule372 #-}
   {-# LINE 738 "./src-ag/GenerateCode.ag" #-}
   rule372 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                             {-# LINE 738 "./src-ag/GenerateCode.ag" #-}
                             map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                             {-# LINE 2810 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule373 #-}
   {-# LINE 739 "./src-ag/GenerateCode.ag" #-}
   rule373 = \ _altSemForm _dataIndex _inhInstance ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) ((_lhsIoptions) :: Options) _params _synInstance _tp ->
                             {-# LINE 739 "./src-ag/GenerateCode.ag" #-}
                             let name = typeName _lhsInt _lhsInr
                                 evalTp | null _params     = id
                                        | otherwise        = idEvalType _lhsIoptions
                             in ( if _lhsIo_newtypes
                                  then [ Code.NewType name _params     name (evalTp _tp    ) ]
                                  else [ Code.Type name _params     (evalTp _tp    ) ] )
                                ++ ( if _altSemForm
                                     then [_dataIndex    , _inhInstance    , _synInstance    ]
                                     else [] )
                             {-# LINE 2824 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule374 #-}
   {-# LINE 753 "./src-ag/GenerateCode.ag" #-}
   rule374 = \ ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) _params _tp ->
                               {-# LINE 753 "./src-ag/GenerateCode.ag" #-}
                               Map.singleton (_lhsInt, _lhsInr) (_params    , _tp    )
                               {-# LINE 2830 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule375 #-}
   {-# LINE 837 "./src-ag/GenerateCode.ag" #-}
   rule375 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) ((_lhsIoptions) :: Options) inh_ syn_ ->
                                 {-# LINE 837 "./src-ag/GenerateCode.ag" #-}
                                 let lhsVars = map (lhsname _lhsIoptions False) (Map.keys syn_)
                                               ++ if _lhsIisLast then [] else [unwrap ++ sem (_lhsInr+1)]
                                     rhsVars = map (lhsname _lhsIoptions True) (Map.keys inh_)
                                     rhs = map SimpleExpr rhsVars
                                     unwrap = if _lhsIo_newtypes then typeName _lhsInt (_lhsInr + 1) ++ " " else ""
                                     var   = "sem"
                                     sem 0 = var
                                     sem n = var ++ "_" ++ show n
                                     ntt   = typeName _lhsInt _lhsInr
                                 in [ EvalDecl ntt (mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars) (InvokeExpr ntt (SimpleExpr $ sem _lhsInr) rhs) ]
                                 {-# LINE 2845 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule376 #-}
   {-# LINE 879 "./src-ag/GenerateCode.ag" #-}
   rule376 = \ ((_lhsInr) :: Int) inh_ syn_ ->
                                   {-# LINE 879 "./src-ag/GenerateCode.ag" #-}
                                   let body = map ind (showsSegment (CSegment inh_ syn_))
                                   in if null body
                                      then []
                                      else ("visit " ++ show _lhsInr ++ ":") : body
                                   {-# LINE 2854 "dist/build/GenerateCode.hs"#-}

-- CSegments ---------------------------------------------------
-- wrapper
data Inh_CSegments  = Inh_CSegments { inh_Inh_CSegments :: (Attributes), nr_Inh_CSegments :: (Int), nt_Inh_CSegments :: (NontermIdent), o_case_Inh_CSegments :: (Bool), o_cata_Inh_CSegments :: (Bool), o_clean_Inh_CSegments :: (Bool), o_costcentre_Inh_CSegments :: (Bool), o_data_Inh_CSegments :: (Maybe Bool), o_linePragmas_Inh_CSegments :: (Bool), o_monadic_Inh_CSegments :: (Bool), o_newtypes_Inh_CSegments :: (Bool), o_pretty_Inh_CSegments :: (Bool), o_rename_Inh_CSegments :: (Bool), o_sem_Inh_CSegments :: (Bool), o_sig_Inh_CSegments :: (Bool), o_splitsems_Inh_CSegments :: (Bool), o_strictwrap_Inh_CSegments :: (Bool), o_traces_Inh_CSegments :: (Bool), o_unbox_Inh_CSegments :: (Bool), options_Inh_CSegments :: (Options), paramMap_Inh_CSegments :: (ParamMap), prefix_Inh_CSegments :: (String), syn_Inh_CSegments :: (Attributes) }
data Syn_CSegments  = Syn_CSegments { comments_Syn_CSegments :: ([String]), isNil_Syn_CSegments :: (Bool), semDom_Syn_CSegments :: ([Decl]), semDomUnfoldGath_Syn_CSegments :: (Map (NontermIdent, Int) ([String], Code.Type)), wrapDecls_Syn_CSegments :: (Decls) }
{-# INLINABLE wrap_CSegments #-}
wrap_CSegments :: T_CSegments  -> Inh_CSegments  -> (Syn_CSegments )
wrap_CSegments (T_CSegments act) (Inh_CSegments _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn
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
data T_CSegments_vIn25  = T_CSegments_vIn25 (Attributes) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (ParamMap) (String) (Attributes)
data T_CSegments_vOut25  = T_CSegments_vOut25 ([String]) (Bool) ([Decl]) (Map (NontermIdent, Int) ([String], Code.Type)) (Decls)
{-# NOINLINE sem_CSegments_Cons #-}
sem_CSegments_Cons :: T_CSegment  -> T_CSegments  -> T_CSegments 
sem_CSegments_Cons arg_hd_ arg_tl_ = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_CSegment (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_CSegments (arg_tl_))
         (T_CSegment_vOut22 _hdIcomments _hdIsemDom _hdIsemDomUnfoldGath _hdIwrapDecls) = inv_CSegment_s23 _hdX23 (T_CSegment_vIn22 _hdOinh _hdOisLast _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_clean _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamMap _hdOprefix _hdOsyn)
         (T_CSegments_vOut25 _tlIcomments _tlIisNil _tlIsemDom _tlIsemDomUnfoldGath _tlIwrapDecls) = inv_CSegments_s26 _tlX26 (T_CSegments_vIn25 _tlOinh _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_clean _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamMap _tlOprefix _tlOsyn)
         _tlOnr = rule377 _lhsInr
         _lhsOisNil :: Bool
         _lhsOisNil = rule378  ()
         _hdOisLast = rule379 _tlIisNil
         _lhsOcomments :: [String]
         _lhsOcomments = rule380 _hdIcomments _tlIcomments
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule381 _hdIsemDom _tlIsemDom
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule382 _hdIsemDomUnfoldGath _tlIsemDomUnfoldGath
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule383 _hdIwrapDecls _tlIwrapDecls
         _hdOinh = rule384 _lhsIinh
         _hdOnr = rule385 _lhsInr
         _hdOnt = rule386 _lhsInt
         _hdOo_case = rule387 _lhsIo_case
         _hdOo_cata = rule388 _lhsIo_cata
         _hdOo_clean = rule389 _lhsIo_clean
         _hdOo_costcentre = rule390 _lhsIo_costcentre
         _hdOo_data = rule391 _lhsIo_data
         _hdOo_linePragmas = rule392 _lhsIo_linePragmas
         _hdOo_monadic = rule393 _lhsIo_monadic
         _hdOo_newtypes = rule394 _lhsIo_newtypes
         _hdOo_pretty = rule395 _lhsIo_pretty
         _hdOo_rename = rule396 _lhsIo_rename
         _hdOo_sem = rule397 _lhsIo_sem
         _hdOo_sig = rule398 _lhsIo_sig
         _hdOo_splitsems = rule399 _lhsIo_splitsems
         _hdOo_strictwrap = rule400 _lhsIo_strictwrap
         _hdOo_traces = rule401 _lhsIo_traces
         _hdOo_unbox = rule402 _lhsIo_unbox
         _hdOoptions = rule403 _lhsIoptions
         _hdOparamMap = rule404 _lhsIparamMap
         _hdOprefix = rule405 _lhsIprefix
         _hdOsyn = rule406 _lhsIsyn
         _tlOinh = rule407 _lhsIinh
         _tlOnt = rule408 _lhsInt
         _tlOo_case = rule409 _lhsIo_case
         _tlOo_cata = rule410 _lhsIo_cata
         _tlOo_clean = rule411 _lhsIo_clean
         _tlOo_costcentre = rule412 _lhsIo_costcentre
         _tlOo_data = rule413 _lhsIo_data
         _tlOo_linePragmas = rule414 _lhsIo_linePragmas
         _tlOo_monadic = rule415 _lhsIo_monadic
         _tlOo_newtypes = rule416 _lhsIo_newtypes
         _tlOo_pretty = rule417 _lhsIo_pretty
         _tlOo_rename = rule418 _lhsIo_rename
         _tlOo_sem = rule419 _lhsIo_sem
         _tlOo_sig = rule420 _lhsIo_sig
         _tlOo_splitsems = rule421 _lhsIo_splitsems
         _tlOo_strictwrap = rule422 _lhsIo_strictwrap
         _tlOo_traces = rule423 _lhsIo_traces
         _tlOo_unbox = rule424 _lhsIo_unbox
         _tlOoptions = rule425 _lhsIoptions
         _tlOparamMap = rule426 _lhsIparamMap
         _tlOprefix = rule427 _lhsIprefix
         _tlOsyn = rule428 _lhsIsyn
         __result_ = T_CSegments_vOut25 _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule377 #-}
   {-# LINE 288 "./src-ag/GenerateCode.ag" #-}
   rule377 = \ ((_lhsInr) :: Int) ->
                    {-# LINE 288 "./src-ag/GenerateCode.ag" #-}
                    _lhsInr + 1
                    {-# LINE 2962 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule378 #-}
   {-# LINE 301 "./src-ag/GenerateCode.ag" #-}
   rule378 = \  (_ :: ()) ->
                         {-# LINE 301 "./src-ag/GenerateCode.ag" #-}
                         False
                         {-# LINE 2968 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule379 #-}
   {-# LINE 302 "./src-ag/GenerateCode.ag" #-}
   rule379 = \ ((_tlIisNil) :: Bool) ->
                         {-# LINE 302 "./src-ag/GenerateCode.ag" #-}
                         _tlIisNil
                         {-# LINE 2974 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule380 #-}
   rule380 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule381 #-}
   rule381 = \ ((_hdIsemDom) :: [Decl]) ((_tlIsemDom) :: [Decl]) ->
     _hdIsemDom ++ _tlIsemDom
   {-# INLINE rule382 #-}
   rule382 = \ ((_hdIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ((_tlIsemDomUnfoldGath) :: Map (NontermIdent, Int) ([String], Code.Type)) ->
     _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
   {-# INLINE rule383 #-}
   rule383 = \ ((_hdIwrapDecls) :: Decls) ((_tlIwrapDecls) :: Decls) ->
     _hdIwrapDecls ++ _tlIwrapDecls
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule411 #-}
   rule411 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule412 #-}
   rule412 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule413 #-}
   rule413 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule414 #-}
   rule414 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule415 #-}
   rule415 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule416 #-}
   rule416 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule417 #-}
   rule417 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule418 #-}
   rule418 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule419 #-}
   rule419 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule420 #-}
   rule420 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule421 #-}
   rule421 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule422 #-}
   rule422 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule423 #-}
   rule423 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule427 #-}
   rule427 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule428 #-}
   rule428 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
{-# NOINLINE sem_CSegments_Nil #-}
sem_CSegments_Nil ::  T_CSegments 
sem_CSegments_Nil  = T_CSegments (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_CSegments_v25 
      v25 = \ (T_CSegments_vIn25 _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamMap _lhsIprefix _lhsIsyn) -> ( let
         _lhsOisNil :: Bool
         _lhsOisNil = rule429  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule430  ()
         _lhsOsemDom :: [Decl]
         _lhsOsemDom = rule431  ()
         _lhsOsemDomUnfoldGath :: Map (NontermIdent, Int) ([String], Code.Type)
         _lhsOsemDomUnfoldGath = rule432  ()
         _lhsOwrapDecls :: Decls
         _lhsOwrapDecls = rule433  ()
         __result_ = T_CSegments_vOut25 _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls
         in __result_ )
     in C_CSegments_s26 v25
   {-# INLINE rule429 #-}
   {-# LINE 303 "./src-ag/GenerateCode.ag" #-}
   rule429 = \  (_ :: ()) ->
                       {-# LINE 303 "./src-ag/GenerateCode.ag" #-}
                       True
                       {-# LINE 3147 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule430 #-}
   rule430 = \  (_ :: ()) ->
     []
   {-# INLINE rule431 #-}
   rule431 = \  (_ :: ()) ->
     []
   {-# INLINE rule432 #-}
   rule432 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule433 #-}
   rule433 = \  (_ :: ()) ->
     []

-- CVisit ------------------------------------------------------
-- wrapper
data Inh_CVisit  = Inh_CVisit { allNts_Inh_CVisit :: (Set NontermIdent), allPragmas_Inh_CVisit :: (PragmaMap), aroundMap_Inh_CVisit :: (Set Identifier), children_Inh_CVisit :: ([(Identifier,Type, ChildKind)]), con_Inh_CVisit :: (ConstructorIdent), contextMap_Inh_CVisit :: (ContextMap), decls_Inh_CVisit :: (Decls), inh_Inh_CVisit :: (Attributes), instVisitNrs_Inh_CVisit :: (Map Identifier Int), isLast_Inh_CVisit :: (Bool), mergeMap_Inh_CVisit :: (Map Identifier (Identifier, [Identifier])), nextIntra_Inh_CVisit :: (Exprs), nextIntraVars_Inh_CVisit :: (Set String), nr_Inh_CVisit :: (Int), nt_Inh_CVisit :: (NontermIdent), o_case_Inh_CVisit :: (Bool), o_cata_Inh_CVisit :: (Bool), o_clean_Inh_CVisit :: (Bool), o_costcentre_Inh_CVisit :: (Bool), o_data_Inh_CVisit :: (Maybe Bool), o_linePragmas_Inh_CVisit :: (Bool), o_monadic_Inh_CVisit :: (Bool), o_newtypes_Inh_CVisit :: (Bool), o_pretty_Inh_CVisit :: (Bool), o_rename_Inh_CVisit :: (Bool), o_sem_Inh_CVisit :: (Bool), o_sig_Inh_CVisit :: (Bool), o_splitsems_Inh_CVisit :: (Bool), o_strictwrap_Inh_CVisit :: (Bool), o_traces_Inh_CVisit :: (Bool), o_unbox_Inh_CVisit :: (Bool), options_Inh_CVisit :: (Options), paramInstMap_Inh_CVisit :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CVisit :: (ParamMap), prefix_Inh_CVisit :: (String), quantMap_Inh_CVisit :: (QuantMap), syn_Inh_CVisit :: (Attributes), terminals_Inh_CVisit :: ([Identifier]), unfoldSemDom_Inh_CVisit :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CVisit :: (Set Identifier), with_sig_Inh_CVisit :: (Bool), wrappers_Inh_CVisit :: (Set NontermIdent) }
data Syn_CVisit  = Syn_CVisit { comments_Syn_CVisit :: ([String]), decls_Syn_CVisit :: (Decls), gatherInstVisitNrs_Syn_CVisit :: (Map Identifier Int), intra_Syn_CVisit :: (Exprs), intraVars_Syn_CVisit :: (Set String), semNames_Syn_CVisit :: ([String]), visitedSet_Syn_CVisit :: (Set Identifier) }
{-# INLINABLE wrap_CVisit #-}
wrap_CVisit :: T_CVisit  -> Inh_CVisit  -> (Syn_CVisit )
wrap_CVisit (T_CVisit act) (Inh_CVisit _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisit_vIn28 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
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
data T_CVisit_vIn28  = T_CVisit_vIn28 (Set NontermIdent) (PragmaMap) (Set Identifier) ([(Identifier,Type, ChildKind)]) (ConstructorIdent) (ContextMap) (Decls) (Attributes) (Map Identifier Int) (Bool) (Map Identifier (Identifier, [Identifier])) (Exprs) (Set String) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (QuantMap) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (Bool) (Set NontermIdent)
data T_CVisit_vOut28  = T_CVisit_vOut28 ([String]) (Decls) (Map Identifier Int) (Exprs) (Set String) ([String]) (Set Identifier)
{-# NOINLINE sem_CVisit_CVisit #-}
sem_CVisit_CVisit :: (Attributes) -> (Attributes) -> T_Sequence  -> T_Sequence  -> (Bool) -> T_CVisit 
sem_CVisit_CVisit arg_inh_ arg_syn_ arg_vss_ arg_intra_ arg_ordered_ = T_CVisit (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_CVisit_v28 
      v28 = \ (T_CVisit_vIn28 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIdecls _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsImergeMap _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _vssX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_vss_))
         _intraX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_intra_))
         (T_Sequence_vOut46 _vssIallTpsFound _vssIblockDecls _vssIcomments _vssIdecls _vssIdeclsAbove _vssIdefinedInsts _vssIexprs _vssItSigs _vssItps _vssIusedVars _vssIvisitedSet) = inv_Sequence_s47 _vssX47 (T_Sequence_vIn46 _vssOallNts _vssOaroundMap _vssOchildren _vssOcon _vssOdeclsAbove _vssOinh _vssOinstVisitNrs _vssOlastExpr _vssOmergeMap _vssOnr _vssOnt _vssOo_case _vssOo_cata _vssOo_clean _vssOo_costcentre _vssOo_data _vssOo_linePragmas _vssOo_monadic _vssOo_newtypes _vssOo_pretty _vssOo_rename _vssOo_sem _vssOo_sig _vssOo_splitsems _vssOo_strictwrap _vssOo_traces _vssOo_unbox _vssOoptions _vssOparamInstMap _vssOparamMap _vssOprefix _vssOsyn _vssOterminals _vssOunfoldSemDom _vssOvisitedSet _vssOwhat)
         (T_Sequence_vOut46 _intraIallTpsFound _intraIblockDecls _intraIcomments _intraIdecls _intraIdeclsAbove _intraIdefinedInsts _intraIexprs _intraItSigs _intraItps _intraIusedVars _intraIvisitedSet) = inv_Sequence_s47 _intraX47 (T_Sequence_vIn46 _intraOallNts _intraOaroundMap _intraOchildren _intraOcon _intraOdeclsAbove _intraOinh _intraOinstVisitNrs _intraOlastExpr _intraOmergeMap _intraOnr _intraOnt _intraOo_case _intraOo_cata _intraOo_clean _intraOo_costcentre _intraOo_data _intraOo_linePragmas _intraOo_monadic _intraOo_newtypes _intraOo_pretty _intraOo_rename _intraOo_sem _intraOo_sig _intraOo_splitsems _intraOo_strictwrap _intraOo_traces _intraOo_unbox _intraOoptions _intraOparamInstMap _intraOparamMap _intraOprefix _intraOsyn _intraOterminals _intraOunfoldSemDom _intraOvisitedSet _intraOwhat)
         _lhsOintra :: Exprs
         _lhsOintra = rule434 _intraIexprs
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule435 _intraIusedVars
         (_higherOrderChildren,_firstOrderChildren) = rule436 _lhsIchildren
         _firstOrderOrig = rule437 _firstOrderChildren
         _funcname = rule438 _lhsIcon _lhsInr _lhsInt _lhsIprefix
         _nextVisitName = rule439 _lhsIisLast _lhsInr _lhsInt _lhsIprefix
         _nextVisitDecl = rule440 _lhsIcon _lhsIdecls _lhsIisLast _lhsInextIntraVars _lhsInr _lhsInt _lhsIprefix _nextVisitName
         _isOneVisit = rule441 _lhsIisLast _lhsInr
         _hasWrappers = rule442 _lhsInt _lhsIwrappers
         _refDecls = rule443 _hasWrappers _isOneVisit _lhsInt _lhsIoptions arg_syn_
         _decls = rule444 _lhsIo_clean _nextVisitDecl _refDecls _typeSigs _vssIdecls
         _vssOlastExpr = rule445 _lhsIo_unbox _lhsIoptions _nextVisitName arg_inh_ arg_syn_
         _intraOlastExpr = rule446  ()
         _lastExprVars = rule447 _lhsIoptions _nextVisitName arg_syn_
         (_blockFunDecls,_blockFirstFunCall) = rule448 _funcname _lastExprVars _nextVisitDecl _o_case _vssIblockDecls
         _costCentreDescr = rule449 _lhsIcon _lhsInr _lhsInt
         _addCostCentre = rule450 _costCentreDescr _lhsIo_costcentre
         _params = rule451 _lhsInt _lhsIparamMap
         _semFun = rule452 _addCostCentre _blockFirstFunCall _decls _declsType _firstOrderOrig _funcname _lhsInr _lhsInt _lhsIo_newtypes _lhsIo_unbox _lhsIoptions _lhsIunfoldSemDom _nextVisitName _o_splitsems _params arg_inh_ arg_ordered_ arg_syn_
         _tsig = rule453 _funcname _semType
         _semType = rule454 _firstOrderOrig _lhsIcontextMap _lhsInr _lhsInt _lhsIoptions _lhsIquantMap _params
         _lhsOdecls :: Decls
         _lhsOdecls = rule455 _blockFunDecls _lhsIwith_sig _o_splitsems _semFun _tsig arg_ordered_
         _typeSigs = rule456 _lhsIo_sig _o_case _vssItSigs
         _o_do = rule457 _lhsIo_monadic arg_ordered_
         _o_case = rule458 _lhsIallPragmas _lhsIcon _lhsInt _lhsIo_case _o_do arg_ordered_
         _declsType = rule459 _o_case _o_do
         _o_splitsems = rule460 _lhsIo_splitsems arg_ordered_
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule461 _lhsInr _vssIdefinedInsts
         _vssOdeclsAbove = rule462  ()
         _intraOdeclsAbove = rule463  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule464 _intraIcomments _lhsInr _vssIcomments
         _vssOwhat = rule465  ()
         _intraOwhat = rule466  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule467 _funcname
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule468 _intraIvisitedSet
         _vssOallNts = rule469 _lhsIallNts
         _vssOaroundMap = rule470 _lhsIaroundMap
         _vssOchildren = rule471 _lhsIchildren
         _vssOcon = rule472 _lhsIcon
         _vssOinh = rule473 _lhsIinh
         _vssOinstVisitNrs = rule474 _lhsIinstVisitNrs
         _vssOmergeMap = rule475 _lhsImergeMap
         _vssOnr = rule476 _lhsInr
         _vssOnt = rule477 _lhsInt
         _vssOo_case = rule478 _o_case
         _vssOo_cata = rule479 _lhsIo_cata
         _vssOo_clean = rule480 _lhsIo_clean
         _vssOo_costcentre = rule481 _lhsIo_costcentre
         _vssOo_data = rule482 _lhsIo_data
         _vssOo_linePragmas = rule483 _lhsIo_linePragmas
         _vssOo_monadic = rule484 _lhsIo_monadic
         _vssOo_newtypes = rule485 _lhsIo_newtypes
         _vssOo_pretty = rule486 _lhsIo_pretty
         _vssOo_rename = rule487 _lhsIo_rename
         _vssOo_sem = rule488 _lhsIo_sem
         _vssOo_sig = rule489 _lhsIo_sig
         _vssOo_splitsems = rule490 _o_splitsems
         _vssOo_strictwrap = rule491 _lhsIo_strictwrap
         _vssOo_traces = rule492 _lhsIo_traces
         _vssOo_unbox = rule493 _lhsIo_unbox
         _vssOoptions = rule494 _lhsIoptions
         _vssOparamInstMap = rule495 _lhsIparamInstMap
         _vssOparamMap = rule496 _lhsIparamMap
         _vssOprefix = rule497 _lhsIprefix
         _vssOsyn = rule498 _lhsIsyn
         _vssOterminals = rule499 _lhsIterminals
         _vssOunfoldSemDom = rule500 _lhsIunfoldSemDom
         _vssOvisitedSet = rule501 _lhsIvisitedSet
         _intraOallNts = rule502 _lhsIallNts
         _intraOaroundMap = rule503 _lhsIaroundMap
         _intraOchildren = rule504 _lhsIchildren
         _intraOcon = rule505 _lhsIcon
         _intraOinh = rule506 _lhsIinh
         _intraOinstVisitNrs = rule507 _lhsIinstVisitNrs
         _intraOmergeMap = rule508 _lhsImergeMap
         _intraOnr = rule509 _lhsInr
         _intraOnt = rule510 _lhsInt
         _intraOo_case = rule511 _o_case
         _intraOo_cata = rule512 _lhsIo_cata
         _intraOo_clean = rule513 _lhsIo_clean
         _intraOo_costcentre = rule514 _lhsIo_costcentre
         _intraOo_data = rule515 _lhsIo_data
         _intraOo_linePragmas = rule516 _lhsIo_linePragmas
         _intraOo_monadic = rule517 _lhsIo_monadic
         _intraOo_newtypes = rule518 _lhsIo_newtypes
         _intraOo_pretty = rule519 _lhsIo_pretty
         _intraOo_rename = rule520 _lhsIo_rename
         _intraOo_sem = rule521 _lhsIo_sem
         _intraOo_sig = rule522 _lhsIo_sig
         _intraOo_splitsems = rule523 _o_splitsems
         _intraOo_strictwrap = rule524 _lhsIo_strictwrap
         _intraOo_traces = rule525 _lhsIo_traces
         _intraOo_unbox = rule526 _lhsIo_unbox
         _intraOoptions = rule527 _lhsIoptions
         _intraOparamInstMap = rule528 _lhsIparamInstMap
         _intraOparamMap = rule529 _lhsIparamMap
         _intraOprefix = rule530 _lhsIprefix
         _intraOsyn = rule531 _lhsIsyn
         _intraOterminals = rule532 _lhsIterminals
         _intraOunfoldSemDom = rule533 _lhsIunfoldSemDom
         _intraOvisitedSet = rule534 _vssIvisitedSet
         __result_ = T_CVisit_vOut28 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisit_s29 v28
   {-# INLINE rule434 #-}
   {-# LINE 312 "./src-ag/GenerateCode.ag" #-}
   rule434 = \ ((_intraIexprs) :: Exprs) ->
                          {-# LINE 312 "./src-ag/GenerateCode.ag" #-}
                          _intraIexprs
                          {-# LINE 3318 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule435 #-}
   {-# LINE 313 "./src-ag/GenerateCode.ag" #-}
   rule435 = \ ((_intraIusedVars) :: Set String) ->
                              {-# LINE 313 "./src-ag/GenerateCode.ag" #-}
                              _intraIusedVars
                              {-# LINE 3324 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule436 #-}
   {-# LINE 443 "./src-ag/GenerateCode.ag" #-}
   rule436 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
                                                                 {-# LINE 443 "./src-ag/GenerateCode.ag" #-}
                                                                 partition (\(_,_,virt) -> isHigherOrder virt) _lhsIchildren
                                                                 {-# LINE 3330 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule437 #-}
   {-# LINE 444 "./src-ag/GenerateCode.ag" #-}
   rule437 = \ _firstOrderChildren ->
                                   {-# LINE 444 "./src-ag/GenerateCode.ag" #-}
                                   map pickOrigType _firstOrderChildren
                                   {-# LINE 3336 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule438 #-}
   {-# LINE 445 "./src-ag/GenerateCode.ag" #-}
   rule438 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) ->
                             {-# LINE 445 "./src-ag/GenerateCode.ag" #-}
                             seqSemname _lhsIprefix _lhsInt _lhsIcon _lhsInr
                             {-# LINE 3342 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule439 #-}
   {-# LINE 446 "./src-ag/GenerateCode.ag" #-}
   rule439 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) ->
                                  {-# LINE 446 "./src-ag/GenerateCode.ag" #-}
                                  if _lhsIisLast then [] else [visitname _lhsIprefix _lhsInt (_lhsInr+1)]
                                  {-# LINE 3348 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule440 #-}
   {-# LINE 447 "./src-ag/GenerateCode.ag" #-}
   rule440 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsIdecls) :: Decls) ((_lhsIisLast) :: Bool) ((_lhsInextIntraVars) :: Set String) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIprefix) :: String) _nextVisitName ->
                                  {-# LINE 447 "./src-ag/GenerateCode.ag" #-}
                                  let  lhs = TupleLhs _nextVisitName
                                       rhs = Let _lhsIdecls (SimpleExpr fun)
                                       fun = seqSemname _lhsIprefix _lhsInt _lhsIcon (_lhsInr+1)
                                  in if _lhsIisLast
                                     then []
                                     else [Decl lhs rhs (Set.fromList _nextVisitName) _lhsInextIntraVars]
                                  {-# LINE 3359 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule441 #-}
   {-# LINE 454 "./src-ag/GenerateCode.ag" #-}
   rule441 = \ ((_lhsIisLast) :: Bool) ((_lhsInr) :: Int) ->
                                {-# LINE 454 "./src-ag/GenerateCode.ag" #-}
                                _lhsIisLast && _lhsInr == 0
                                {-# LINE 3365 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule442 #-}
   {-# LINE 455 "./src-ag/GenerateCode.ag" #-}
   rule442 = \ ((_lhsInt) :: NontermIdent) ((_lhsIwrappers) :: Set NontermIdent) ->
                                {-# LINE 455 "./src-ag/GenerateCode.ag" #-}
                                _lhsInt `Set.member` _lhsIwrappers
                                {-# LINE 3371 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule443 #-}
   {-# LINE 456 "./src-ag/GenerateCode.ag" #-}
   rule443 = \ _hasWrappers _isOneVisit ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) syn_ ->
                             {-# LINE 456 "./src-ag/GenerateCode.ag" #-}
                             if _isOneVisit     && _hasWrappers     && reference _lhsIoptions
                             then let synAttrs = Map.toList syn_
                                      synNT = "Syn" ++ "_" ++ getName _lhsInt
                                      synVars = [ SimpleExpr (attrname _lhsIoptions False _LHS a) | (a,_) <- synAttrs ]
                                      rhs = App synNT synVars
                                      lhs = Fun "___node" []
                                  in [Decl lhs rhs Set.empty Set.empty]
                             else []
                             {-# LINE 3384 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule444 #-}
   {-# LINE 464 "./src-ag/GenerateCode.ag" #-}
   rule444 = \ ((_lhsIo_clean) :: Bool) _nextVisitDecl _refDecls _typeSigs ((_vssIdecls) :: Decls) ->
                          {-# LINE 464 "./src-ag/GenerateCode.ag" #-}
                          if _lhsIo_clean
                            then _vssIdecls ++ _nextVisitDecl ++ _refDecls
                            else _typeSigs ++ _vssIdecls ++ _nextVisitDecl ++ _refDecls
                          {-# LINE 3392 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule445 #-}
   {-# LINE 467 "./src-ag/GenerateCode.ag" #-}
   rule445 = \ ((_lhsIo_unbox) :: Bool) ((_lhsIoptions) :: Options) _nextVisitName inh_ syn_ ->
                             {-# LINE 467 "./src-ag/GenerateCode.ag" #-}
                             mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_) $ map (SimpleExpr . lhsname _lhsIoptions False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                             {-# LINE 3398 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule446 #-}
   {-# LINE 468 "./src-ag/GenerateCode.ag" #-}
   rule446 = \  (_ :: ()) ->
                               {-# LINE 468 "./src-ag/GenerateCode.ag" #-}
                               error "lastExpr: not used here"
                               {-# LINE 3404 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule447 #-}
   {-# LINE 469 "./src-ag/GenerateCode.ag" #-}
   rule447 = \ ((_lhsIoptions) :: Options) _nextVisitName syn_ ->
                                 {-# LINE 469 "./src-ag/GenerateCode.ag" #-}
                                 map (lhsname _lhsIoptions False) (Map.keys syn_) ++ _nextVisitName
                                 {-# LINE 3410 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule448 #-}
   {-# LINE 470 "./src-ag/GenerateCode.ag" #-}
   rule448 = \ _funcname _lastExprVars _nextVisitDecl _o_case ((_vssIblockDecls) :: DeclBlocks) ->
                                                           {-# LINE 470 "./src-ag/GenerateCode.ag" #-}
                                                           mkPartitionedFunction _funcname     _o_case     _nextVisitDecl     _lastExprVars     _vssIblockDecls
                                                           {-# LINE 3416 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule449 #-}
   {-# LINE 472 "./src-ag/GenerateCode.ag" #-}
   rule449 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ->
                                    {-# LINE 472 "./src-ag/GenerateCode.ag" #-}
                                    "b" ++ ":" ++ show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show _lhsInr
                                    {-# LINE 3422 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule450 #-}
   {-# LINE 473 "./src-ag/GenerateCode.ag" #-}
   rule450 = \ _costCentreDescr ((_lhsIo_costcentre) :: Bool) ->
                                  {-# LINE 473 "./src-ag/GenerateCode.ag" #-}
                                  \v -> if _lhsIo_costcentre
                                        then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                        else v
                                  {-# LINE 3430 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule451 #-}
   {-# LINE 477 "./src-ag/GenerateCode.ag" #-}
   rule451 = \ ((_lhsInt) :: NontermIdent) ((_lhsIparamMap) :: ParamMap) ->
                           {-# LINE 477 "./src-ag/GenerateCode.ag" #-}
                           map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                           {-# LINE 3436 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule452 #-}
   {-# LINE 478 "./src-ag/GenerateCode.ag" #-}
   rule452 = \ _addCostCentre _blockFirstFunCall _decls _declsType _firstOrderOrig _funcname ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIo_newtypes) :: Bool) ((_lhsIo_unbox) :: Bool) ((_lhsIoptions) :: Options) ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) _nextVisitName _o_splitsems _params inh_ ordered_ syn_ ->
                           {-# LINE 478 "./src-ag/GenerateCode.ag" #-}
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
                                                              else TypedExpr expr (idEvalType _lhsIoptions $ typeToCodeType (Just _lhsInt) _params     $ removeDeforested tp)
                                mbEvalTp | null _params     = const Nothing
                                         | otherwise        = Just . (idEvalType _lhsIoptions)
                                rhs = wrap
                                    . mkSemFun _lhsInt _lhsInr [mkLambdaArg (lhsname _lhsIoptions True nm) (mbEvalTp $ typeToCodeType (Just _lhsInt) _params     $ removeDeforested tp) | (nm,tp) <- Map.assocs inh_]
                                    $ _addCostCentre
                                    $ if ordered_ && _o_splitsems
                                      then _blockFirstFunCall
                                      else mkDecls _declsType     _decls
                                           . ResultExpr (typeName _lhsInt _lhsInr)
                                           . mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_)
                                           $ map (SimpleExpr . lhsname _lhsIoptions False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                                wrap = if  _lhsIo_newtypes
                                           then \x -> App (typeName _lhsInt _lhsInr) [x]
                                           else id
                           in Decl lhs rhs Set.empty Set.empty
                           {-# LINE 3467 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule453 #-}
   {-# LINE 509 "./src-ag/GenerateCode.ag" #-}
   rule453 = \ _funcname _semType ->
                         {-# LINE 509 "./src-ag/GenerateCode.ag" #-}
                         TSig _funcname _semType
                         {-# LINE 3473 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule454 #-}
   {-# LINE 510 "./src-ag/GenerateCode.ag" #-}
   rule454 = \ _firstOrderOrig ((_lhsIcontextMap) :: ContextMap) ((_lhsInr) :: Int) ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIquantMap) :: QuantMap) _params ->
                            {-# LINE 510 "./src-ag/GenerateCode.ag" #-}
                            let argType (NT tp tps _)  r | tp /= _SELF = typeAppStrs (sdtype tp) tps `Arr` r
                                                         | tp == _SELF = error "GenerateCode: found an intra-type with type SELF, which should have been prevented by CRule.tps"
                                argType (Haskell tp) r                 = SimpleType tp          `Arr` r
                                argType _ _ = error "Self type not allowed here"
                                evalTp | null _params     = id
                                       | otherwise        = idEvalType _lhsIoptions
                            in appQuant _lhsIquantMap _lhsInt $ appContext _lhsIcontextMap _lhsInt $ evalTp $
                               if  _lhsInr == 0
                                   then foldr argType (typeAppStrs (sdtype   _lhsInt        ) _params    ) (map (\(_,t,_) -> t) _firstOrderOrig    )
                                   else foldr argType (typeAppStrs (typeName _lhsInt _lhsInr) _params    ) []
                            {-# LINE 3488 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule455 #-}
   {-# LINE 521 "./src-ag/GenerateCode.ag" #-}
   rule455 = \ _blockFunDecls ((_lhsIwith_sig) :: Bool) _o_splitsems _semFun _tsig ordered_ ->
                           {-# LINE 521 "./src-ag/GenerateCode.ag" #-}
                           ( if  _lhsIwith_sig
                             then [_tsig, _semFun]
                             else [_semFun]
                           ) ++
                           ( if ordered_ && _o_splitsems
                             then _blockFunDecls
                             else []
                           )
                           {-# LINE 3501 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule456 #-}
   {-# LINE 529 "./src-ag/GenerateCode.ag" #-}
   rule456 = \ ((_lhsIo_sig) :: Bool) _o_case ((_vssItSigs) :: [Decl]) ->
                              {-# LINE 529 "./src-ag/GenerateCode.ag" #-}
                              if  _lhsIo_sig && not _o_case
                                  then  _vssItSigs
                                  else  []
                              {-# LINE 3509 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule457 #-}
   {-# LINE 532 "./src-ag/GenerateCode.ag" #-}
   rule457 = \ ((_lhsIo_monadic) :: Bool) ordered_ ->
                           {-# LINE 532 "./src-ag/GenerateCode.ag" #-}
                           ordered_ && _lhsIo_monadic
                           {-# LINE 3515 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule458 #-}
   {-# LINE 533 "./src-ag/GenerateCode.ag" #-}
   rule458 = \ ((_lhsIallPragmas) :: PragmaMap) ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) ((_lhsIo_case) :: Bool) _o_do ordered_ ->
                           {-# LINE 533 "./src-ag/GenerateCode.ag" #-}
                           not _o_do     && _lhsIo_case && ordered_ && not (hasPragma _lhsIallPragmas _lhsInt _lhsIcon _NOCASE)
                           {-# LINE 3521 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule459 #-}
   {-# LINE 534 "./src-ag/GenerateCode.ag" #-}
   rule459 = \ _o_case _o_do ->
                              {-# LINE 534 "./src-ag/GenerateCode.ag" #-}
                              if _o_do
                              then DeclsDo
                              else if _o_case
                                   then DeclsCase
                                   else DeclsLet
                              {-# LINE 3531 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule460 #-}
   {-# LINE 539 "./src-ag/GenerateCode.ag" #-}
   rule460 = \ ((_lhsIo_splitsems) :: Bool) ordered_ ->
                                {-# LINE 539 "./src-ag/GenerateCode.ag" #-}
                                ordered_ && _lhsIo_splitsems
                                {-# LINE 3537 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule461 #-}
   {-# LINE 573 "./src-ag/GenerateCode.ag" #-}
   rule461 = \ ((_lhsInr) :: Int) ((_vssIdefinedInsts) :: [Identifier]) ->
                                 {-# LINE 573 "./src-ag/GenerateCode.ag" #-}
                                 Map.fromList [(i,_lhsInr) | i <- _vssIdefinedInsts]
                                 {-# LINE 3543 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule462 #-}
   {-# LINE 616 "./src-ag/GenerateCode.ag" #-}
   rule462 = \  (_ :: ()) ->
                         {-# LINE 616 "./src-ag/GenerateCode.ag" #-}
                         []
                         {-# LINE 3549 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule463 #-}
   {-# LINE 617 "./src-ag/GenerateCode.ag" #-}
   rule463 = \  (_ :: ()) ->
                           {-# LINE 617 "./src-ag/GenerateCode.ag" #-}
                           error "declsAbove: not used here"
                           {-# LINE 3555 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule464 #-}
   {-# LINE 900 "./src-ag/GenerateCode.ag" #-}
   rule464 = \ ((_intraIcomments) :: [String]) ((_lhsInr) :: Int) ((_vssIcomments) :: [String]) ->
                                   {-# LINE 900 "./src-ag/GenerateCode.ag" #-}
                                   let body = map ind (_vssIcomments ++ _intraIcomments)
                                   in if null body
                                      then []
                                      else ("visit " ++ show _lhsInr ++ ":") : body
                                   {-# LINE 3564 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule465 #-}
   {-# LINE 904 "./src-ag/GenerateCode.ag" #-}
   rule465 = \  (_ :: ()) ->
                                  {-# LINE 904 "./src-ag/GenerateCode.ag" #-}
                                  "local"
                                  {-# LINE 3570 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule466 #-}
   {-# LINE 905 "./src-ag/GenerateCode.ag" #-}
   rule466 = \  (_ :: ()) ->
                                  {-# LINE 905 "./src-ag/GenerateCode.ag" #-}
                                  "intra"
                                  {-# LINE 3576 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule467 #-}
   {-# LINE 1167 "./src-ag/GenerateCode.ag" #-}
   rule467 = \ _funcname ->
                       {-# LINE 1167 "./src-ag/GenerateCode.ag" #-}
                       [_funcname    ]
                       {-# LINE 3582 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule468 #-}
   rule468 = \ ((_intraIvisitedSet) :: Set Identifier) ->
     _intraIvisitedSet
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule472 #-}
   rule472 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule473 #-}
   rule473 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule474 #-}
   rule474 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule478 #-}
   rule478 = \ _o_case ->
     _o_case
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule483 #-}
   rule483 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule484 #-}
   rule484 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule485 #-}
   rule485 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule486 #-}
   rule486 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule487 #-}
   rule487 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule490 #-}
   rule490 = \ _o_splitsems ->
     _o_splitsems
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule506 #-}
   rule506 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule507 #-}
   rule507 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule508 #-}
   rule508 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule509 #-}
   rule509 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule510 #-}
   rule510 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule511 #-}
   rule511 = \ _o_case ->
     _o_case
   {-# INLINE rule512 #-}
   rule512 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule521 #-}
   rule521 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule522 #-}
   rule522 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule523 #-}
   rule523 = \ _o_splitsems ->
     _o_splitsems
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule529 #-}
   rule529 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule530 #-}
   rule530 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule532 #-}
   rule532 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule534 #-}
   rule534 = \ ((_vssIvisitedSet) :: Set Identifier) ->
     _vssIvisitedSet

-- CVisits -----------------------------------------------------
-- wrapper
data Inh_CVisits  = Inh_CVisits { allNts_Inh_CVisits :: (Set NontermIdent), allPragmas_Inh_CVisits :: (PragmaMap), aroundMap_Inh_CVisits :: (Set Identifier), children_Inh_CVisits :: ([(Identifier,Type, ChildKind)]), con_Inh_CVisits :: (ConstructorIdent), contextMap_Inh_CVisits :: (ContextMap), inh_Inh_CVisits :: (Attributes), instVisitNrs_Inh_CVisits :: (Map Identifier Int), mergeMap_Inh_CVisits :: (Map Identifier (Identifier, [Identifier])), nr_Inh_CVisits :: (Int), nt_Inh_CVisits :: (NontermIdent), o_case_Inh_CVisits :: (Bool), o_cata_Inh_CVisits :: (Bool), o_clean_Inh_CVisits :: (Bool), o_costcentre_Inh_CVisits :: (Bool), o_data_Inh_CVisits :: (Maybe Bool), o_linePragmas_Inh_CVisits :: (Bool), o_monadic_Inh_CVisits :: (Bool), o_newtypes_Inh_CVisits :: (Bool), o_pretty_Inh_CVisits :: (Bool), o_rename_Inh_CVisits :: (Bool), o_sem_Inh_CVisits :: (Bool), o_sig_Inh_CVisits :: (Bool), o_splitsems_Inh_CVisits :: (Bool), o_strictwrap_Inh_CVisits :: (Bool), o_traces_Inh_CVisits :: (Bool), o_unbox_Inh_CVisits :: (Bool), options_Inh_CVisits :: (Options), paramInstMap_Inh_CVisits :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_CVisits :: (ParamMap), prefix_Inh_CVisits :: (String), quantMap_Inh_CVisits :: (QuantMap), syn_Inh_CVisits :: (Attributes), terminals_Inh_CVisits :: ([Identifier]), unfoldSemDom_Inh_CVisits :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_CVisits :: (Set Identifier), with_sig_Inh_CVisits :: (Bool), wrappers_Inh_CVisits :: (Set NontermIdent) }
data Syn_CVisits  = Syn_CVisits { comments_Syn_CVisits :: ([String]), decls_Syn_CVisits :: (Decls), gatherInstVisitNrs_Syn_CVisits :: (Map Identifier Int), intra_Syn_CVisits :: (Exprs), intraVars_Syn_CVisits :: (Set String), isNil_Syn_CVisits :: (Bool), semNames_Syn_CVisits :: ([String]), visitedSet_Syn_CVisits :: (Set Identifier) }
{-# INLINABLE wrap_CVisits #-}
wrap_CVisits :: T_CVisits  -> Inh_CVisits  -> (Syn_CVisits )
wrap_CVisits (T_CVisits act) (Inh_CVisits _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers
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
data T_CVisits_vIn31  = T_CVisits_vIn31 (Set NontermIdent) (PragmaMap) (Set Identifier) ([(Identifier,Type, ChildKind)]) (ConstructorIdent) (ContextMap) (Attributes) (Map Identifier Int) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (QuantMap) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (Bool) (Set NontermIdent)
data T_CVisits_vOut31  = T_CVisits_vOut31 ([String]) (Decls) (Map Identifier Int) (Exprs) (Set String) (Bool) ([String]) (Set Identifier)
{-# NOINLINE sem_CVisits_Cons #-}
sem_CVisits_Cons :: T_CVisit  -> T_CVisits  -> T_CVisits 
sem_CVisits_Cons arg_hd_ arg_tl_ = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_CVisit (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_CVisits (arg_tl_))
         (T_CVisit_vOut28 _hdIcomments _hdIdecls _hdIgatherInstVisitNrs _hdIintra _hdIintraVars _hdIsemNames _hdIvisitedSet) = inv_CVisit_s29 _hdX29 (T_CVisit_vIn28 _hdOallNts _hdOallPragmas _hdOaroundMap _hdOchildren _hdOcon _hdOcontextMap _hdOdecls _hdOinh _hdOinstVisitNrs _hdOisLast _hdOmergeMap _hdOnextIntra _hdOnextIntraVars _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_clean _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOquantMap _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwith_sig _hdOwrappers)
         (T_CVisits_vOut31 _tlIcomments _tlIdecls _tlIgatherInstVisitNrs _tlIintra _tlIintraVars _tlIisNil _tlIsemNames _tlIvisitedSet) = inv_CVisits_s32 _tlX32 (T_CVisits_vIn31 _tlOallNts _tlOallPragmas _tlOaroundMap _tlOchildren _tlOcon _tlOcontextMap _tlOinh _tlOinstVisitNrs _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_clean _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOquantMap _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwith_sig _tlOwrappers)
         _tlOnr = rule535 _lhsInr
         _lhsOisNil :: Bool
         _lhsOisNil = rule536  ()
         _hdOisLast = rule537 _tlIisNil
         _hdOnextIntra = rule538 _tlIintra
         _hdOnextIntraVars = rule539 _tlIintraVars
         _lhsOintra :: Exprs
         _lhsOintra = rule540 _hdIintra
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule541 _hdIintraVars
         _lhsOdecls :: Decls
         _lhsOdecls = rule542 _hdIdecls
         _hdOdecls = rule543 _tlIdecls
         _lhsOcomments :: [String]
         _lhsOcomments = rule544 _hdIcomments _tlIcomments
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule545 _hdIgatherInstVisitNrs _tlIgatherInstVisitNrs
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule546 _hdIsemNames _tlIsemNames
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule547 _tlIvisitedSet
         _hdOallNts = rule548 _lhsIallNts
         _hdOallPragmas = rule549 _lhsIallPragmas
         _hdOaroundMap = rule550 _lhsIaroundMap
         _hdOchildren = rule551 _lhsIchildren
         _hdOcon = rule552 _lhsIcon
         _hdOcontextMap = rule553 _lhsIcontextMap
         _hdOinh = rule554 _lhsIinh
         _hdOinstVisitNrs = rule555 _lhsIinstVisitNrs
         _hdOmergeMap = rule556 _lhsImergeMap
         _hdOnr = rule557 _lhsInr
         _hdOnt = rule558 _lhsInt
         _hdOo_case = rule559 _lhsIo_case
         _hdOo_cata = rule560 _lhsIo_cata
         _hdOo_clean = rule561 _lhsIo_clean
         _hdOo_costcentre = rule562 _lhsIo_costcentre
         _hdOo_data = rule563 _lhsIo_data
         _hdOo_linePragmas = rule564 _lhsIo_linePragmas
         _hdOo_monadic = rule565 _lhsIo_monadic
         _hdOo_newtypes = rule566 _lhsIo_newtypes
         _hdOo_pretty = rule567 _lhsIo_pretty
         _hdOo_rename = rule568 _lhsIo_rename
         _hdOo_sem = rule569 _lhsIo_sem
         _hdOo_sig = rule570 _lhsIo_sig
         _hdOo_splitsems = rule571 _lhsIo_splitsems
         _hdOo_strictwrap = rule572 _lhsIo_strictwrap
         _hdOo_traces = rule573 _lhsIo_traces
         _hdOo_unbox = rule574 _lhsIo_unbox
         _hdOoptions = rule575 _lhsIoptions
         _hdOparamInstMap = rule576 _lhsIparamInstMap
         _hdOparamMap = rule577 _lhsIparamMap
         _hdOprefix = rule578 _lhsIprefix
         _hdOquantMap = rule579 _lhsIquantMap
         _hdOsyn = rule580 _lhsIsyn
         _hdOterminals = rule581 _lhsIterminals
         _hdOunfoldSemDom = rule582 _lhsIunfoldSemDom
         _hdOvisitedSet = rule583 _lhsIvisitedSet
         _hdOwith_sig = rule584 _lhsIwith_sig
         _hdOwrappers = rule585 _lhsIwrappers
         _tlOallNts = rule586 _lhsIallNts
         _tlOallPragmas = rule587 _lhsIallPragmas
         _tlOaroundMap = rule588 _lhsIaroundMap
         _tlOchildren = rule589 _lhsIchildren
         _tlOcon = rule590 _lhsIcon
         _tlOcontextMap = rule591 _lhsIcontextMap
         _tlOinh = rule592 _lhsIinh
         _tlOinstVisitNrs = rule593 _lhsIinstVisitNrs
         _tlOmergeMap = rule594 _lhsImergeMap
         _tlOnt = rule595 _lhsInt
         _tlOo_case = rule596 _lhsIo_case
         _tlOo_cata = rule597 _lhsIo_cata
         _tlOo_clean = rule598 _lhsIo_clean
         _tlOo_costcentre = rule599 _lhsIo_costcentre
         _tlOo_data = rule600 _lhsIo_data
         _tlOo_linePragmas = rule601 _lhsIo_linePragmas
         _tlOo_monadic = rule602 _lhsIo_monadic
         _tlOo_newtypes = rule603 _lhsIo_newtypes
         _tlOo_pretty = rule604 _lhsIo_pretty
         _tlOo_rename = rule605 _lhsIo_rename
         _tlOo_sem = rule606 _lhsIo_sem
         _tlOo_sig = rule607 _lhsIo_sig
         _tlOo_splitsems = rule608 _lhsIo_splitsems
         _tlOo_strictwrap = rule609 _lhsIo_strictwrap
         _tlOo_traces = rule610 _lhsIo_traces
         _tlOo_unbox = rule611 _lhsIo_unbox
         _tlOoptions = rule612 _lhsIoptions
         _tlOparamInstMap = rule613 _lhsIparamInstMap
         _tlOparamMap = rule614 _lhsIparamMap
         _tlOprefix = rule615 _lhsIprefix
         _tlOquantMap = rule616 _lhsIquantMap
         _tlOsyn = rule617 _lhsIsyn
         _tlOterminals = rule618 _lhsIterminals
         _tlOunfoldSemDom = rule619 _lhsIunfoldSemDom
         _tlOvisitedSet = rule620 _hdIvisitedSet
         _tlOwith_sig = rule621 _lhsIwith_sig
         _tlOwrappers = rule622 _lhsIwrappers
         __result_ = T_CVisits_vOut31 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule535 #-}
   {-# LINE 284 "./src-ag/GenerateCode.ag" #-}
   rule535 = \ ((_lhsInr) :: Int) ->
                    {-# LINE 284 "./src-ag/GenerateCode.ag" #-}
                    _lhsInr + 1
                    {-# LINE 3930 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule536 #-}
   {-# LINE 297 "./src-ag/GenerateCode.ag" #-}
   rule536 = \  (_ :: ()) ->
                         {-# LINE 297 "./src-ag/GenerateCode.ag" #-}
                         False
                         {-# LINE 3936 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule537 #-}
   {-# LINE 298 "./src-ag/GenerateCode.ag" #-}
   rule537 = \ ((_tlIisNil) :: Bool) ->
                         {-# LINE 298 "./src-ag/GenerateCode.ag" #-}
                         _tlIisNil
                         {-# LINE 3942 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule538 #-}
   {-# LINE 315 "./src-ag/GenerateCode.ag" #-}
   rule538 = \ ((_tlIintra) :: Exprs) ->
                            {-# LINE 315 "./src-ag/GenerateCode.ag" #-}
                            _tlIintra
                            {-# LINE 3948 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule539 #-}
   {-# LINE 316 "./src-ag/GenerateCode.ag" #-}
   rule539 = \ ((_tlIintraVars) :: Set String) ->
                                {-# LINE 316 "./src-ag/GenerateCode.ag" #-}
                                _tlIintraVars
                                {-# LINE 3954 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule540 #-}
   {-# LINE 317 "./src-ag/GenerateCode.ag" #-}
   rule540 = \ ((_hdIintra) :: Exprs) ->
                         {-# LINE 317 "./src-ag/GenerateCode.ag" #-}
                         _hdIintra
                         {-# LINE 3960 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule541 #-}
   {-# LINE 318 "./src-ag/GenerateCode.ag" #-}
   rule541 = \ ((_hdIintraVars) :: Set String) ->
                             {-# LINE 318 "./src-ag/GenerateCode.ag" #-}
                             _hdIintraVars
                             {-# LINE 3966 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule542 #-}
   {-# LINE 433 "./src-ag/GenerateCode.ag" #-}
   rule542 = \ ((_hdIdecls) :: Decls) ->
                        {-# LINE 433 "./src-ag/GenerateCode.ag" #-}
                        _hdIdecls
                        {-# LINE 3972 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule543 #-}
   {-# LINE 434 "./src-ag/GenerateCode.ag" #-}
   rule543 = \ ((_tlIdecls) :: Decls) ->
                        {-# LINE 434 "./src-ag/GenerateCode.ag" #-}
                        _tlIdecls
                        {-# LINE 3978 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule544 #-}
   rule544 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule545 #-}
   rule545 = \ ((_hdIgatherInstVisitNrs) :: Map Identifier Int) ((_tlIgatherInstVisitNrs) :: Map Identifier Int) ->
     _hdIgatherInstVisitNrs `Map.union` _tlIgatherInstVisitNrs
   {-# INLINE rule546 #-}
   rule546 = \ ((_hdIsemNames) :: [String]) ((_tlIsemNames) :: [String]) ->
     _hdIsemNames ++ _tlIsemNames
   {-# INLINE rule547 #-}
   rule547 = \ ((_tlIvisitedSet) :: Set Identifier) ->
     _tlIvisitedSet
   {-# INLINE rule548 #-}
   rule548 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule549 #-}
   rule549 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule552 #-}
   rule552 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule553 #-}
   rule553 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule554 #-}
   rule554 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule555 #-}
   rule555 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule557 #-}
   rule557 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule558 #-}
   rule558 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule559 #-}
   rule559 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule560 #-}
   rule560 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule561 #-}
   rule561 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule562 #-}
   rule562 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule563 #-}
   rule563 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule564 #-}
   rule564 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule565 #-}
   rule565 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule566 #-}
   rule566 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule567 #-}
   rule567 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule568 #-}
   rule568 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule569 #-}
   rule569 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule570 #-}
   rule570 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule571 #-}
   rule571 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule572 #-}
   rule572 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule573 #-}
   rule573 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule574 #-}
   rule574 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule575 #-}
   rule575 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule577 #-}
   rule577 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule578 #-}
   rule578 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule579 #-}
   rule579 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule580 #-}
   rule580 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule581 #-}
   rule581 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule582 #-}
   rule582 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule583 #-}
   rule583 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule584 #-}
   rule584 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule585 #-}
   rule585 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule586 #-}
   rule586 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule587 #-}
   rule587 = \ ((_lhsIallPragmas) :: PragmaMap) ->
     _lhsIallPragmas
   {-# INLINE rule588 #-}
   rule588 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule589 #-}
   rule589 = \ ((_lhsIchildren) :: [(Identifier,Type, ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule590 #-}
   rule590 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule591 #-}
   rule591 = \ ((_lhsIcontextMap) :: ContextMap) ->
     _lhsIcontextMap
   {-# INLINE rule592 #-}
   rule592 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule593 #-}
   rule593 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule594 #-}
   rule594 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule595 #-}
   rule595 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule600 #-}
   rule600 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule605 #-}
   rule605 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule607 #-}
   rule607 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule608 #-}
   rule608 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule609 #-}
   rule609 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule610 #-}
   rule610 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule611 #-}
   rule611 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule612 #-}
   rule612 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule613 #-}
   rule613 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule614 #-}
   rule614 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule616 #-}
   rule616 = \ ((_lhsIquantMap) :: QuantMap) ->
     _lhsIquantMap
   {-# INLINE rule617 #-}
   rule617 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule618 #-}
   rule618 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule619 #-}
   rule619 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule620 #-}
   rule620 = \ ((_hdIvisitedSet) :: Set Identifier) ->
     _hdIvisitedSet
   {-# INLINE rule621 #-}
   rule621 = \ ((_lhsIwith_sig) :: Bool) ->
     _lhsIwith_sig
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_CVisits_Nil #-}
sem_CVisits_Nil ::  T_CVisits 
sem_CVisits_Nil  = T_CVisits (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_CVisits_v31 
      v31 = \ (T_CVisits_vIn31 _lhsIallNts _lhsIallPragmas _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIinh _lhsIinstVisitNrs _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIquantMap _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwith_sig _lhsIwrappers) -> ( let
         _lhsOisNil :: Bool
         _lhsOisNil = rule623  ()
         _lhsOintra :: Exprs
         _lhsOintra = rule624  ()
         _lhsOintraVars :: Set String
         _lhsOintraVars = rule625  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule626  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule627  ()
         _lhsOgatherInstVisitNrs :: Map Identifier Int
         _lhsOgatherInstVisitNrs = rule628  ()
         _lhsOsemNames :: [String]
         _lhsOsemNames = rule629  ()
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule630 _lhsIvisitedSet
         __result_ = T_CVisits_vOut31 _lhsOcomments _lhsOdecls _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames _lhsOvisitedSet
         in __result_ )
     in C_CVisits_s32 v31
   {-# INLINE rule623 #-}
   {-# LINE 299 "./src-ag/GenerateCode.ag" #-}
   rule623 = \  (_ :: ()) ->
                       {-# LINE 299 "./src-ag/GenerateCode.ag" #-}
                       True
                       {-# LINE 4247 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule624 #-}
   {-# LINE 319 "./src-ag/GenerateCode.ag" #-}
   rule624 = \  (_ :: ()) ->
                       {-# LINE 319 "./src-ag/GenerateCode.ag" #-}
                       []
                       {-# LINE 4253 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule625 #-}
   {-# LINE 320 "./src-ag/GenerateCode.ag" #-}
   rule625 = \  (_ :: ()) ->
                           {-# LINE 320 "./src-ag/GenerateCode.ag" #-}
                           Set.empty
                           {-# LINE 4259 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule626 #-}
   {-# LINE 432 "./src-ag/GenerateCode.ag" #-}
   rule626 = \  (_ :: ()) ->
                        {-# LINE 432 "./src-ag/GenerateCode.ag" #-}
                        []
                        {-# LINE 4265 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule627 #-}
   rule627 = \  (_ :: ()) ->
     []
   {-# INLINE rule628 #-}
   rule628 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule629 #-}
   rule629 = \  (_ :: ()) ->
     []
   {-# INLINE rule630 #-}
   rule630 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
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
         _nextOblockNr = rule631 _lhsIblockNr
         _lambdaName = rule632 _lhsIblockNr _lhsIprefix
         _pragmaDecl = rule633 _lambdaName
         _lhsOcallExpr :: Expr
         _lhsOcallExpr = rule634 _freeVars _lambdaName
         _freeVars = rule635 _nextIfreeVars arg_defs_ arg_visit_
         _decl = rule636 _freeVars _lambdaName _lhsIoptCase _nextIcallExpr arg_defs_ arg_visit_
         _lhsOdecls :: [Decl]
         _lhsOdecls = rule637 _decl _lhsIblockNr _nextIdecls _pragmaDecl
         _lhsOfreeVars :: [String]
         _lhsOfreeVars = rule638 _freeVars
         _nextOlastExprVars = rule639 _lhsIlastExprVars
         _nextOnextVisitDecls = rule640 _lhsInextVisitDecls
         _nextOoptCase = rule641 _lhsIoptCase
         _nextOprefix = rule642 _lhsIprefix
         __result_ = T_DeclBlocks_vOut34 _lhsOcallExpr _lhsOdecls _lhsOfreeVars
         in __result_ )
     in C_DeclBlocks_s35 v34
   {-# INLINE rule631 #-}
   {-# LINE 667 "./src-ag/GenerateCode.ag" #-}
   rule631 = \ ((_lhsIblockNr) :: Int) ->
                       {-# LINE 667 "./src-ag/GenerateCode.ag" #-}
                       _lhsIblockNr + 1
                       {-# LINE 4342 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule632 #-}
   {-# LINE 672 "./src-ag/GenerateCode.ag" #-}
   rule632 = \ ((_lhsIblockNr) :: Int) ((_lhsIprefix) :: String) ->
                         {-# LINE 672 "./src-ag/GenerateCode.ag" #-}
                         _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                         {-# LINE 4348 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule633 #-}
   {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
   rule633 = \ _lambdaName ->
                         {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
                         PragmaDecl ("NOINLINE " ++ _lambdaName    )
                         {-# LINE 4354 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule634 #-}
   {-# LINE 674 "./src-ag/GenerateCode.ag" #-}
   rule634 = \ _freeVars _lambdaName ->
                       {-# LINE 674 "./src-ag/GenerateCode.ag" #-}
                       App _lambdaName     (map SimpleExpr _freeVars    )
                       {-# LINE 4360 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule635 #-}
   {-# LINE 678 "./src-ag/GenerateCode.ag" #-}
   rule635 = \ ((_nextIfreeVars) :: [String]) defs_ visit_ ->
                       {-# LINE 678 "./src-ag/GenerateCode.ag" #-}
                       freevars _nextIfreeVars (visit_ : defs_)
                       {-# LINE 4366 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule636 #-}
   {-# LINE 685 "./src-ag/GenerateCode.ag" #-}
   rule636 = \ _freeVars _lambdaName ((_lhsIoptCase) :: Bool) ((_nextIcallExpr) :: Expr) defs_ visit_ ->
                   {-# LINE 685 "./src-ag/GenerateCode.ag" #-}
                   mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ [visit_]) _nextIcallExpr
                   {-# LINE 4372 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule637 #-}
   {-# LINE 686 "./src-ag/GenerateCode.ag" #-}
   rule637 = \ _decl ((_lhsIblockNr) :: Int) ((_nextIdecls) :: [Decl]) _pragmaDecl ->
                    {-# LINE 686 "./src-ag/GenerateCode.ag" #-}
                    (if _lhsIblockNr > 1 then [_pragmaDecl    ] else []) ++ [_decl    ] ++ _nextIdecls
                    {-# LINE 4378 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule638 #-}
   rule638 = \ _freeVars ->
     _freeVars
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIlastExprVars) :: [String]) ->
     _lhsIlastExprVars
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsInextVisitDecls) :: [Decl]) ->
     _lhsInextVisitDecls
   {-# INLINE rule641 #-}
   rule641 = \ ((_lhsIoptCase) :: Bool) ->
     _lhsIoptCase
   {-# INLINE rule642 #-}
   rule642 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
{-# NOINLINE sem_DeclBlocks_DeclTerminator #-}
sem_DeclBlocks_DeclTerminator :: ([Decl]) -> (Expr) -> T_DeclBlocks 
sem_DeclBlocks_DeclTerminator arg_defs_ arg_result_ = T_DeclBlocks (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_DeclBlocks_v34 
      v34 = \ (T_DeclBlocks_vIn34 _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix) -> ( let
         _lambdaName = rule643 _lhsIblockNr _lhsIprefix
         _pragmaDecl = rule644 _lambdaName
         _lhsOcallExpr :: Expr
         _lhsOcallExpr = rule645 _freeVars _lambdaName
         _freeVars = rule646 _lhsIlastExprVars _lhsInextVisitDecls arg_defs_
         _lhsOdecls :: [Decl]
         _lhsOdecls = rule647 _freeVars _lambdaName _lhsInextVisitDecls _lhsIoptCase arg_defs_ arg_result_
         _lhsOfreeVars :: [String]
         _lhsOfreeVars = rule648 _freeVars
         __result_ = T_DeclBlocks_vOut34 _lhsOcallExpr _lhsOdecls _lhsOfreeVars
         in __result_ )
     in C_DeclBlocks_s35 v34
   {-# INLINE rule643 #-}
   {-# LINE 672 "./src-ag/GenerateCode.ag" #-}
   rule643 = \ ((_lhsIblockNr) :: Int) ((_lhsIprefix) :: String) ->
                         {-# LINE 672 "./src-ag/GenerateCode.ag" #-}
                         _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                         {-# LINE 4418 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule644 #-}
   {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
   rule644 = \ _lambdaName ->
                         {-# LINE 673 "./src-ag/GenerateCode.ag" #-}
                         PragmaDecl ("NOINLINE " ++ _lambdaName    )
                         {-# LINE 4424 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule645 #-}
   {-# LINE 674 "./src-ag/GenerateCode.ag" #-}
   rule645 = \ _freeVars _lambdaName ->
                       {-# LINE 674 "./src-ag/GenerateCode.ag" #-}
                       App _lambdaName     (map SimpleExpr _freeVars    )
                       {-# LINE 4430 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule646 #-}
   {-# LINE 676 "./src-ag/GenerateCode.ag" #-}
   rule646 = \ ((_lhsIlastExprVars) :: [String]) ((_lhsInextVisitDecls) :: [Decl]) defs_ ->
                       {-# LINE 676 "./src-ag/GenerateCode.ag" #-}
                       freevars _lhsIlastExprVars (defs_ ++ _lhsInextVisitDecls)
                       {-# LINE 4436 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule647 #-}
   {-# LINE 683 "./src-ag/GenerateCode.ag" #-}
   rule647 = \ _freeVars _lambdaName ((_lhsInextVisitDecls) :: [Decl]) ((_lhsIoptCase) :: Bool) defs_ result_ ->
                    {-# LINE 683 "./src-ag/GenerateCode.ag" #-}
                    [ mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ _lhsInextVisitDecls) result_ ]
                    {-# LINE 4442 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule648 #-}
   rule648 = \ _freeVars ->
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
         _lhsOlambdas = rule649 _blocksIdecls
         _lhsOfirstCall :: Expr
         _lhsOfirstCall = rule650 _blocksIcallExpr
         _blocksOblockNr = rule651  ()
         _blocksOlastExprVars = rule652 _lhsIlastExprVars
         _blocksOnextVisitDecls = rule653 _lhsInextVisitDecls
         _blocksOoptCase = rule654 _lhsIoptCase
         _blocksOprefix = rule655 _lhsIprefix
         __result_ = T_DeclBlocksRoot_vOut37 _lhsOfirstCall _lhsOlambdas
         in __result_ )
     in C_DeclBlocksRoot_s38 v37
   {-# INLINE rule649 #-}
   {-# LINE 658 "./src-ag/GenerateCode.ag" #-}
   rule649 = \ ((_blocksIdecls) :: [Decl]) ->
                       {-# LINE 658 "./src-ag/GenerateCode.ag" #-}
                       _blocksIdecls
                       {-# LINE 4503 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule650 #-}
   {-# LINE 659 "./src-ag/GenerateCode.ag" #-}
   rule650 = \ ((_blocksIcallExpr) :: Expr) ->
                        {-# LINE 659 "./src-ag/GenerateCode.ag" #-}
                        _blocksIcallExpr
                        {-# LINE 4509 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule651 #-}
   {-# LINE 664 "./src-ag/GenerateCode.ag" #-}
   rule651 = \  (_ :: ()) ->
                         {-# LINE 664 "./src-ag/GenerateCode.ag" #-}
                         1
                         {-# LINE 4515 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule652 #-}
   rule652 = \ ((_lhsIlastExprVars) :: [String]) ->
     _lhsIlastExprVars
   {-# INLINE rule653 #-}
   rule653 = \ ((_lhsInextVisitDecls) :: [Decl]) ->
     _lhsInextVisitDecls
   {-# INLINE rule654 #-}
   rule654 = \ ((_lhsIoptCase) :: Bool) ->
     _lhsIoptCase
   {-# INLINE rule655 #-}
   rule655 = \ ((_lhsIprefix) :: String) ->
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
         _lhsOdefinedInsts = rule656 _patsIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule657 _patsIpatternAttributes
         _copy = rule658 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule659 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule656 #-}
   rule656 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule657 #-}
   rule657 = \ ((_patsIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patsIpatternAttributes
   {-# INLINE rule658 #-}
   rule658 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule659 #-}
   rule659 = \ _copy ->
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
         _lhsOdefinedInsts = rule660 _patsIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule661 _patsIpatternAttributes
         _copy = rule662 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule663 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule660 #-}
   rule660 = \ ((_patsIdefinedInsts) :: [Identifier]) ->
     _patsIdefinedInsts
   {-# INLINE rule661 #-}
   rule661 = \ ((_patsIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patsIpatternAttributes
   {-# INLINE rule662 #-}
   rule662 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule663 #-}
   rule663 = \ _copy ->
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
         _lhsOdefinedInsts = rule664 _patIdefinedInsts arg_attr_ arg_field_
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule665 _patIpatternAttributes arg_attr_ arg_field_
         _copy = rule666 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule667 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule664 #-}
   {-# LINE 265 "./src-ag/GenerateCode.ag" #-}
   rule664 = \ ((_patIdefinedInsts) :: [Identifier]) attr_ field_ ->
                               {-# LINE 265 "./src-ag/GenerateCode.ag" #-}
                               (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                               {-# LINE 4649 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule665 #-}
   {-# LINE 273 "./src-ag/GenerateCode.ag" #-}
   rule665 = \ ((_patIpatternAttributes) :: [(Identifier, Identifier)]) attr_ field_ ->
                                {-# LINE 273 "./src-ag/GenerateCode.ag" #-}
                                (field_,attr_) : _patIpatternAttributes
                                {-# LINE 4655 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule666 #-}
   rule666 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule667 #-}
   rule667 = \ _copy ->
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
         _lhsOdefinedInsts = rule668 _patIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule669 _patIpatternAttributes
         _copy = rule670 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule671 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule668 #-}
   rule668 = \ ((_patIdefinedInsts) :: [Identifier]) ->
     _patIdefinedInsts
   {-# INLINE rule669 #-}
   rule669 = \ ((_patIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _patIpatternAttributes
   {-# INLINE rule670 #-}
   rule670 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule671 #-}
   rule671 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 ) -> ( let
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule672  ()
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule673  ()
         _copy = rule674 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule675 _copy
         __result_ = T_Pattern_vOut40 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule672 #-}
   rule672 = \  (_ :: ()) ->
     []
   {-# INLINE rule673 #-}
   rule673 = \  (_ :: ()) ->
     []
   {-# INLINE rule674 #-}
   rule674 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule675 #-}
   rule675 = \ _copy ->
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
         _lhsOdefinedInsts = rule676 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule677 _hdIpatternAttributes _tlIpatternAttributes
         _copy = rule678 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule679 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule676 #-}
   rule676 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule677 #-}
   rule677 = \ ((_hdIpatternAttributes) :: [(Identifier, Identifier)]) ((_tlIpatternAttributes) :: [(Identifier, Identifier)]) ->
     _hdIpatternAttributes ++ _tlIpatternAttributes
   {-# INLINE rule678 #-}
   rule678 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule679 #-}
   rule679 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 ) -> ( let
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule680  ()
         _lhsOpatternAttributes :: [(Identifier, Identifier)]
         _lhsOpatternAttributes = rule681  ()
         _copy = rule682  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule683 _copy
         __result_ = T_Patterns_vOut43 _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule680 #-}
   rule680 = \  (_ :: ()) ->
     []
   {-# INLINE rule681 #-}
   rule681 = \  (_ :: ()) ->
     []
   {-# INLINE rule682 #-}
   rule682 = \  (_ :: ()) ->
     []
   {-# INLINE rule683 #-}
   rule683 = \ _copy ->
     _copy

-- Sequence ----------------------------------------------------
-- wrapper
data Inh_Sequence  = Inh_Sequence { allNts_Inh_Sequence :: (Set NontermIdent), aroundMap_Inh_Sequence :: (Set Identifier), children_Inh_Sequence :: ([(Identifier,Type,ChildKind)]), con_Inh_Sequence :: (ConstructorIdent), declsAbove_Inh_Sequence :: ([Decl]), inh_Inh_Sequence :: (Attributes), instVisitNrs_Inh_Sequence :: (Map Identifier Int), lastExpr_Inh_Sequence :: (Expr), mergeMap_Inh_Sequence :: (Map Identifier (Identifier, [Identifier])), nr_Inh_Sequence :: (Int), nt_Inh_Sequence :: (NontermIdent), o_case_Inh_Sequence :: (Bool), o_cata_Inh_Sequence :: (Bool), o_clean_Inh_Sequence :: (Bool), o_costcentre_Inh_Sequence :: (Bool), o_data_Inh_Sequence :: (Maybe Bool), o_linePragmas_Inh_Sequence :: (Bool), o_monadic_Inh_Sequence :: (Bool), o_newtypes_Inh_Sequence :: (Bool), o_pretty_Inh_Sequence :: (Bool), o_rename_Inh_Sequence :: (Bool), o_sem_Inh_Sequence :: (Bool), o_sig_Inh_Sequence :: (Bool), o_splitsems_Inh_Sequence :: (Bool), o_strictwrap_Inh_Sequence :: (Bool), o_traces_Inh_Sequence :: (Bool), o_unbox_Inh_Sequence :: (Bool), options_Inh_Sequence :: (Options), paramInstMap_Inh_Sequence :: (Map Identifier (NontermIdent, [String])), paramMap_Inh_Sequence :: (ParamMap), prefix_Inh_Sequence :: (String), syn_Inh_Sequence :: (Attributes), terminals_Inh_Sequence :: ([Identifier]), unfoldSemDom_Inh_Sequence :: (NontermIdent -> Int -> [String] -> Code.Type), visitedSet_Inh_Sequence :: (Set Identifier), what_Inh_Sequence :: (String) }
data Syn_Sequence  = Syn_Sequence { allTpsFound_Syn_Sequence :: (Bool), blockDecls_Syn_Sequence :: (DeclBlocks), comments_Syn_Sequence :: ([String]), decls_Syn_Sequence :: (Decls), declsAbove_Syn_Sequence :: ([Decl]), definedInsts_Syn_Sequence :: ([Identifier]), exprs_Syn_Sequence :: (Exprs), tSigs_Syn_Sequence :: ([Decl]), tps_Syn_Sequence :: ([Type]), usedVars_Syn_Sequence :: (Set String), visitedSet_Syn_Sequence :: (Set Identifier) }
{-# INLINABLE wrap_Sequence #-}
wrap_Sequence :: T_Sequence  -> Inh_Sequence  -> (Syn_Sequence )
wrap_Sequence (T_Sequence act) (Inh_Sequence _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat
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
data T_Sequence_vIn46  = T_Sequence_vIn46 (Set NontermIdent) (Set Identifier) ([(Identifier,Type,ChildKind)]) (ConstructorIdent) ([Decl]) (Attributes) (Map Identifier Int) (Expr) (Map Identifier (Identifier, [Identifier])) (Int) (NontermIdent) (Bool) (Bool) (Bool) (Bool) (Maybe Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Bool) (Options) (Map Identifier (NontermIdent, [String])) (ParamMap) (String) (Attributes) ([Identifier]) (NontermIdent -> Int -> [String] -> Code.Type) (Set Identifier) (String)
data T_Sequence_vOut46  = T_Sequence_vOut46 (Bool) (DeclBlocks) ([String]) (Decls) ([Decl]) ([Identifier]) (Exprs) ([Decl]) ([Type]) (Set String) (Set Identifier)
{-# NOINLINE sem_Sequence_Cons #-}
sem_Sequence_Cons :: T_CRule  -> T_Sequence  -> T_Sequence 
sem_Sequence_Cons arg_hd_ arg_tl_ = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_CRule (arg_hd_))
         _tlX47 = Control.Monad.Identity.runIdentity (attach_T_Sequence (arg_tl_))
         (T_CRule_vOut19 _hdIallTpsFound _hdIbldBlocksFun _hdIcomments _hdIdecls _hdIdeclsAbove _hdIdefinedInsts _hdIexprs _hdItSigs _hdItps _hdIusedVars _hdIvisitedSet) = inv_CRule_s20 _hdX20 (T_CRule_vIn19 _hdOallNts _hdOaroundMap _hdOchildren _hdOcon _hdOdeclsAbove _hdOinh _hdOinstVisitNrs _hdOmergeMap _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_clean _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_monadic _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOoptions _hdOparamInstMap _hdOparamMap _hdOprefix _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOvisitedSet _hdOwhat)
         (T_Sequence_vOut46 _tlIallTpsFound _tlIblockDecls _tlIcomments _tlIdecls _tlIdeclsAbove _tlIdefinedInsts _tlIexprs _tlItSigs _tlItps _tlIusedVars _tlIvisitedSet) = inv_Sequence_s47 _tlX47 (T_Sequence_vIn46 _tlOallNts _tlOaroundMap _tlOchildren _tlOcon _tlOdeclsAbove _tlOinh _tlOinstVisitNrs _tlOlastExpr _tlOmergeMap _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_clean _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_monadic _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOoptions _tlOparamInstMap _tlOparamMap _tlOprefix _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOvisitedSet _tlOwhat)
         _lhsOblockDecls :: DeclBlocks
         _lhsOblockDecls = rule684 _hdIbldBlocksFun _tlIblockDecls
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule685 _hdIallTpsFound _tlIallTpsFound
         _lhsOcomments :: [String]
         _lhsOcomments = rule686 _hdIcomments _tlIcomments
         _lhsOdecls :: Decls
         _lhsOdecls = rule687 _hdIdecls _tlIdecls
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule688 _hdIdefinedInsts _tlIdefinedInsts
         _lhsOexprs :: Exprs
         _lhsOexprs = rule689 _hdIexprs _tlIexprs
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule690 _hdItSigs _tlItSigs
         _lhsOtps :: [Type]
         _lhsOtps = rule691 _hdItps _tlItps
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule692 _hdIusedVars _tlIusedVars
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule693 _tlIdeclsAbove
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule694 _tlIvisitedSet
         _hdOallNts = rule695 _lhsIallNts
         _hdOaroundMap = rule696 _lhsIaroundMap
         _hdOchildren = rule697 _lhsIchildren
         _hdOcon = rule698 _lhsIcon
         _hdOdeclsAbove = rule699 _lhsIdeclsAbove
         _hdOinh = rule700 _lhsIinh
         _hdOinstVisitNrs = rule701 _lhsIinstVisitNrs
         _hdOmergeMap = rule702 _lhsImergeMap
         _hdOnr = rule703 _lhsInr
         _hdOnt = rule704 _lhsInt
         _hdOo_case = rule705 _lhsIo_case
         _hdOo_cata = rule706 _lhsIo_cata
         _hdOo_clean = rule707 _lhsIo_clean
         _hdOo_costcentre = rule708 _lhsIo_costcentre
         _hdOo_data = rule709 _lhsIo_data
         _hdOo_linePragmas = rule710 _lhsIo_linePragmas
         _hdOo_monadic = rule711 _lhsIo_monadic
         _hdOo_newtypes = rule712 _lhsIo_newtypes
         _hdOo_pretty = rule713 _lhsIo_pretty
         _hdOo_rename = rule714 _lhsIo_rename
         _hdOo_sem = rule715 _lhsIo_sem
         _hdOo_sig = rule716 _lhsIo_sig
         _hdOo_splitsems = rule717 _lhsIo_splitsems
         _hdOo_strictwrap = rule718 _lhsIo_strictwrap
         _hdOo_traces = rule719 _lhsIo_traces
         _hdOo_unbox = rule720 _lhsIo_unbox
         _hdOoptions = rule721 _lhsIoptions
         _hdOparamInstMap = rule722 _lhsIparamInstMap
         _hdOparamMap = rule723 _lhsIparamMap
         _hdOprefix = rule724 _lhsIprefix
         _hdOsyn = rule725 _lhsIsyn
         _hdOterminals = rule726 _lhsIterminals
         _hdOunfoldSemDom = rule727 _lhsIunfoldSemDom
         _hdOvisitedSet = rule728 _lhsIvisitedSet
         _hdOwhat = rule729 _lhsIwhat
         _tlOallNts = rule730 _lhsIallNts
         _tlOaroundMap = rule731 _lhsIaroundMap
         _tlOchildren = rule732 _lhsIchildren
         _tlOcon = rule733 _lhsIcon
         _tlOdeclsAbove = rule734 _hdIdeclsAbove
         _tlOinh = rule735 _lhsIinh
         _tlOinstVisitNrs = rule736 _lhsIinstVisitNrs
         _tlOlastExpr = rule737 _lhsIlastExpr
         _tlOmergeMap = rule738 _lhsImergeMap
         _tlOnr = rule739 _lhsInr
         _tlOnt = rule740 _lhsInt
         _tlOo_case = rule741 _lhsIo_case
         _tlOo_cata = rule742 _lhsIo_cata
         _tlOo_clean = rule743 _lhsIo_clean
         _tlOo_costcentre = rule744 _lhsIo_costcentre
         _tlOo_data = rule745 _lhsIo_data
         _tlOo_linePragmas = rule746 _lhsIo_linePragmas
         _tlOo_monadic = rule747 _lhsIo_monadic
         _tlOo_newtypes = rule748 _lhsIo_newtypes
         _tlOo_pretty = rule749 _lhsIo_pretty
         _tlOo_rename = rule750 _lhsIo_rename
         _tlOo_sem = rule751 _lhsIo_sem
         _tlOo_sig = rule752 _lhsIo_sig
         _tlOo_splitsems = rule753 _lhsIo_splitsems
         _tlOo_strictwrap = rule754 _lhsIo_strictwrap
         _tlOo_traces = rule755 _lhsIo_traces
         _tlOo_unbox = rule756 _lhsIo_unbox
         _tlOoptions = rule757 _lhsIoptions
         _tlOparamInstMap = rule758 _lhsIparamInstMap
         _tlOparamMap = rule759 _lhsIparamMap
         _tlOprefix = rule760 _lhsIprefix
         _tlOsyn = rule761 _lhsIsyn
         _tlOterminals = rule762 _lhsIterminals
         _tlOunfoldSemDom = rule763 _lhsIunfoldSemDom
         _tlOvisitedSet = rule764 _hdIvisitedSet
         _tlOwhat = rule765 _lhsIwhat
         __result_ = T_Sequence_vOut46 _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_Sequence_s47 v46
   {-# INLINE rule684 #-}
   {-# LINE 627 "./src-ag/GenerateCode.ag" #-}
   rule684 = \ ((_hdIbldBlocksFun) :: DeclBlocks -> DeclBlocks) ((_tlIblockDecls) :: DeclBlocks) ->
                         {-# LINE 627 "./src-ag/GenerateCode.ag" #-}
                         _hdIbldBlocksFun _tlIblockDecls
                         {-# LINE 4958 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule685 #-}
   rule685 = \ ((_hdIallTpsFound) :: Bool) ((_tlIallTpsFound) :: Bool) ->
     _hdIallTpsFound && _tlIallTpsFound
   {-# INLINE rule686 #-}
   rule686 = \ ((_hdIcomments) :: [String]) ((_tlIcomments) :: [String]) ->
     _hdIcomments ++ _tlIcomments
   {-# INLINE rule687 #-}
   rule687 = \ ((_hdIdecls) :: Decls) ((_tlIdecls) :: Decls) ->
     _hdIdecls ++ _tlIdecls
   {-# INLINE rule688 #-}
   rule688 = \ ((_hdIdefinedInsts) :: [Identifier]) ((_tlIdefinedInsts) :: [Identifier]) ->
     _hdIdefinedInsts ++ _tlIdefinedInsts
   {-# INLINE rule689 #-}
   rule689 = \ ((_hdIexprs) :: Exprs) ((_tlIexprs) :: Exprs) ->
     _hdIexprs ++ _tlIexprs
   {-# INLINE rule690 #-}
   rule690 = \ ((_hdItSigs) :: [Decl]) ((_tlItSigs) :: [Decl]) ->
     _hdItSigs ++ _tlItSigs
   {-# INLINE rule691 #-}
   rule691 = \ ((_hdItps) :: [Type]) ((_tlItps) :: [Type]) ->
     _hdItps ++ _tlItps
   {-# INLINE rule692 #-}
   rule692 = \ ((_hdIusedVars) :: Set String) ((_tlIusedVars) :: Set String) ->
     _hdIusedVars `Set.union` _tlIusedVars
   {-# INLINE rule693 #-}
   rule693 = \ ((_tlIdeclsAbove) :: [Decl]) ->
     _tlIdeclsAbove
   {-# INLINE rule694 #-}
   rule694 = \ ((_tlIvisitedSet) :: Set Identifier) ->
     _tlIvisitedSet
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule696 #-}
   rule696 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule697 #-}
   rule697 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule698 #-}
   rule698 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule699 #-}
   rule699 = \ ((_lhsIdeclsAbove) :: [Decl]) ->
     _lhsIdeclsAbove
   {-# INLINE rule700 #-}
   rule700 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule701 #-}
   rule701 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule702 #-}
   rule702 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule703 #-}
   rule703 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule704 #-}
   rule704 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule705 #-}
   rule705 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule706 #-}
   rule706 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule707 #-}
   rule707 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule716 #-}
   rule716 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule718 #-}
   rule718 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule721 #-}
   rule721 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule724 #-}
   rule724 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule725 #-}
   rule725 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
   {-# INLINE rule729 #-}
   rule729 = \ ((_lhsIwhat) :: String) ->
     _lhsIwhat
   {-# INLINE rule730 #-}
   rule730 = \ ((_lhsIallNts) :: Set NontermIdent) ->
     _lhsIallNts
   {-# INLINE rule731 #-}
   rule731 = \ ((_lhsIaroundMap) :: Set Identifier) ->
     _lhsIaroundMap
   {-# INLINE rule732 #-}
   rule732 = \ ((_lhsIchildren) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIchildren
   {-# INLINE rule733 #-}
   rule733 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule734 #-}
   rule734 = \ ((_hdIdeclsAbove) :: [Decl]) ->
     _hdIdeclsAbove
   {-# INLINE rule735 #-}
   rule735 = \ ((_lhsIinh) :: Attributes) ->
     _lhsIinh
   {-# INLINE rule736 #-}
   rule736 = \ ((_lhsIinstVisitNrs) :: Map Identifier Int) ->
     _lhsIinstVisitNrs
   {-# INLINE rule737 #-}
   rule737 = \ ((_lhsIlastExpr) :: Expr) ->
     _lhsIlastExpr
   {-# INLINE rule738 #-}
   rule738 = \ ((_lhsImergeMap) :: Map Identifier (Identifier, [Identifier])) ->
     _lhsImergeMap
   {-# INLINE rule739 #-}
   rule739 = \ ((_lhsInr) :: Int) ->
     _lhsInr
   {-# INLINE rule740 #-}
   rule740 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule741 #-}
   rule741 = \ ((_lhsIo_case) :: Bool) ->
     _lhsIo_case
   {-# INLINE rule742 #-}
   rule742 = \ ((_lhsIo_cata) :: Bool) ->
     _lhsIo_cata
   {-# INLINE rule743 #-}
   rule743 = \ ((_lhsIo_clean) :: Bool) ->
     _lhsIo_clean
   {-# INLINE rule744 #-}
   rule744 = \ ((_lhsIo_costcentre) :: Bool) ->
     _lhsIo_costcentre
   {-# INLINE rule745 #-}
   rule745 = \ ((_lhsIo_data) :: Maybe Bool) ->
     _lhsIo_data
   {-# INLINE rule746 #-}
   rule746 = \ ((_lhsIo_linePragmas) :: Bool) ->
     _lhsIo_linePragmas
   {-# INLINE rule747 #-}
   rule747 = \ ((_lhsIo_monadic) :: Bool) ->
     _lhsIo_monadic
   {-# INLINE rule748 #-}
   rule748 = \ ((_lhsIo_newtypes) :: Bool) ->
     _lhsIo_newtypes
   {-# INLINE rule749 #-}
   rule749 = \ ((_lhsIo_pretty) :: Bool) ->
     _lhsIo_pretty
   {-# INLINE rule750 #-}
   rule750 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule751 #-}
   rule751 = \ ((_lhsIo_sem) :: Bool) ->
     _lhsIo_sem
   {-# INLINE rule752 #-}
   rule752 = \ ((_lhsIo_sig) :: Bool) ->
     _lhsIo_sig
   {-# INLINE rule753 #-}
   rule753 = \ ((_lhsIo_splitsems) :: Bool) ->
     _lhsIo_splitsems
   {-# INLINE rule754 #-}
   rule754 = \ ((_lhsIo_strictwrap) :: Bool) ->
     _lhsIo_strictwrap
   {-# INLINE rule755 #-}
   rule755 = \ ((_lhsIo_traces) :: Bool) ->
     _lhsIo_traces
   {-# INLINE rule756 #-}
   rule756 = \ ((_lhsIo_unbox) :: Bool) ->
     _lhsIo_unbox
   {-# INLINE rule757 #-}
   rule757 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule758 #-}
   rule758 = \ ((_lhsIparamInstMap) :: Map Identifier (NontermIdent, [String])) ->
     _lhsIparamInstMap
   {-# INLINE rule759 #-}
   rule759 = \ ((_lhsIparamMap) :: ParamMap) ->
     _lhsIparamMap
   {-# INLINE rule760 #-}
   rule760 = \ ((_lhsIprefix) :: String) ->
     _lhsIprefix
   {-# INLINE rule761 #-}
   rule761 = \ ((_lhsIsyn) :: Attributes) ->
     _lhsIsyn
   {-# INLINE rule762 #-}
   rule762 = \ ((_lhsIterminals) :: [Identifier]) ->
     _lhsIterminals
   {-# INLINE rule763 #-}
   rule763 = \ ((_lhsIunfoldSemDom) :: NontermIdent -> Int -> [String] -> Code.Type) ->
     _lhsIunfoldSemDom
   {-# INLINE rule764 #-}
   rule764 = \ ((_hdIvisitedSet) :: Set Identifier) ->
     _hdIvisitedSet
   {-# INLINE rule765 #-}
   rule765 = \ ((_lhsIwhat) :: String) ->
     _lhsIwhat
{-# NOINLINE sem_Sequence_Nil #-}
sem_Sequence_Nil ::  T_Sequence 
sem_Sequence_Nil  = T_Sequence (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Sequence_v46 
      v46 = \ (T_Sequence_vIn46 _lhsIallNts _lhsIaroundMap _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsImergeMap _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_clean _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_monadic _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIoptions _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIvisitedSet _lhsIwhat) -> ( let
         _lhsOblockDecls :: DeclBlocks
         _lhsOblockDecls = rule766 _lhsIdeclsAbove _lhsIlastExpr
         _lhsOallTpsFound :: Bool
         _lhsOallTpsFound = rule767  ()
         _lhsOcomments :: [String]
         _lhsOcomments = rule768  ()
         _lhsOdecls :: Decls
         _lhsOdecls = rule769  ()
         _lhsOdefinedInsts :: [Identifier]
         _lhsOdefinedInsts = rule770  ()
         _lhsOexprs :: Exprs
         _lhsOexprs = rule771  ()
         _lhsOtSigs :: [Decl]
         _lhsOtSigs = rule772  ()
         _lhsOtps :: [Type]
         _lhsOtps = rule773  ()
         _lhsOusedVars :: Set String
         _lhsOusedVars = rule774  ()
         _lhsOdeclsAbove :: [Decl]
         _lhsOdeclsAbove = rule775 _lhsIdeclsAbove
         _lhsOvisitedSet :: Set Identifier
         _lhsOvisitedSet = rule776 _lhsIvisitedSet
         __result_ = T_Sequence_vOut46 _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars _lhsOvisitedSet
         in __result_ )
     in C_Sequence_s47 v46
   {-# INLINE rule766 #-}
   {-# LINE 629 "./src-ag/GenerateCode.ag" #-}
   rule766 = \ ((_lhsIdeclsAbove) :: [Decl]) ((_lhsIlastExpr) :: Expr) ->
                         {-# LINE 629 "./src-ag/GenerateCode.ag" #-}
                         DeclTerminator _lhsIdeclsAbove _lhsIlastExpr
                         {-# LINE 5239 "dist/build/GenerateCode.hs"#-}
   {-# INLINE rule767 #-}
   rule767 = \  (_ :: ()) ->
     True
   {-# INLINE rule768 #-}
   rule768 = \  (_ :: ()) ->
     []
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
     []
   {-# INLINE rule773 #-}
   rule773 = \  (_ :: ()) ->
     []
   {-# INLINE rule774 #-}
   rule774 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule775 #-}
   rule775 = \ ((_lhsIdeclsAbove) :: [Decl]) ->
     _lhsIdeclsAbove
   {-# INLINE rule776 #-}
   rule776 = \ ((_lhsIvisitedSet) :: Set Identifier) ->
     _lhsIvisitedSet
