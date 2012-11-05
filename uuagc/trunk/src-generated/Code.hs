

-- UUAGC 0.9.42.1 (src-ag/Code.ag)
module Code where
{-# LINE 2 "./src-ag/Code.ag" #-}

import Pretty
import Patterns
import Data.List(partition)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 15 "dist/build/Code.hs" #-}
{-# LINE 146 "./src-ag/Code.ag" #-}

-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes.
--          If there are none, no unboxing can take place,
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox noInh exprs | not unbox || noInh || length exprs == 1 = TupleExpr exprs
                              | otherwise                               = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox noInh tps | not unbox || noInh || length tps == 1 = TupleType tps
                            | otherwise                             = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox noInh comps | not unbox || noInh || length comps == 1 = TupleLhs comps
                              | otherwise                               = UnboxedTupleLhs comps
{-# LINE 33 "dist/build/Code.hs" #-}
-- CaseAlt -----------------------------------------------------
{-
   alternatives:
      alternative CaseAlt:
         child left           : Lhs 
         child expr           : Expr 
-}
data CaseAlt = CaseAlt (Lhs) (Expr)
-- CaseAlts ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CaseAlt 
         child tl             : CaseAlts 
      alternative Nil:
-}
type CaseAlts = [CaseAlt]
-- Chunk -------------------------------------------------------
{-
   alternatives:
      alternative Chunk:
         child name           : {String}
         child comment        : Decl 
         child info           : Decls 
         child dataDef        : Decls 
         child cataFun        : Decls 
         child semDom         : Decls 
         child semWrapper     : Decls 
         child semFunctions   : Decls 
         child semNames       : {[String]}
-}
data Chunk = Chunk (String) (Decl) (Decls) (Decls) (Decls) (Decls) (Decls) (Decls) (([String]))
-- Chunks ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Chunk 
         child tl             : Chunks 
      alternative Nil:
-}
type Chunks = [Chunk]
-- DataAlt -----------------------------------------------------
{-
   alternatives:
      alternative DataAlt:
         child name           : {String}
         child args           : Types 
      alternative Record:
         child name           : {String}
         child args           : NamedTypes 
-}
data DataAlt = DataAlt (String) (Types)
             | Record (String) (NamedTypes)
-- DataAlts ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : DataAlt 
         child tl             : DataAlts 
      alternative Nil:
-}
type DataAlts = [DataAlt]
-- Decl --------------------------------------------------------
{-
   alternatives:
      alternative Decl:
         child left           : Lhs 
         child rhs            : Expr 
         child binds          : {Set String}
         child uses           : {Set String}
      alternative Bind:
         child left           : Lhs 
         child rhs            : Expr 
      alternative BindLet:
         child left           : Lhs 
         child rhs            : Expr 
      alternative Data:
         child name           : {String}
         child params         : {[String]}
         child alts           : DataAlts 
         child strict         : {Bool}
         child derivings      : {[String]}
      alternative NewType:
         child name           : {String}
         child params         : {[String]}
         child con            : {String}
         child tp             : Type 
      alternative Type:
         child name           : {String}
         child params         : {[String]}
         child tp             : Type 
      alternative TSig:
         child name           : {String}
         child tp             : Type 
      alternative Comment:
         child txt            : {String}
      alternative PragmaDecl:
         child txt            : {String}
      alternative Resume:
         child monadic        : {Bool}
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
      alternative EvalDecl:
         child nt             : {String}
         child left           : Lhs 
         child rhs            : Expr 
-}
data Decl = Decl (Lhs) (Expr) ((Set String)) ((Set String))
          | Bind (Lhs) (Expr)
          | BindLet (Lhs) (Expr)
          | Data (String) (([String])) (DataAlts) (Bool) (([String]))
          | NewType (String) (([String])) (String) (Type)
          | Type (String) (([String])) (Type)
          | TSig (String) (Type)
          | Comment (String)
          | PragmaDecl (String)
          | Resume (Bool) (String) (Lhs) (Expr)
          | EvalDecl (String) (Lhs) (Expr)
-- Decls -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
type Decls = [Decl]
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative Let:
         child decls          : Decls 
         child body           : Expr 
      alternative Case:
         child expr           : Expr 
         child alts           : CaseAlts 
      alternative Do:
         child stmts          : Decls 
         child body           : Expr 
      alternative Lambda:
         child args           : Exprs 
         child body           : Expr 
      alternative TupleExpr:
         child exprs          : Exprs 
      alternative UnboxedTupleExpr:
         child exprs          : Exprs 
      alternative App:
         child name           : {String}
         child args           : Exprs 
      alternative SimpleExpr:
         child txt            : {String}
      alternative TextExpr:
         child lns            : {[String]}
      alternative Trace:
         child txt            : {String}
         child expr           : Expr 
      alternative PragmaExpr:
         child onLeftSide     : {Bool}
         child onNewLine      : {Bool}
         child txt            : {String}
         child expr           : Expr 
      alternative LineExpr:
         child expr           : Expr 
      alternative TypedExpr:
         child expr           : Expr 
         child tp             : Type 
      alternative ResultExpr:
         child nt             : {String}
         child expr           : Expr 
      alternative InvokeExpr:
         child nt             : {String}
         child expr           : Expr 
         child args           : Exprs 
      alternative ResumeExpr:
         child nt             : {String}
         child expr           : Expr 
         child left           : Lhs 
         child rhs            : Expr 
      alternative SemFun:
         child nt             : {String}
         child args           : Exprs 
         child body           : Expr 
-}
data Expr = Let (Decls) (Expr)
          | Case (Expr) (CaseAlts)
          | Do (Decls) (Expr)
          | Lambda (Exprs) (Expr)
          | TupleExpr (Exprs)
          | UnboxedTupleExpr (Exprs)
          | App (String) (Exprs)
          | SimpleExpr (String)
          | TextExpr (([String]))
          | Trace (String) (Expr)
          | PragmaExpr (Bool) (Bool) (String) (Expr)
          | LineExpr (Expr)
          | TypedExpr (Expr) (Type)
          | ResultExpr (String) (Expr)
          | InvokeExpr (String) (Expr) (Exprs)
          | ResumeExpr (String) (Expr) (Lhs) (Expr)
          | SemFun (String) (Exprs) (Expr)
-- Exprs -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Expr 
         child tl             : Exprs 
      alternative Nil:
-}
type Exprs = [Expr]
-- Lhs ---------------------------------------------------------
{-
   alternatives:
      alternative Pattern3:
         child pat3           : {Pattern}
      alternative Pattern3SM:
         child pat3           : {Pattern}
      alternative TupleLhs:
         child comps          : {[String]}
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
      alternative Unwrap:
         child name           : {String}
         child sub            : Lhs 
-}
data Lhs = Pattern3 (Pattern)
         | Pattern3SM (Pattern)
         | TupleLhs (([String]))
         | UnboxedTupleLhs (([String]))
         | Fun (String) (Exprs)
         | Unwrap (String) (Lhs)
-- NamedType ---------------------------------------------------
{-
   alternatives:
      alternative Named:
         child strict         : {Bool}
         child name           : {String}
         child tp             : Type 
-}
data NamedType = Named (Bool) (String) (Type)
-- NamedTypes --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : NamedType 
         child tl             : NamedTypes 
      alternative Nil:
-}
type NamedTypes = [NamedType]
-- Program -----------------------------------------------------
{-
   alternatives:
      alternative Program:
         child chunks         : Chunks 
         child ordered        : {Bool}
-}
data Program = Program (Chunks) (Bool)
-- Type --------------------------------------------------------
{-
   alternatives:
      alternative Arr:
         child left           : Type 
         child right          : Type 
      alternative CtxApp:
         child left           : {[(String, [String])]}
         child right          : Type 
      alternative QuantApp:
         child left           : {String}
         child right          : Type 
      alternative TypeApp:
         child func           : Type 
         child args           : Types 
      alternative TupleType:
         child tps            : Types 
      alternative UnboxedTupleType:
         child tps            : Types 
      alternative List:
         child tp             : Type 
      alternative SimpleType:
         child txt            : {String}
      alternative NontermType:
         child name           : {String}
         child params         : {[String]}
         child deforested     : {Bool}
      alternative TMaybe:
         child tp             : Type 
      alternative TEither:
         child left           : Type 
         child right          : Type 
      alternative TMap:
         child key            : Type 
         child value          : Type 
      alternative TIntMap:
         child value          : Type 
-}
data Type = Arr (Type) (Type)
          | CtxApp (([(String, [String])])) (Type)
          | QuantApp (String) (Type)
          | TypeApp (Type) (Types)
          | TupleType (Types)
          | UnboxedTupleType (Types)
          | List (Type)
          | SimpleType (String)
          | NontermType (String) (([String])) (Bool)
          | TMaybe (Type)
          | TEither (Type) (Type)
          | TMap (Type) (Type)
          | TIntMap (Type)
          deriving ( Show)
-- Types -------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
      alternative Nil:
-}
type Types = [Type]