module CommonTypes where

--import UU.Pretty (PP,text,pp)
import Pretty
import UU.Scanner.Position(Pos,noPos)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set


type Blocks = Map String ([String], Pos)

data Identifier  = Ident {getName::String , getPos::Pos}

instance Eq Identifier where
 Ident x _ == Ident y _ = x == y

instance Ord Identifier where 
 compare (Ident x _) (Ident y _) = compare x y

instance Show Identifier where
  show ident = getName ident

instance PP Identifier where
  pp = text . getName

data Type = Haskell String
          | NT Name

data ComplexType = List Type
                 | Tuple [(Identifier, Type)]
                 | Maybe Type

instance Show ComplexType where
  show (List  t ) = "[" ++ show t ++ "]"
  show (Tuple ts) = "(" ++ showList [ show n ++ ": " ++ show t | (n,t) <- ts ] "" ++ ")"
  show (Maybe t ) = "Maybe " ++ show t

instance Show Type where
  show (Haskell t) = t
  show (NT nt  ) = getName nt

instance Eq Type where
  NT x == NT y = x == y
  _      == _      = False
  
type Attributes  = Map Name Type
type TypeSyns    = [(Nonterminal,ComplexType)]

type AttrNames   = [(Name,Type,(String,String,String))]
type UseMap      = Map Nonterminal (Map Name (String,String,String))
type PragmaMap   = Map Nonterminal (Map Constructor (Set Name))
type Fields      = [(Name,Type)]
type Derivings   = Map Nonterminal (Set Name)
type Strings     = [String]
type Name        = Identifier
type Nonterminal = Name
type Constructor = Name
type AttrOrderMap = Map Nonterminal (Map Constructor (Set Dependency))
data Dependency = Dependency (Identifier,Identifier) (Identifier,Identifier) deriving (Eq,Ord,Show)

type AttrEnv = ( [Name]
               , [(Name,Name)]
               )

identifier x   = Ident x noPos               
nullIdent = identifier ""
_LHS   = identifier "lhs" 
_SELF  = identifier "SELF" 
_LOC   = identifier "loc" 
_INST  = identifier "inst"
_INST' = identifier "inst'"
_FIELD = identifier "field"
_FIRST = identifier "first"
_LAST  = identifier "last"

sdtype :: Nonterminal -> String
sdtype nt = "T_"++getName nt

cataname ::  String -> Name -> String
cataname pre name = pre++getName name

conname :: Bool -> Nonterminal -> Constructor -> String
conname rename nt con | rename =  getName nt ++ "_" ++ getName con
                      | otherwise = getName con

semname  ::  String -> Nonterminal -> Constructor -> String
semname pre nt con =  pre ++ (getName nt ++ "_" ++ getName con)

lhsname :: Bool -> Name -> String
lhsname isIn = attrname isIn _LHS

attrname :: Bool -> Name -> Name -> String
attrname isIn field attr | field == _LOC   = locname attr 
                         | field == _INST  = instname attr
                         | field == _INST' = inst'name attr
                         | field == _FIELD = fieldname attr
                         | otherwise       = let direction | isIn      = "I" 
                                                           | otherwise = "O"
                                             in '_' : getName field ++ direction ++ getName attr
                               
locname v   = '_' : getName v
instname v  = getName v ++ "_val_"
inst'name v = getName v ++ "_"
fieldname v =  getName v++"_"

typeToString :: Nonterminal -> Type -> String
typeToString _ (Haskell t)  = t
typeToString nt (NT t   ) | t == _SELF = getName nt
                          | otherwise  = getName t

ind :: String -> String
ind s = replicate 3 ' ' ++ s

_NOCASE :: Name
_NOCASE = identifier "nocase"

hasPragma :: PragmaMap -> Nonterminal -> Constructor -> Name -> Bool
hasPragma mp nt con nm
  = nm `Set.member` Map.findWithDefault Set.empty con (Map.findWithDefault Map.empty nt mp)
  
isNonterminal :: Type -> Bool
isNonterminal (NT _) = True
isNonterminal _      = False

extractNonterminal :: Type -> Nonterminal
extractNonterminal (NT n) = n

