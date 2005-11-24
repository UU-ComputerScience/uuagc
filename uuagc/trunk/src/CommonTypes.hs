--  $Header: /data/cvs-rep/uust/tools/ag/CommonTypes.hs,v 1.10 2005/03/11 16:33:44 uust Exp $
--  $Name:  $ (version name)

module CommonTypes where

import UU.Pretty
import UU.Scanner.Position
import UU.DData.Map(Map)
import UU.DData.Set(Set)


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

instance Show Type where
  show (Haskell t) = t
  show (NT nt  ) = getName nt

instance Eq Type where
  NT x == NT y = x == y
  _      == _      = False
  
type Attributes  = Map Name Type
type TypeSyns    = [(Nonterminal,ComplexType)]

type AttrNames   = [(Name,Type,(String,String))]
type UseMap      = Map Nonterminal (Map Name (String,String))
type Fields      = [(Name,Type)]
type Derivings   = Map Nonterminal (Set Name)
type Strings     = [String]
type Name        = Identifier
type Nonterminal = Name
type Constructor = Name

type AttrEnv = ( [Name]
               , [(Name,Name)]
               )
identifier x   = Ident x noPos               
nullIdent = identifier ""
_LHS  = identifier "lhs" 
_SELF = identifier "SELF" 
_LOC  = identifier "loc" 


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
attrname isIn field attr | field == _LOC = locname attr 
                         | otherwise     = let direction | isIn      = "I" 
                                                         | otherwise = "O"
                                           in '_' : getName field ++ direction ++ getName attr
                               
locname v   = '_' : getName v
fieldname v =  getName v++"_"

typeToString :: Nonterminal -> Type -> String
typeToString _ (Haskell t)  = t
typeToString nt (NT t   ) | t == _SELF = getName nt
                          | otherwise  = getName t
