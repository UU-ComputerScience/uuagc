module TigerTypes where

import UU.DData.Map(Map)
import UU.Scanner.Position

data Ident = Id {getName :: String, getPos :: Pos}  


instance Eq Ident where
  Id x _ == Id y _ = x == y
  
instance Ord Ident where
  compare (Id x _) (Id y _) = compare x y
  
instance Show Ident where
  show (Id x p) = show x ++ " at " ++ show p

type VarIdent = Ident

type TypeIdent = Ident

data Type
   = Var    TypeIdent
   | Array  TypeIdent
   | Record [TypedVar]  deriving Show

data TypedVar = TypedVar VarIdent TypeIdent  deriving Show

-- note that TYPE structure is cyclic for recursive record and array types
-- only shallow structural matching possible
data TYPE = INT                      -- primitive int type
          | STRING                   -- primitive string type
          | VOID                     -- no value
          | NIL                      -- empty record type
          | LOOPCOUNTER              -- type of for loop counter, cannot be used as lhs of assignment
          | ARRAY  TypeIdent         -- original name of array type
                   TypeRef           -- internal name of array type 
                   TYPE              -- component type
          | RECORD TypeIdent         -- original name of record type
                   TypeRef           -- internal name of record type 
                   (Map VarIdent (Pos,TYPE))  -- field types
          | ERROR                    -- error type, matches all types         

instance Show TYPE where
  show x = case x of
            INT     -> "int"             
            STRING  -> "string"                 
            VOID    -> "no value"  
            NIL     -> "empty record" 
            LOOPCOUNTER -> "int:loopcounter" 
            ARRAY  nm _ c -> show nm  
            RECORD nm _ fs  -> show nm 
            ERROR  -> "erroneus type" 

instance Eq TYPE where
 ERROR          == ERROR         = True
 INT            == INT           = True
 LOOPCOUNTER    == LOOPCOUNTER   = True
 STRING         == STRING        = True
 VOID           == VOID          = True
 NIL            == NIL           = True
 ARRAY  _ t1 _  == ARRAY _ t2 _  = t1 == t2 
 RECORD _ t1 _  == RECORD _ t2 _ = t1 == t2 
 _              == _             = False

type TypeRef = Int
