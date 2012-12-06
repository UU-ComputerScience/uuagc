module CommonTypes (module Options, module CommonTypes) where

import Options
import UU.Scanner.Position(Pos)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Monoid(mappend,Monoid)
import Data.Char
import Pretty

type Blocks = Map BlockInfo [([String], Pos)]
type BlockInfo = (BlockKind, Maybe NontermIdent)
data BlockKind
  = BlockImport
  | BlockPragma
  | BlockMain
  | BlockData
  | BlockRec
  | BlockOther
  deriving (Eq, Ord, Show)

instance PP Identifier where
  pp = text . getName

data Type = Haskell String
          | NT Identifier [String]
               Bool  -- True: deforested nonterminal, False: nonterminal type
          | Self     -- reference to the enclosing nonterminal type
          deriving (Eq)

data ComplexType = List Type
                 | Tuple [(Identifier, Type)]
                 | Maybe Type
                 | Either Type Type
                 | Map Type Type
                 | IntMap Type
                 | OrdSet Type
                 | IntSet

instance Show ComplexType where
  show (List  t )     = "[" ++ show t ++ "]"
  show (Tuple ts)     = "(" ++ showList [ show n ++ ": " ++ show t | (n,t) <- ts ] "" ++ ")"
  show (Maybe t )     = "Maybe " ++ show t
  show (Either t1 t2) = "Either " ++ show t1 ++ " " ++ show t2
  show (Map t1 t2)    = "Map " ++ show t1 ++ " " ++ show t2
  show (IntMap t1)    = "IntMap " ++ show t1
  show (OrdSet t1)    = "Set" ++ show t1
  show IntSet         = "IntSet"

instance Show Type where
  show = typeToHaskellString Nothing []

type Attributes  = Map Identifier Type
type TypeSyns    = [(NontermIdent,ComplexType)]
type ParamMap    = Map NontermIdent [Identifier]
type AttrNames   = [(Identifier,Type,(String,String,String))]
type UseMap      = Map NontermIdent (Map Identifier (String,String,String))
type PragmaMap   = Map NontermIdent (Map ConstructorIdent (Set Identifier))
type AttrMap     = Map NontermIdent (Map ConstructorIdent (Set (Identifier,Identifier)))
type UniqueMap   = Map NontermIdent (Map ConstructorIdent (Map Identifier Identifier))
type Derivings   = Map NontermIdent (Set Identifier)
type ClassContext = [(Identifier, [String])]
type ContextMap  = Map NontermIdent ClassContext
type QuantMap    = Map NontermIdent [String]
type Strings     = [String]
type ConstructorIdent = Identifier
type AttrOrderMap = Map NontermIdent (Map ConstructorIdent (Set Dependency))
type VisitIdentifier = Int
type StateIdentifier = Int
data Dependency = Dependency Occurrence Occurrence deriving (Eq,Ord,Show)
data Occurrence
  = OccAttr Identifier Identifier
  | OccRule Identifier
  deriving (Eq,Ord,Show)

type AttrEnv = ( [Identifier]
               , [(Identifier,Identifier)]
               )

nullIdent, _LHS, _SELF, _LOC, _INST, _INST', _FIELD, _FIRST, _LAST :: Identifier
nullIdent = identifier ""
_LHS   = identifier "lhs"
_SELF  = identifier "SELF"
_LOC   = identifier "loc"
_INST  = identifier "inst"
_INST' = identifier "inst'"
_FIELD = identifier "field"
_FIRST = identifier "first__"
_LAST  = identifier "last__"

idLateBindingAttr :: Identifier
idLateBindingAttr = identifier "lateSemDict"

lateBindingTypeNm :: String -> String
lateBindingTypeNm modNm = "Late_" ++ modNm ++ "_"

lateBindingFieldNm :: String -> String
lateBindingFieldNm modNm = "late_" ++ modNm ++ "_"

lateBindingType :: String -> Type
lateBindingType modNm = Haskell (lateBindingTypeNm modNm)

lateSemNtLabel :: NontermIdent -> String
lateSemNtLabel nt = "mk_" ++ getName nt

lateSemConLabel :: NontermIdent -> ConstructorIdent -> String
lateSemConLabel nt con = "mk_" ++ getName nt ++ "_" ++ getName con

sdtype :: NontermIdent -> String
sdtype nt = "T_"++getName nt

mkNtType :: Identifier -> [String] -> Type
mkNtType nt args
  | take 2 (getName nt) == "T_" = NT nt args True
  | otherwise                   = NT nt args False

cataname ::  String -> Identifier -> String
cataname pre name = pre++getName name

conname :: Bool -> NontermIdent -> ConstructorIdent -> String
conname ren nt con | ren =  capitalize (getName nt) ++ "_" ++ getName con
                   | otherwise = getName con

capitalize        :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs

semname  ::  String -> NontermIdent -> ConstructorIdent -> String
semname pre nt con =  pre ++ (getName nt ++ "_" ++ getName con)

recordFieldname :: NontermIdent -> ConstructorIdent -> Identifier -> String
recordFieldname nt con nm = getName nm ++ "_" ++ getName nt ++ "_" ++ getName con

lhsname :: Bool -> Identifier -> String
lhsname isIn = attrname isIn _LHS

attrname :: Bool -> Identifier -> Identifier -> String
attrname isIn field attr | field == _LOC   = locname attr
                         | field == _INST  = instname attr
                         | field == _INST' = inst'name attr
                         | field == _FIELD = fieldname attr
                         | otherwise       = let direction | isIn      = "I"
                                                           | otherwise = "O"
                                             in '_' : getName field ++ direction ++ getName attr

locname, instname, inst'name, fieldname :: Identifier -> String
locname v   = '_' : getName v
instname v  = getName v ++ "_val_"
inst'name v = getName v ++ "_inst_"
fieldname v =  getName v++"_"

typeToAGString :: Type -> String
typeToAGString tp
  = case tp of
      Haskell t     -> t
      NT nt tps for -> formatNonterminalToHaskell for (getName nt) (map (\s -> "{" ++ s ++ "}") tps)
      Self          -> error "Self type is not allowed here."

removeDeforested :: Type -> Type
removeDeforested (NT nt args _) = NT nt args False
removeDeforested tp             = tp

forceDeforested :: Type -> Type
forceDeforested (NT nt args _) = NT nt args True
forceDeforested tp             = tp

typeToHaskellString :: Maybe NontermIdent -> [String] -> Type -> String
typeToHaskellString mbNt params tp
  = case tp of
      Haskell t -> filter (/= '@') t -- Apparently haskell types can contain @ to refer to
                                     -- a type parameter, removing @ makes it backwards compatible
      NT nt tps for | nt == _SELF -> formatNonterminalToHaskell for (maybe "?SELF?" getName mbNt) params
                    | otherwise   -> formatNonterminalToHaskell for (getName nt) tps
      Self -> maybe "?SELF?" getName mbNt

formatNonterminalToHaskell :: Bool -> String -> [String] -> String
formatNonterminalToHaskell for nt tps
  = unwords ((pref ++ nt) : tps)
  where pref | for       = "T_"
             | otherwise = ""

ind :: String -> String
ind s = replicate 3 ' ' ++ s

_NOCASE :: Identifier
_NOCASE = identifier "nocase"

hasPragma :: PragmaMap -> NontermIdent -> ConstructorIdent -> Identifier -> Bool
hasPragma mp nt con nm
  = nm `Set.member` Map.findWithDefault Set.empty con (Map.findWithDefault Map.empty nt mp)

isNonterminal :: Type -> Bool
isNonterminal (NT _ _ _) = True
isNonterminal _          = False

isSELFNonterminal :: Type -> Bool
-- isSELFNonterminal (NT nt _ _) | nt == _SELF = True
isSELFNonterminal Self                      = True
isSELFNonterminal _                         = False

extractNonterminal :: Type -> NontermIdent
extractNonterminal (NT n _ _) = n
extractNonterminal _          = error "Must be NT"

nontermArgs :: Type -> [String]
nontermArgs tp
  = case tp of
      NT _ args _ -> args
      _           -> []

deforestedNt :: Identifier -> Maybe Identifier
deforestedNt nm
  | take 2 (getName nm) == "T_" = Just (Ident (drop 2 (getName nm)) (getPos nm))
  | otherwise = Nothing

data StateCtx
  = NoneVis
  | OneVis !Int
  | ManyVis
  deriving (Eq, Show, Ord)

data ChildKind
  = ChildSyntax        -- This child is defined by syntax
  | ChildAttr          -- This child is defined by an attribute
  | ChildReplace Type  -- This child replaces a child with type Type
  deriving (Eq, Show)

-- Given a map that represents a relation, returns the transitive closure of this relation
closeMap :: Ord a => Map a (Set a) -> Map a (Set a)
closeMap mp0 = close (Map.keysSet mp0) mp0 where
  rev = revDeps mp0
  close todo mp0' = case Set.minView todo of
    Nothing         -> mp0'
    Just (k, todo1) -> let find x = Map.findWithDefault Set.empty x mp0'
                           vals0  = find k
                           valsL  = Set.toList vals0
                           vals1  = foldr Set.union vals0 $ map find valsL
                       in if Set.size vals0 == Set.size vals1
                          then close todo1 mp0'  -- note: monotonically increasing set
                          else let mp1   = Map.insert k vals1 mp0'
                                   refs  = Map.findWithDefault Set.empty k rev
                                   todo2 = Set.union refs todo1
                               in close todo2 mp1

revDeps :: Ord a => Map a (Set a) -> Map a (Set a)
revDeps mp = Map.fromListWith Set.union [ (a,Set.singleton k) | (k,s) <- Map.assocs mp, a <- Set.toList s ]

data HigherOrderInfo = HigherOrderInfo
  { hoNtDeps     :: Set NontermIdent
  , hoNtRevDeps  :: Set NontermIdent
  , hoAcyclic    :: Bool
  }

data VisitKind
  = VisitPure Bool  -- ordered or not
  | VisitMonadic
  deriving (Eq,Ord)

isLazyKind :: VisitKind -> Bool
isLazyKind (VisitPure False) = True
isLazyKind _                 = False

instance Show VisitKind where
  show (VisitPure False) = "Lazy"
  show (VisitPure True)  = "Ordered"
  show VisitMonadic      = "Monadic"

unionWithMappend :: (Monoid a, Ord k) => Map k a -> Map k a -> Map k a
unionWithMappend = Map.unionWith mappend


data FormatMode
  = FormatDo
  | FormatLetDecl
  | FormatLetLine
  deriving (Eq, Ord, Show)
