module SequentialTypes where

import CommonTypes
import Data.Graph (Table,Vertex)
import UU.DData.Map (Map)

data CodeAttr = CAAttrOcc AttrOcc 
              | CAChildVisit ChildVisit deriving (Eq)
data AttrOcc = AOLocal  Nonterminal Constructor Name
             | AOLHSInh Nonterminal Constructor Name
             | AOLHSSyn Nonterminal Constructor Name
             | AORHSInh Nonterminal Nonterminal Constructor Name Name -- rhs nt, lhs nt, constructor, field, attribute
             | AORHSSyn Nonterminal Nonterminal Constructor Name Name -- rhs nt, lhs nt, constructor, field, attribute
                deriving (Eq,Show)
data ChildVisit = ChildVisit Name Name Int [Vertex] [Vertex] deriving (Eq,Show) -- field, rhs nt, visit nr., inh, syn
data NTAttr = NTAInh Name Name -- nt, attribute
            | NTASyn Name Name -- nt, attribute
               deriving Show

isLocal :: AttrOcc -> Bool
isLocal (AOLocal _ _ _) = True
isLocal _ = False

isInh :: AttrOcc -> Bool
isInh (AOLHSInh _ _ _) = True
isInh (AORHSInh _ _ _ _ _) = True
isInh _ = False

isSyn :: AttrOcc -> Bool
isSyn ao = not (isLocal ao || isInh ao)

isRhs :: AttrOcc -> Bool
isRhs (AORHSInh _ _ _ _ _) = True
isRhs (AORHSSyn _ _ _ _ _) = True
isRhs _ = False

isLhs :: AttrOcc -> Bool
isLhs ao = not (isLocal ao || isRhs ao)

isEqualField :: AttrOcc -> AttrOcc -> Bool
isEqualField a b = getLhsNt a == getLhsNt b && getRhsNt a == getRhsNt b && getCon a == getCon b && getField a == getField b

getLhsNt :: AttrOcc -> Name
getLhsNt (AOLocal  nt c a)       = nt
getLhsNt (AOLHSInh nt c a)       = nt
getLhsNt (AOLHSSyn nt c a)       = nt
getLhsNt (AORHSInh rnt nt c f a) = nt
getLhsNt (AORHSSyn rnt nt c f a) = nt

getCon (AOLocal  nt c a)       = c
getCon (AOLHSInh nt c a)       = c
getCon (AOLHSSyn nt c a)       = c
getCon (AORHSInh rnt nt c f a) = c
getCon (AORHSSyn rnt nt c f a) = c
 
getRhsNt :: AttrOcc -> Maybe Name
getRhsNt (AORHSInh rnt nt c f a) = Just rnt
getRhsNt (AORHSSyn rnt nt c f a) = Just rnt
getRhsNt _ = Nothing

getField :: AttrOcc -> Maybe Name
getField (AORHSInh rnt nt c f a) = Just f
getField (AORHSSyn rnt nt c f a) = Just f
getField  _ = Nothing

getAttr :: AttrOcc -> Name
getAttr (AOLocal  nt c a)       = a
getAttr (AOLHSInh nt c a)       = a
getAttr (AOLHSSyn nt c a)       = a
getAttr (AORHSInh rnt nt c f a) = a
getAttr (AORHSSyn rnt nt c f a) = a

showuse (AOLocal _ _ attr) = locname attr
showuse (AOLHSInh _ _ attr) = attrname True _LHS attr
showuse (AOLHSSyn _ _ attr) = attrname False _LHS attr
showuse (AORHSInh _ _ _ field attr) = attrname False field attr
showuse (AORHSSyn _ _ _ field attr) = attrname True field attr

instance Show CodeAttr where
  show (CAAttrOcc ao) = show ao
  show (CAChildVisit cv) = show cv

lhsshow (NTAInh field attr) = lhsname True attr
lhsshow (NTASyn field attr) = lhsname False attr 

-- Is this code attribute an attribute occurrance (True) or a child visit (False)?
isAttrOcc :: CodeAttr -> Bool
isAttrOcc (CAAttrOcc _) = True
isAttrOcc _ = False

-- Name of a NTAttr when used inside a function
rhsshow :: Name -> NTAttr -> String
rhsshow field (NTAInh _ attr) = attrname False field attr
rhsshow field (NTASyn _ attr) = attrname True field attr 

fromCA (CAAttrOcc ao) = ao

prettyAO (AOLocal  nt c a)       = show nt ++ "." ++ show c ++ ", local attribute " ++ show a
prettyAO (AOLHSInh nt c a)       = show nt ++ "." ++ show c ++ ", lhs inherited attribute " ++ show a
prettyAO (AOLHSSyn nt c a)       = show nt ++ "." ++ show c ++ ", lhs synthesized attribute " ++ show a
prettyAO (AORHSInh rnt nt c f a) = show nt ++ "." ++ show c ++ ", inherited attribute " ++ show a ++ " of " ++ show f
prettyAO (AORHSSyn rnt nt c f a) = show nt ++ "." ++ show c ++ ", synthesized attribute " ++ show a ++ " of " ++ show f

mapAccum :: (a -> b -> (c,a)) -> a -> [b] -> ([c],a)
mapAccum f nil [] = ([],nil)
mapAccum f nil (x:xs) = let (y,z) = f nil x
                            (rest,a) = mapAccum f z xs
                        in (y : rest,a)
