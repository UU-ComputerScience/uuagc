module SequentialTypes where

import CommonTypes

data CodeAttr = CAAttrOcc AttrOcc 
              | CAChildVisit ChildVisit deriving (Eq)
data AttrOcc = AOLocal  Name Name Name -- lhs nt, constructor, attribute
             | AOLHSInh Name Name Name -- lhs nt, constructor, attribute
             | AOLHSSyn Name Name Name -- lhs nt, constructor, attribute
             | AORHSInh Name Name Name Name Name -- rhs nt, lhs nt, constructor, field, attribute
             | AORHSSyn Name Name Name Name Name -- rhs nt, lhs nt, constructor, field, attribute
             | AOTuple [AttrOcc] deriving Eq -- ,Show)
data ChildVisit = ChildVisit Name Int Bool deriving (Eq,Show) -- field, visit nr., isLastVisit
data NTAttr = NTAInh Name Name -- nt, attribute
            | NTASyn Name Name -- nt, attribute


instance Show AttrOcc where
  show (AOLocal _ _ attr) = locname attr
  show (AOLHSInh _ _ attr) = attrname True _LHS attr
  show (AOLHSSyn _ _ attr) = attrname False _LHS attr
  show (AORHSInh _ _ _ field attr) = attrname False field attr
  show (AORHSSyn _ _ _ field attr) = attrname True field attr
  show (AOTuple aos) = "(" ++ concatMap (\ao -> show ao ++ ",") aos ++ ")"

instance Show CodeAttr where
  show (CAAttrOcc ao) = show ao
  show (CAChildVisit cv) = show cv

instance Show NTAttr where
  show (NTAInh field attr) = lhsname True attr
  show (NTASyn field attr) = lhsname False attr 

-- Is this code attribute an attribute occurrance (True) or a child visit (False)?
isAttrOcc :: CodeAttr -> Bool
isAttrOcc (CAAttrOcc _) = True
isAttrOcc _ = False

-- Has a code attribute a semantic definition associated with it?
hasRule :: CodeAttr -> Bool
hasRule (CAAttrOcc (AOLocal _ _ _)) = True
hasRule (CAAttrOcc (AOLHSInh _ _ _)) = False
hasRule (CAAttrOcc (AOLHSSyn _ _ _)) = True
hasRule (CAAttrOcc (AORHSInh _ _ _ _ _)) = True
hasRule (CAAttrOcc (AORHSSyn _ _ _ _ _)) = False
hasRule (CAAttrOcc (AOTuple _)) = True
hasRule (CAChildVisit _) = True

-- Name of a NTAttr when used inside a function
rhsshow :: Name -> NTAttr -> String
rhsshow field (NTAInh _ attr) = attrname False field attr
rhsshow field (NTASyn _ attr) = attrname True field attr 
