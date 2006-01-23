module SequentialTypes where

import CommonTypes

data CodeAttr = CAAttrOcc AttrOcc 
              | CAChildVisit ChildVisit deriving (Eq)
data AttrOcc = AOLocal  Name Name Name -- lhs nt, constructor, attribute
             | AOLHSInh Name Name Name -- lhs nt, constructor, attribute
             | AOLHSSyn Name Name Name -- lhs nt, constructor, attribute
             | AORHSInh Name Name Name Name Name -- rhs nt, lhs nt, constructor, field, attribute
             | AORHSSyn Name Name Name Name Name -- rhs nt, lhs nt, constructor, field, attribute
             | AOTuple [AttrOcc] deriving (Eq,Show)
data ChildVisit = ChildVisit Name Int Bool deriving (Eq,Show) -- field, visit nr., isLastVisit
data NTAttr = NTAInh Name Name -- nt, attribute
            | NTASyn Name Name -- nt, attribute


showuse (AOLocal _ _ attr) = locname attr
showuse (AOLHSInh _ _ attr) = attrname True _LHS attr
showuse (AOLHSSyn _ _ attr) = attrname False _LHS attr
showuse (AORHSInh _ _ _ field attr) = attrname False field attr
showuse (AORHSSyn _ _ _ field attr) = attrname True field attr
showuse (AOTuple aos) = "(" ++ concatMap (\ao -> show ao ++ ",") aos ++ ")"

instance Show CodeAttr where
  show (CAAttrOcc ao) = show ao
  show (CAChildVisit cv) = show cv

lhsshow (NTAInh field attr) = lhsname True attr
lhsshow (NTASyn field attr) = lhsname False attr 

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

showPath :: [CodeAttr] -> [String]
showPath cas = fst $ mapPass showOrigin (nullIdent,nullIdent) cas

showOrigin :: (Nonterminal,Constructor) -> CodeAttr -> (String,(Nonterminal,Constructor))
showOrigin (nt,c) ca = let ao = fromCA ca
                           nt' = getNt ao
                           c' = getCon ao
                           ntcon = if nt /= nt' || c /= c'
                                    then ((show nt' ++ "-" ++ show c' ++ ": ") ++)
                                    else id
                       in (ntcon (prettyAO ao),(nt',c'))

fromCA (CAAttrOcc ao) = ao

getNt (AOLocal  nt c a)       = nt
getNt (AOLHSInh nt c a)       = nt
getNt (AOLHSSyn nt c a)       = nt
getNt (AORHSInh rnt nt c f a) = nt
getNt (AORHSSyn rnt nt c f a) = nt
getNt (AOTuple aos)           = getNt (head aos)

getCon (AOLocal  nt c a)       = c
getCon (AOLHSInh nt c a)       = c
getCon (AOLHSSyn nt c a)       = c
getCon (AORHSInh rnt nt c f a) = c
getCon (AORHSSyn rnt nt c f a) = c
getCon (AOTuple aos)           = getCon (head aos)
 
prettyAO (AOLocal  nt c a)       = show _LOC ++ "." ++ show a
prettyAO (AOLHSInh nt c a)       = "{" ++ show _LHS ++ "." ++ show a ++ "}"
prettyAO (AOLHSSyn nt c a)       = show _LHS ++ "." ++ show a
prettyAO (AORHSInh rnt nt c f a) = show f ++ "." ++ show a
prettyAO (AORHSSyn rnt nt c f a) = "{" ++ show f ++ "." ++ show a ++ "}"
prettyAO (AOTuple aos)           = "(" ++ concatMap prettyAO aos ++")"

mapPass :: (a -> b -> (c,a)) -> a -> [b] -> ([c],a)
mapPass f nil [] = ([],nil)
mapPass f nil (x:xs) = let (y,z) = f nil x
                           (rest,a) = mapPass f z xs
                       in (y : rest,a)
