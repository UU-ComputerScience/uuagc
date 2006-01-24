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


isLocal :: AttrOcc -> Bool
isLocal (AOLocal _ _ _) = True
isLocal (AOTuple aos) = isLocal (head aos)
isLocal _ = False

isInh :: AttrOcc -> Bool
isInh (AOLHSInh _ _ _) = True
isInh (AORHSInh _ _ _ _ _) = True
isInh (AOTuple aos) = isInh (head aos)
isInh _ = False

isSyn :: AttrOcc -> Bool
isSyn ao = not (isLocal ao || isInh ao)

isRhs :: AttrOcc -> Bool
isRhs (AORHSInh _ _ _ _ _) = True
isRhs (AORHSSyn _ _ _ _ _) = True
isRhs (AOTuple aos) = isRhs (head aos)
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
getLhsNt (AOTuple aos)           = getLhsNt (head aos)

getCon (AOLocal  nt c a)       = c
getCon (AOLHSInh nt c a)       = c
getCon (AOLHSSyn nt c a)       = c
getCon (AORHSInh rnt nt c f a) = c
getCon (AORHSSyn rnt nt c f a) = c
getCon (AOTuple aos)           = getCon (head aos)
 
getRhsNt :: AttrOcc -> Maybe Name
getRhsNt (AORHSInh rnt nt c f a) = Just c
getRhsNt (AORHSSyn rnt nt c f a) = Just c
getRhsNt (AOTuple aos)           = getRhsNt (head aos)
getRhsNt _ = Nothing

getField :: AttrOcc -> Maybe Name
getField (AORHSInh rnt nt c f a) = Just f
getField (AORHSSyn rnt nt c f a) = Just f
getField (AOTuple aos)           = getField (head aos)
getField  _ = Nothing

getAttr :: AttrOcc -> Name
getAttr (AOLocal  nt c a)       = a
getAttr (AOLHSInh nt c a)       = a
getAttr (AOLHSSyn nt c a)       = a
getAttr (AORHSInh rnt nt c f a) = a
getAttr (AORHSSyn rnt nt c f a) = a
getAttr (AOTuple aos)           = getAttr (head aos)

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
showPath cas = fst $ mapAccum showOrigin (nullIdent,nullIdent) cas

showOrigin :: (Nonterminal,Constructor) -> CodeAttr -> (String,(Nonterminal,Constructor))
showOrigin (nt,c) ca = let ao = fromCA ca
                           nt' = getLhsNt ao
                           c' = getCon ao
                           ntcon = if nt /= nt' || c /= c'
                                    then ((show nt' ++ "-" ++ show c' ++ ": ") ++)
                                    else id
                       in (ntcon (prettyAO ao),(nt',c'))

fromCA (CAAttrOcc ao) = ao

prettyAO (AOLocal  nt c a)       = show _LOC ++ "." ++ show a
prettyAO (AOLHSInh nt c a)       = "{" ++ show _LHS ++ "." ++ show a ++ "}"
prettyAO (AOLHSSyn nt c a)       = show _LHS ++ "." ++ show a
prettyAO (AORHSInh rnt nt c f a) = show f ++ "." ++ show a
prettyAO (AORHSSyn rnt nt c f a) = "{" ++ show f ++ "." ++ show a ++ "}"
prettyAO (AOTuple aos)           = "(" ++ concatMap prettyAO aos ++")"

mapAccum :: (a -> b -> (c,a)) -> a -> [b] -> ([c],a)
mapAccum f nil [] = ([],nil)
mapAccum f nil (x:xs) = let (y,z) = f nil x
                            (rest,a) = mapAccum f z xs
                        in (y : rest,a)
