module FixedPointHelper(testGenFP
                       ,toFPIdentifier
                       ,FPInfo(..)
                       ,newFPInfo
                       ,FPAttrInfo(..)
                       ,newFPAttrInfo
                       ,defaultFPAttrInfo
                       ,MapNnTConAttrsFPInfo
                       ,MapConAttrsFPInfo
                       ,MapAttrsFPInfo
                       ,MapAttrFPInfo
                       ,fromFPInfoVoidFPInfoRules
                       ,isOutFP
                       ,isInFPInfo
                       ,isOutFPInfo
                       ,FPEvalTxt(..)
                       ,newFPEval
                       ,getFPEvalTxtIx
                       ,notFPEval
                       ,getFPId
                       ,isOutMapAttrsFPInfo
                       ,isInMapAttrsFPInfo
                       ,condMaybe
                       ) where

import qualified Data.Map as Map
import Data.Map(Map)
import Expression
import CommonTypes
import UU.Scanner.Position(noPos)

type MapNnTConAttrsFPInfo a = Map NontermIdent (MapConAttrsFPInfo a)
type MapConAttrsFPInfo a    = Map ConstructorIdent (MapAttrsFPInfo a)
type MapAttrsFPInfo a       = Map Int (FPInfo a)
type MapAttrFPInfo          = Map (Identifier,Identifier) FPAttrInfo


data FPInfo a = FPInfo { fpAttrInfo  :: MapAttrFPInfo 
                       , fpOtherInfo :: [a]
                       } 

data FPAttrInfo = FPAttrInfo {isInFP   :: Bool
                             ,bottomFP :: Expression
                             }

data FPEvalTxt = FixedPointEval Int 
               | NotFixedPointEval 
                 deriving (Show,Read)

getFPId :: FPEvalTxt -> Int
getFPId (FixedPointEval i) = i
getFPId _                  = -1

newFPEval :: Int -> FPEvalTxt
newFPEval = FixedPointEval

newFPInfo :: FPInfo a
newFPInfo = FPInfo { fpAttrInfo = Map.empty
                   , fpOtherInfo = []
                   }

notFPEval :: FPEvalTxt
notFPEval = NotFixedPointEval

isOutFP :: FPAttrInfo -> Bool
isOutFP = not.isInFP 

isMapAttrsFPPInfo :: (FPAttrInfo -> Bool) 
                  -> Int -> Identifier -> Identifier -> MapAttrsFPInfo a -> Maybe Bool
isMapAttrsFPPInfo p i f a m = do fpInfo <- Map.lookup i m
                                 isPFPInfo p f a fpInfo

isInMapAttrsFPInfo :: Int -> Identifier -> Identifier -> MapAttrsFPInfo a -> Maybe Bool
isInMapAttrsFPInfo = isMapAttrsFPPInfo isInFP

isOutMapAttrsFPInfo :: Int -> Identifier -> Identifier -> MapAttrsFPInfo a -> Maybe Bool
isOutMapAttrsFPInfo = isMapAttrsFPPInfo isOutFP

isPFPInfo :: (FPAttrInfo -> Bool) -> Identifier -> Identifier -> FPInfo a -> Maybe Bool
isPFPInfo p f a fpInfo =  maybe (Nothing) (return.p) $ Map.lookup (f,a) $ fpAttrInfo fpInfo

isInFPInfo :: Identifier -> Identifier -> FPInfo a -> Maybe Bool
isInFPInfo = isPFPInfo isInFP 

isOutFPInfo :: Identifier -> Identifier -> FPInfo a -> Maybe Bool
isOutFPInfo = isPFPInfo isOutFP

newFPAttrInfo :: Bool -> Expression -> FPAttrInfo 
newFPAttrInfo b e =  (defaultFPAttrInfo e) { isInFP = b }

defaultFPAttrInfo :: Expression -> FPAttrInfo 
defaultFPAttrInfo e  = FPAttrInfo { isInFP = False
                                  , bottomFP = e
                                  }

getFPEvalTxtIx :: FPEvalTxt -> Int
getFPEvalTxtIx (FixedPointEval i) = i

testGenFP :: Map NontermIdent (Map ConstructorIdent (Map Int (FPInfo a))) -> Bool
testGenFP = Map.fold ((||).Map.fold ((||).Map.fold ((||).not.Map.null.fpAttrInfo) False) False) False 
 
toFPIdentifier :: Identifier -> Identifier -> Identifier
toFPIdentifier f a = Ident (getName f ++ "_" ++ getName a) noPos

fromFPIdentifer :: Identifier -> (Identifier,Identifier)
fromFPIdentifer (Ident s _) = let (a,b) = break (=='_') s
                              in (Ident a noPos, Ident (tail b) noPos) 

fromFPInfoVoidFPInfoRules :: FPInfo a -> FPInfo b
fromFPInfoVoidFPInfoRules fpInfoV = FPInfo {fpAttrInfo = fpAttrInfo fpInfoV
                                           ,fpOtherInfo = []
                                           }


cond :: a -> a -> Bool -> a
cond x _ True  = x
cond _ y _     = y

condMaybe :: Maybe Bool -> a -> a -> a
condMaybe (Just True) x _ = x
condMaybe _           _ y = y
