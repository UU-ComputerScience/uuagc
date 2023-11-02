

-- UUAGC 0.9.56 (src-ag/LOAG/Rep.ag)
module LOAG.Rep where

import CommonTypes
import AbstractSyntax
import LOAG.Common
import qualified Data.Array as A
import qualified Data.Map   as Map
import qualified Data.Set   as Set


import Data.List (intercalate, foldl', nub)
import Data.Tuple (swap)
import Control.Arrow
-- FieldAtt ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : FieldAtt 
   alternatives:
      alternative FieldAtt:
         child t              : {MyType}
         child p              : {PLabel}
         child f              : {FLabel}
         child a              : {ALabel}
         visit 0:
            local self        : _
-}
data FieldAtt = FieldAtt {t_FieldAtt_FieldAtt :: MyType,p_FieldAtt_FieldAtt :: PLabel,f_FieldAtt_FieldAtt :: FLabel,a_FieldAtt_FieldAtt :: ALabel}
-- FieldAtts ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : FieldAtts 
   alternatives:
      alternative Cons:
         child hd             : FieldAtt 
         child tl             : FieldAtts 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
type FieldAtts = [FieldAtt]
-- LOAGRep -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : LOAGRep 
   alternatives:
      alternative LOAGRep:
         child ps             : {[PLabel]}
         child ap             : {A_P}
         child an             : {A_N}
         child ain            : {AI_N}
         child asn            : {AS_N}
         child sfp            : {SF_P}
         child pmp            : {PMP}
         child pmpr           : {PMP_R}
         child nmp            : {NMP}
         child nmpr           : {NMP_R}
         child gen            : {A.Array Int Int}
         child inss           : {A.Array Int [Int]}
         child ofld           : {A.Array Int Int}
         child fty            : {FTY}
         child fieldMap       : {FMap}
         child fsInP          : {Map.Map PLabel [(PLabel,FLabel)]}
         visit 0:
            local self        : _
-}
data LOAGRep = LOAGRep {ps_LOAGRep_LOAGRep :: ([PLabel]),ap_LOAGRep_LOAGRep :: A_P,an_LOAGRep_LOAGRep :: A_N,ain_LOAGRep_LOAGRep :: AI_N,asn_LOAGRep_LOAGRep :: AS_N,sfp_LOAGRep_LOAGRep :: SF_P,pmp_LOAGRep_LOAGRep :: PMP,pmpr_LOAGRep_LOAGRep :: PMP_R,nmp_LOAGRep_LOAGRep :: NMP,nmpr_LOAGRep_LOAGRep :: NMP_R,gen_LOAGRep_LOAGRep :: (A.Array Int Int),inss_LOAGRep_LOAGRep :: (A.Array Int [Int]),ofld_LOAGRep_LOAGRep :: (A.Array Int Int),fty_LOAGRep_LOAGRep :: FTY,fieldMap_LOAGRep_LOAGRep :: FMap,fsInP_LOAGRep_LOAGRep :: (Map.Map PLabel [(PLabel,FLabel)])}
-- MySegment ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : MySegment 
   alternatives:
      alternative MySegment:
         child visnr          : {Int}
         child inhAttr        : {[Int]}
         child synAttr        : {[Int]}
         child inhOccs        : {Maybe [Int]}
         child synOccs        : {Maybe [Int]}
         visit 0:
            local self        : _
-}
data MySegment = MySegment {visnr_MySegment_MySegment :: Int,inhAttr_MySegment_MySegment :: ([Int]),synAttr_MySegment_MySegment :: ([Int]),inhOccs_MySegment_MySegment :: (Maybe [Int]),synOccs_MySegment_MySegment :: (Maybe [Int])}
               deriving ( Show)
-- MySegments --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : MySegments 
   alternatives:
      alternative Cons:
         child hd             : MySegment 
         child tl             : MySegments 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
type MySegments = [MySegment]