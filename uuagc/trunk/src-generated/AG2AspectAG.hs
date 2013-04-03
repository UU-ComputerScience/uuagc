{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AG2AspectAG where
{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 16 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 23 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 29 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 35 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 8 "./src-ag/AG2AspectAG.ag" #-}

import Options

import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Pretty
import PPUtil
import UU.Scanner.Position

import AbstractSyntax
import TokenDef
import CommonTypes

-- import Debug.Trace
{-# LINE 56 "dist/build/AG2AspectAG.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 28 "./src-ag/AG2AspectAG.ag" #-}

pragmaAspectAG =  pp  "{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction , TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}"

{-# LINE 63 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 33 "./src-ag/AG2AspectAG.ag" #-}

ppName l = ppListSep "" "" "_" l
{-# LINE 68 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 70 "./src-ag/AG2AspectAG.ag" #-}

type FieldMap  = [(Identifier, Type)]
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap)
{-# LINE 74 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 342 "./src-ag/AG2AspectAG.ag" #-}

filterAtts newAtts = filter (\att -> Map.member (identifier att) newAtts)
filterNotAtts newAtts = filter (\att -> not (Map.member (identifier att) newAtts))

defAtt  att = "data " >|< attTName att >|< "; " >|< attName att >|< " = proxy :: Proxy " >|< attTName att
attName att = pp $ "att_" ++ att
attTName att = pp $ "Att_" ++ att


defAttRec  recPref ppNt atts noGroup  =
           let     recName    = ppName [recPref, ppNt]
                   fields     = ppCommas (map (\(a,t) -> ppName [pp a, recName ] >|< " ::" >|< ppShow t) (groupAtts atts noGroup))
           in
                   "data " >|<  recName >|< " = " >|< recName >|< " { " >|<   fields  >|< " }"

groupAtts atts noGroup = (Map.toAscList . Map.difference atts) noGroup

-- it defines selectors with the form:
-- l1_nt_prod(x, _, .., _) = x
-- ln_nt_prod(_, .., _, x) = x
defLocalAtts prodName total actual (l:ls) =  ppName [pp l, prodName] >|<
                                             ppListSep "(" ")" ","  (replicate (actual-1) "_" ++ "x" : replicate (total-actual) "_") >|<
                                             pp " = x" >-<
                                             defLocalAtts prodName total (actual+1) ls
defLocalAtts _        _     _      []     =  empty

{-# LINE 103 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 397 "./src-ag/AG2AspectAG.ag" #-}

ntsList att ppNtL = "nts_" ++ att ++ " = " >|<  ppListSep "" "" " .*. " ((map fst ppNtL) ++ [pp "hNil"])

filterNts att = filter ( Map.member (identifier att) . snd )
{-# LINE 110 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 455 "./src-ag/AG2AspectAG.ag" #-}

data PPRule = PPRule Identifier Identifier Bool ([(Identifier,Type)] -> [Identifier] -> PP_Doc)

ppRule (field,attr) owrt def = PPRule field attr owrt def
ruleField (PPRule field  _     _     _  ) = field
ruleAttr  (PPRule _      attr  _     _  ) = attr
ruleOwrt  (PPRule _      _     owrt  _  ) = owrt
ruleDef   (PPRule _      _     _     def) = def

{-# LINE 122 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 494 "./src-ag/AG2AspectAG.ag" #-}


defInhGRule ppNt prodName newNT newProd ch rules inhNoGroup synNoGroup chids locals =
                       let  ppAtt = ppName [pp "inh", prodName]
                            ppR =  ppAtt >|< pp " = inhdefM att_inh nts_group $" >-<
                                   indent 4  "do " >-<
                                   indent 5  "loc <- at loc" >-<
                                   indent 5  "lhs <- at lhs" >-<
                                   indent 5  ch >-<
                                   indent 5  "return $"  >-<
                                   indent 6  (foldr (>-<) (pp "emptyRecord") (map (chGRule ppNt prodName rules inhNoGroup synNoGroup chids locals) chids))
                       in  if (newNT || (not newNT && newProd))
                           then (ppR, [ ppAtt ])
                           else (empty, [])

chGRule ppNt prodName rules inhNoGroup synNoGroup chids locals (idCh,tp) =
                       let  chName = ppName [pp "ch", pp idCh, prodName]
                            ppTp   = ppShow tp
                            chRules = ppCommas $ mapGRuleDefs (== idCh)  rules inhNoGroup synNoGroup chids locals
                       in   if (isNonterminal tp)
                             then   chName >|< ".=." >-<
                                    indent 1 "InhG_" >|< ppShow tp >|< pp " {"  >-<
                                    indent 2 chRules >-<
                                    indent 1 "} .*. "
                             else   empty


defSynGRule ppNt prod newNT newProd ch rules inhNoGroup synNoGroup chids locals =
                       let  ppAtt = ppName [pp "syn", ppNt, pp prod]
                            ppTAtt = "SynG_" >|< ppNt
                            ppR =  ppAtt >|< pp " = syndefM att_syn $" >-<
                                   indent 4  "do " >-<
                                   indent 5  "loc <- at loc" >-<
                                   indent 5  "lhs <- at lhs" >-<
                                   indent 5  ch >-<
                                   indent 5  "return $"  >-<
                                   indent 6  ppTAtt >|< pp " {"  >-<
                                   indent 7  (ppCommas $ mapGRuleDefs ((== "lhs") . show)  rules inhNoGroup synNoGroup chids locals) >-<
                                   indent 6  "}"
                       in  if (newNT || (not newNT && newProd))
                           then (ppR, [ ppAtt ])
                           else (empty, [])

defLocRule ppNt prod newNT newProd ch rules inhNoGroup synNoGroup chids locals =
                       let  ppAtt  = ppName [pp "loc", ppNt, pp prod]
                            ppTAtt = ppName [pp "Loc", ppNt, pp prod]
                            ppR =    ppAtt >|< pp " = locdefM att_loc $" >-<
                                     indent 4  "do " >-<
                                     indent 5  "loc <- at loc" >-<
                                     indent 5  "lhs <- at lhs" >-<
                                     indent 5  ch >-<
                                     indent 5  "return $"  >-<
                                     indent 6  (ppListSep "(" ")" "," $ mapLRuleDefs rules inhNoGroup synNoGroup chids locals)
                       in  (ppR, [ ppAtt ])

defInstRules ppNt prod newNT newProd ch rules chids locals
                                             = let  ppAsp     = ppName [pp "inst", ppNt, pp prod]
                                                    instRules = filter ((=="inst") . show . ruleField) rules
                                                    ppAtt att = ppListSep "`ext` " "" "_" [pp "inst_ch", pp att, ppNt, pp prod]
                                               in   (  ppAsp >|< pp " = emptyRule " >|< (map (ppAtt . ruleAttr) instRules) >-<
                                                       (vlist $ map (defInstRule  ppNt prod ch chids locals) instRules)
                                                    ,  [ ppAsp ])


defInstRule  ppNt prod ch chids locals rule =
                       let  ppAtt  = ppName [pp "ch", pp (ruleAttr rule), ppNt, pp prod]
                       in   pp "inst_" >|< ppAtt >|< pp " = instdefM " >|< ppAtt >|< pp " $" >-<
                            indent 4  "do " >-<
                            indent 5  "loc <- at loc" >-<
                            indent 5  "lhs <- at lhs" >-<
                            indent 5  ch >-<
                            indent 5  "return $"  >-<
                            indent 6  ((ruleDef rule) chids locals)


defSynRules ppNt prod newNT newProd newAtts ch rules inhNoGroup synNoGroup chids locals
                                             = let  synRules     = filter ( (=="lhs") . show . ruleField)  rules
                                                    ngRules      = filter ((flip elem synNoGroup) . getName . ruleAttr) synRules
                                                    (ppR, ppRA)  = unzip $  map (defSynRule True ppNt prod newNT newProd newAtts ch chids locals) ngRules
                                               in   (vlist ppR, concat ppRA )

modSynRules ppNt prod newNT newProd newAtts ch rules inhNoGroup synNoGroup chids locals
                                             = let  synRules     = filter ( (=="lhs") . show . ruleField)  rules
                                                    ngRules      = filter ((flip elem synNoGroup) . getName . ruleAttr) synRules
                                                    (ppR, ppRA)  = unzip $  map (defSynRule False ppNt prod newNT newProd newAtts ch chids locals) ngRules
                                               in   (vlist ppR, concat ppRA )

defSynRule  new ppNt prod newNT newProd newAtts ch chids locals rule =
                       let  att    = ruleAttr rule
                            newAtt = Map.member att newAtts
                            owrt   = ruleOwrt rule
                            ppAtt  = ppName [pp att, pp (if new then "syn" else "synM"), ppNt, pp prod]
                            ppR def =   ppAtt >|< pp (" = " ++ def ++ " ") >|< attName (show att) >|< pp " $" >-<
                                        indent 4  "do " >-<
                                        indent 5  "loc <- at loc" >-<
                                        indent 5  "lhs <- at lhs" >-<
                                        indent 5  ch >-<
                                        indent 5  "return $"  >-<
                                        indent 6  ((ruleDef rule) chids locals)
                       in   
                           if new
                           then  if (not owrt && (newNT || (not newNT && newProd) || newAtt))
                                 then  (ppR "syndefM", [ ppAtt ])
                                 else  (empty,         [])
                           else  if owrt
                                 then  (ppR "synmodM", [ ppAtt ])
                                 else  (empty,         [])



defInhRules ppNt prodName newNT newProd newAtts ch rules inhNoGroup synNoGroup chids locals
                                             = let  ngRules      = filter ((flip elem inhNoGroup) . getName . ruleAttr) rules
                                                    (ppR, ppRA)  = unzip $ map (defInhRule True  ppNt prodName newNT newProd newAtts ch ngRules inhNoGroup synNoGroup chids locals) inhNoGroup
                                               in   (vlist ppR, concat ppRA)

modInhRules ppNt prodName newNT newProd newAtts ch rules inhNoGroup synNoGroup chids locals
                                             = let  ngRules      = filter ((flip elem inhNoGroup) . getName . ruleAttr) rules
                                                    (ppR, ppRA)  = unzip $ map (defInhRule  False ppNt prodName newNT newProd newAtts ch ngRules inhNoGroup synNoGroup chids locals) inhNoGroup
                                               in   (vlist ppR, concat ppRA)


defInhRule new ppNt prodName newNT newProd newAtts ch rules inhNoGroup synNoGroup chids locals att =
                       let  ppAtt       =  ppName [pp att, pp (if new then "inh" else "inhM"),prodName]
                            newAtt      =  Map.member (identifier att) newAtts
                            chRMaybe    =  map (chRule new ppNt prodName att rules inhNoGroup synNoGroup chids locals) chids
                            chR         =  [ x | (Just x) <- chRMaybe ]
                            ppR def  =  ppAtt >|< pp (" = " ++ def ++ " ") >|< attName att >|< " nts_" >|< att >|< " $" >-<
                                        indent 4  "do " >-<
                                        indent 5  "loc <- at loc" >-<
                                        indent 5  "lhs <- at lhs" >-<
                                        indent 5  ch >-<
                                        indent 5  "return $"  >-<
                                        indent 6  (foldr (>-<) (pp "emptyRecord") chR)
                       in
                           if new
                           then  if (newNT || (not newNT && newProd) || newAtt)
                                 then  (ppR "inhdefM", [ ppAtt ])
                                 else  (empty,         [])
                           else  if (not . null) chR
                                 then  (ppR "inhmodM", [ ppAtt ])
                                 else  (empty,         [])


chRule new ppNt prodName att rules inhNoGroup synNoGroup chids locals (idCh,tp) =
                       let  chName = ppName [pp "ch", pp idCh, prodName]
                            ppTp   = ppShow tp
                            chRule = inhRuleDef new (== idCh) (== att) rules inhNoGroup synNoGroup chids locals  -- it's supposed to be only one
                       in   if (isNonterminal tp && (not . null) chRule)
                             then   Just $ chName >|< ".=. (" >|< chRule >|< ") .*. "
                             else   Nothing


mapLRuleDefs rules inhNoGroup synNoGroup chids locals
                 = map appSnd $ sortBy cmpField $ filter ((== "loc") . show . ruleField)  rules
                                      where cmpField r1 r2 = compare (ruleField r1) (ruleField r2)
                                            appSnd rule = (ruleDef rule)  chids locals


mapGRuleDefs filt rules inhNoGroup synNoGroup chids locals
                 = map appSnd $ sortBy cmpField $ filter (not . (flip elem inhNoGroup) . getName . ruleAttr)
                                                $ filter (not . (flip elem synNoGroup) . getName . ruleAttr)
                                                $ filter ( filt . ruleField)  rules
                                      where cmpField r1 r2 = compare (ruleField r1) (ruleField r2)
                                            appSnd rule = (ruleDef rule)  chids locals

inhRuleDef new filt1 filt2 rules inhNoGroup synNoGroup chids locals
                 = map appSnd $ sortBy cmpField $ filter ( (== not new) . ruleOwrt)
                                                $ filter ((flip elem inhNoGroup) . getName . ruleAttr)
                                                $ filter ( filt2 . getName . ruleAttr)
                                                $ filter ( filt1 . ruleField)  rules
                                      where cmpField r1 r2 = compare (ruleField r1) (ruleField r2)
                                            appSnd rule = (ruleDef rule)  chids locals

defRule ppNt (field,att) noGroup rhs = \chids locals ->
                                     let ppAtt = if (elem (getName att) noGroup)
                                                  then empty
                                                  else case (show field) of
                                                        "lhs"     -> att >|< "_" >|< pp "SynG" >|< pp "_" >|< ppNt  >|< " = "
                                                        "loc"     -> empty
                                                        "inst"    -> empty
                                                        otherwise -> att >|< "_" >|< pp "InhG" >|< pp "_" >|<
                                                                     (maybe (error $ "lhs field " ++ show field ++" is not a child")
                                                                            ppShow (lookup field chids))
                                                                     >|< " = "
                                     in  ppAtt >|< (rhs noGroup field chids locals)


rhsRule ppNt ppProd tks noGroup field chids locals  =  vlist . lines2PP . (map (token2PP ppNt ppProd field chids locals noGroup )) $ tks


lines2PP [] = []
lines2PP xs = map line2PP . shiftLeft . getLines $ xs


token2PP ppNt ppProd field chids locals noGroup  tk
  = case tk of
      AGLocal var pos _        -> (pos, if (elem var locals)
                                        then  (ppListSep "(" "" "_" [pp var, ppNt, ppProd]) >|< pp " (loc # att_loc)) "
                                        else  pp var)
      AGField field attr pos _ -> let ppChT =    maybe (error $ "rhs field " ++ show field ++ " is not a child") ppShow (lookup field chids)
                                      ppAtt =    case (show field) of
                                                 "lhs"      ->  attName "inh"
                                                 "loc"      ->  attName "loc"
                                                 otherwise  ->  attName "syn"
                                      ppSubAtt = case (show field) of
                                                 "lhs"      -> ppName [pp (getName attr), pp "InhG", ppNt]
                                                 "loc"      -> ppName [pp (getName attr), ppNt, ppProd]
                                                 otherwise  -> ppName [pp (getName attr), pp "SynG", ppChT]
                                  in  (pos, if ((elem (getName attr) noGroup) && ((show field) /= "loc"))
                                             then pp "(" >|< pp (getName field)  >|< " # " >|< attName (getName attr) >|< pp ")"
                                             else pp "(" >|< ppSubAtt >|< " (" >|< pp (getName field) >|< " # " >|< ppAtt >|< ")) ")
      HsToken value pos        -> (pos, pp value)
      CharToken value pos      -> (pos, pp (show value))
      StrToken value pos       -> (pos, pp (show value))
      Err mesg pos             -> (pos, pp $ " ***" ++ mesg ++ "*** ")

line2PP ts =         let f (p,t) r = let ct = column p
                                     in \c -> pp (spaces (ct-c)) >|< t >|< r (length (show t) +ct)
                         spaces x | x < 0 = ""
                                  | otherwise = replicate x ' '
                     in foldr f (pp . const "") ts 1

{-# LINE 347 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 721 "./src-ag/AG2AspectAG.ag" #-}

ppMacro (Macro con children) = "( atts_" >|< show con >|< ", " >|<  ppListSep "" "" " <.> " ppChildren  >|<")"
                where   ppChildren = map  ppChild children
                        ppChild (RuleChild  ch n) = chName ch >|< " ==> " >|< ppMacro n
                        ppChild (ChildChild ch n) = chName ch >|< " --> " >|< n
                        ppChild (ValueChild ch n) = chName ch >|< " ~~> " >|< n
                        chName ch = ppName [pp "ch", pp ch, pp con]
{-# LINE 357 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 754 "./src-ag/AG2AspectAG.ag" #-}

ppNoGroupAtts syn noGroup = let synatts = Map.keys $ Map.filterWithKey (\att _ -> elem (getName att) noGroup) syn
                            in  map (flip (>|<) "_inh") noGroup ++  map (flip (>|<) "_syn") synatts

ruleName att prodName = ppName [att,prodName]

elemNT a b = False
{-# LINE 367 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 797 "./src-ag/AG2AspectAG.ag" #-}

attTypes atts = map (\(a,t) -> "(HCons (LVPair (Proxy Att_" >|< a >|< ") " >|< ppShow t >|< ") ") $ Map.toAscList atts
{-# LINE 372 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 851 "./src-ag/AG2AspectAG.ag" #-}

attVars atts = map (\(a,_) -> "_" >|< a >|< " ") $ Map.toAscList atts
attFields atts noGroup ppNt =
     let ng = map (\(a,_) -> attName (getName a) >|< " .=. _" >|< a >|< " .*. ") $ Map.toAscList noGroup
         g  = ppCommas $ map (\(a,_) -> ppName [pp a, pp "InhG",ppNt]  >|< "= _" >|< a) $ Map.toAscList $ Map.difference atts noGroup
     in "(" >|< ng >|< "att_inh .=. " >|< ppName [pp "InhG", ppNt] >|< " { " >|< g >|< " } .*. emptyRecord)"
{-# LINE 381 "dist/build/AG2AspectAG.hs" #-}
-- Child -------------------------------------------------------
-- wrapper
data Inh_Child  = Inh_Child { ext_Inh_Child :: (Maybe String), inhMap_Inh_Child :: (Map Identifier Attributes), inhNoGroup_Inh_Child :: ([String]), newAtts_Inh_Child :: ( Attributes ), o_noGroup_Inh_Child :: ([String]), o_rename_Inh_Child :: (Bool), ppNt_Inh_Child :: (PP_Doc), ppProd_Inh_Child :: (PP_Doc), synMap_Inh_Child :: (Map Identifier Attributes), synNoGroup_Inh_Child :: ([String]) }
data Syn_Child  = Syn_Child { idCL_Syn_Child :: ([(Identifier,Type)]), ppCSF_Syn_Child :: ([(Identifier,(PP_Doc,PP_Doc))]), ppDL_Syn_Child :: ([PP_Doc]), ppL_Syn_Child :: (PP_Doc), ppLI_Syn_Child :: ([PP_Doc]), ppR_Syn_Child :: (PP_Doc), prdInh_Syn_Child :: (Attributes) }
{-# INLINABLE wrap_Child #-}
wrap_Child :: T_Child  -> Inh_Child  -> (Syn_Child )
wrap_Child (T_Child act) (Inh_Child _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Child_vIn1 _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup
        (T_Child_vOut1 _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh) <- return (inv_Child_s2 sem arg)
        return (Syn_Child _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh)
   )

-- cata
{-# INLINE sem_Child #-}
sem_Child :: Child  -> T_Child 
sem_Child ( Child name_ tp_ kind_ ) = sem_Child_Child name_ tp_ kind_

-- semantic domain
newtype T_Child  = T_Child {
                           attach_T_Child :: Identity (T_Child_s2 )
                           }
newtype T_Child_s2  = C_Child_s2 {
                                 inv_Child_s2 :: (T_Child_v1 )
                                 }
data T_Child_s3  = C_Child_s3
type T_Child_v1  = (T_Child_vIn1 ) -> (T_Child_vOut1 )
data T_Child_vIn1  = T_Child_vIn1 (Maybe String) (Map Identifier Attributes) ([String]) ( Attributes ) ([String]) (Bool) (PP_Doc) (PP_Doc) (Map Identifier Attributes) ([String])
data T_Child_vOut1  = T_Child_vOut1 ([(Identifier,Type)]) ([(Identifier,(PP_Doc,PP_Doc))]) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) (Attributes)
{-# NOINLINE sem_Child_Child #-}
sem_Child_Child :: (Identifier) -> (Type) -> (ChildKind) -> T_Child 
sem_Child_Child arg_name_ arg_tp_ arg_kind_ = T_Child (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Child_v1 
      v1 = \ (T_Child_vIn1 _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _chnt = rule0 arg_name_ arg_tp_
         _inh = rule1 _chnt _lhsIinhMap
         _syn = rule2 _chnt _lhsIsynMap
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule3 _inh
         _ppCh = rule4 arg_name_
         _ppTCh = rule5 arg_tp_
         _chName = rule6 _lhsIppNt _lhsIppProd _ppCh
         _lhsOppDL :: [PP_Doc]
         _lhsOppDL = rule7 _chName _ppTCh arg_kind_
         _chLabel = rule8 _chName
         _chTLabel = rule9 _chName
         _lhsOppL :: PP_Doc
         _lhsOppL = rule10 _chLabel _chTLabel _ppTCh arg_kind_
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule11 _chLabel _chTLabel
         _lhsOppR :: PP_Doc
         _lhsOppR = rule12 _lhsIppNt _lhsIppProd arg_name_
         _lhsOidCL :: [(Identifier,Type)]
         _lhsOidCL = rule13 arg_name_ arg_tp_
         _lhsOppCSF :: [(Identifier,(PP_Doc,PP_Doc))]
         _lhsOppCSF = rule14 _chLabel arg_kind_ arg_name_ arg_tp_
         __result_ = T_Child_vOut1 _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh
         in __result_ )
     in C_Child_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
   rule0 = \ name_ tp_ ->
                       {-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                       case tp_ of
                         NT nt _ _ -> nt
                         Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                         Haskell t -> identifier ""
                       {-# LINE 452 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
   rule1 = \ _chnt ((_lhsIinhMap) :: Map Identifier Attributes) ->
                      {-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                      {-# LINE 458 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
   rule2 = \ _chnt ((_lhsIsynMap) :: Map Identifier Attributes) ->
                      {-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                      Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                      {-# LINE 464 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 67 "./src-ag/AG2AspectAG.ag" #-}
   rule3 = \ _inh ->
                              {-# LINE 67 "./src-ag/AG2AspectAG.ag" #-}
                              _inh
                              {-# LINE 470 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule4 #-}
   {-# LINE 182 "./src-ag/AG2AspectAG.ag" #-}
   rule4 = \ name_ ->
                                                      {-# LINE 182 "./src-ag/AG2AspectAG.ag" #-}
                                                      pp name_
                                                      {-# LINE 476 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule5 #-}
   {-# LINE 183 "./src-ag/AG2AspectAG.ag" #-}
   rule5 = \ tp_ ->
                                                      {-# LINE 183 "./src-ag/AG2AspectAG.ag" #-}
                                                      ppShow tp_
                                                      {-# LINE 482 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule6 #-}
   {-# LINE 184 "./src-ag/AG2AspectAG.ag" #-}
   rule6 = \ ((_lhsIppNt) :: PP_Doc) ((_lhsIppProd) :: PP_Doc) _ppCh ->
                                                      {-# LINE 184 "./src-ag/AG2AspectAG.ag" #-}
                                                      ppName [_ppCh    , _lhsIppNt, _lhsIppProd]
                                                      {-# LINE 488 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule7 #-}
   {-# LINE 242 "./src-ag/AG2AspectAG.ag" #-}
   rule7 = \ _chName _ppTCh kind_ ->
                                                    {-# LINE 242 "./src-ag/AG2AspectAG.ag" #-}
                                                    case kind_ of
                                                     ChildSyntax    ->  [ _chName      >|< pp " :: " >|< _ppTCh     ]
                                                     _              ->  []
                                                    {-# LINE 496 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule8 #-}
   {-# LINE 285 "./src-ag/AG2AspectAG.ag" #-}
   rule8 = \ _chName ->
                                                     {-# LINE 285 "./src-ag/AG2AspectAG.ag" #-}
                                                     "ch_" >|< _chName
                                                     {-# LINE 502 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule9 #-}
   {-# LINE 286 "./src-ag/AG2AspectAG.ag" #-}
   rule9 = \ _chName ->
                                                     {-# LINE 286 "./src-ag/AG2AspectAG.ag" #-}
                                                     "Ch_" >|< _chName
                                                     {-# LINE 508 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 287 "./src-ag/AG2AspectAG.ag" #-}
   rule10 = \ _chLabel _chTLabel _ppTCh kind_ ->
                                                     {-# LINE 287 "./src-ag/AG2AspectAG.ag" #-}
                                                     "data " >|< _chTLabel     >|< "; " >|< _chLabel     >|< pp " = proxy :: " >|<
                                                     case kind_ of
                                                      ChildSyntax    ->  "Proxy " >|< "(" >|< _chTLabel     >|< ", " >|< _ppTCh     >|< ")"
                                                      _              ->  "SemType " >|< _ppTCh     >|< pp " nt =>  Proxy " >|<
                                                                         "(" >|< _chTLabel     >|< ", nt)"
                                                     {-# LINE 518 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 293 "./src-ag/AG2AspectAG.ag" #-}
   rule11 = \ _chLabel _chTLabel ->
                                                     {-# LINE 293 "./src-ag/AG2AspectAG.ag" #-}
                                                     [ _chLabel    , _chTLabel     ]
                                                     {-# LINE 524 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 451 "./src-ag/AG2AspectAG.ag" #-}
   rule12 = \ ((_lhsIppNt) :: PP_Doc) ((_lhsIppProd) :: PP_Doc) name_ ->
                                                     {-# LINE 451 "./src-ag/AG2AspectAG.ag" #-}
                                                     let chName = ppListSep "" "" "_" [pp name_, _lhsIppNt, _lhsIppProd]
                                                     in  pp name_ >|< " <- at ch_" >|< chName
                                                     {-# LINE 531 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule13 #-}
   {-# LINE 489 "./src-ag/AG2AspectAG.ag" #-}
   rule13 = \ name_ tp_ ->
                                                    {-# LINE 489 "./src-ag/AG2AspectAG.ag" #-}
                                                    [ (name_, removeDeforested tp_ ) ]
                                                    {-# LINE 537 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule14 #-}
   {-# LINE 827 "./src-ag/AG2AspectAG.ag" #-}
   rule14 = \ _chLabel kind_ name_ tp_ ->
                                              {-# LINE 827 "./src-ag/AG2AspectAG.ag" #-}
                                              let
                                                   semC   = if (isNonterminal tp_)
                                                             then "sem_" >|< ppShow tp_ >|<  " _" >|< name_
                                                             else "sem_Lit _" >|< name_
                                              in   case kind_ of
                                                        ChildSyntax ->  [(name_, (  _chLabel     >|< " .=. (" >|< semC >|< ") .*. "
                                                                                ,  _chLabel     >|< " .=. _" >|< name_ >|< " .*. "))]
                                                        _           ->  []
                                              {-# LINE 550 "dist/build/AG2AspectAG.hs"#-}

-- Children ----------------------------------------------------
-- wrapper
data Inh_Children  = Inh_Children { ext_Inh_Children :: (Maybe String), inhMap_Inh_Children :: (Map Identifier Attributes), inhNoGroup_Inh_Children :: ([String]), newAtts_Inh_Children :: ( Attributes ), o_noGroup_Inh_Children :: ([String]), o_rename_Inh_Children :: (Bool), ppNt_Inh_Children :: (PP_Doc), ppProd_Inh_Children :: (PP_Doc), synMap_Inh_Children :: (Map Identifier Attributes), synNoGroup_Inh_Children :: ([String]) }
data Syn_Children  = Syn_Children { idCL_Syn_Children :: ([(Identifier,Type)]), ppCSF_Syn_Children :: ([(Identifier,(PP_Doc,PP_Doc))]), ppDL_Syn_Children :: ([PP_Doc]), ppL_Syn_Children :: (PP_Doc), ppLI_Syn_Children :: ([PP_Doc]), ppR_Syn_Children :: (PP_Doc), prdInh_Syn_Children :: (Attributes) }
{-# INLINABLE wrap_Children #-}
wrap_Children :: T_Children  -> Inh_Children  -> (Syn_Children )
wrap_Children (T_Children act) (Inh_Children _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Children_vIn4 _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup
        (T_Children_vOut4 _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh) <- return (inv_Children_s5 sem arg)
        return (Syn_Children _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh)
   )

-- cata
{-# NOINLINE sem_Children #-}
sem_Children :: Children  -> T_Children 
sem_Children list = Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list)

-- semantic domain
newtype T_Children  = T_Children {
                                 attach_T_Children :: Identity (T_Children_s5 )
                                 }
newtype T_Children_s5  = C_Children_s5 {
                                       inv_Children_s5 :: (T_Children_v4 )
                                       }
data T_Children_s6  = C_Children_s6
type T_Children_v4  = (T_Children_vIn4 ) -> (T_Children_vOut4 )
data T_Children_vIn4  = T_Children_vIn4 (Maybe String) (Map Identifier Attributes) ([String]) ( Attributes ) ([String]) (Bool) (PP_Doc) (PP_Doc) (Map Identifier Attributes) ([String])
data T_Children_vOut4  = T_Children_vOut4 ([(Identifier,Type)]) ([(Identifier,(PP_Doc,PP_Doc))]) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) (Attributes)
{-# NOINLINE sem_Children_Cons #-}
sem_Children_Cons :: T_Child  -> T_Children  -> T_Children 
sem_Children_Cons arg_hd_ arg_tl_ = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Child (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_tl_))
         (T_Child_vOut1 _hdIidCL _hdIppCSF _hdIppDL _hdIppL _hdIppLI _hdIppR _hdIprdInh) = inv_Child_s2 _hdX2 (T_Child_vIn1 _hdOext _hdOinhMap _hdOinhNoGroup _hdOnewAtts _hdOo_noGroup _hdOo_rename _hdOppNt _hdOppProd _hdOsynMap _hdOsynNoGroup)
         (T_Children_vOut4 _tlIidCL _tlIppCSF _tlIppDL _tlIppL _tlIppLI _tlIppR _tlIprdInh) = inv_Children_s5 _tlX5 (T_Children_vIn4 _tlOext _tlOinhMap _tlOinhNoGroup _tlOnewAtts _tlOo_noGroup _tlOo_rename _tlOppNt _tlOppProd _tlOsynMap _tlOsynNoGroup)
         _lhsOppDL :: [PP_Doc]
         _lhsOppDL = rule15 _hdIppDL _tlIppDL
         _lhsOidCL :: [(Identifier,Type)]
         _lhsOidCL = rule16 _hdIidCL _tlIidCL
         _lhsOppCSF :: [(Identifier,(PP_Doc,PP_Doc))]
         _lhsOppCSF = rule17 _hdIppCSF _tlIppCSF
         _lhsOppL :: PP_Doc
         _lhsOppL = rule18 _hdIppL _tlIppL
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule19 _hdIppLI _tlIppLI
         _lhsOppR :: PP_Doc
         _lhsOppR = rule20 _hdIppR _tlIppR
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule21 _hdIprdInh _tlIprdInh
         _hdOext = rule22 _lhsIext
         _hdOinhMap = rule23 _lhsIinhMap
         _hdOinhNoGroup = rule24 _lhsIinhNoGroup
         _hdOnewAtts = rule25 _lhsInewAtts
         _hdOo_noGroup = rule26 _lhsIo_noGroup
         _hdOo_rename = rule27 _lhsIo_rename
         _hdOppNt = rule28 _lhsIppNt
         _hdOppProd = rule29 _lhsIppProd
         _hdOsynMap = rule30 _lhsIsynMap
         _hdOsynNoGroup = rule31 _lhsIsynNoGroup
         _tlOext = rule32 _lhsIext
         _tlOinhMap = rule33 _lhsIinhMap
         _tlOinhNoGroup = rule34 _lhsIinhNoGroup
         _tlOnewAtts = rule35 _lhsInewAtts
         _tlOo_noGroup = rule36 _lhsIo_noGroup
         _tlOo_rename = rule37 _lhsIo_rename
         _tlOppNt = rule38 _lhsIppNt
         _tlOppProd = rule39 _lhsIppProd
         _tlOsynMap = rule40 _lhsIsynMap
         _tlOsynNoGroup = rule41 _lhsIsynNoGroup
         __result_ = T_Children_vOut4 _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule15 #-}
   {-# LINE 238 "./src-ag/AG2AspectAG.ag" #-}
   rule15 = \ ((_hdIppDL) :: [PP_Doc]) ((_tlIppDL) :: [PP_Doc]) ->
                                                                                  {-# LINE 238 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  _hdIppDL ++ _tlIppDL
                                                                                  {-# LINE 635 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule16 #-}
   rule16 = \ ((_hdIidCL) :: [(Identifier,Type)]) ((_tlIidCL) :: [(Identifier,Type)]) ->
     _hdIidCL ++ _tlIidCL
   {-# INLINE rule17 #-}
   rule17 = \ ((_hdIppCSF) :: [(Identifier,(PP_Doc,PP_Doc))]) ((_tlIppCSF) :: [(Identifier,(PP_Doc,PP_Doc))]) ->
     _hdIppCSF ++ _tlIppCSF
   {-# INLINE rule18 #-}
   rule18 = \ ((_hdIppL) :: PP_Doc) ((_tlIppL) :: PP_Doc) ->
     _hdIppL >-< _tlIppL
   {-# INLINE rule19 #-}
   rule19 = \ ((_hdIppLI) :: [PP_Doc]) ((_tlIppLI) :: [PP_Doc]) ->
     _hdIppLI ++ _tlIppLI
   {-# INLINE rule20 #-}
   rule20 = \ ((_hdIppR) :: PP_Doc) ((_tlIppR) :: PP_Doc) ->
     _hdIppR >-< _tlIppR
   {-# INLINE rule21 #-}
   rule21 = \ ((_hdIprdInh) :: Attributes) ((_tlIprdInh) :: Attributes) ->
     _hdIprdInh `Map.union` _tlIprdInh
   {-# INLINE rule22 #-}
   rule22 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIppProd) :: PP_Doc) ->
     _lhsIppProd
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule35 #-}
   rule35 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIppProd) :: PP_Doc) ->
     _lhsIppProd
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
{-# NOINLINE sem_Children_Nil #-}
sem_Children_Nil ::  T_Children 
sem_Children_Nil  = T_Children (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Children_v4 
      v4 = \ (T_Children_vIn4 _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _lhsOppDL :: [PP_Doc]
         _lhsOppDL = rule42  ()
         _lhsOidCL :: [(Identifier,Type)]
         _lhsOidCL = rule43  ()
         _lhsOppCSF :: [(Identifier,(PP_Doc,PP_Doc))]
         _lhsOppCSF = rule44  ()
         _lhsOppL :: PP_Doc
         _lhsOppL = rule45  ()
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule46  ()
         _lhsOppR :: PP_Doc
         _lhsOppR = rule47  ()
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule48  ()
         __result_ = T_Children_vOut4 _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh
         in __result_ )
     in C_Children_s5 v4
   {-# INLINE rule42 #-}
   {-# LINE 239 "./src-ag/AG2AspectAG.ag" #-}
   rule42 = \  (_ :: ()) ->
                                                                                  {-# LINE 239 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  []
                                                                                  {-# LINE 743 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule43 #-}
   rule43 = \  (_ :: ()) ->
     []
   {-# INLINE rule44 #-}
   rule44 = \  (_ :: ()) ->
     []
   {-# INLINE rule45 #-}
   rule45 = \  (_ :: ()) ->
     empty
   {-# INLINE rule46 #-}
   rule46 = \  (_ :: ()) ->
     []
   {-# INLINE rule47 #-}
   rule47 = \  (_ :: ()) ->
     empty
   {-# INLINE rule48 #-}
   rule48 = \  (_ :: ()) ->
     Map.empty

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { ppNt_Inh_Expression :: (PP_Doc), ppProd_Inh_Expression :: (PP_Doc) }
data Syn_Expression  = Syn_Expression { ppRE_Syn_Expression :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIppNt _lhsIppProd) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Expression_vIn7 _lhsIppNt _lhsIppProd
        (T_Expression_vOut7 _lhsOppRE) <- return (inv_Expression_s8 sem arg)
        return (Syn_Expression _lhsOppRE)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s8 )
                                     }
newtype T_Expression_s8  = C_Expression_s8 {
                                           inv_Expression_s8 :: (T_Expression_v7 )
                                           }
data T_Expression_s9  = C_Expression_s9
type T_Expression_v7  = (T_Expression_vIn7 ) -> (T_Expression_vOut7 )
data T_Expression_vIn7  = T_Expression_vIn7 (PP_Doc) (PP_Doc)
data T_Expression_vOut7  = T_Expression_vOut7 ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression _ arg_tks_ = T_Expression (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_Expression_v7 
      v7 = \ (T_Expression_vIn7 _lhsIppNt _lhsIppProd) -> ( let
         _lhsOppRE :: [String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc
         _lhsOppRE = rule49 _lhsIppNt _lhsIppProd arg_tks_
         __result_ = T_Expression_vOut7 _lhsOppRE
         in __result_ )
     in C_Expression_s8 v7
   {-# INLINE rule49 #-}
   {-# LINE 484 "./src-ag/AG2AspectAG.ag" #-}
   rule49 = \ ((_lhsIppNt) :: PP_Doc) ((_lhsIppProd) :: PP_Doc) tks_ ->
                                                      {-# LINE 484 "./src-ag/AG2AspectAG.ag" #-}
                                                      rhsRule _lhsIppNt _lhsIppProd tks_
                                                      {-# LINE 810 "dist/build/AG2AspectAG.hs"#-}

-- Grammar -----------------------------------------------------
-- wrapper
data Inh_Grammar  = Inh_Grammar { agi_Inh_Grammar :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))), ext_Inh_Grammar :: (Maybe String), options_Inh_Grammar :: (Options) }
data Syn_Grammar  = Syn_Grammar { imp_Syn_Grammar :: (PP_Doc), pp_Syn_Grammar :: (PP_Doc) }
{-# INLINABLE wrap_Grammar #-}
wrap_Grammar :: T_Grammar  -> Inh_Grammar  -> (Syn_Grammar )
wrap_Grammar (T_Grammar act) (Inh_Grammar _lhsIagi _lhsIext _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Grammar_vIn10 _lhsIagi _lhsIext _lhsIoptions
        (T_Grammar_vOut10 _lhsOimp _lhsOpp) <- return (inv_Grammar_s11 sem arg)
        return (Syn_Grammar _lhsOimp _lhsOpp)
   )

-- cata
{-# INLINE sem_Grammar #-}
sem_Grammar :: Grammar  -> T_Grammar 
sem_Grammar ( Grammar typeSyns_ useMap_ derivings_ wrappers_ nonts_ pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ ) = sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ ( sem_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_

-- semantic domain
newtype T_Grammar  = T_Grammar {
                               attach_T_Grammar :: Identity (T_Grammar_s11 )
                               }
newtype T_Grammar_s11  = C_Grammar_s11 {
                                       inv_Grammar_s11 :: (T_Grammar_v10 )
                                       }
data T_Grammar_s12  = C_Grammar_s12
type T_Grammar_v10  = (T_Grammar_vIn10 ) -> (T_Grammar_vOut10 )
data T_Grammar_vIn10  = T_Grammar_vIn10 ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) (Maybe String) (Options)
data T_Grammar_vOut10  = T_Grammar_vOut10 (PP_Doc) (PP_Doc)
{-# NOINLINE sem_Grammar_Grammar #-}
sem_Grammar_Grammar :: (TypeSyns) -> (UseMap) -> (Derivings) -> (Set NontermIdent) -> T_Nonterminals  -> (PragmaMap) -> (AttrOrderMap) -> (ParamMap) -> (ContextMap) -> (QuantMap) -> (UniqueMap) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) -> (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) -> T_Grammar 
sem_Grammar_Grammar arg_typeSyns_ _ arg_derivings_ _ arg_nonts_ _ _ _ _ _ _ _ _ _ = T_Grammar (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_Grammar_v10 
      v10 = \ (T_Grammar_vIn10 _lhsIagi _lhsIext _lhsIoptions) -> ( let
         _nontsX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_nonts_))
         (T_Nonterminals_vOut25 _nontsIextendedNTs _nontsIinhMap' _nontsIppA _nontsIppAI _nontsIppCata _nontsIppD _nontsIppDI _nontsIppL _nontsIppLI _nontsIppNtL _nontsIppR _nontsIppSF _nontsIppW _nontsIsynMap') = inv_Nonterminals_s26 _nontsX26 (T_Nonterminals_vIn25 _nontsOderivs _nontsOext _nontsOinhMap _nontsOnewAtts _nontsOnewNTs _nontsOnewProds _nontsOo_noGroup _nontsOo_rename _nontsOsynMap _nontsOtSyns)
         _nontsOinhMap = rule50 _nontsIinhMap'
         _nontsOsynMap = rule51 _nontsIsynMap'
         _nontsOo_rename = rule52 _lhsIoptions
         _o_noGroup = rule53 _lhsIoptions
         _nontsOo_noGroup = rule54 _o_noGroup
         _newAtts = rule55 _lhsIagi
         _nontsOnewAtts = rule56 _newAtts
         _newProds = rule57 _lhsIagi
         _nontsOnewProds = rule58 _newProds
         _nontsOnewNTs = rule59 _lhsIagi _nontsIextendedNTs
         _lhsOimp :: PP_Doc
         _lhsOimp = rule60 _lhsIext _nontsIppDI _nontsIppLI _ppAI _ppANT
         _lhsOpp :: PP_Doc
         _lhsOpp = rule61 _lhsIoptions _nontsIppCata _nontsIppD _nontsIppL _nontsIppSF _nontsIppW _ppA _ppR
         _nontsOderivs = rule62 arg_derivings_
         _nontsOtSyns = rule63 arg_typeSyns_
         _ppA = rule64 _lhsIext _newAtts _nontsIppA _o_noGroup
         _ppAI = rule65 _lhsIext _newAtts _nontsIppAI _o_noGroup
         _ppANT = rule66 _newAtts _o_noGroup
         _ppNtL = rule67 _nontsIppNtL
         _ppR = rule68 _newAtts _nontsIppR _o_noGroup _ppNtL
         _nontsOext = rule69 _lhsIext
         __result_ = T_Grammar_vOut10 _lhsOimp _lhsOpp
         in __result_ )
     in C_Grammar_s11 v10
   {-# INLINE rule50 #-}
   {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
   rule50 = \ ((_nontsIinhMap') :: Map Identifier Attributes) ->
                             {-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIinhMap'
                             {-# LINE 881 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule51 #-}
   {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
   rule51 = \ ((_nontsIsynMap') :: Map Identifier Attributes) ->
                             {-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                             _nontsIsynMap'
                             {-# LINE 887 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule52 #-}
   {-# LINE 43 "./src-ag/AG2AspectAG.ag" #-}
   rule52 = \ ((_lhsIoptions) :: Options) ->
                                   {-# LINE 43 "./src-ag/AG2AspectAG.ag" #-}
                                   rename    _lhsIoptions
                                   {-# LINE 893 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule53 #-}
   {-# LINE 47 "./src-ag/AG2AspectAG.ag" #-}
   rule53 = \ ((_lhsIoptions) :: Options) ->
                                   {-# LINE 47 "./src-ag/AG2AspectAG.ag" #-}
                                   sort $ noGroup    _lhsIoptions
                                   {-# LINE 899 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule54 #-}
   {-# LINE 48 "./src-ag/AG2AspectAG.ag" #-}
   rule54 = \ _o_noGroup ->
                                   {-# LINE 48 "./src-ag/AG2AspectAG.ag" #-}
                                   _o_noGroup
                                   {-# LINE 905 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule55 #-}
   {-# LINE 80 "./src-ag/AG2AspectAG.ag" #-}
   rule55 = \ ((_lhsIagi) :: (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) ->
                                          {-# LINE 80 "./src-ag/AG2AspectAG.ag" #-}
                                          case _lhsIagi of
                                                  (_,_,atts) -> ( Map.unions . (\(a,b) -> a++b) . unzip . Map.elems) atts
                                          {-# LINE 912 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule56 #-}
   {-# LINE 82 "./src-ag/AG2AspectAG.ag" #-}
   rule56 = \ _newAtts ->
                                          {-# LINE 82 "./src-ag/AG2AspectAG.ag" #-}
                                          _newAtts
                                          {-# LINE 918 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule57 #-}
   {-# LINE 88 "./src-ag/AG2AspectAG.ag" #-}
   rule57 = \ ((_lhsIagi) :: (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) ->
                                           {-# LINE 88 "./src-ag/AG2AspectAG.ag" #-}
                                           case _lhsIagi of
                                                  (_,prods,_) -> prods
                                           {-# LINE 925 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule58 #-}
   {-# LINE 90 "./src-ag/AG2AspectAG.ag" #-}
   rule58 = \ _newProds ->
                                           {-# LINE 90 "./src-ag/AG2AspectAG.ag" #-}
                                           _newProds
                                           {-# LINE 931 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule59 #-}
   {-# LINE 112 "./src-ag/AG2AspectAG.ag" #-}
   rule59 = \ ((_lhsIagi) :: (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) ((_nontsIextendedNTs) :: Set NontermIdent) ->
                                          {-# LINE 112 "./src-ag/AG2AspectAG.ag" #-}
                                          case _lhsIagi of
                                                  (newNTs,_,_) -> Set.difference newNTs _nontsIextendedNTs
                                          {-# LINE 938 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule60 #-}
   {-# LINE 127 "./src-ag/AG2AspectAG.ag" #-}
   rule60 = \ ((_lhsIext) :: Maybe String) ((_nontsIppDI) :: [PP_Doc]) ((_nontsIppLI) :: [PP_Doc]) _ppAI _ppANT ->
                                                     {-# LINE 127 "./src-ag/AG2AspectAG.ag" #-}
                                                     "import Language.Grammars.AspectAG" >-<
                                                     "import Language.Grammars.AspectAG.Derive" >-<
                                                     "import Data.HList.Label4" >-<
                                                     "import Data.HList.TypeEqGeneric1" >-<
                                                     "import Data.HList.TypeCastGeneric1" >-<
                                                     maybe empty ("import qualified" >#<) _lhsIext >-<
                                                     maybe empty (\ext -> "import" >#< ext >#< ppListSep "(" ")" "," (_nontsIppDI ++ _nontsIppLI ++ _ppAI     ++ _ppANT    )) _lhsIext
                                                     {-# LINE 950 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule61 #-}
   {-# LINE 140 "./src-ag/AG2AspectAG.ag" #-}
   rule61 = \ ((_lhsIoptions) :: Options) ((_nontsIppCata) :: PP_Doc) ((_nontsIppD) :: PP_Doc) ((_nontsIppL) :: PP_Doc) ((_nontsIppSF) :: PP_Doc) ((_nontsIppW) :: PP_Doc) _ppA _ppR ->
                                                     {-# LINE 140 "./src-ag/AG2AspectAG.ag" #-}
                                                     (if dataTypes _lhsIoptions
                                                     then  "-- datatypes"               >-< _nontsIppD >-<
                                                           "-- labels"                  >-< _nontsIppL
                                                     else  empty)
                                                     >-<
                                                     (if folds _lhsIoptions
                                                     then  "-- attributes"              >-< _ppA     >-<
                                                           "-- rules"                   >-< _ppR     >-<
                                                           "-- catas"                   >-< _nontsIppCata
                                                     else  empty)
                                                     >-<
                                                     (if semfuns _lhsIoptions
                                                     then  "-- semantic functions"      >-< _nontsIppSF
                                                     else  empty)
                                                     >-<
                                                     (if wrappers _lhsIoptions
                                                     then  "-- wrappers"    >-< _nontsIppW
                                                     else  empty)
                                                     {-# LINE 973 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule62 #-}
   {-# LINE 202 "./src-ag/AG2AspectAG.ag" #-}
   rule62 = \ derivings_ ->
                                                     {-# LINE 202 "./src-ag/AG2AspectAG.ag" #-}
                                                     derivings_
                                                     {-# LINE 979 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule63 #-}
   {-# LINE 251 "./src-ag/AG2AspectAG.ag" #-}
   rule63 = \ typeSyns_ ->
                                                                                  {-# LINE 251 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  typeSyns_
                                                                                  {-# LINE 985 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule64 #-}
   {-# LINE 300 "./src-ag/AG2AspectAG.ag" #-}
   rule64 = \ ((_lhsIext) :: Maybe String) _newAtts ((_nontsIppA) :: PP_Doc) _o_noGroup ->
                                                     {-# LINE 300 "./src-ag/AG2AspectAG.ag" #-}
                                                     vlist (map defAtt (filterAtts _newAtts     _o_noGroup    )) >-<
                                                     defAtt "loc" >-<
                                                     (case _lhsIext of
                                                       Nothing    ->  defAtt "inh" >-< defAtt "syn"
                                                       otherwise  ->  empty) >-<
                                                     _nontsIppA
                                                     {-# LINE 996 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule65 #-}
   {-# LINE 308 "./src-ag/AG2AspectAG.ag" #-}
   rule65 = \ ((_lhsIext) :: Maybe String) _newAtts ((_nontsIppAI) :: [PP_Doc]) _o_noGroup ->
                                                  {-# LINE 308 "./src-ag/AG2AspectAG.ag" #-}
                                                  let atts =  filterNotAtts _newAtts     _o_noGroup
                                                  in  (foldr (\a as -> attName a : as) [] atts) ++
                                                      (foldr (\a as -> attTName a : as) [] atts) ++
                                                      (case _lhsIext of
                                                        Nothing    ->  []
                                                        otherwise  ->  [ attName "inh", attName "syn", attTName "inh", attTName "syn" ]) ++
                                                      _nontsIppAI
                                                  {-# LINE 1008 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule66 #-}
   {-# LINE 318 "./src-ag/AG2AspectAG.ag" #-}
   rule66 = \ _newAtts _o_noGroup ->
                                                  {-# LINE 318 "./src-ag/AG2AspectAG.ag" #-}
                                                  let atts =  filterNotAtts _newAtts     _o_noGroup
                                                  in  (foldr (\a as -> ("nts_" >|< a) : as) [] atts)
                                                  {-# LINE 1015 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule67 #-}
   {-# LINE 392 "./src-ag/AG2AspectAG.ag" #-}
   rule67 = \ ((_nontsIppNtL) :: [(PP_Doc, Attributes)]) ->
                                                     {-# LINE 392 "./src-ag/AG2AspectAG.ag" #-}
                                                     _nontsIppNtL
                                                     {-# LINE 1021 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule68 #-}
   {-# LINE 393 "./src-ag/AG2AspectAG.ag" #-}
   rule68 = \ _newAtts ((_nontsIppR) :: PP_Doc) _o_noGroup _ppNtL ->
                                                     {-# LINE 393 "./src-ag/AG2AspectAG.ag" #-}
                                                     ntsList "group" _ppNtL      >-<
                                                     vlist (map (\att -> ntsList att (filterNts att _ppNtL    )) (filterAtts _newAtts _o_noGroup    ))  >-<
                                                     _nontsIppR
                                                     {-# LINE 1029 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken {  }
data Syn_HsToken  = Syn_HsToken {  }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsToken_vIn13 
        (T_HsToken_vOut13 ) <- return (inv_HsToken_s14 sem arg)
        return (Syn_HsToken )
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal var_ pos_ rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField field_ attr_ pos_ rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken value_ pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken value_ pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken value_ pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err mesg_ pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s14 )
                               }
newtype T_HsToken_s14  = C_HsToken_s14 {
                                       inv_HsToken_s14 :: (T_HsToken_v13 )
                                       }
data T_HsToken_s15  = C_HsToken_s15
type T_HsToken_v13  = (T_HsToken_vIn13 ) -> (T_HsToken_vOut13 )
data T_HsToken_vIn13  = T_HsToken_vIn13 
data T_HsToken_vOut13  = T_HsToken_vOut13 
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal _ _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField _ _ _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ _ = T_HsToken (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_HsToken_v13 
      v13 = \ (T_HsToken_vIn13 ) -> ( let
         __result_ = T_HsToken_vOut13 
         in __result_ )
     in C_HsToken_s14 v13

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens {  }
data Syn_HsTokens  = Syn_HsTokens {  }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokens_vIn16 
        (T_HsTokens_vOut16 ) <- return (inv_HsTokens_s17 sem arg)
        return (Syn_HsTokens )
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s17 )
                                 }
newtype T_HsTokens_s17  = C_HsTokens_s17 {
                                         inv_HsTokens_s17 :: (T_HsTokens_v16 )
                                         }
data T_HsTokens_s18  = C_HsTokens_s18
type T_HsTokens_v16  = (T_HsTokens_vIn16 ) -> (T_HsTokens_vOut16 )
data T_HsTokens_vIn16  = T_HsTokens_vIn16 
data T_HsTokens_vOut16  = T_HsTokens_vOut16 
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ (T_HsTokens_vIn16 ) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut13 ) = inv_HsToken_s14 _hdX14 (T_HsToken_vIn13 )
         (T_HsTokens_vOut16 ) = inv_HsTokens_s17 _tlX17 (T_HsTokens_vIn16 )
         __result_ = T_HsTokens_vOut16 
         in __result_ )
     in C_HsTokens_s17 v16
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_HsTokens_v16 
      v16 = \ (T_HsTokens_vIn16 ) -> ( let
         __result_ = T_HsTokens_vOut16 
         in __result_ )
     in C_HsTokens_s17 v16

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot {  }
data Syn_HsTokensRoot  = Syn_HsTokensRoot {  }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokensRoot_vIn19 
        (T_HsTokensRoot_vOut19 ) <- return (inv_HsTokensRoot_s20 sem arg)
        return (Syn_HsTokensRoot )
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s20 )
                                         }
newtype T_HsTokensRoot_s20  = C_HsTokensRoot_s20 {
                                                 inv_HsTokensRoot_s20 :: (T_HsTokensRoot_v19 )
                                                 }
data T_HsTokensRoot_s21  = C_HsTokensRoot_s21
type T_HsTokensRoot_v19  = (T_HsTokensRoot_vIn19 ) -> (T_HsTokensRoot_vOut19 )
data T_HsTokensRoot_vIn19  = T_HsTokensRoot_vIn19 
data T_HsTokensRoot_vOut19  = T_HsTokensRoot_vOut19 
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_HsTokensRoot_v19 
      v19 = \ (T_HsTokensRoot_vIn19 ) -> ( let
         _tokensX17 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut16 ) = inv_HsTokens_s17 _tokensX17 (T_HsTokens_vIn16 )
         __result_ = T_HsTokensRoot_vOut19 
         in __result_ )
     in C_HsTokensRoot_s20 v19

-- Nonterminal -------------------------------------------------
-- wrapper
data Inh_Nonterminal  = Inh_Nonterminal { derivs_Inh_Nonterminal :: (Derivings), ext_Inh_Nonterminal :: (Maybe String), inhMap_Inh_Nonterminal :: (Map Identifier Attributes), newAtts_Inh_Nonterminal :: ( Attributes ), newNTs_Inh_Nonterminal :: (Set NontermIdent), newProds_Inh_Nonterminal :: ( DataTypes ), o_noGroup_Inh_Nonterminal :: ([String]), o_rename_Inh_Nonterminal :: (Bool), synMap_Inh_Nonterminal :: (Map Identifier Attributes), tSyns_Inh_Nonterminal :: (TypeSyns) }
data Syn_Nonterminal  = Syn_Nonterminal { extendedNTs_Syn_Nonterminal :: (Set NontermIdent), inhMap'_Syn_Nonterminal :: (Map Identifier Attributes), ppA_Syn_Nonterminal :: (PP_Doc), ppAI_Syn_Nonterminal :: ([PP_Doc]), ppCata_Syn_Nonterminal :: (PP_Doc), ppD_Syn_Nonterminal :: (PP_Doc), ppDI_Syn_Nonterminal :: ([PP_Doc]), ppL_Syn_Nonterminal :: (PP_Doc), ppLI_Syn_Nonterminal :: ([PP_Doc]), ppNtL_Syn_Nonterminal :: ([(PP_Doc, Attributes)]), ppR_Syn_Nonterminal :: (PP_Doc), ppSF_Syn_Nonterminal :: (PP_Doc), ppW_Syn_Nonterminal :: (PP_Doc), synMap'_Syn_Nonterminal :: (Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminal #-}
wrap_Nonterminal :: T_Nonterminal  -> Inh_Nonterminal  -> (Syn_Nonterminal )
wrap_Nonterminal (T_Nonterminal act) (Inh_Nonterminal _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminal_vIn22 _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns
        (T_Nonterminal_vOut22 _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap') <- return (inv_Nonterminal_s23 sem arg)
        return (Syn_Nonterminal _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap')
   )

-- cata
{-# INLINE sem_Nonterminal #-}
sem_Nonterminal :: Nonterminal  -> T_Nonterminal 
sem_Nonterminal ( Nonterminal nt_ params_ inh_ syn_ prods_ ) = sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ ( sem_Productions prods_ )

-- semantic domain
newtype T_Nonterminal  = T_Nonterminal {
                                       attach_T_Nonterminal :: Identity (T_Nonterminal_s23 )
                                       }
newtype T_Nonterminal_s23  = C_Nonterminal_s23 {
                                               inv_Nonterminal_s23 :: (T_Nonterminal_v22 )
                                               }
data T_Nonterminal_s24  = C_Nonterminal_s24
type T_Nonterminal_v22  = (T_Nonterminal_vIn22 ) -> (T_Nonterminal_vOut22 )
data T_Nonterminal_vIn22  = T_Nonterminal_vIn22 (Derivings) (Maybe String) (Map Identifier Attributes) ( Attributes ) (Set NontermIdent) ( DataTypes ) ([String]) (Bool) (Map Identifier Attributes) (TypeSyns)
data T_Nonterminal_vOut22  = T_Nonterminal_vOut22 (Set NontermIdent) (Map Identifier Attributes) (PP_Doc) ([PP_Doc]) (PP_Doc) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) ([(PP_Doc, Attributes)]) (PP_Doc) (PP_Doc) (PP_Doc) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminal_Nonterminal #-}
sem_Nonterminal_Nonterminal :: (NontermIdent) -> ([Identifier]) -> (Attributes) -> (Attributes) -> T_Productions  -> T_Nonterminal 
sem_Nonterminal_Nonterminal arg_nt_ _ arg_inh_ arg_syn_ arg_prods_ = T_Nonterminal (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_Nonterminal_v22 
      v22 = \ (T_Nonterminal_vIn22 _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) -> ( let
         _prodsX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_prods_))
         (T_Productions_vOut37 _prodsIhasMoreProds _prodsIppA _prodsIppCata _prodsIppDL _prodsIppL _prodsIppLI _prodsIppR _prodsIppRA _prodsIppSF _prodsIppSPF _prodsIprdInh) = inv_Productions_s38 _prodsX38 (T_Productions_vIn37 _prodsOext _prodsOinh _prodsOinhMap _prodsOinhNoGroup _prodsOnewAtts _prodsOnewNT _prodsOnewProds _prodsOo_noGroup _prodsOo_rename _prodsOppNt _prodsOsyn _prodsOsynMap _prodsOsynNoGroup)
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule70 arg_inh_ arg_nt_
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule71 arg_nt_ arg_syn_
         _inhNoGroup = rule72 _lhsIo_noGroup _prodsIprdInh
         _synNoGroup = rule73 _lhsIo_noGroup arg_syn_
         _prodsOinhNoGroup = rule74 _inhNoGroup
         _prodsOsynNoGroup = rule75 _synNoGroup
         _prodsOnewProds = rule76 _lhsInewProds arg_nt_
         _lhsOextendedNTs :: Set NontermIdent
         _lhsOextendedNTs = rule77 _prodsIhasMoreProds arg_nt_
         _ppNt = rule78 arg_nt_
         _prodsOppNt = rule79 _ppNt
         _lhsOppD :: PP_Doc
         _lhsOppD = rule80 _lhsIderivs _lhsInewNTs _lhsItSyns _ppNt _prodsIppDL arg_nt_
         _lhsOppDI :: [PP_Doc]
         _lhsOppDI = rule81 _lhsInewNTs _ppNt arg_nt_
         _ntLabel = rule82 _ppNt
         _lhsOppL :: PP_Doc
         _lhsOppL = rule83 _lhsInewNTs _ntLabel _ppNt _prodsIppL arg_nt_
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule84 _lhsInewNTs _ntLabel _prodsIppLI arg_nt_
         _lhsOppA :: PP_Doc
         _lhsOppA = rule85 _inhNoGroup _lhsInewNTs _ppNt _prodsIppA _synNoGroup arg_inh_ arg_nt_ arg_syn_
         _lhsOppAI :: [PP_Doc]
         _lhsOppAI = rule86 _lhsInewNTs _ppNt arg_nt_
         _lhsOppNtL :: [(PP_Doc, Attributes)]
         _lhsOppNtL = rule87 arg_inh_ arg_nt_ arg_syn_
         _prodsOnewNT = rule88 _lhsInewNTs arg_nt_
         _lhsOppR :: PP_Doc
         _lhsOppR = rule89 _prodsIppR arg_nt_
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule90 _ppNt _prodsIppCata
         _prodsOsyn = rule91 arg_syn_
         _prodsOinh = rule92 arg_inh_
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule93 _inhNoGroup _ppNt _prodsIppSPF _synNoGroup
         _lhsOppW :: PP_Doc
         _lhsOppW = rule94 _inhNoGroup _ppNt arg_inh_
         _prodsOext = rule95 _lhsIext
         _prodsOinhMap = rule96 _lhsIinhMap
         _prodsOnewAtts = rule97 _lhsInewAtts
         _prodsOo_noGroup = rule98 _lhsIo_noGroup
         _prodsOo_rename = rule99 _lhsIo_rename
         _prodsOsynMap = rule100 _lhsIsynMap
         __result_ = T_Nonterminal_vOut22 _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap'
         in __result_ )
     in C_Nonterminal_s23 v22
   {-# INLINE rule70 #-}
   {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
   rule70 = \ inh_ nt_ ->
                                 {-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ inh_
                                 {-# LINE 1320 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
   rule71 = \ nt_ syn_ ->
                                 {-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                 Map.singleton nt_ syn_
                                 {-# LINE 1326 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 51 "./src-ag/AG2AspectAG.ag" #-}
   rule72 = \ ((_lhsIo_noGroup) :: [String]) ((_prodsIprdInh) :: Attributes) ->
                                        {-# LINE 51 "./src-ag/AG2AspectAG.ag" #-}
                                        Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) _prodsIprdInh
                                        {-# LINE 1332 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule73 #-}
   {-# LINE 52 "./src-ag/AG2AspectAG.ag" #-}
   rule73 = \ ((_lhsIo_noGroup) :: [String]) syn_ ->
                                        {-# LINE 52 "./src-ag/AG2AspectAG.ag" #-}
                                        Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) syn_
                                        {-# LINE 1338 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule74 #-}
   {-# LINE 57 "./src-ag/AG2AspectAG.ag" #-}
   rule74 = \ _inhNoGroup ->
                                          {-# LINE 57 "./src-ag/AG2AspectAG.ag" #-}
                                          map show $ Map.keys _inhNoGroup
                                          {-# LINE 1344 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule75 #-}
   {-# LINE 58 "./src-ag/AG2AspectAG.ag" #-}
   rule75 = \ _synNoGroup ->
                                          {-# LINE 58 "./src-ag/AG2AspectAG.ag" #-}
                                          map show $ Map.keys _synNoGroup
                                          {-# LINE 1350 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule76 #-}
   {-# LINE 94 "./src-ag/AG2AspectAG.ag" #-}
   rule76 = \ ((_lhsInewProds) ::  DataTypes ) nt_ ->
                                   {-# LINE 94 "./src-ag/AG2AspectAG.ag" #-}
                                   case Map.lookup nt_ _lhsInewProds of
                                          Just prds -> prds
                                          Nothing   -> Map.empty
                                   {-# LINE 1358 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule77 #-}
   {-# LINE 107 "./src-ag/AG2AspectAG.ag" #-}
   rule77 = \ ((_prodsIhasMoreProds) ::  Bool ) nt_ ->
                                                  {-# LINE 107 "./src-ag/AG2AspectAG.ag" #-}
                                                  if _prodsIhasMoreProds
                                                  then Set.singleton nt_
                                                  else Set.empty
                                                  {-# LINE 1366 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule78 #-}
   {-# LINE 173 "./src-ag/AG2AspectAG.ag" #-}
   rule78 = \ nt_ ->
                                                      {-# LINE 173 "./src-ag/AG2AspectAG.ag" #-}
                                                      pp nt_
                                                      {-# LINE 1372 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule79 #-}
   {-# LINE 190 "./src-ag/AG2AspectAG.ag" #-}
   rule79 = \ _ppNt ->
                                                     {-# LINE 190 "./src-ag/AG2AspectAG.ag" #-}
                                                     _ppNt
                                                     {-# LINE 1378 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule80 #-}
   {-# LINE 209 "./src-ag/AG2AspectAG.ag" #-}
   rule80 = \ ((_lhsIderivs) :: Derivings) ((_lhsInewNTs) :: Set NontermIdent) ((_lhsItSyns) :: TypeSyns) _ppNt ((_prodsIppDL) :: [PP_Doc]) nt_ ->
                                       {-# LINE 209 "./src-ag/AG2AspectAG.ag" #-}
                                       if (Set.member nt_ _lhsInewNTs)
                                       then  case (lookup nt_ _lhsItSyns) of
                                                      Nothing ->  "data " >|< _ppNt
                                                                   >|< " = " >|< vlist_sep " | " _prodsIppDL >-<
                                                                  case (Map.lookup nt_ _lhsIderivs) of
                                                                   Just ntds -> pp "  deriving " >|<  (ppListSep "(" ")" ", " $ Set.elems ntds)
                                                                   Nothing   -> empty
                                                      Just tp ->  "type " >|< _ppNt     >|< " = " >|< ppShow tp
                                       else  empty
                                       {-# LINE 1392 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule81 #-}
   {-# LINE 222 "./src-ag/AG2AspectAG.ag" #-}
   rule81 = \ ((_lhsInewNTs) :: Set NontermIdent) _ppNt nt_ ->
                                       {-# LINE 222 "./src-ag/AG2AspectAG.ag" #-}
                                       if (not $ Set.member nt_ _lhsInewNTs)
                                       then  [ _ppNt     ]
                                       else  [ ]
                                       {-# LINE 1400 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule82 #-}
   {-# LINE 262 "./src-ag/AG2AspectAG.ag" #-}
   rule82 = \ _ppNt ->
                                                    {-# LINE 262 "./src-ag/AG2AspectAG.ag" #-}
                                                    "nt_" >|< _ppNt
                                                    {-# LINE 1406 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule83 #-}
   {-# LINE 264 "./src-ag/AG2AspectAG.ag" #-}
   rule83 = \ ((_lhsInewNTs) :: Set NontermIdent) _ntLabel _ppNt ((_prodsIppL) :: PP_Doc) nt_ ->
                                                     {-# LINE 264 "./src-ag/AG2AspectAG.ag" #-}
                                                     ( if (Set.member nt_ _lhsInewNTs)
                                                       then _ntLabel     >|< " = proxy :: Proxy " >|< _ppNt
                                                       else empty)  >-<
                                                     _prodsIppL
                                                     {-# LINE 1415 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule84 #-}
   {-# LINE 269 "./src-ag/AG2AspectAG.ag" #-}
   rule84 = \ ((_lhsInewNTs) :: Set NontermIdent) _ntLabel ((_prodsIppLI) :: [PP_Doc]) nt_ ->
                                                     {-# LINE 269 "./src-ag/AG2AspectAG.ag" #-}
                                                     ( if (not $ Set.member nt_ _lhsInewNTs)
                                                       then [ _ntLabel     ]
                                                       else [ ])  ++
                                                     _prodsIppLI
                                                     {-# LINE 1424 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule85 #-}
   {-# LINE 324 "./src-ag/AG2AspectAG.ag" #-}
   rule85 = \ _inhNoGroup ((_lhsInewNTs) :: Set NontermIdent) _ppNt ((_prodsIppA) :: PP_Doc) _synNoGroup inh_ nt_ syn_ ->
                                                     {-# LINE 324 "./src-ag/AG2AspectAG.ag" #-}
                                                     ( if (Set.member nt_ _lhsInewNTs)
                                                       then
                                                              defAttRec (pp "InhG") _ppNt     inh_ _inhNoGroup     >-<
                                                              defAttRec (pp "SynG") _ppNt     syn_ _synNoGroup
                                                       else   empty) >-<
                                                     _prodsIppA
                                                     {-# LINE 1435 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule86 #-}
   {-# LINE 338 "./src-ag/AG2AspectAG.ag" #-}
   rule86 = \ ((_lhsInewNTs) :: Set NontermIdent) _ppNt nt_ ->
                                                     {-# LINE 338 "./src-ag/AG2AspectAG.ag" #-}
                                                     if (not $ Set.member nt_ _lhsInewNTs)
                                                     then [ ppName [(pp "InhG"), _ppNt     ] >#< pp "(..)", ppName [(pp "SynG"), _ppNt     ] >#< pp "(..)" ]
                                                     else [ ]
                                                     {-# LINE 1443 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule87 #-}
   {-# LINE 406 "./src-ag/AG2AspectAG.ag" #-}
   rule87 = \ inh_ nt_ syn_ ->
                                                     {-# LINE 406 "./src-ag/AG2AspectAG.ag" #-}
                                                     [ ("nt_" >|< nt_, Map.union inh_ syn_) ]
                                                     {-# LINE 1449 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule88 #-}
   {-# LINE 415 "./src-ag/AG2AspectAG.ag" #-}
   rule88 = \ ((_lhsInewNTs) :: Set NontermIdent) nt_ ->
                                                         {-# LINE 415 "./src-ag/AG2AspectAG.ag" #-}
                                                         Set.member nt_ _lhsInewNTs
                                                         {-# LINE 1455 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule89 #-}
   {-# LINE 425 "./src-ag/AG2AspectAG.ag" #-}
   rule89 = \ ((_prodsIppR) :: PP_Doc) nt_ ->
                                                     {-# LINE 425 "./src-ag/AG2AspectAG.ag" #-}
                                                     pp "----" >|< pp nt_ >-< _prodsIppR
                                                     {-# LINE 1461 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule90 #-}
   {-# LINE 735 "./src-ag/AG2AspectAG.ag" #-}
   rule90 = \ _ppNt ((_prodsIppCata) :: PP_Doc) ->
                                                      {-# LINE 735 "./src-ag/AG2AspectAG.ag" #-}
                                                      "----" >|< _ppNt     >-< _prodsIppCata
                                                      {-# LINE 1467 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule91 #-}
   {-# LINE 766 "./src-ag/AG2AspectAG.ag" #-}
   rule91 = \ syn_ ->
                                                     {-# LINE 766 "./src-ag/AG2AspectAG.ag" #-}
                                                     syn_
                                                     {-# LINE 1473 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule92 #-}
   {-# LINE 767 "./src-ag/AG2AspectAG.ag" #-}
   rule92 = \ inh_ ->
                                                     {-# LINE 767 "./src-ag/AG2AspectAG.ag" #-}
                                                     inh_
                                                     {-# LINE 1479 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule93 #-}
   {-# LINE 779 "./src-ag/AG2AspectAG.ag" #-}
   rule93 = \ _inhNoGroup _ppNt ((_prodsIppSPF) :: PP_Doc) _synNoGroup ->
                                           {-# LINE 779 "./src-ag/AG2AspectAG.ag" #-}
                                           let      inhAtts = attTypes _inhNoGroup
                                                    synAtts = attTypes _synNoGroup
                                           in
                                                    "----" >|< _ppNt     >-<
                                                    "type T_" >|< _ppNt     >|< " = " >|<
                                                    "(Record " >|<
                                                    inhAtts >|<
                                                    "(HCons (LVPair (Proxy Att_inh) InhG_" >|< _ppNt     >|< ") HNil))" >|<
                                                    replicate (length inhAtts) ")" >|< " -> " >|<
                                                    "(Record " >|<
                                                    synAtts >|<
                                                    "(HCons (LVPair (Proxy Att_syn) SynG_" >|< _ppNt     >|< ") HNil))" >|<
                                                    replicate (length synAtts) ")" >-<
                                                    "-- instance SemType T_" >|< _ppNt     >|< " " >|< _ppNt     >-<
                                                    "-- sem_" >|< _ppNt     >|< " :: " >|< _ppNt     >|< " -> T_" >|<  _ppNt     >-<
                                                    _prodsIppSPF
                                           {-# LINE 1500 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule94 #-}
   {-# LINE 847 "./src-ag/AG2AspectAG.ag" #-}
   rule94 = \ _inhNoGroup _ppNt inh_ ->
                                              {-# LINE 847 "./src-ag/AG2AspectAG.ag" #-}
                                              ppName [pp "wrap", _ppNt    ] >|< " sem " >|< attVars inh_ >|< " = " >-<
                                              "   sem " >|< attFields inh_ _inhNoGroup     _ppNt
                                              {-# LINE 1507 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule97 #-}
   rule97 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule98 #-}
   rule98 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule99 #-}
   rule99 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap

-- Nonterminals ------------------------------------------------
-- wrapper
data Inh_Nonterminals  = Inh_Nonterminals { derivs_Inh_Nonterminals :: (Derivings), ext_Inh_Nonterminals :: (Maybe String), inhMap_Inh_Nonterminals :: (Map Identifier Attributes), newAtts_Inh_Nonterminals :: ( Attributes ), newNTs_Inh_Nonterminals :: (Set NontermIdent), newProds_Inh_Nonterminals :: ( DataTypes ), o_noGroup_Inh_Nonterminals :: ([String]), o_rename_Inh_Nonterminals :: (Bool), synMap_Inh_Nonterminals :: (Map Identifier Attributes), tSyns_Inh_Nonterminals :: (TypeSyns) }
data Syn_Nonterminals  = Syn_Nonterminals { extendedNTs_Syn_Nonterminals :: (Set NontermIdent), inhMap'_Syn_Nonterminals :: (Map Identifier Attributes), ppA_Syn_Nonterminals :: (PP_Doc), ppAI_Syn_Nonterminals :: ([PP_Doc]), ppCata_Syn_Nonterminals :: (PP_Doc), ppD_Syn_Nonterminals :: (PP_Doc), ppDI_Syn_Nonterminals :: ([PP_Doc]), ppL_Syn_Nonterminals :: (PP_Doc), ppLI_Syn_Nonterminals :: ([PP_Doc]), ppNtL_Syn_Nonterminals :: ([(PP_Doc, Attributes)]), ppR_Syn_Nonterminals :: (PP_Doc), ppSF_Syn_Nonterminals :: (PP_Doc), ppW_Syn_Nonterminals :: (PP_Doc), synMap'_Syn_Nonterminals :: (Map Identifier Attributes) }
{-# INLINABLE wrap_Nonterminals #-}
wrap_Nonterminals :: T_Nonterminals  -> Inh_Nonterminals  -> (Syn_Nonterminals )
wrap_Nonterminals (T_Nonterminals act) (Inh_Nonterminals _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Nonterminals_vIn25 _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns
        (T_Nonterminals_vOut25 _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap') <- return (inv_Nonterminals_s26 sem arg)
        return (Syn_Nonterminals _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap')
   )

-- cata
{-# NOINLINE sem_Nonterminals #-}
sem_Nonterminals :: Nonterminals  -> T_Nonterminals 
sem_Nonterminals list = Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list)

-- semantic domain
newtype T_Nonterminals  = T_Nonterminals {
                                         attach_T_Nonterminals :: Identity (T_Nonterminals_s26 )
                                         }
newtype T_Nonterminals_s26  = C_Nonterminals_s26 {
                                                 inv_Nonterminals_s26 :: (T_Nonterminals_v25 )
                                                 }
data T_Nonterminals_s27  = C_Nonterminals_s27
type T_Nonterminals_v25  = (T_Nonterminals_vIn25 ) -> (T_Nonterminals_vOut25 )
data T_Nonterminals_vIn25  = T_Nonterminals_vIn25 (Derivings) (Maybe String) (Map Identifier Attributes) ( Attributes ) (Set NontermIdent) ( DataTypes ) ([String]) (Bool) (Map Identifier Attributes) (TypeSyns)
data T_Nonterminals_vOut25  = T_Nonterminals_vOut25 (Set NontermIdent) (Map Identifier Attributes) (PP_Doc) ([PP_Doc]) (PP_Doc) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) ([(PP_Doc, Attributes)]) (PP_Doc) (PP_Doc) (PP_Doc) (Map Identifier Attributes)
{-# NOINLINE sem_Nonterminals_Cons #-}
sem_Nonterminals_Cons :: T_Nonterminal  -> T_Nonterminals  -> T_Nonterminals 
sem_Nonterminals_Cons arg_hd_ arg_tl_ = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ (T_Nonterminals_vIn25 _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) -> ( let
         _hdX23 = Control.Monad.Identity.runIdentity (attach_T_Nonterminal (arg_hd_))
         _tlX26 = Control.Monad.Identity.runIdentity (attach_T_Nonterminals (arg_tl_))
         (T_Nonterminal_vOut22 _hdIextendedNTs _hdIinhMap' _hdIppA _hdIppAI _hdIppCata _hdIppD _hdIppDI _hdIppL _hdIppLI _hdIppNtL _hdIppR _hdIppSF _hdIppW _hdIsynMap') = inv_Nonterminal_s23 _hdX23 (T_Nonterminal_vIn22 _hdOderivs _hdOext _hdOinhMap _hdOnewAtts _hdOnewNTs _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOsynMap _hdOtSyns)
         (T_Nonterminals_vOut25 _tlIextendedNTs _tlIinhMap' _tlIppA _tlIppAI _tlIppCata _tlIppD _tlIppDI _tlIppL _tlIppLI _tlIppNtL _tlIppR _tlIppSF _tlIppW _tlIsynMap') = inv_Nonterminals_s26 _tlX26 (T_Nonterminals_vIn25 _tlOderivs _tlOext _tlOinhMap _tlOnewAtts _tlOnewNTs _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOsynMap _tlOtSyns)
         _lhsOextendedNTs :: Set NontermIdent
         _lhsOextendedNTs = rule101 _hdIextendedNTs _tlIextendedNTs
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule102 _hdIinhMap' _tlIinhMap'
         _lhsOppA :: PP_Doc
         _lhsOppA = rule103 _hdIppA _tlIppA
         _lhsOppAI :: [PP_Doc]
         _lhsOppAI = rule104 _hdIppAI _tlIppAI
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule105 _hdIppCata _tlIppCata
         _lhsOppD :: PP_Doc
         _lhsOppD = rule106 _hdIppD _tlIppD
         _lhsOppDI :: [PP_Doc]
         _lhsOppDI = rule107 _hdIppDI _tlIppDI
         _lhsOppL :: PP_Doc
         _lhsOppL = rule108 _hdIppL _tlIppL
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule109 _hdIppLI _tlIppLI
         _lhsOppNtL :: [(PP_Doc, Attributes)]
         _lhsOppNtL = rule110 _hdIppNtL _tlIppNtL
         _lhsOppR :: PP_Doc
         _lhsOppR = rule111 _hdIppR _tlIppR
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule112 _hdIppSF _tlIppSF
         _lhsOppW :: PP_Doc
         _lhsOppW = rule113 _hdIppW _tlIppW
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule114 _hdIsynMap' _tlIsynMap'
         _hdOderivs = rule115 _lhsIderivs
         _hdOext = rule116 _lhsIext
         _hdOinhMap = rule117 _lhsIinhMap
         _hdOnewAtts = rule118 _lhsInewAtts
         _hdOnewNTs = rule119 _lhsInewNTs
         _hdOnewProds = rule120 _lhsInewProds
         _hdOo_noGroup = rule121 _lhsIo_noGroup
         _hdOo_rename = rule122 _lhsIo_rename
         _hdOsynMap = rule123 _lhsIsynMap
         _hdOtSyns = rule124 _lhsItSyns
         _tlOderivs = rule125 _lhsIderivs
         _tlOext = rule126 _lhsIext
         _tlOinhMap = rule127 _lhsIinhMap
         _tlOnewAtts = rule128 _lhsInewAtts
         _tlOnewNTs = rule129 _lhsInewNTs
         _tlOnewProds = rule130 _lhsInewProds
         _tlOo_noGroup = rule131 _lhsIo_noGroup
         _tlOo_rename = rule132 _lhsIo_rename
         _tlOsynMap = rule133 _lhsIsynMap
         _tlOtSyns = rule134 _lhsItSyns
         __result_ = T_Nonterminals_vOut25 _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule101 #-}
   rule101 = \ ((_hdIextendedNTs) :: Set NontermIdent) ((_tlIextendedNTs) :: Set NontermIdent) ->
     _hdIextendedNTs `Set.union` _tlIextendedNTs
   {-# INLINE rule102 #-}
   rule102 = \ ((_hdIinhMap') :: Map Identifier Attributes) ((_tlIinhMap') :: Map Identifier Attributes) ->
     _hdIinhMap' `Map.union` _tlIinhMap'
   {-# INLINE rule103 #-}
   rule103 = \ ((_hdIppA) :: PP_Doc) ((_tlIppA) :: PP_Doc) ->
     _hdIppA >-< _tlIppA
   {-# INLINE rule104 #-}
   rule104 = \ ((_hdIppAI) :: [PP_Doc]) ((_tlIppAI) :: [PP_Doc]) ->
     _hdIppAI ++ _tlIppAI
   {-# INLINE rule105 #-}
   rule105 = \ ((_hdIppCata) :: PP_Doc) ((_tlIppCata) :: PP_Doc) ->
     _hdIppCata >-< _tlIppCata
   {-# INLINE rule106 #-}
   rule106 = \ ((_hdIppD) :: PP_Doc) ((_tlIppD) :: PP_Doc) ->
     _hdIppD >-< _tlIppD
   {-# INLINE rule107 #-}
   rule107 = \ ((_hdIppDI) :: [PP_Doc]) ((_tlIppDI) :: [PP_Doc]) ->
     _hdIppDI ++ _tlIppDI
   {-# INLINE rule108 #-}
   rule108 = \ ((_hdIppL) :: PP_Doc) ((_tlIppL) :: PP_Doc) ->
     _hdIppL >-< _tlIppL
   {-# INLINE rule109 #-}
   rule109 = \ ((_hdIppLI) :: [PP_Doc]) ((_tlIppLI) :: [PP_Doc]) ->
     _hdIppLI ++ _tlIppLI
   {-# INLINE rule110 #-}
   rule110 = \ ((_hdIppNtL) :: [(PP_Doc, Attributes)]) ((_tlIppNtL) :: [(PP_Doc, Attributes)]) ->
     _hdIppNtL ++ _tlIppNtL
   {-# INLINE rule111 #-}
   rule111 = \ ((_hdIppR) :: PP_Doc) ((_tlIppR) :: PP_Doc) ->
     _hdIppR >-< _tlIppR
   {-# INLINE rule112 #-}
   rule112 = \ ((_hdIppSF) :: PP_Doc) ((_tlIppSF) :: PP_Doc) ->
     _hdIppSF >-< _tlIppSF
   {-# INLINE rule113 #-}
   rule113 = \ ((_hdIppW) :: PP_Doc) ((_tlIppW) :: PP_Doc) ->
     _hdIppW >-< _tlIppW
   {-# INLINE rule114 #-}
   rule114 = \ ((_hdIsynMap') :: Map Identifier Attributes) ((_tlIsynMap') :: Map Identifier Attributes) ->
     _hdIsynMap' `Map.union` _tlIsynMap'
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIderivs) :: Derivings) ->
     _lhsIderivs
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsInewNTs) :: Set NontermIdent) ->
     _lhsInewNTs
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsInewProds) ::  DataTypes ) ->
     _lhsInewProds
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule123 #-}
   rule123 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsItSyns) :: TypeSyns) ->
     _lhsItSyns
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIderivs) :: Derivings) ->
     _lhsIderivs
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule128 #-}
   rule128 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule129 #-}
   rule129 = \ ((_lhsInewNTs) :: Set NontermIdent) ->
     _lhsInewNTs
   {-# INLINE rule130 #-}
   rule130 = \ ((_lhsInewProds) ::  DataTypes ) ->
     _lhsInewProds
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsItSyns) :: TypeSyns) ->
     _lhsItSyns
{-# NOINLINE sem_Nonterminals_Nil #-}
sem_Nonterminals_Nil ::  T_Nonterminals 
sem_Nonterminals_Nil  = T_Nonterminals (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_Nonterminals_v25 
      v25 = \ (T_Nonterminals_vIn25 _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) -> ( let
         _lhsOextendedNTs :: Set NontermIdent
         _lhsOextendedNTs = rule135  ()
         _lhsOinhMap' :: Map Identifier Attributes
         _lhsOinhMap' = rule136  ()
         _lhsOppA :: PP_Doc
         _lhsOppA = rule137  ()
         _lhsOppAI :: [PP_Doc]
         _lhsOppAI = rule138  ()
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule139  ()
         _lhsOppD :: PP_Doc
         _lhsOppD = rule140  ()
         _lhsOppDI :: [PP_Doc]
         _lhsOppDI = rule141  ()
         _lhsOppL :: PP_Doc
         _lhsOppL = rule142  ()
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule143  ()
         _lhsOppNtL :: [(PP_Doc, Attributes)]
         _lhsOppNtL = rule144  ()
         _lhsOppR :: PP_Doc
         _lhsOppR = rule145  ()
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule146  ()
         _lhsOppW :: PP_Doc
         _lhsOppW = rule147  ()
         _lhsOsynMap' :: Map Identifier Attributes
         _lhsOsynMap' = rule148  ()
         __result_ = T_Nonterminals_vOut25 _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap'
         in __result_ )
     in C_Nonterminals_s26 v25
   {-# INLINE rule135 #-}
   rule135 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule136 #-}
   rule136 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule137 #-}
   rule137 = \  (_ :: ()) ->
     empty
   {-# INLINE rule138 #-}
   rule138 = \  (_ :: ()) ->
     []
   {-# INLINE rule139 #-}
   rule139 = \  (_ :: ()) ->
     empty
   {-# INLINE rule140 #-}
   rule140 = \  (_ :: ()) ->
     empty
   {-# INLINE rule141 #-}
   rule141 = \  (_ :: ()) ->
     []
   {-# INLINE rule142 #-}
   rule142 = \  (_ :: ()) ->
     empty
   {-# INLINE rule143 #-}
   rule143 = \  (_ :: ()) ->
     []
   {-# INLINE rule144 #-}
   rule144 = \  (_ :: ()) ->
     []
   {-# INLINE rule145 #-}
   rule145 = \  (_ :: ()) ->
     empty
   {-# INLINE rule146 #-}
   rule146 = \  (_ :: ()) ->
     empty
   {-# INLINE rule147 #-}
   rule147 = \  (_ :: ()) ->
     empty
   {-# INLINE rule148 #-}
   rule148 = \  (_ :: ()) ->
     Map.empty

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern {  }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: (Pattern), info_Syn_Pattern :: ((Identifier, Identifier)) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Pattern_vIn28 
        (T_Pattern_vOut28 _lhsOcopy _lhsOinfo) <- return (inv_Pattern_s29 sem arg)
        return (Syn_Pattern _lhsOcopy _lhsOinfo)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias field_ attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s29 )
                               }
newtype T_Pattern_s29  = C_Pattern_s29 {
                                       inv_Pattern_s29 :: (T_Pattern_v28 )
                                       }
data T_Pattern_s30  = C_Pattern_s30
type T_Pattern_v28  = (T_Pattern_vIn28 ) -> (T_Pattern_vOut28 )
data T_Pattern_vIn28  = T_Pattern_vIn28 
data T_Pattern_vOut28  = T_Pattern_vOut28 (Pattern) ((Identifier, Identifier))
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIcopy) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 )
         _lhsOinfo :: (Identifier, Identifier)
         _lhsOinfo = rule149  ()
         _copy = rule150 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule151 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOinfo
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule149 #-}
   {-# LINE 383 "./src-ag/AG2AspectAG.ag" #-}
   rule149 = \  (_ :: ()) ->
                                                    {-# LINE 383 "./src-ag/AG2AspectAG.ag" #-}
                                                    error "Pattern Constr undefined!!"
                                                    {-# LINE 1858 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule150 #-}
   rule150 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule151 #-}
   rule151 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patsX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut31 _patsIcopy) = inv_Patterns_s32 _patsX32 (T_Patterns_vIn31 )
         _lhsOinfo :: (Identifier, Identifier)
         _lhsOinfo = rule152  ()
         _copy = rule153 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule154 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOinfo
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule152 #-}
   {-# LINE 384 "./src-ag/AG2AspectAG.ag" #-}
   rule152 = \  (_ :: ()) ->
                                                    {-# LINE 384 "./src-ag/AG2AspectAG.ag" #-}
                                                    error "Pattern Product undefined!!"
                                                    {-# LINE 1887 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule153 #-}
   rule153 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule154 #-}
   rule154 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIcopy _patIinfo) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 )
         _lhsOinfo :: (Identifier, Identifier)
         _lhsOinfo = rule155 arg_attr_ arg_field_
         _copy = rule156 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule157 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOinfo
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule155 #-}
   {-# LINE 382 "./src-ag/AG2AspectAG.ag" #-}
   rule155 = \ attr_ field_ ->
                                                    {-# LINE 382 "./src-ag/AG2AspectAG.ag" #-}
                                                    (field_, attr_)
                                                    {-# LINE 1916 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule156 #-}
   rule156 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule157 #-}
   rule157 = \ _copy ->
     _copy
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _patX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut28 _patIcopy _patIinfo) = inv_Pattern_s29 _patX29 (T_Pattern_vIn28 )
         _copy = rule158 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule159 _copy
         _lhsOinfo :: (Identifier, Identifier)
         _lhsOinfo = rule160 _patIinfo
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOinfo
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule158 #-}
   rule158 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule159 #-}
   rule159 = \ _copy ->
     _copy
   {-# INLINE rule160 #-}
   rule160 = \ ((_patIinfo) :: (Identifier, Identifier)) ->
     _patIinfo
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Pattern_v28 
      v28 = \ (T_Pattern_vIn28 ) -> ( let
         _lhsOinfo :: (Identifier, Identifier)
         _lhsOinfo = rule161  ()
         _copy = rule162 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule163 _copy
         __result_ = T_Pattern_vOut28 _lhsOcopy _lhsOinfo
         in __result_ )
     in C_Pattern_s29 v28
   {-# INLINE rule161 #-}
   {-# LINE 385 "./src-ag/AG2AspectAG.ag" #-}
   rule161 = \  (_ :: ()) ->
                                                    {-# LINE 385 "./src-ag/AG2AspectAG.ag" #-}
                                                    error "Pattern Underscore undefined!!"
                                                    {-# LINE 1969 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule162 #-}
   rule162 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule163 #-}
   rule163 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns {  }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: (Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Patterns_vIn31 
        (T_Patterns_vOut31 _lhsOcopy) <- return (inv_Patterns_s32 sem arg)
        return (Syn_Patterns _lhsOcopy)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s32 )
                                 }
newtype T_Patterns_s32  = C_Patterns_s32 {
                                         inv_Patterns_s32 :: (T_Patterns_v31 )
                                         }
data T_Patterns_s33  = C_Patterns_s33
type T_Patterns_v31  = (T_Patterns_vIn31 ) -> (T_Patterns_vOut31 )
data T_Patterns_vIn31  = T_Patterns_vIn31 
data T_Patterns_vOut31  = T_Patterns_vOut31 (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ (T_Patterns_vIn31 ) -> ( let
         _hdX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX32 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut28 _hdIcopy _hdIinfo) = inv_Pattern_s29 _hdX29 (T_Pattern_vIn28 )
         (T_Patterns_vOut31 _tlIcopy) = inv_Patterns_s32 _tlX32 (T_Patterns_vIn31 )
         _copy = rule164 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule165 _copy
         __result_ = T_Patterns_vOut31 _lhsOcopy
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule164 #-}
   rule164 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule165 #-}
   rule165 = \ _copy ->
     _copy
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_Patterns_v31 
      v31 = \ (T_Patterns_vIn31 ) -> ( let
         _copy = rule166  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule167 _copy
         __result_ = T_Patterns_vOut31 _lhsOcopy
         in __result_ )
     in C_Patterns_s32 v31
   {-# INLINE rule166 #-}
   rule166 = \  (_ :: ()) ->
     []
   {-# INLINE rule167 #-}
   rule167 = \ _copy ->
     _copy

-- Production --------------------------------------------------
-- wrapper
data Inh_Production  = Inh_Production { ext_Inh_Production :: (Maybe String), inh_Inh_Production :: ( Attributes ), inhMap_Inh_Production :: (Map Identifier Attributes), inhNoGroup_Inh_Production :: ([String]), newAtts_Inh_Production :: ( Attributes ), newNT_Inh_Production :: (Bool), newProds_Inh_Production :: ( Map.Map ConstructorIdent FieldMap ), o_noGroup_Inh_Production :: ([String]), o_rename_Inh_Production :: (Bool), ppNt_Inh_Production :: (PP_Doc), syn_Inh_Production :: ( Attributes ), synMap_Inh_Production :: (Map Identifier Attributes), synNoGroup_Inh_Production :: ([String]) }
data Syn_Production  = Syn_Production { hasMoreProds_Syn_Production :: ( Bool ), ppA_Syn_Production :: (PP_Doc), ppCata_Syn_Production :: (PP_Doc), ppD_Syn_Production :: (PP_Doc), ppDI_Syn_Production :: ([PP_Doc]), ppL_Syn_Production :: (PP_Doc), ppLI_Syn_Production :: ([PP_Doc]), ppR_Syn_Production :: (PP_Doc), ppRA_Syn_Production :: ([PP_Doc]), ppSF_Syn_Production :: (PP_Doc), ppSPF_Syn_Production :: (PP_Doc), prdInh_Syn_Production :: (Attributes) }
{-# INLINABLE wrap_Production #-}
wrap_Production :: T_Production  -> Inh_Production  -> (Syn_Production )
wrap_Production (T_Production act) (Inh_Production _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Production_vIn34 _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup
        (T_Production_vOut34 _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh) <- return (inv_Production_s35 sem arg)
        return (Syn_Production _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh)
   )

-- cata
{-# INLINE sem_Production #-}
sem_Production :: Production  -> T_Production 
sem_Production ( Production con_ params_ constraints_ children_ rules_ typeSigs_ macro_ ) = sem_Production_Production con_ params_ constraints_ ( sem_Children children_ ) ( sem_Rules rules_ ) ( sem_TypeSigs typeSigs_ ) macro_

-- semantic domain
newtype T_Production  = T_Production {
                                     attach_T_Production :: Identity (T_Production_s35 )
                                     }
newtype T_Production_s35  = C_Production_s35 {
                                             inv_Production_s35 :: (T_Production_v34 )
                                             }
data T_Production_s36  = C_Production_s36
type T_Production_v34  = (T_Production_vIn34 ) -> (T_Production_vOut34 )
data T_Production_vIn34  = T_Production_vIn34 (Maybe String) ( Attributes ) (Map Identifier Attributes) ([String]) ( Attributes ) (Bool) ( Map.Map ConstructorIdent FieldMap ) ([String]) (Bool) (PP_Doc) ( Attributes ) (Map Identifier Attributes) ([String])
data T_Production_vOut34  = T_Production_vOut34 ( Bool ) (PP_Doc) (PP_Doc) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) (PP_Doc) (Attributes)
{-# NOINLINE sem_Production_Production #-}
sem_Production_Production :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_Children  -> T_Rules  -> T_TypeSigs  -> (MaybeMacro) -> T_Production 
sem_Production_Production arg_con_ _ _ arg_children_ arg_rules_ arg_typeSigs_ arg_macro_ = T_Production (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_Production_v34 
      v34 = \ (T_Production_vIn34 _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_Children (arg_children_))
         _rulesX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_rules_))
         _typeSigsX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_typeSigs_))
         (T_Children_vOut4 _childrenIidCL _childrenIppCSF _childrenIppDL _childrenIppL _childrenIppLI _childrenIppR _childrenIprdInh) = inv_Children_s5 _childrenX5 (T_Children_vIn4 _childrenOext _childrenOinhMap _childrenOinhNoGroup _childrenOnewAtts _childrenOo_noGroup _childrenOo_rename _childrenOppNt _childrenOppProd _childrenOsynMap _childrenOsynNoGroup)
         (T_Rules_vOut43 _rulesIlocals _rulesIppRL) = inv_Rules_s44 _rulesX44 (T_Rules_vIn43 _rulesOext _rulesOinhNoGroup _rulesOnewAtts _rulesOnewProd _rulesOo_noGroup _rulesOppNt _rulesOppProd _rulesOsynNoGroup)
         (T_TypeSigs_vOut49 ) = inv_TypeSigs_s50 _typeSigsX50 (T_TypeSigs_vIn49 )
         _lhsOhasMoreProds ::  Bool 
         _lhsOhasMoreProds = rule168 _lhsInewProds arg_con_
         _ppProd = rule169 arg_con_
         _prodName = rule170 _lhsIppNt _ppProd
         _conName = rule171 _lhsIo_rename _ppProd _prodName
         _childrenOppProd = rule172 _ppProd
         _rulesOppProd = rule173 _ppProd
         _lhsOppD :: PP_Doc
         _lhsOppD = rule174 _childrenIppDL _conName
         _lhsOppL :: PP_Doc
         _lhsOppL = rule175 _childrenIppL _lhsInewProds arg_con_
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule176 _childrenIppLI _lhsInewProds arg_con_
         _lhsOppA :: PP_Doc
         _lhsOppA = rule177 _prodName _rulesIlocals
         _newProd = rule178 _lhsInewProds arg_con_
         (_ppR,_ppRA) = rule179 _childrenIidCL _childrenIppR _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsIppNt _lhsIsynNoGroup _newProd _prodName _rulesIlocals _rulesIppRL arg_con_
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule180 _lhsIext _lhsInewNT _newProd _ppRA _prodName arg_macro_
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule181 _childrenIppCSF _conName _lhsIppNt _prodName arg_con_
         _lhsOppSPF :: PP_Doc
         _lhsOppSPF = rule182 _childrenIppCSF _lhsIppNt _prodName arg_con_
         _lhsOppDI :: [PP_Doc]
         _lhsOppDI = rule183  ()
         _lhsOppR :: PP_Doc
         _lhsOppR = rule184 _ppR
         _lhsOppRA :: [PP_Doc]
         _lhsOppRA = rule185 _ppRA
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule186 _childrenIprdInh
         _childrenOext = rule187 _lhsIext
         _childrenOinhMap = rule188 _lhsIinhMap
         _childrenOinhNoGroup = rule189 _lhsIinhNoGroup
         _childrenOnewAtts = rule190 _lhsInewAtts
         _childrenOo_noGroup = rule191 _lhsIo_noGroup
         _childrenOo_rename = rule192 _lhsIo_rename
         _childrenOppNt = rule193 _lhsIppNt
         _childrenOsynMap = rule194 _lhsIsynMap
         _childrenOsynNoGroup = rule195 _lhsIsynNoGroup
         _rulesOext = rule196 _lhsIext
         _rulesOinhNoGroup = rule197 _lhsIinhNoGroup
         _rulesOnewAtts = rule198 _lhsInewAtts
         _rulesOnewProd = rule199 _newProd
         _rulesOo_noGroup = rule200 _lhsIo_noGroup
         _rulesOppNt = rule201 _lhsIppNt
         _rulesOsynNoGroup = rule202 _lhsIsynNoGroup
         __result_ = T_Production_vOut34 _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh
         in __result_ )
     in C_Production_s35 v34
   {-# INLINE rule168 #-}
   {-# LINE 103 "./src-ag/AG2AspectAG.ag" #-}
   rule168 = \ ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) con_ ->
                                                 {-# LINE 103 "./src-ag/AG2AspectAG.ag" #-}
                                                 not $ Map.member con_ _lhsInewProds
                                                 {-# LINE 2148 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule169 #-}
   {-# LINE 176 "./src-ag/AG2AspectAG.ag" #-}
   rule169 = \ con_ ->
                                                      {-# LINE 176 "./src-ag/AG2AspectAG.ag" #-}
                                                      pp con_
                                                      {-# LINE 2154 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule170 #-}
   {-# LINE 177 "./src-ag/AG2AspectAG.ag" #-}
   rule170 = \ ((_lhsIppNt) :: PP_Doc) _ppProd ->
                                                      {-# LINE 177 "./src-ag/AG2AspectAG.ag" #-}
                                                      ppName [_lhsIppNt, _ppProd    ]
                                                      {-# LINE 2160 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule171 #-}
   {-# LINE 178 "./src-ag/AG2AspectAG.ag" #-}
   rule171 = \ ((_lhsIo_rename) :: Bool) _ppProd _prodName ->
                                                      {-# LINE 178 "./src-ag/AG2AspectAG.ag" #-}
                                                      if _lhsIo_rename
                                                      then _prodName
                                                      else _ppProd
                                                      {-# LINE 2168 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule172 #-}
   {-# LINE 195 "./src-ag/AG2AspectAG.ag" #-}
   rule172 = \ _ppProd ->
                                                     {-# LINE 195 "./src-ag/AG2AspectAG.ag" #-}
                                                     _ppProd
                                                     {-# LINE 2174 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule173 #-}
   {-# LINE 196 "./src-ag/AG2AspectAG.ag" #-}
   rule173 = \ _ppProd ->
                                                     {-# LINE 196 "./src-ag/AG2AspectAG.ag" #-}
                                                     _ppProd
                                                     {-# LINE 2180 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule174 #-}
   {-# LINE 228 "./src-ag/AG2AspectAG.ag" #-}
   rule174 = \ ((_childrenIppDL) :: [PP_Doc]) _conName ->
                                                    {-# LINE 228 "./src-ag/AG2AspectAG.ag" #-}
                                                    _conName     >|< ppListSep " {" "}" ", " _childrenIppDL
                                                    {-# LINE 2186 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule175 #-}
   {-# LINE 275 "./src-ag/AG2AspectAG.ag" #-}
   rule175 = \ ((_childrenIppL) :: PP_Doc) ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) con_ ->
                                                     {-# LINE 275 "./src-ag/AG2AspectAG.ag" #-}
                                                     if (Map.member con_ _lhsInewProds)
                                                       then _childrenIppL
                                                       else empty
                                                     {-# LINE 2194 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule176 #-}
   {-# LINE 279 "./src-ag/AG2AspectAG.ag" #-}
   rule176 = \ ((_childrenIppLI) :: [PP_Doc]) ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) con_ ->
                                                     {-# LINE 279 "./src-ag/AG2AspectAG.ag" #-}
                                                     if (not $ Map.member con_ _lhsInewProds)
                                                       then _childrenIppLI
                                                       else []
                                                     {-# LINE 2202 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule177 #-}
   {-# LINE 332 "./src-ag/AG2AspectAG.ag" #-}
   rule177 = \ _prodName ((_rulesIlocals) :: [Identifier]) ->
                                                     {-# LINE 332 "./src-ag/AG2AspectAG.ag" #-}
                                                     defLocalAtts _prodName     (length _rulesIlocals) 1 $ sort _rulesIlocals
                                                     {-# LINE 2208 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule178 #-}
   {-# LINE 428 "./src-ag/AG2AspectAG.ag" #-}
   rule178 = \ ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) con_ ->
                                                     {-# LINE 428 "./src-ag/AG2AspectAG.ag" #-}
                                                     Map.member con_ _lhsInewProds
                                                     {-# LINE 2214 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule179 #-}
   {-# LINE 430 "./src-ag/AG2AspectAG.ag" #-}
   rule179 = \ ((_childrenIidCL) :: [(Identifier,Type)]) ((_childrenIppR) :: PP_Doc) ((_lhsIinhNoGroup) :: [String]) ((_lhsInewAtts) ::  Attributes ) ((_lhsInewNT) :: Bool) ((_lhsIppNt) :: PP_Doc) ((_lhsIsynNoGroup) :: [String]) _newProd _prodName ((_rulesIlocals) :: [Identifier]) ((_rulesIppRL) :: [ PPRule ]) con_ ->
                                {-# LINE 430 "./src-ag/AG2AspectAG.ag" #-}
                                let  (instR, instRA)  = defInstRules  _lhsIppNt con_ _lhsInewNT _newProd
                                                                      _childrenIppR _rulesIppRL _childrenIidCL _rulesIlocals
                                     (locR,  locRA)   = defLocRule    _lhsIppNt con_ _lhsInewNT _newProd
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (inhGR, inhGRA)  = defInhGRule   _lhsIppNt _prodName     _lhsInewNT _newProd
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (synGR, synGRA)  = defSynGRule   _lhsIppNt con_ _lhsInewNT _newProd
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (inhR,  inhRA)   = defInhRules   _lhsIppNt _prodName     _lhsInewNT _newProd     _lhsInewAtts
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (synR,  synRA)   = defSynRules   _lhsIppNt con_ _lhsInewNT _newProd     _lhsInewAtts
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (inhMR,  inhMRA) = modInhRules   _lhsIppNt _prodName     _lhsInewNT _newProd     _lhsInewAtts
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup _childrenIidCL _rulesIlocals
                                     (synMR,  synMRA) = modSynRules   _lhsIppNt con_ _lhsInewNT _newProd     _lhsInewAtts
                                                                      _childrenIppR _rulesIppRL _lhsIinhNoGroup _lhsIsynNoGroup  _childrenIidCL _rulesIlocals
                                in   ( vlist [instR,locR,inhGR,synGR,inhR,synR,inhMR,synMR]
                                     , instRA ++ locRA ++ inhGRA ++ synGRA ++ inhMRA ++ synMRA ++ inhRA ++ synRA)
                                {-# LINE 2237 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule180 #-}
   {-# LINE 740 "./src-ag/AG2AspectAG.ag" #-}
   rule180 = \ ((_lhsIext) :: Maybe String) ((_lhsInewNT) :: Bool) _newProd _ppRA _prodName macro_ ->
                                              {-# LINE 740 "./src-ag/AG2AspectAG.ag" #-}
                                              let  extend = maybe  []
                                                                   (  \ext ->  if (_lhsInewNT || (not _lhsInewNT && _newProd    ))
                                                                               then []
                                                                               else [ ext >|< ".atts_" >|< _prodName     ])
                                                                   _lhsIext
                                                   macro  = case macro_ of
                                                                               Nothing ->  []
                                                                               Just macro ->  [ "agMacro " >|<  ppMacro macro  ]
                                                   atts = sortBy (\a b -> compare (show a) (show b)) _ppRA
                                              in   "atts_" >|< _prodName     >|< " = " >|<
                                                                                 ppListSep "" "" " `ext` "
                                                                                 (atts ++ macro ++ extend ) >-<
                                                   "semP_" >|< _prodName     >|< pp " = knit atts_" >|< _prodName
                                              {-# LINE 2255 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule181 #-}
   {-# LINE 804 "./src-ag/AG2AspectAG.ag" #-}
   rule181 = \ ((_childrenIppCSF) :: [(Identifier,(PP_Doc,PP_Doc))]) _conName ((_lhsIppNt) :: PP_Doc) _prodName con_ ->
                                              {-# LINE 804 "./src-ag/AG2AspectAG.ag" #-}
                                              let  chi = _childrenIppCSF
                                                   ppPattern = case (show con_) of
                                                                "Cons"    -> ppParams (ppListSep "" "" " : ")
                                                                "Nil"     -> pp "[]"
                                                                otherwise -> _conName     >|< " " >|< (ppParams ppSpaced)
                                                   ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                              in   "sem_" >|< _lhsIppNt >|< " (" >|< ppPattern >|< ") = sem_" >|< _prodName     >|<
                                                   " (" >|< map (fst . snd) chi >|< "emptyRecord)"
                                              {-# LINE 2268 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule182 #-}
   {-# LINE 816 "./src-ag/AG2AspectAG.ag" #-}
   rule182 = \ ((_childrenIppCSF) :: [(Identifier,(PP_Doc,PP_Doc))]) ((_lhsIppNt) :: PP_Doc) _prodName con_ ->
                                              {-# LINE 816 "./src-ag/AG2AspectAG.ag" #-}
                                              let  chi = _childrenIppCSF
                                                   ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                              in   "sem_" >|< _lhsIppNt >|< "_" >|< con_ >#< ppParams ppSpaced >|< " = semP_" >|< _prodName     >|<
                                                   " (" >|< map (snd . snd) chi >|< "emptyRecord)"
                                              {-# LINE 2277 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule183 #-}
   rule183 = \  (_ :: ()) ->
     []
   {-# INLINE rule184 #-}
   rule184 = \ _ppR ->
     _ppR
   {-# INLINE rule185 #-}
   rule185 = \ _ppRA ->
     _ppRA
   {-# INLINE rule186 #-}
   rule186 = \ ((_childrenIprdInh) :: Attributes) ->
     _childrenIprdInh
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule195 #-}
   rule195 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
   {-# INLINE rule196 #-}
   rule196 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule197 #-}
   rule197 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule198 #-}
   rule198 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule199 #-}
   rule199 = \ _newProd ->
     _newProd
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup

-- Productions -------------------------------------------------
-- wrapper
data Inh_Productions  = Inh_Productions { ext_Inh_Productions :: (Maybe String), inh_Inh_Productions :: ( Attributes ), inhMap_Inh_Productions :: (Map Identifier Attributes), inhNoGroup_Inh_Productions :: ([String]), newAtts_Inh_Productions :: ( Attributes ), newNT_Inh_Productions :: (Bool), newProds_Inh_Productions :: ( Map.Map ConstructorIdent FieldMap ), o_noGroup_Inh_Productions :: ([String]), o_rename_Inh_Productions :: (Bool), ppNt_Inh_Productions :: (PP_Doc), syn_Inh_Productions :: ( Attributes ), synMap_Inh_Productions :: (Map Identifier Attributes), synNoGroup_Inh_Productions :: ([String]) }
data Syn_Productions  = Syn_Productions { hasMoreProds_Syn_Productions :: ( Bool ), ppA_Syn_Productions :: (PP_Doc), ppCata_Syn_Productions :: (PP_Doc), ppDL_Syn_Productions :: ([PP_Doc]), ppL_Syn_Productions :: (PP_Doc), ppLI_Syn_Productions :: ([PP_Doc]), ppR_Syn_Productions :: (PP_Doc), ppRA_Syn_Productions :: ([PP_Doc]), ppSF_Syn_Productions :: (PP_Doc), ppSPF_Syn_Productions :: (PP_Doc), prdInh_Syn_Productions :: (Attributes) }
{-# INLINABLE wrap_Productions #-}
wrap_Productions :: T_Productions  -> Inh_Productions  -> (Syn_Productions )
wrap_Productions (T_Productions act) (Inh_Productions _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Productions_vIn37 _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup
        (T_Productions_vOut37 _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh) <- return (inv_Productions_s38 sem arg)
        return (Syn_Productions _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh)
   )

-- cata
{-# NOINLINE sem_Productions #-}
sem_Productions :: Productions  -> T_Productions 
sem_Productions list = Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list)

-- semantic domain
newtype T_Productions  = T_Productions {
                                       attach_T_Productions :: Identity (T_Productions_s38 )
                                       }
newtype T_Productions_s38  = C_Productions_s38 {
                                               inv_Productions_s38 :: (T_Productions_v37 )
                                               }
data T_Productions_s39  = C_Productions_s39
type T_Productions_v37  = (T_Productions_vIn37 ) -> (T_Productions_vOut37 )
data T_Productions_vIn37  = T_Productions_vIn37 (Maybe String) ( Attributes ) (Map Identifier Attributes) ([String]) ( Attributes ) (Bool) ( Map.Map ConstructorIdent FieldMap ) ([String]) (Bool) (PP_Doc) ( Attributes ) (Map Identifier Attributes) ([String])
data T_Productions_vOut37  = T_Productions_vOut37 ( Bool ) (PP_Doc) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) ([PP_Doc]) (PP_Doc) (PP_Doc) (Attributes)
{-# NOINLINE sem_Productions_Cons #-}
sem_Productions_Cons :: T_Production  -> T_Productions  -> T_Productions 
sem_Productions_Cons arg_hd_ arg_tl_ = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Productions_v37 
      v37 = \ (T_Productions_vIn37 _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_Production (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_Productions (arg_tl_))
         (T_Production_vOut34 _hdIhasMoreProds _hdIppA _hdIppCata _hdIppD _hdIppDI _hdIppL _hdIppLI _hdIppR _hdIppRA _hdIppSF _hdIppSPF _hdIprdInh) = inv_Production_s35 _hdX35 (T_Production_vIn34 _hdOext _hdOinh _hdOinhMap _hdOinhNoGroup _hdOnewAtts _hdOnewNT _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOppNt _hdOsyn _hdOsynMap _hdOsynNoGroup)
         (T_Productions_vOut37 _tlIhasMoreProds _tlIppA _tlIppCata _tlIppDL _tlIppL _tlIppLI _tlIppR _tlIppRA _tlIppSF _tlIppSPF _tlIprdInh) = inv_Productions_s38 _tlX38 (T_Productions_vIn37 _tlOext _tlOinh _tlOinhMap _tlOinhNoGroup _tlOnewAtts _tlOnewNT _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOppNt _tlOsyn _tlOsynMap _tlOsynNoGroup)
         _hdOinhNoGroup = rule203 _hdIprdInh _lhsIinhNoGroup
         _lhsOppDL :: [PP_Doc]
         _lhsOppDL = rule204 _hdIppD _tlIppDL
         _lhsOhasMoreProds ::  Bool 
         _lhsOhasMoreProds = rule205 _hdIhasMoreProds _tlIhasMoreProds
         _lhsOppA :: PP_Doc
         _lhsOppA = rule206 _hdIppA _tlIppA
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule207 _hdIppCata _tlIppCata
         _lhsOppL :: PP_Doc
         _lhsOppL = rule208 _hdIppL _tlIppL
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule209 _hdIppLI _tlIppLI
         _lhsOppR :: PP_Doc
         _lhsOppR = rule210 _hdIppR _tlIppR
         _lhsOppRA :: [PP_Doc]
         _lhsOppRA = rule211 _hdIppRA _tlIppRA
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule212 _hdIppSF _tlIppSF
         _lhsOppSPF :: PP_Doc
         _lhsOppSPF = rule213 _hdIppSPF _tlIppSPF
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule214 _hdIprdInh _tlIprdInh
         _hdOext = rule215 _lhsIext
         _hdOinh = rule216 _lhsIinh
         _hdOinhMap = rule217 _lhsIinhMap
         _hdOnewAtts = rule218 _lhsInewAtts
         _hdOnewNT = rule219 _lhsInewNT
         _hdOnewProds = rule220 _lhsInewProds
         _hdOo_noGroup = rule221 _lhsIo_noGroup
         _hdOo_rename = rule222 _lhsIo_rename
         _hdOppNt = rule223 _lhsIppNt
         _hdOsyn = rule224 _lhsIsyn
         _hdOsynMap = rule225 _lhsIsynMap
         _hdOsynNoGroup = rule226 _lhsIsynNoGroup
         _tlOext = rule227 _lhsIext
         _tlOinh = rule228 _lhsIinh
         _tlOinhMap = rule229 _lhsIinhMap
         _tlOinhNoGroup = rule230 _lhsIinhNoGroup
         _tlOnewAtts = rule231 _lhsInewAtts
         _tlOnewNT = rule232 _lhsInewNT
         _tlOnewProds = rule233 _lhsInewProds
         _tlOo_noGroup = rule234 _lhsIo_noGroup
         _tlOo_rename = rule235 _lhsIo_rename
         _tlOppNt = rule236 _lhsIppNt
         _tlOsyn = rule237 _lhsIsyn
         _tlOsynMap = rule238 _lhsIsynMap
         _tlOsynNoGroup = rule239 _lhsIsynNoGroup
         __result_ = T_Productions_vOut37 _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule203 #-}
   {-# LINE 62 "./src-ag/AG2AspectAG.ag" #-}
   rule203 = \ ((_hdIprdInh) :: Attributes) ((_lhsIinhNoGroup) :: [String]) ->
                                {-# LINE 62 "./src-ag/AG2AspectAG.ag" #-}
                                filter (flip Map.member _hdIprdInh . identifier) _lhsIinhNoGroup
                                {-# LINE 2436 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule204 #-}
   {-# LINE 234 "./src-ag/AG2AspectAG.ag" #-}
   rule204 = \ ((_hdIppD) :: PP_Doc) ((_tlIppDL) :: [PP_Doc]) ->
                                                                                  {-# LINE 234 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  _hdIppD : _tlIppDL
                                                                                  {-# LINE 2442 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule205 #-}
   rule205 = \ ((_hdIhasMoreProds) ::  Bool ) ((_tlIhasMoreProds) ::  Bool ) ->
     _hdIhasMoreProds  ||  _tlIhasMoreProds
   {-# INLINE rule206 #-}
   rule206 = \ ((_hdIppA) :: PP_Doc) ((_tlIppA) :: PP_Doc) ->
     _hdIppA >-< _tlIppA
   {-# INLINE rule207 #-}
   rule207 = \ ((_hdIppCata) :: PP_Doc) ((_tlIppCata) :: PP_Doc) ->
     _hdIppCata >-< _tlIppCata
   {-# INLINE rule208 #-}
   rule208 = \ ((_hdIppL) :: PP_Doc) ((_tlIppL) :: PP_Doc) ->
     _hdIppL >-< _tlIppL
   {-# INLINE rule209 #-}
   rule209 = \ ((_hdIppLI) :: [PP_Doc]) ((_tlIppLI) :: [PP_Doc]) ->
     _hdIppLI ++ _tlIppLI
   {-# INLINE rule210 #-}
   rule210 = \ ((_hdIppR) :: PP_Doc) ((_tlIppR) :: PP_Doc) ->
     _hdIppR >-< _tlIppR
   {-# INLINE rule211 #-}
   rule211 = \ ((_hdIppRA) :: [PP_Doc]) ((_tlIppRA) :: [PP_Doc]) ->
     _hdIppRA ++ _tlIppRA
   {-# INLINE rule212 #-}
   rule212 = \ ((_hdIppSF) :: PP_Doc) ((_tlIppSF) :: PP_Doc) ->
     _hdIppSF >-< _tlIppSF
   {-# INLINE rule213 #-}
   rule213 = \ ((_hdIppSPF) :: PP_Doc) ((_tlIppSPF) :: PP_Doc) ->
     _hdIppSPF >-< _tlIppSPF
   {-# INLINE rule214 #-}
   rule214 = \ ((_hdIprdInh) :: Attributes) ((_tlIprdInh) :: Attributes) ->
     _hdIprdInh `Map.union` _tlIprdInh
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule216 #-}
   rule216 = \ ((_lhsIinh) ::  Attributes ) ->
     _lhsIinh
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsInewNT) :: Bool) ->
     _lhsInewNT
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) ->
     _lhsInewProds
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule223 #-}
   rule223 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIsyn) ::  Attributes ) ->
     _lhsIsyn
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule228 #-}
   rule228 = \ ((_lhsIinh) ::  Attributes ) ->
     _lhsIinh
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsIinhMap) :: Map Identifier Attributes) ->
     _lhsIinhMap
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsInewNT) :: Bool) ->
     _lhsInewNT
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsInewProds) ::  Map.Map ConstructorIdent FieldMap ) ->
     _lhsInewProds
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIo_rename) :: Bool) ->
     _lhsIo_rename
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIsyn) ::  Attributes ) ->
     _lhsIsyn
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIsynMap) :: Map Identifier Attributes) ->
     _lhsIsynMap
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
{-# NOINLINE sem_Productions_Nil #-}
sem_Productions_Nil ::  T_Productions 
sem_Productions_Nil  = T_Productions (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_Productions_v37 
      v37 = \ (T_Productions_vIn37 _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) -> ( let
         _lhsOppDL :: [PP_Doc]
         _lhsOppDL = rule240  ()
         _lhsOhasMoreProds ::  Bool 
         _lhsOhasMoreProds = rule241  ()
         _lhsOppA :: PP_Doc
         _lhsOppA = rule242  ()
         _lhsOppCata :: PP_Doc
         _lhsOppCata = rule243  ()
         _lhsOppL :: PP_Doc
         _lhsOppL = rule244  ()
         _lhsOppLI :: [PP_Doc]
         _lhsOppLI = rule245  ()
         _lhsOppR :: PP_Doc
         _lhsOppR = rule246  ()
         _lhsOppRA :: [PP_Doc]
         _lhsOppRA = rule247  ()
         _lhsOppSF :: PP_Doc
         _lhsOppSF = rule248  ()
         _lhsOppSPF :: PP_Doc
         _lhsOppSPF = rule249  ()
         _lhsOprdInh :: Attributes
         _lhsOprdInh = rule250  ()
         __result_ = T_Productions_vOut37 _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh
         in __result_ )
     in C_Productions_s38 v37
   {-# INLINE rule240 #-}
   {-# LINE 235 "./src-ag/AG2AspectAG.ag" #-}
   rule240 = \  (_ :: ()) ->
                                                                                  {-# LINE 235 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  []
                                                                                  {-# LINE 2585 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule241 #-}
   rule241 = \  (_ :: ()) ->
     False
   {-# INLINE rule242 #-}
   rule242 = \  (_ :: ()) ->
     empty
   {-# INLINE rule243 #-}
   rule243 = \  (_ :: ()) ->
     empty
   {-# INLINE rule244 #-}
   rule244 = \  (_ :: ()) ->
     empty
   {-# INLINE rule245 #-}
   rule245 = \  (_ :: ()) ->
     []
   {-# INLINE rule246 #-}
   rule246 = \  (_ :: ()) ->
     empty
   {-# INLINE rule247 #-}
   rule247 = \  (_ :: ()) ->
     []
   {-# INLINE rule248 #-}
   rule248 = \  (_ :: ()) ->
     empty
   {-# INLINE rule249 #-}
   rule249 = \  (_ :: ()) ->
     empty
   {-# INLINE rule250 #-}
   rule250 = \  (_ :: ()) ->
     Map.empty

-- Rule --------------------------------------------------------
-- wrapper
data Inh_Rule  = Inh_Rule { ext_Inh_Rule :: (Maybe String), inhNoGroup_Inh_Rule :: ([String]), newAtts_Inh_Rule :: ( Attributes ), newProd_Inh_Rule :: (Bool), o_noGroup_Inh_Rule :: ([String]), ppNt_Inh_Rule :: (PP_Doc), ppProd_Inh_Rule :: (PP_Doc), synNoGroup_Inh_Rule :: ([String]) }
data Syn_Rule  = Syn_Rule { locals_Syn_Rule :: ([Identifier]), ppRL_Syn_Rule :: ([ PPRule ]) }
{-# INLINABLE wrap_Rule #-}
wrap_Rule :: T_Rule  -> Inh_Rule  -> (Syn_Rule )
wrap_Rule (T_Rule act) (Inh_Rule _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rule_vIn40 _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup
        (T_Rule_vOut40 _lhsOlocals _lhsOppRL) <- return (inv_Rule_s41 sem arg)
        return (Syn_Rule _lhsOlocals _lhsOppRL)
   )

-- cata
{-# INLINE sem_Rule #-}
sem_Rule :: Rule  -> T_Rule 
sem_Rule ( Rule mbName_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ ) = sem_Rule_Rule mbName_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_

-- semantic domain
newtype T_Rule  = T_Rule {
                         attach_T_Rule :: Identity (T_Rule_s41 )
                         }
newtype T_Rule_s41  = C_Rule_s41 {
                                 inv_Rule_s41 :: (T_Rule_v40 )
                                 }
data T_Rule_s42  = C_Rule_s42
type T_Rule_v40  = (T_Rule_vIn40 ) -> (T_Rule_vOut40 )
data T_Rule_vIn40  = T_Rule_vIn40 (Maybe String) ([String]) ( Attributes ) (Bool) ([String]) (PP_Doc) (PP_Doc) ([String])
data T_Rule_vOut40  = T_Rule_vOut40 ([Identifier]) ([ PPRule ])
{-# NOINLINE sem_Rule_Rule #-}
sem_Rule_Rule :: (Maybe Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Bool) -> (Maybe Error) -> (Bool) -> T_Rule 
sem_Rule_Rule _ arg_pattern_ arg_rhs_ arg_owrt_ _ arg_explicit_ _ _ _ _ = T_Rule (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Rule_v40 
      v40 = \ (T_Rule_vIn40 _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) -> ( let
         _patternX29 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX8 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut28 _patternIcopy _patternIinfo) = inv_Pattern_s29 _patternX29 (T_Pattern_vIn28 )
         (T_Expression_vOut7 _rhsIppRE) = inv_Expression_s8 _rhsX8 (T_Expression_vIn7 _rhsOppNt _rhsOppProd)
         _lhsOlocals :: [Identifier]
         _lhsOlocals = rule251 _patternIinfo
         _lhsOppRL :: [ PPRule ]
         _lhsOppRL = rule252 _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _patternIinfo _rhsIppRE arg_explicit_ arg_owrt_
         _rhsOppNt = rule253 _lhsIppNt
         _rhsOppProd = rule254 _lhsIppProd
         __result_ = T_Rule_vOut40 _lhsOlocals _lhsOppRL
         in __result_ )
     in C_Rule_s41 v40
   {-# INLINE rule251 #-}
   {-# LINE 375 "./src-ag/AG2AspectAG.ag" #-}
   rule251 = \ ((_patternIinfo) :: (Identifier, Identifier)) ->
                                                       {-# LINE 375 "./src-ag/AG2AspectAG.ag" #-}
                                                       if (show (fst _patternIinfo) == "loc")
                                                        then [ snd _patternIinfo ]
                                                        else [ ]
                                                       {-# LINE 2674 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule252 #-}
   {-# LINE 472 "./src-ag/AG2AspectAG.ag" #-}
   rule252 = \ ((_lhsInewAtts) ::  Attributes ) ((_lhsInewProd) :: Bool) ((_lhsIo_noGroup) :: [String]) ((_lhsIppNt) :: PP_Doc) ((_patternIinfo) :: (Identifier, Identifier)) ((_rhsIppRE) :: [String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc) explicit_ owrt_ ->
                                                                             {-# LINE 472 "./src-ag/AG2AspectAG.ag" #-}
                                                                             if (not explicit_ &&  not _lhsInewProd && not (Map.member (snd _patternIinfo) _lhsInewAtts) )
                                                                             then []
                                                                             else [ ppRule _patternIinfo owrt_ (defRule _lhsIppNt _patternIinfo _lhsIo_noGroup _rhsIppRE) ]
                                                                             {-# LINE 2682 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIppProd) :: PP_Doc) ->
     _lhsIppProd

-- Rules -------------------------------------------------------
-- wrapper
data Inh_Rules  = Inh_Rules { ext_Inh_Rules :: (Maybe String), inhNoGroup_Inh_Rules :: ([String]), newAtts_Inh_Rules :: ( Attributes ), newProd_Inh_Rules :: (Bool), o_noGroup_Inh_Rules :: ([String]), ppNt_Inh_Rules :: (PP_Doc), ppProd_Inh_Rules :: (PP_Doc), synNoGroup_Inh_Rules :: ([String]) }
data Syn_Rules  = Syn_Rules { locals_Syn_Rules :: ([Identifier]), ppRL_Syn_Rules :: ([ PPRule ]) }
{-# INLINABLE wrap_Rules #-}
wrap_Rules :: T_Rules  -> Inh_Rules  -> (Syn_Rules )
wrap_Rules (T_Rules act) (Inh_Rules _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_Rules_vIn43 _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup
        (T_Rules_vOut43 _lhsOlocals _lhsOppRL) <- return (inv_Rules_s44 sem arg)
        return (Syn_Rules _lhsOlocals _lhsOppRL)
   )

-- cata
{-# NOINLINE sem_Rules #-}
sem_Rules :: Rules  -> T_Rules 
sem_Rules list = Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list)

-- semantic domain
newtype T_Rules  = T_Rules {
                           attach_T_Rules :: Identity (T_Rules_s44 )
                           }
newtype T_Rules_s44  = C_Rules_s44 {
                                   inv_Rules_s44 :: (T_Rules_v43 )
                                   }
data T_Rules_s45  = C_Rules_s45
type T_Rules_v43  = (T_Rules_vIn43 ) -> (T_Rules_vOut43 )
data T_Rules_vIn43  = T_Rules_vIn43 (Maybe String) ([String]) ( Attributes ) (Bool) ([String]) (PP_Doc) (PP_Doc) ([String])
data T_Rules_vOut43  = T_Rules_vOut43 ([Identifier]) ([ PPRule ])
{-# NOINLINE sem_Rules_Cons #-}
sem_Rules_Cons :: T_Rule  -> T_Rules  -> T_Rules 
sem_Rules_Cons arg_hd_ arg_tl_ = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Rules_v43 
      v43 = \ (T_Rules_vIn43 _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Rule (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Rules (arg_tl_))
         (T_Rule_vOut40 _hdIlocals _hdIppRL) = inv_Rule_s41 _hdX41 (T_Rule_vIn40 _hdOext _hdOinhNoGroup _hdOnewAtts _hdOnewProd _hdOo_noGroup _hdOppNt _hdOppProd _hdOsynNoGroup)
         (T_Rules_vOut43 _tlIlocals _tlIppRL) = inv_Rules_s44 _tlX44 (T_Rules_vIn43 _tlOext _tlOinhNoGroup _tlOnewAtts _tlOnewProd _tlOo_noGroup _tlOppNt _tlOppProd _tlOsynNoGroup)
         _lhsOppRL :: [ PPRule ]
         _lhsOppRL = rule255 _hdIppRL _tlIppRL
         _lhsOlocals :: [Identifier]
         _lhsOlocals = rule256 _hdIlocals _tlIlocals
         _hdOext = rule257 _lhsIext
         _hdOinhNoGroup = rule258 _lhsIinhNoGroup
         _hdOnewAtts = rule259 _lhsInewAtts
         _hdOnewProd = rule260 _lhsInewProd
         _hdOo_noGroup = rule261 _lhsIo_noGroup
         _hdOppNt = rule262 _lhsIppNt
         _hdOppProd = rule263 _lhsIppProd
         _hdOsynNoGroup = rule264 _lhsIsynNoGroup
         _tlOext = rule265 _lhsIext
         _tlOinhNoGroup = rule266 _lhsIinhNoGroup
         _tlOnewAtts = rule267 _lhsInewAtts
         _tlOnewProd = rule268 _lhsInewProd
         _tlOo_noGroup = rule269 _lhsIo_noGroup
         _tlOppNt = rule270 _lhsIppNt
         _tlOppProd = rule271 _lhsIppProd
         _tlOsynNoGroup = rule272 _lhsIsynNoGroup
         __result_ = T_Rules_vOut43 _lhsOlocals _lhsOppRL
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule255 #-}
   {-# LINE 468 "./src-ag/AG2AspectAG.ag" #-}
   rule255 = \ ((_hdIppRL) :: [ PPRule ]) ((_tlIppRL) :: [ PPRule ]) ->
                                                                                  {-# LINE 468 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  _hdIppRL ++ _tlIppRL
                                                                                  {-# LINE 2759 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule256 #-}
   rule256 = \ ((_hdIlocals) :: [Identifier]) ((_tlIlocals) :: [Identifier]) ->
     _hdIlocals ++ _tlIlocals
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsInewProd) :: Bool) ->
     _lhsInewProd
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIppProd) :: PP_Doc) ->
     _lhsIppProd
   {-# INLINE rule264 #-}
   rule264 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIext) :: Maybe String) ->
     _lhsIext
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIinhNoGroup) :: [String]) ->
     _lhsIinhNoGroup
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsInewAtts) ::  Attributes ) ->
     _lhsInewAtts
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsInewProd) :: Bool) ->
     _lhsInewProd
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIo_noGroup) :: [String]) ->
     _lhsIo_noGroup
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsIppNt) :: PP_Doc) ->
     _lhsIppNt
   {-# INLINE rule271 #-}
   rule271 = \ ((_lhsIppProd) :: PP_Doc) ->
     _lhsIppProd
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIsynNoGroup) :: [String]) ->
     _lhsIsynNoGroup
{-# NOINLINE sem_Rules_Nil #-}
sem_Rules_Nil ::  T_Rules 
sem_Rules_Nil  = T_Rules (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Rules_v43 
      v43 = \ (T_Rules_vIn43 _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) -> ( let
         _lhsOppRL :: [ PPRule ]
         _lhsOppRL = rule273  ()
         _lhsOlocals :: [Identifier]
         _lhsOlocals = rule274  ()
         __result_ = T_Rules_vOut43 _lhsOlocals _lhsOppRL
         in __result_ )
     in C_Rules_s44 v43
   {-# INLINE rule273 #-}
   {-# LINE 469 "./src-ag/AG2AspectAG.ag" #-}
   rule273 = \  (_ :: ()) ->
                                                                                  {-# LINE 469 "./src-ag/AG2AspectAG.ag" #-}
                                                                                  []
                                                                                  {-# LINE 2830 "dist/build/AG2AspectAG.hs"#-}
   {-# INLINE rule274 #-}
   rule274 = \  (_ :: ()) ->
     []

-- TypeSig -----------------------------------------------------
-- wrapper
data Inh_TypeSig  = Inh_TypeSig {  }
data Syn_TypeSig  = Syn_TypeSig {  }
{-# INLINABLE wrap_TypeSig #-}
wrap_TypeSig :: T_TypeSig  -> Inh_TypeSig  -> (Syn_TypeSig )
wrap_TypeSig (T_TypeSig act) (Inh_TypeSig ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSig_vIn46 
        (T_TypeSig_vOut46 ) <- return (inv_TypeSig_s47 sem arg)
        return (Syn_TypeSig )
   )

-- cata
{-# INLINE sem_TypeSig #-}
sem_TypeSig :: TypeSig  -> T_TypeSig 
sem_TypeSig ( TypeSig name_ tp_ ) = sem_TypeSig_TypeSig name_ tp_

-- semantic domain
newtype T_TypeSig  = T_TypeSig {
                               attach_T_TypeSig :: Identity (T_TypeSig_s47 )
                               }
newtype T_TypeSig_s47  = C_TypeSig_s47 {
                                       inv_TypeSig_s47 :: (T_TypeSig_v46 )
                                       }
data T_TypeSig_s48  = C_TypeSig_s48
type T_TypeSig_v46  = (T_TypeSig_vIn46 ) -> (T_TypeSig_vOut46 )
data T_TypeSig_vIn46  = T_TypeSig_vIn46 
data T_TypeSig_vOut46  = T_TypeSig_vOut46 
{-# NOINLINE sem_TypeSig_TypeSig #-}
sem_TypeSig_TypeSig :: (Identifier) -> (Type) -> T_TypeSig 
sem_TypeSig_TypeSig _ _ = T_TypeSig (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_TypeSig_v46 
      v46 = \ (T_TypeSig_vIn46 ) -> ( let
         __result_ = T_TypeSig_vOut46 
         in __result_ )
     in C_TypeSig_s47 v46

-- TypeSigs ----------------------------------------------------
-- wrapper
data Inh_TypeSigs  = Inh_TypeSigs {  }
data Syn_TypeSigs  = Syn_TypeSigs {  }
{-# INLINABLE wrap_TypeSigs #-}
wrap_TypeSigs :: T_TypeSigs  -> Inh_TypeSigs  -> (Syn_TypeSigs )
wrap_TypeSigs (T_TypeSigs act) (Inh_TypeSigs ) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_TypeSigs_vIn49 
        (T_TypeSigs_vOut49 ) <- return (inv_TypeSigs_s50 sem arg)
        return (Syn_TypeSigs )
   )

-- cata
{-# NOINLINE sem_TypeSigs #-}
sem_TypeSigs :: TypeSigs  -> T_TypeSigs 
sem_TypeSigs list = Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list)

-- semantic domain
newtype T_TypeSigs  = T_TypeSigs {
                                 attach_T_TypeSigs :: Identity (T_TypeSigs_s50 )
                                 }
newtype T_TypeSigs_s50  = C_TypeSigs_s50 {
                                         inv_TypeSigs_s50 :: (T_TypeSigs_v49 )
                                         }
data T_TypeSigs_s51  = C_TypeSigs_s51
type T_TypeSigs_v49  = (T_TypeSigs_vIn49 ) -> (T_TypeSigs_vOut49 )
data T_TypeSigs_vIn49  = T_TypeSigs_vIn49 
data T_TypeSigs_vOut49  = T_TypeSigs_vOut49 
{-# NOINLINE sem_TypeSigs_Cons #-}
sem_TypeSigs_Cons :: T_TypeSig  -> T_TypeSigs  -> T_TypeSigs 
sem_TypeSigs_Cons arg_hd_ arg_tl_ = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ (T_TypeSigs_vIn49 ) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_TypeSig (arg_hd_))
         _tlX50 = Control.Monad.Identity.runIdentity (attach_T_TypeSigs (arg_tl_))
         (T_TypeSig_vOut46 ) = inv_TypeSig_s47 _hdX47 (T_TypeSig_vIn46 )
         (T_TypeSigs_vOut49 ) = inv_TypeSigs_s50 _tlX50 (T_TypeSigs_vIn49 )
         __result_ = T_TypeSigs_vOut49 
         in __result_ )
     in C_TypeSigs_s50 v49
{-# NOINLINE sem_TypeSigs_Nil #-}
sem_TypeSigs_Nil ::  T_TypeSigs 
sem_TypeSigs_Nil  = T_TypeSigs (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_TypeSigs_v49 
      v49 = \ (T_TypeSigs_vIn49 ) -> ( let
         __result_ = T_TypeSigs_vOut49 
         in __result_ )
     in C_TypeSigs_s50 v49
