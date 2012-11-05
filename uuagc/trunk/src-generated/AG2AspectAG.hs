

-- UUAGC 0.9.42.1 (src-ag/AG2AspectAG.ag)
module AG2AspectAG where
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
{-# LINE 25 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 37 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 44 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 50 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 56 "dist/build/AG2AspectAG.hs" #-}
{-# LINE 28 "./src-ag/AG2AspectAG.ag" #-}

pragmaAspectAG =  pp  "{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction , TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}"

{-# LINE 61 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 33 "./src-ag/AG2AspectAG.ag" #-}

ppName l = ppListSep "" "" "_" l
{-# LINE 66 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 70 "./src-ag/AG2AspectAG.ag" #-}

type FieldMap  = [(Identifier, Type)]
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap)
{-# LINE 72 "dist/build/AG2AspectAG.hs" #-}

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

{-# LINE 101 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 397 "./src-ag/AG2AspectAG.ag" #-}

ntsList att ppNtL = "nts_" ++ att ++ " = " >|<  ppListSep "" "" " .*. " ((map fst ppNtL) ++ [pp "hNil"])

filterNts att = filter ( Map.member (identifier att) . snd )
{-# LINE 108 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 455 "./src-ag/AG2AspectAG.ag" #-}

data PPRule = PPRule Identifier Identifier Bool ([(Identifier,Type)] -> [Identifier] -> PP_Doc)

ppRule (field,attr) owrt def = PPRule field attr owrt def
ruleField (PPRule field  _     _     _  ) = field
ruleAttr  (PPRule _      attr  _     _  ) = attr
ruleOwrt  (PPRule _      _     owrt  _  ) = owrt
ruleDef   (PPRule _      _     _     def) = def

{-# LINE 120 "dist/build/AG2AspectAG.hs" #-}

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

{-# LINE 345 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 721 "./src-ag/AG2AspectAG.ag" #-}

ppMacro (Macro con children) = "( atts_" >|< show con >|< ", " >|<  ppListSep "" "" " <.> " ppChildren  >|<")"
                where   ppChildren = map  ppChild children
                        ppChild (RuleChild  ch n) = chName ch >|< " ==> " >|< ppMacro n
                        ppChild (ChildChild ch n) = chName ch >|< " --> " >|< n
                        ppChild (ValueChild ch n) = chName ch >|< " ~~> " >|< n
                        chName ch = ppName [pp "ch", pp ch, pp con]
{-# LINE 355 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 754 "./src-ag/AG2AspectAG.ag" #-}

ppNoGroupAtts syn noGroup = let synatts = Map.keys $ Map.filterWithKey (\att _ -> elem (getName att) noGroup) syn
                            in  map (flip (>|<) "_inh") noGroup ++  map (flip (>|<) "_syn") synatts

ruleName att prodName = ppName [att,prodName]

elemNT a b = False
{-# LINE 365 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 797 "./src-ag/AG2AspectAG.ag" #-}

attTypes atts = map (\(a,t) -> "(HCons (LVPair (Proxy Att_" >|< a >|< ") " >|< ppShow t >|< ") ") $ Map.toAscList atts
{-# LINE 370 "dist/build/AG2AspectAG.hs" #-}

{-# LINE 851 "./src-ag/AG2AspectAG.ag" #-}

attVars atts = map (\(a,_) -> "_" >|< a >|< " ") $ Map.toAscList atts
attFields atts noGroup ppNt =
     let ng = map (\(a,_) -> attName (getName a) >|< " .=. _" >|< a >|< " .*. ") $ Map.toAscList noGroup
         g  = ppCommas $ map (\(a,_) -> ppName [pp a, pp "InhG",ppNt]  >|< "= _" >|< a) $ Map.toAscList $ Map.difference atts noGroup
     in "(" >|< ng >|< "att_inh .=. " >|< ppName [pp "InhG", ppNt] >|< " { " >|< g >|< " } .*. emptyRecord)"
{-# LINE 379 "dist/build/AG2AspectAG.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
         ppDL                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
         prdInh               : Attributes
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local ppCh        : _
            local ppTCh       : _
            local chName      : _
            local chLabel     : _
            local chTLabel    : _
            local chnt        : _
            local inh         : _
            local syn         : _
-}
-- cata
sem_Child :: Child ->
             T_Child
sem_Child (Child _name _tp _kind) =
    (sem_Child_Child _name _tp _kind)
-- semantic domain
newtype T_Child = T_Child ((Maybe String) ->
                           (Map Identifier Attributes) ->
                           ([String]) ->
                           ( Attributes ) ->
                           ([String]) ->
                           Bool ->
                           PP_Doc ->
                           PP_Doc ->
                           (Map Identifier Attributes) ->
                           ([String]) ->
                           ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,Attributes))
data Inh_Child = Inh_Child {ext_Inh_Child :: (Maybe String),inhMap_Inh_Child :: (Map Identifier Attributes),inhNoGroup_Inh_Child :: ([String]),newAtts_Inh_Child :: ( Attributes ),o_noGroup_Inh_Child :: ([String]),o_rename_Inh_Child :: Bool,ppNt_Inh_Child :: PP_Doc,ppProd_Inh_Child :: PP_Doc,synMap_Inh_Child :: (Map Identifier Attributes),synNoGroup_Inh_Child :: ([String])}
data Syn_Child = Syn_Child {idCL_Syn_Child :: ([(Identifier,Type)]),ppCSF_Syn_Child :: ([(Identifier,(PP_Doc,PP_Doc))]),ppDL_Syn_Child :: ([PP_Doc]),ppL_Syn_Child :: PP_Doc,ppLI_Syn_Child :: ([PP_Doc]),ppR_Syn_Child :: PP_Doc,prdInh_Syn_Child :: Attributes}
wrap_Child :: T_Child ->
              Inh_Child ->
              Syn_Child
wrap_Child (T_Child sem) (Inh_Child _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh) = sem _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup
     in  (Syn_Child _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child
sem_Child_Child name_ tp_ kind_ =
    (T_Child (\ _lhsIext
                _lhsIinhMap
                _lhsIinhNoGroup
                _lhsInewAtts
                _lhsIo_noGroup
                _lhsIo_rename
                _lhsIppNt
                _lhsIppProd
                _lhsIsynMap
                _lhsIsynNoGroup ->
                  (let _lhsOprdInh :: Attributes
                       _lhsOppDL :: ([PP_Doc])
                       _lhsOppL :: PP_Doc
                       _lhsOppLI :: ([PP_Doc])
                       _lhsOppR :: PP_Doc
                       _lhsOidCL :: ([(Identifier,Type)])
                       _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                       -- "./src-ag/AG2AspectAG.ag"(line 67, column 12)
                       _lhsOprdInh =
                           ({-# LINE 67 "./src-ag/AG2AspectAG.ag" #-}
                            _inh
                            {-# LINE 468 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 182, column 25)
                       _ppCh =
                           ({-# LINE 182 "./src-ag/AG2AspectAG.ag" #-}
                            pp name_
                            {-# LINE 474 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 183, column 25)
                       _ppTCh =
                           ({-# LINE 183 "./src-ag/AG2AspectAG.ag" #-}
                            ppShow tp_
                            {-# LINE 480 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 184, column 25)
                       _chName =
                           ({-# LINE 184 "./src-ag/AG2AspectAG.ag" #-}
                            ppName [_ppCh    , _lhsIppNt, _lhsIppProd]
                            {-# LINE 486 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 242, column 25)
                       _lhsOppDL =
                           ({-# LINE 242 "./src-ag/AG2AspectAG.ag" #-}
                            case kind_ of
                             ChildSyntax    ->  [ _chName      >|< pp " :: " >|< _ppTCh     ]
                             _              ->  []
                            {-# LINE 494 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 285, column 25)
                       _chLabel =
                           ({-# LINE 285 "./src-ag/AG2AspectAG.ag" #-}
                            "ch_" >|< _chName
                            {-# LINE 500 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 286, column 25)
                       _chTLabel =
                           ({-# LINE 286 "./src-ag/AG2AspectAG.ag" #-}
                            "Ch_" >|< _chName
                            {-# LINE 506 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 287, column 25)
                       _lhsOppL =
                           ({-# LINE 287 "./src-ag/AG2AspectAG.ag" #-}
                            "data " >|< _chTLabel     >|< "; " >|< _chLabel     >|< pp " = proxy :: " >|<
                            case kind_ of
                             ChildSyntax    ->  "Proxy " >|< "(" >|< _chTLabel     >|< ", " >|< _ppTCh     >|< ")"
                             _              ->  "SemType " >|< _ppTCh     >|< pp " nt =>  Proxy " >|<
                                                "(" >|< _chTLabel     >|< ", nt)"
                            {-# LINE 516 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 293, column 25)
                       _lhsOppLI =
                           ({-# LINE 293 "./src-ag/AG2AspectAG.ag" #-}
                            [ _chLabel    , _chTLabel     ]
                            {-# LINE 522 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 451, column 25)
                       _lhsOppR =
                           ({-# LINE 451 "./src-ag/AG2AspectAG.ag" #-}
                            let chName = ppListSep "" "" "_" [pp name_, _lhsIppNt, _lhsIppProd]
                            in  pp name_ >|< " <- at ch_" >|< chName
                            {-# LINE 529 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 489, column 25)
                       _lhsOidCL =
                           ({-# LINE 489 "./src-ag/AG2AspectAG.ag" #-}
                            [ (name_, removeDeforested tp_ ) ]
                            {-# LINE 535 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/AG2AspectAG.ag"(line 826, column 25)
                       _lhsOppCSF =
                           ({-# LINE 826 "./src-ag/AG2AspectAG.ag" #-}
                            let
                                 semC   = if (isNonterminal tp_)
                                           then "sem_" >|< ppShow tp_ >|<  " _" >|< name_
                                           else "sem_Lit _" >|< name_
                            in   case kind_ of
                                      ChildSyntax ->  [(name_, (  _chLabel     >|< " .=. (" >|< semC >|< ") .*. "
                                                              ,  _chLabel     >|< " .=. _" >|< name_ >|< " .*. "))]
                                      _           ->  []
                            {-# LINE 548 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "./src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier ""
                            {-# LINE 557 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "./src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 563 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- "./src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "./src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 569 "dist/build/AG2AspectAG.hs" #-}
                            )
                   in  ( _lhsOidCL,_lhsOppCSF,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))))
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
         ppDL                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
         prdInh               : Attributes
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
      alternative Nil:
-}
-- cata
sem_Children :: Children ->
                T_Children
sem_Children list =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list))
-- semantic domain
newtype T_Children = T_Children ((Maybe String) ->
                                 (Map Identifier Attributes) ->
                                 ([String]) ->
                                 ( Attributes ) ->
                                 ([String]) ->
                                 Bool ->
                                 PP_Doc ->
                                 PP_Doc ->
                                 (Map Identifier Attributes) ->
                                 ([String]) ->
                                 ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,Attributes))
data Inh_Children = Inh_Children {ext_Inh_Children :: (Maybe String),inhMap_Inh_Children :: (Map Identifier Attributes),inhNoGroup_Inh_Children :: ([String]),newAtts_Inh_Children :: ( Attributes ),o_noGroup_Inh_Children :: ([String]),o_rename_Inh_Children :: Bool,ppNt_Inh_Children :: PP_Doc,ppProd_Inh_Children :: PP_Doc,synMap_Inh_Children :: (Map Identifier Attributes),synNoGroup_Inh_Children :: ([String])}
data Syn_Children = Syn_Children {idCL_Syn_Children :: ([(Identifier,Type)]),ppCSF_Syn_Children :: ([(Identifier,(PP_Doc,PP_Doc))]),ppDL_Syn_Children :: ([PP_Doc]),ppL_Syn_Children :: PP_Doc,ppLI_Syn_Children :: ([PP_Doc]),ppR_Syn_Children :: PP_Doc,prdInh_Syn_Children :: Attributes}
wrap_Children :: T_Children ->
                 Inh_Children ->
                 Syn_Children
wrap_Children (T_Children sem) (Inh_Children _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup) =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh) = sem _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup
     in  (Syn_Children _lhsOidCL _lhsOppCSF _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh))
sem_Children_Cons :: T_Child ->
                     T_Children ->
                     T_Children
sem_Children_Cons (T_Child hd_) (T_Children tl_) =
    (T_Children (\ _lhsIext
                   _lhsIinhMap
                   _lhsIinhNoGroup
                   _lhsInewAtts
                   _lhsIo_noGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynMap
                   _lhsIsynNoGroup ->
                     (let _lhsOppDL :: ([PP_Doc])
                          _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          _lhsOprdInh :: Attributes
                          _hdOext :: (Maybe String)
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOinhNoGroup :: ([String])
                          _hdOnewAtts :: ( Attributes )
                          _hdOo_noGroup :: ([String])
                          _hdOo_rename :: Bool
                          _hdOppNt :: PP_Doc
                          _hdOppProd :: PP_Doc
                          _hdOsynMap :: (Map Identifier Attributes)
                          _hdOsynNoGroup :: ([String])
                          _tlOext :: (Maybe String)
                          _tlOinhMap :: (Map Identifier Attributes)
                          _tlOinhNoGroup :: ([String])
                          _tlOnewAtts :: ( Attributes )
                          _tlOo_noGroup :: ([String])
                          _tlOo_rename :: Bool
                          _tlOppNt :: PP_Doc
                          _tlOppProd :: PP_Doc
                          _tlOsynMap :: (Map Identifier Attributes)
                          _tlOsynNoGroup :: ([String])
                          _hdIidCL :: ([(Identifier,Type)])
                          _hdIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _hdIppDL :: ([PP_Doc])
                          _hdIppL :: PP_Doc
                          _hdIppLI :: ([PP_Doc])
                          _hdIppR :: PP_Doc
                          _hdIprdInh :: Attributes
                          _tlIidCL :: ([(Identifier,Type)])
                          _tlIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _tlIppDL :: ([PP_Doc])
                          _tlIppL :: PP_Doc
                          _tlIppLI :: ([PP_Doc])
                          _tlIppR :: PP_Doc
                          _tlIprdInh :: Attributes
                          -- "./src-ag/AG2AspectAG.ag"(line 238, column 33)
                          _lhsOppDL =
                              ({-# LINE 238 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIppDL ++ _tlIppDL
                               {-# LINE 684 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 487, column 31)
                          _lhsOidCL =
                              ({-# LINE 487 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIidCL ++ _tlIidCL
                               {-# LINE 690 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 822, column 34)
                          _lhsOppCSF =
                              ({-# LINE 822 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIppCSF ++ _tlIppCSF
                               {-# LINE 696 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                          _lhsOppL =
                              ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIppL >-< _tlIppL
                               {-# LINE 702 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                          _lhsOppLI =
                              ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIppLI ++ _tlIppLI
                               {-# LINE 708 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                          _lhsOppR =
                              ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIppR >-< _tlIppR
                               {-# LINE 714 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 65, column 57)
                          _lhsOprdInh =
                              ({-# LINE 65 "./src-ag/AG2AspectAG.ag" #-}
                               _hdIprdInh `Map.union` _tlIprdInh
                               {-# LINE 720 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOext =
                              ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 726 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 732 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhNoGroup =
                              ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 738 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnewAtts =
                              ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsInewAtts
                               {-# LINE 744 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_noGroup =
                              ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_noGroup
                               {-# LINE 750 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_rename =
                              ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 756 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppNt =
                              ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 762 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppProd =
                              ({-# LINE 192 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 768 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 774 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynNoGroup =
                              ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 780 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOext =
                              ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 786 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 792 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhNoGroup =
                              ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 798 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnewAtts =
                              ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsInewAtts
                               {-# LINE 804 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_noGroup =
                              ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_noGroup
                               {-# LINE 810 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_rename =
                              ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 816 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppNt =
                              ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 822 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppProd =
                              ({-# LINE 192 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 828 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 834 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynNoGroup =
                              ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 840 "dist/build/AG2AspectAG.hs" #-}
                               )
                          ( _hdIidCL,_hdIppCSF,_hdIppDL,_hdIppL,_hdIppLI,_hdIppR,_hdIprdInh) =
                              hd_ _hdOext _hdOinhMap _hdOinhNoGroup _hdOnewAtts _hdOo_noGroup _hdOo_rename _hdOppNt _hdOppProd _hdOsynMap _hdOsynNoGroup
                          ( _tlIidCL,_tlIppCSF,_tlIppDL,_tlIppL,_tlIppLI,_tlIppR,_tlIprdInh) =
                              tl_ _tlOext _tlOinhMap _tlOinhNoGroup _tlOnewAtts _tlOo_noGroup _tlOo_rename _tlOppNt _tlOppProd _tlOsynMap _tlOsynNoGroup
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))))
sem_Children_Nil :: T_Children
sem_Children_Nil =
    (T_Children (\ _lhsIext
                   _lhsIinhMap
                   _lhsIinhNoGroup
                   _lhsInewAtts
                   _lhsIo_noGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynMap
                   _lhsIsynNoGroup ->
                     (let _lhsOppDL :: ([PP_Doc])
                          _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          _lhsOprdInh :: Attributes
                          -- "./src-ag/AG2AspectAG.ag"(line 239, column 33)
                          _lhsOppDL =
                              ({-# LINE 239 "./src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 870 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 487, column 31)
                          _lhsOidCL =
                              ({-# LINE 487 "./src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 876 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 822, column 34)
                          _lhsOppCSF =
                              ({-# LINE 822 "./src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 882 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                          _lhsOppL =
                              ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 888 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                          _lhsOppLI =
                              ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 894 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                          _lhsOppR =
                              ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 900 "dist/build/AG2AspectAG.hs" #-}
                               )
                          -- use rule "./src-ag/AG2AspectAG.ag"(line 65, column 57)
                          _lhsOprdInh =
                              ({-# LINE 65 "./src-ag/AG2AspectAG.ag" #-}
                               Map.empty
                               {-# LINE 906 "dist/build/AG2AspectAG.hs" #-}
                               )
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
      synthesized attribute:
         ppRE                 : [String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (Expression _pos _tks) =
    (sem_Expression_Expression _pos _tks)
-- semantic domain
newtype T_Expression = T_Expression (PP_Doc ->
                                     PP_Doc ->
                                     ( ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)))
data Inh_Expression = Inh_Expression {ppNt_Inh_Expression :: PP_Doc,ppProd_Inh_Expression :: PP_Doc}
data Syn_Expression = Syn_Expression {ppRE_Syn_Expression :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression (T_Expression sem) (Inh_Expression _lhsIppNt _lhsIppProd) =
    (let ( _lhsOppRE) = sem _lhsIppNt _lhsIppProd
     in  (Syn_Expression _lhsOppRE))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression
sem_Expression_Expression pos_ tks_ =
    (T_Expression (\ _lhsIppNt
                     _lhsIppProd ->
                       (let _lhsOppRE :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
                            -- "./src-ag/AG2AspectAG.ag"(line 484, column 25)
                            _lhsOppRE =
                                ({-# LINE 484 "./src-ag/AG2AspectAG.ag" #-}
                                 rhsRule _lhsIppNt _lhsIppProd tks_
                                 {-# LINE 950 "dist/build/AG2AspectAG.hs" #-}
                                 )
                        in  ( _lhsOppRE))))
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         agi                  : (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))
         ext                  : Maybe String
         options              : Options
      synthesized attributes:
         imp                  : PP_Doc
         pp                   : PP_Doc
   alternatives:
      alternative Grammar:
         child typeSyns       : {TypeSyns}
         child useMap         : {UseMap}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : Nonterminals 
         child pragmas        : {PragmaMap}
         child manualAttrOrderMap : {AttrOrderMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child uniqueMap      : {UniqueMap}
         child augmentsMap    : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))}
         visit 0:
            local o_noGroup   : _
            local newAtts     : _
            local newProds    : _
            local ppA         : _
            local ppAI        : _
            local ppANT       : _
            local ppNtL       : _
            local ppR         : _
-}
-- cata
sem_Grammar :: Grammar ->
               T_Grammar
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap) =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap)
-- semantic domain
newtype T_Grammar = T_Grammar (((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) ->
                               (Maybe String) ->
                               Options ->
                               ( PP_Doc,PP_Doc))
data Inh_Grammar = Inh_Grammar {agi_Inh_Grammar :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))),ext_Inh_Grammar :: (Maybe String),options_Inh_Grammar :: Options}
data Syn_Grammar = Syn_Grammar {imp_Syn_Grammar :: PP_Doc,pp_Syn_Grammar :: PP_Doc}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar (T_Grammar sem) (Inh_Grammar _lhsIagi _lhsIext _lhsIoptions) =
    (let ( _lhsOimp,_lhsOpp) = sem _lhsIagi _lhsIext _lhsIoptions
     in  (Syn_Grammar _lhsOimp _lhsOpp))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals ->
                       PragmaMap ->
                       AttrOrderMap ->
                       ParamMap ->
                       ContextMap ->
                       QuantMap ->
                       UniqueMap ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                       T_Grammar
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_ =
    (T_Grammar (\ _lhsIagi
                  _lhsIext
                  _lhsIoptions ->
                    (let _nontsOo_rename :: Bool
                         _nontsOo_noGroup :: ([String])
                         _nontsOnewAtts :: ( Attributes )
                         _nontsOnewProds :: ( DataTypes )
                         _nontsOnewNTs :: (Set NontermIdent)
                         _lhsOimp :: PP_Doc
                         _lhsOpp :: PP_Doc
                         _nontsOderivs :: Derivings
                         _nontsOtSyns :: TypeSyns
                         _nontsOinhMap :: (Map Identifier Attributes)
                         _nontsOsynMap :: (Map Identifier Attributes)
                         _nontsOext :: (Maybe String)
                         _nontsIextendedNTs :: (Set NontermIdent)
                         _nontsIinhMap' :: (Map Identifier Attributes)
                         _nontsIppA :: PP_Doc
                         _nontsIppAI :: ([PP_Doc])
                         _nontsIppCata :: PP_Doc
                         _nontsIppD :: PP_Doc
                         _nontsIppDI :: ([PP_Doc])
                         _nontsIppL :: PP_Doc
                         _nontsIppLI :: ([PP_Doc])
                         _nontsIppNtL :: ([(PP_Doc, Attributes)])
                         _nontsIppR :: PP_Doc
                         _nontsIppSF :: PP_Doc
                         _nontsIppW :: PP_Doc
                         _nontsIsynMap' :: (Map Identifier Attributes)
                         -- "./src-ag/AG2AspectAG.ag"(line 43, column 14)
                         _nontsOo_rename =
                             ({-# LINE 43 "./src-ag/AG2AspectAG.ag" #-}
                              rename    _lhsIoptions
                              {-# LINE 1056 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 47, column 14)
                         _o_noGroup =
                             ({-# LINE 47 "./src-ag/AG2AspectAG.ag" #-}
                              sort $ noGroup    _lhsIoptions
                              {-# LINE 1062 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 48, column 14)
                         _nontsOo_noGroup =
                             ({-# LINE 48 "./src-ag/AG2AspectAG.ag" #-}
                              _o_noGroup
                              {-# LINE 1068 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 80, column 23)
                         _newAtts =
                             ({-# LINE 80 "./src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (_,_,atts) -> ( Map.unions . (\(a,b) -> a++b) . unzip . Map.elems) atts
                              {-# LINE 1075 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 82, column 23)
                         _nontsOnewAtts =
                             ({-# LINE 82 "./src-ag/AG2AspectAG.ag" #-}
                              _newAtts
                              {-# LINE 1081 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 88, column 23)
                         _newProds =
                             ({-# LINE 88 "./src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                     (_,prods,_) -> prods
                              {-# LINE 1088 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 90, column 23)
                         _nontsOnewProds =
                             ({-# LINE 90 "./src-ag/AG2AspectAG.ag" #-}
                              _newProds
                              {-# LINE 1094 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 112, column 23)
                         _nontsOnewNTs =
                             ({-# LINE 112 "./src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (newNTs,_,_) -> Set.difference newNTs _nontsIextendedNTs
                              {-# LINE 1101 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 127, column 25)
                         _lhsOimp =
                             ({-# LINE 127 "./src-ag/AG2AspectAG.ag" #-}
                              "import Language.Grammars.AspectAG" >-<
                              "import Language.Grammars.AspectAG.Derive" >-<
                              "import Data.HList.Label4" >-<
                              "import Data.HList.TypeEqGeneric1" >-<
                              "import Data.HList.TypeCastGeneric1" >-<
                              maybe empty ("import qualified" >#<) _lhsIext >-<
                              maybe empty (\ext -> "import" >#< ext >#< ppListSep "(" ")" "," (_nontsIppDI ++ _nontsIppLI ++ _ppAI     ++ _ppANT    )) _lhsIext
                              {-# LINE 1113 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 140, column 25)
                         _lhsOpp =
                             ({-# LINE 140 "./src-ag/AG2AspectAG.ag" #-}
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
                              {-# LINE 1136 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 202, column 25)
                         _nontsOderivs =
                             ({-# LINE 202 "./src-ag/AG2AspectAG.ag" #-}
                              derivings_
                              {-# LINE 1142 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 251, column 34)
                         _nontsOtSyns =
                             ({-# LINE 251 "./src-ag/AG2AspectAG.ag" #-}
                              typeSyns_
                              {-# LINE 1148 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 300, column 25)
                         _ppA =
                             ({-# LINE 300 "./src-ag/AG2AspectAG.ag" #-}
                              vlist (map defAtt (filterAtts _newAtts     _o_noGroup    )) >-<
                              defAtt "loc" >-<
                              (case _lhsIext of
                                Nothing    ->  defAtt "inh" >-< defAtt "syn"
                                otherwise  ->  empty) >-<
                              _nontsIppA
                              {-# LINE 1159 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 307, column 25)
                         _ppAI =
                             ({-# LINE 307 "./src-ag/AG2AspectAG.ag" #-}
                              let atts =  filterNotAtts _newAtts     _o_noGroup
                              in  (foldr (\a as -> attName a : as) [] atts) ++
                                  (foldr (\a as -> attTName a : as) [] atts) ++
                                  (case _lhsIext of
                                    Nothing    ->  []
                                    otherwise  ->  [ attName "inh", attName "syn", attTName "inh", attTName "syn" ]) ++
                                  _nontsIppAI
                              {-# LINE 1171 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 317, column 25)
                         _ppANT =
                             ({-# LINE 317 "./src-ag/AG2AspectAG.ag" #-}
                              let atts =  filterNotAtts _newAtts     _o_noGroup
                              in  (foldr (\a as -> ("nts_" >|< a) : as) [] atts)
                              {-# LINE 1178 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 392, column 25)
                         _ppNtL =
                             ({-# LINE 392 "./src-ag/AG2AspectAG.ag" #-}
                              _nontsIppNtL
                              {-# LINE 1184 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/AG2AspectAG.ag"(line 393, column 25)
                         _ppR =
                             ({-# LINE 393 "./src-ag/AG2AspectAG.ag" #-}
                              ntsList "group" _ppNtL      >-<
                              vlist (map (\att -> ntsList att (filterNts att _ppNtL    )) (filterAtts _newAtts _o_noGroup    ))  >-<
                              _nontsIppR
                              {-# LINE 1192 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/DistChildAttr.ag"(line 15, column 13)
                         _nontsOinhMap =
                             ({-# LINE 15 "./src-ag/DistChildAttr.ag" #-}
                              _nontsIinhMap'
                              {-# LINE 1198 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- "./src-ag/DistChildAttr.ag"(line 16, column 13)
                         _nontsOsynMap =
                             ({-# LINE 16 "./src-ag/DistChildAttr.ag" #-}
                              _nontsIsynMap'
                              {-# LINE 1204 "dist/build/AG2AspectAG.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOext =
                             ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                              _lhsIext
                              {-# LINE 1210 "dist/build/AG2AspectAG.hs" #-}
                              )
                         ( _nontsIextendedNTs,_nontsIinhMap',_nontsIppA,_nontsIppAI,_nontsIppCata,_nontsIppD,_nontsIppDI,_nontsIppL,_nontsIppLI,_nontsIppNtL,_nontsIppR,_nontsIppSF,_nontsIppW,_nontsIsynMap') =
                             nonts_ _nontsOderivs _nontsOext _nontsOinhMap _nontsOnewAtts _nontsOnewNTs _nontsOnewProds _nontsOo_noGroup _nontsOo_rename _nontsOsynMap _nontsOtSyns
                     in  ( _lhsOimp,_lhsOpp))))
-- HsToken -----------------------------------------------------
{-
   alternatives:
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken ->
               T_HsToken
sem_HsToken (AGLocal _var _pos _rdesc) =
    (sem_HsToken_AGLocal _var _pos _rdesc)
sem_HsToken (AGField _field _attr _pos _rdesc) =
    (sem_HsToken_AGField _field _attr _pos _rdesc)
sem_HsToken (HsToken _value _pos) =
    (sem_HsToken_HsToken _value _pos)
sem_HsToken (CharToken _value _pos) =
    (sem_HsToken_CharToken _value _pos)
sem_HsToken (StrToken _value _pos) =
    (sem_HsToken_StrToken _value _pos)
sem_HsToken (Err _mesg _pos) =
    (sem_HsToken_Err _mesg _pos)
-- semantic domain
newtype T_HsToken = T_HsToken (( ))
data Inh_HsToken = Inh_HsToken {}
data Syn_HsToken = Syn_HsToken {}
wrap_HsToken :: T_HsToken ->
                Inh_HsToken ->
                Syn_HsToken
wrap_HsToken (T_HsToken sem) (Inh_HsToken) =
    (let ( ) = sem
     in  (Syn_HsToken))
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGLocal var_ pos_ rdesc_ =
    (T_HsToken (let
                in  ( )))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGField field_ attr_ pos_ rdesc_ =
    (T_HsToken (let
                in  ( )))
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken
sem_HsToken_HsToken value_ pos_ =
    (T_HsToken (let
                in  ( )))
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken
sem_HsToken_CharToken value_ pos_ =
    (T_HsToken (let
                in  ( )))
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken
sem_HsToken_StrToken value_ pos_ =
    (T_HsToken (let
                in  ( )))
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken
sem_HsToken_Err mesg_ pos_ =
    (T_HsToken (let
                in  ( )))
-- HsTokens ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
-- cata
sem_HsTokens :: HsTokens ->
                T_HsTokens
sem_HsTokens list =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list))
-- semantic domain
newtype T_HsTokens = T_HsTokens (( ))
data Inh_HsTokens = Inh_HsTokens {}
data Syn_HsTokens = Syn_HsTokens {}
wrap_HsTokens :: T_HsTokens ->
                 Inh_HsTokens ->
                 Syn_HsTokens
wrap_HsTokens (T_HsTokens sem) (Inh_HsTokens) =
    (let ( ) = sem
     in  (Syn_HsTokens))
sem_HsTokens_Cons :: T_HsToken ->
                     T_HsTokens ->
                     T_HsTokens
sem_HsTokens_Cons (T_HsToken hd_) (T_HsTokens tl_) =
    (T_HsTokens (let
                 in  ( )))
sem_HsTokens_Nil :: T_HsTokens
sem_HsTokens_Nil =
    (T_HsTokens (let
                 in  ( )))
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot ->
                    T_HsTokensRoot
sem_HsTokensRoot (HsTokensRoot _tokens) =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens))
-- semantic domain
newtype T_HsTokensRoot = T_HsTokensRoot (( ))
data Inh_HsTokensRoot = Inh_HsTokensRoot {}
data Syn_HsTokensRoot = Syn_HsTokensRoot {}
wrap_HsTokensRoot :: T_HsTokensRoot ->
                     Inh_HsTokensRoot ->
                     Syn_HsTokensRoot
wrap_HsTokensRoot (T_HsTokensRoot sem) (Inh_HsTokensRoot) =
    (let ( ) = sem
     in  (Syn_HsTokensRoot))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens ->
                                 T_HsTokensRoot
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_) =
    (T_HsTokensRoot (let
                     in  ( )))
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         derivs               : Derivings
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         newAtts              :  Attributes 
         newNTs               : Set NontermIdent
         newProds             :  DataTypes 
         o_noGroup            : [String]
         o_rename             : Bool
         synMap               : Map Identifier Attributes
         tSyns                : TypeSyns
      synthesized attributes:
         extendedNTs          : Set NontermIdent
         inhMap'              : Map Identifier Attributes
         ppA                  : PP_Doc
         ppAI                 : [PP_Doc]
         ppCata               : PP_Doc
         ppD                  : PP_Doc
         ppDI                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppNtL                : [(PP_Doc, Attributes)]
         ppR                  : PP_Doc
         ppSF                 : PP_Doc
         ppW                  : PP_Doc
         synMap'              : Map Identifier Attributes
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 0:
            local inhNoGroup  : _
            local synNoGroup  : _
            local ppNt        : _
            local ntLabel     : _
-}
-- cata
sem_Nonterminal :: Nonterminal ->
                   T_Nonterminal
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods) =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods))
-- semantic domain
newtype T_Nonterminal = T_Nonterminal (Derivings ->
                                       (Maybe String) ->
                                       (Map Identifier Attributes) ->
                                       ( Attributes ) ->
                                       (Set NontermIdent) ->
                                       ( DataTypes ) ->
                                       ([String]) ->
                                       Bool ->
                                       (Map Identifier Attributes) ->
                                       TypeSyns ->
                                       ( (Set NontermIdent),(Map Identifier Attributes),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),([(PP_Doc, Attributes)]),PP_Doc,PP_Doc,PP_Doc,(Map Identifier Attributes)))
data Inh_Nonterminal = Inh_Nonterminal {derivs_Inh_Nonterminal :: Derivings,ext_Inh_Nonterminal :: (Maybe String),inhMap_Inh_Nonterminal :: (Map Identifier Attributes),newAtts_Inh_Nonterminal :: ( Attributes ),newNTs_Inh_Nonterminal :: (Set NontermIdent),newProds_Inh_Nonterminal :: ( DataTypes ),o_noGroup_Inh_Nonterminal :: ([String]),o_rename_Inh_Nonterminal :: Bool,synMap_Inh_Nonterminal :: (Map Identifier Attributes),tSyns_Inh_Nonterminal :: TypeSyns}
data Syn_Nonterminal = Syn_Nonterminal {extendedNTs_Syn_Nonterminal :: (Set NontermIdent),inhMap'_Syn_Nonterminal :: (Map Identifier Attributes),ppA_Syn_Nonterminal :: PP_Doc,ppAI_Syn_Nonterminal :: ([PP_Doc]),ppCata_Syn_Nonterminal :: PP_Doc,ppD_Syn_Nonterminal :: PP_Doc,ppDI_Syn_Nonterminal :: ([PP_Doc]),ppL_Syn_Nonterminal :: PP_Doc,ppLI_Syn_Nonterminal :: ([PP_Doc]),ppNtL_Syn_Nonterminal :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminal :: PP_Doc,ppSF_Syn_Nonterminal :: PP_Doc,ppW_Syn_Nonterminal :: PP_Doc,synMap'_Syn_Nonterminal :: (Map Identifier Attributes)}
wrap_Nonterminal :: T_Nonterminal ->
                    Inh_Nonterminal ->
                    Syn_Nonterminal
wrap_Nonterminal (T_Nonterminal sem) (Inh_Nonterminal _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) =
    (let ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap') = sem _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns
     in  (Syn_Nonterminal _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap'))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions ->
                               T_Nonterminal
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_) =
    (T_Nonterminal (\ _lhsIderivs
                      _lhsIext
                      _lhsIinhMap
                      _lhsInewAtts
                      _lhsInewNTs
                      _lhsInewProds
                      _lhsIo_noGroup
                      _lhsIo_rename
                      _lhsIsynMap
                      _lhsItSyns ->
                        (let _prodsOinhNoGroup :: ([String])
                             _prodsOsynNoGroup :: ([String])
                             _prodsOnewProds :: ( Map.Map ConstructorIdent FieldMap )
                             _lhsOextendedNTs :: (Set NontermIdent)
                             _prodsOppNt :: PP_Doc
                             _lhsOppD :: PP_Doc
                             _lhsOppDI :: ([PP_Doc])
                             _lhsOppL :: PP_Doc
                             _lhsOppLI :: ([PP_Doc])
                             _lhsOppA :: PP_Doc
                             _lhsOppAI :: ([PP_Doc])
                             _lhsOppNtL :: ([(PP_Doc, Attributes)])
                             _prodsOnewNT :: Bool
                             _lhsOppR :: PP_Doc
                             _lhsOppCata :: PP_Doc
                             _prodsOsyn :: ( Attributes )
                             _prodsOinh :: ( Attributes )
                             _lhsOppSF :: PP_Doc
                             _lhsOppW :: PP_Doc
                             _lhsOinhMap' :: (Map Identifier Attributes)
                             _lhsOsynMap' :: (Map Identifier Attributes)
                             _prodsOext :: (Maybe String)
                             _prodsOinhMap :: (Map Identifier Attributes)
                             _prodsOnewAtts :: ( Attributes )
                             _prodsOo_noGroup :: ([String])
                             _prodsOo_rename :: Bool
                             _prodsOsynMap :: (Map Identifier Attributes)
                             _prodsIhasMoreProds :: ( Bool )
                             _prodsIppA :: PP_Doc
                             _prodsIppCata :: PP_Doc
                             _prodsIppDL :: ([PP_Doc])
                             _prodsIppL :: PP_Doc
                             _prodsIppLI :: ([PP_Doc])
                             _prodsIppR :: PP_Doc
                             _prodsIppRA :: ([PP_Doc])
                             _prodsIppSF :: PP_Doc
                             _prodsIppSPF :: PP_Doc
                             _prodsIprdInh :: Attributes
                             -- "./src-ag/AG2AspectAG.ag"(line 51, column 18)
                             _inhNoGroup =
                                 ({-# LINE 51 "./src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) _prodsIprdInh
                                  {-# LINE 1489 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 52, column 18)
                             _synNoGroup =
                                 ({-# LINE 52 "./src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) syn_
                                  {-# LINE 1495 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 57, column 18)
                             _prodsOinhNoGroup =
                                 ({-# LINE 57 "./src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _inhNoGroup
                                  {-# LINE 1501 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 58, column 18)
                             _prodsOsynNoGroup =
                                 ({-# LINE 58 "./src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _synNoGroup
                                  {-# LINE 1507 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 94, column 17)
                             _prodsOnewProds =
                                 ({-# LINE 94 "./src-ag/AG2AspectAG.ag" #-}
                                  case Map.lookup nt_ _lhsInewProds of
                                         Just prds -> prds
                                         Nothing   -> Map.empty
                                  {-# LINE 1515 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 107, column 31)
                             _lhsOextendedNTs =
                                 ({-# LINE 107 "./src-ag/AG2AspectAG.ag" #-}
                                  if _prodsIhasMoreProds
                                  then Set.singleton nt_
                                  else Set.empty
                                  {-# LINE 1523 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 173, column 25)
                             _ppNt =
                                 ({-# LINE 173 "./src-ag/AG2AspectAG.ag" #-}
                                  pp nt_
                                  {-# LINE 1529 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 190, column 25)
                             _prodsOppNt =
                                 ({-# LINE 190 "./src-ag/AG2AspectAG.ag" #-}
                                  _ppNt
                                  {-# LINE 1535 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 208, column 25)
                             _lhsOppD =
                                 ({-# LINE 208 "./src-ag/AG2AspectAG.ag" #-}
                                  if (Set.member nt_ _lhsInewNTs)
                                  then  case (lookup nt_ _lhsItSyns) of
                                                 Nothing ->  "data " >|< _ppNt
                                                              >|< " = " >|< vlist_sep " | " _prodsIppDL >-<
                                                             case (Map.lookup nt_ _lhsIderivs) of
                                                              Just ntds -> pp "  deriving " >|<  (ppListSep "(" ")" ", " $ Set.elems ntds)
                                                              Nothing   -> empty
                                                 Just tp ->  "type " >|< _ppNt     >|< " = " >|< ppShow tp
                                  else  empty
                                  {-# LINE 1549 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 221, column 25)
                             _lhsOppDI =
                                 ({-# LINE 221 "./src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then  [ _ppNt     ]
                                  else  [ ]
                                  {-# LINE 1557 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 262, column 25)
                             _ntLabel =
                                 ({-# LINE 262 "./src-ag/AG2AspectAG.ag" #-}
                                  "nt_" >|< _ppNt
                                  {-# LINE 1563 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 264, column 25)
                             _lhsOppL =
                                 ({-# LINE 264 "./src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then _ntLabel     >|< " = proxy :: Proxy " >|< _ppNt
                                    else empty)  >-<
                                  _prodsIppL
                                  {-# LINE 1572 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 269, column 25)
                             _lhsOppLI =
                                 ({-# LINE 269 "./src-ag/AG2AspectAG.ag" #-}
                                  ( if (not $ Set.member nt_ _lhsInewNTs)
                                    then [ _ntLabel     ]
                                    else [ ])  ++
                                  _prodsIppLI
                                  {-# LINE 1581 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 324, column 25)
                             _lhsOppA =
                                 ({-# LINE 324 "./src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then
                                           defAttRec (pp "InhG") _ppNt     inh_ _inhNoGroup     >-<
                                           defAttRec (pp "SynG") _ppNt     syn_ _synNoGroup
                                    else   empty) >-<
                                  _prodsIppA
                                  {-# LINE 1592 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 338, column 25)
                             _lhsOppAI =
                                 ({-# LINE 338 "./src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then [ ppName [(pp "InhG"), _ppNt     ] >#< pp "(..)", ppName [(pp "SynG"), _ppNt     ] >#< pp "(..)" ]
                                  else [ ]
                                  {-# LINE 1600 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 406, column 25)
                             _lhsOppNtL =
                                 ({-# LINE 406 "./src-ag/AG2AspectAG.ag" #-}
                                  [ ("nt_" >|< nt_, Map.union inh_ syn_) ]
                                  {-# LINE 1606 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 415, column 25)
                             _prodsOnewNT =
                                 ({-# LINE 415 "./src-ag/AG2AspectAG.ag" #-}
                                  Set.member nt_ _lhsInewNTs
                                  {-# LINE 1612 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 425, column 25)
                             _lhsOppR =
                                 ({-# LINE 425 "./src-ag/AG2AspectAG.ag" #-}
                                  pp "----" >|< pp nt_ >-< _prodsIppR
                                  {-# LINE 1618 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 735, column 25)
                             _lhsOppCata =
                                 ({-# LINE 735 "./src-ag/AG2AspectAG.ag" #-}
                                  "----" >|< _ppNt     >-< _prodsIppCata
                                  {-# LINE 1624 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 766, column 25)
                             _prodsOsyn =
                                 ({-# LINE 766 "./src-ag/AG2AspectAG.ag" #-}
                                  syn_
                                  {-# LINE 1630 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 767, column 25)
                             _prodsOinh =
                                 ({-# LINE 767 "./src-ag/AG2AspectAG.ag" #-}
                                  inh_
                                  {-# LINE 1636 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 778, column 25)
                             _lhsOppSF =
                                 ({-# LINE 778 "./src-ag/AG2AspectAG.ag" #-}
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
                                  {-# LINE 1657 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 846, column 25)
                             _lhsOppW =
                                 ({-# LINE 846 "./src-ag/AG2AspectAG.ag" #-}
                                  ppName [pp "wrap", _ppNt    ] >|< " sem " >|< attVars inh_ >|< " = " >-<
                                  "   sem " >|< attFields inh_ _inhNoGroup     _ppNt
                                  {-# LINE 1664 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "./src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 1670 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "./src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 1676 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOext =
                                 ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 1682 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1688 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOnewAtts =
                                 ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 1694 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_noGroup =
                                 ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 1700 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_rename =
                                 ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 1706 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1712 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             ( _prodsIhasMoreProds,_prodsIppA,_prodsIppCata,_prodsIppDL,_prodsIppL,_prodsIppLI,_prodsIppR,_prodsIppRA,_prodsIppSF,_prodsIppSPF,_prodsIprdInh) =
                                 prods_ _prodsOext _prodsOinh _prodsOinhMap _prodsOinhNoGroup _prodsOnewAtts _prodsOnewNT _prodsOnewProds _prodsOo_noGroup _prodsOo_rename _prodsOppNt _prodsOsyn _prodsOsynMap _prodsOsynNoGroup
                         in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))))
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         derivs               : Derivings
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         newAtts              :  Attributes 
         newNTs               : Set NontermIdent
         newProds             :  DataTypes 
         o_noGroup            : [String]
         o_rename             : Bool
         synMap               : Map Identifier Attributes
         tSyns                : TypeSyns
      synthesized attributes:
         extendedNTs          : Set NontermIdent
         inhMap'              : Map Identifier Attributes
         ppA                  : PP_Doc
         ppAI                 : [PP_Doc]
         ppCata               : PP_Doc
         ppD                  : PP_Doc
         ppDI                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppNtL                : [(PP_Doc, Attributes)]
         ppR                  : PP_Doc
         ppSF                 : PP_Doc
         ppW                  : PP_Doc
         synMap'              : Map Identifier Attributes
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
      alternative Nil:
-}
-- cata
sem_Nonterminals :: Nonterminals ->
                    T_Nonterminals
sem_Nonterminals list =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list))
-- semantic domain
newtype T_Nonterminals = T_Nonterminals (Derivings ->
                                         (Maybe String) ->
                                         (Map Identifier Attributes) ->
                                         ( Attributes ) ->
                                         (Set NontermIdent) ->
                                         ( DataTypes ) ->
                                         ([String]) ->
                                         Bool ->
                                         (Map Identifier Attributes) ->
                                         TypeSyns ->
                                         ( (Set NontermIdent),(Map Identifier Attributes),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),([(PP_Doc, Attributes)]),PP_Doc,PP_Doc,PP_Doc,(Map Identifier Attributes)))
data Inh_Nonterminals = Inh_Nonterminals {derivs_Inh_Nonterminals :: Derivings,ext_Inh_Nonterminals :: (Maybe String),inhMap_Inh_Nonterminals :: (Map Identifier Attributes),newAtts_Inh_Nonterminals :: ( Attributes ),newNTs_Inh_Nonterminals :: (Set NontermIdent),newProds_Inh_Nonterminals :: ( DataTypes ),o_noGroup_Inh_Nonterminals :: ([String]),o_rename_Inh_Nonterminals :: Bool,synMap_Inh_Nonterminals :: (Map Identifier Attributes),tSyns_Inh_Nonterminals :: TypeSyns}
data Syn_Nonterminals = Syn_Nonterminals {extendedNTs_Syn_Nonterminals :: (Set NontermIdent),inhMap'_Syn_Nonterminals :: (Map Identifier Attributes),ppA_Syn_Nonterminals :: PP_Doc,ppAI_Syn_Nonterminals :: ([PP_Doc]),ppCata_Syn_Nonterminals :: PP_Doc,ppD_Syn_Nonterminals :: PP_Doc,ppDI_Syn_Nonterminals :: ([PP_Doc]),ppL_Syn_Nonterminals :: PP_Doc,ppLI_Syn_Nonterminals :: ([PP_Doc]),ppNtL_Syn_Nonterminals :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminals :: PP_Doc,ppSF_Syn_Nonterminals :: PP_Doc,ppW_Syn_Nonterminals :: PP_Doc,synMap'_Syn_Nonterminals :: (Map Identifier Attributes)}
wrap_Nonterminals :: T_Nonterminals ->
                     Inh_Nonterminals ->
                     Syn_Nonterminals
wrap_Nonterminals (T_Nonterminals sem) (Inh_Nonterminals _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns) =
    (let ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap') = sem _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns
     in  (Syn_Nonterminals _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap'))
sem_Nonterminals_Cons :: T_Nonterminal ->
                         T_Nonterminals ->
                         T_Nonterminals
sem_Nonterminals_Cons (T_Nonterminal hd_) (T_Nonterminals tl_) =
    (T_Nonterminals (\ _lhsIderivs
                       _lhsIext
                       _lhsIinhMap
                       _lhsInewAtts
                       _lhsInewNTs
                       _lhsInewProds
                       _lhsIo_noGroup
                       _lhsIo_rename
                       _lhsIsynMap
                       _lhsItSyns ->
                         (let _lhsOextendedNTs :: (Set NontermIdent)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOppA :: PP_Doc
                              _lhsOppAI :: ([PP_Doc])
                              _lhsOppCata :: PP_Doc
                              _lhsOppD :: PP_Doc
                              _lhsOppDI :: ([PP_Doc])
                              _lhsOppL :: PP_Doc
                              _lhsOppLI :: ([PP_Doc])
                              _lhsOppNtL :: ([(PP_Doc, Attributes)])
                              _lhsOppR :: PP_Doc
                              _lhsOppSF :: PP_Doc
                              _lhsOppW :: PP_Doc
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              _hdOderivs :: Derivings
                              _hdOext :: (Maybe String)
                              _hdOinhMap :: (Map Identifier Attributes)
                              _hdOnewAtts :: ( Attributes )
                              _hdOnewNTs :: (Set NontermIdent)
                              _hdOnewProds :: ( DataTypes )
                              _hdOo_noGroup :: ([String])
                              _hdOo_rename :: Bool
                              _hdOsynMap :: (Map Identifier Attributes)
                              _hdOtSyns :: TypeSyns
                              _tlOderivs :: Derivings
                              _tlOext :: (Maybe String)
                              _tlOinhMap :: (Map Identifier Attributes)
                              _tlOnewAtts :: ( Attributes )
                              _tlOnewNTs :: (Set NontermIdent)
                              _tlOnewProds :: ( DataTypes )
                              _tlOo_noGroup :: ([String])
                              _tlOo_rename :: Bool
                              _tlOsynMap :: (Map Identifier Attributes)
                              _tlOtSyns :: TypeSyns
                              _hdIextendedNTs :: (Set NontermIdent)
                              _hdIinhMap' :: (Map Identifier Attributes)
                              _hdIppA :: PP_Doc
                              _hdIppAI :: ([PP_Doc])
                              _hdIppCata :: PP_Doc
                              _hdIppD :: PP_Doc
                              _hdIppDI :: ([PP_Doc])
                              _hdIppL :: PP_Doc
                              _hdIppLI :: ([PP_Doc])
                              _hdIppNtL :: ([(PP_Doc, Attributes)])
                              _hdIppR :: PP_Doc
                              _hdIppSF :: PP_Doc
                              _hdIppW :: PP_Doc
                              _hdIsynMap' :: (Map Identifier Attributes)
                              _tlIextendedNTs :: (Set NontermIdent)
                              _tlIinhMap' :: (Map Identifier Attributes)
                              _tlIppA :: PP_Doc
                              _tlIppAI :: ([PP_Doc])
                              _tlIppCata :: PP_Doc
                              _tlIppD :: PP_Doc
                              _tlIppDI :: ([PP_Doc])
                              _tlIppL :: PP_Doc
                              _tlIppLI :: ([PP_Doc])
                              _tlIppNtL :: ([(PP_Doc, Attributes)])
                              _tlIppR :: PP_Doc
                              _tlIppSF :: PP_Doc
                              _tlIppW :: PP_Doc
                              _tlIsynMap' :: (Map Identifier Attributes)
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 105, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 105 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIextendedNTs `Set.union` _tlIextendedNTs
                                   {-# LINE 1857 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "./src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 1863 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 321, column 64)
                              _lhsOppA =
                                  ({-# LINE 321 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppA >-< _tlIppA
                                   {-# LINE 1869 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 335, column 42)
                              _lhsOppAI =
                                  ({-# LINE 335 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppAI ++ _tlIppAI
                                   {-# LINE 1875 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 732, column 67)
                              _lhsOppCata =
                                  ({-# LINE 732 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppCata >-< _tlIppCata
                                   {-# LINE 1881 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 205, column 52)
                              _lhsOppD =
                                  ({-# LINE 205 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppD >-< _tlIppD
                                   {-# LINE 1887 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 205, column 86)
                              _lhsOppDI =
                                  ({-# LINE 205 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppDI ++ _tlIppDI
                                   {-# LINE 1893 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                              _lhsOppL =
                                  ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppL >-< _tlIppL
                                   {-# LINE 1899 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                              _lhsOppLI =
                                  ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppLI ++ _tlIppLI
                                   {-# LINE 1905 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 403, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 403 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppNtL ++ _tlIppNtL
                                   {-# LINE 1911 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                              _lhsOppR =
                                  ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppR >-< _tlIppR
                                   {-# LINE 1917 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 773, column 66)
                              _lhsOppSF =
                                  ({-# LINE 773 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppSF >-< _tlIppSF
                                   {-# LINE 1923 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 843, column 42)
                              _lhsOppW =
                                  ({-# LINE 843 "./src-ag/AG2AspectAG.ag" #-}
                                   _hdIppW >-< _tlIppW
                                   {-# LINE 1929 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "./src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 1935 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOderivs =
                                  ({-# LINE 199 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 1941 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOext =
                                  ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 1947 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1953 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewAtts =
                                  ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 1959 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewNTs =
                                  ({-# LINE 99 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 1965 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewProds =
                                  ({-# LINE 85 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 1971 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_noGroup =
                                  ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 1977 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_rename =
                                  ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1983 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1989 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOtSyns =
                                  ({-# LINE 248 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 1995 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOderivs =
                                  ({-# LINE 199 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 2001 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOext =
                                  ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 2007 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 2013 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewAtts =
                                  ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 2019 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewNTs =
                                  ({-# LINE 99 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 2025 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewProds =
                                  ({-# LINE 85 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 2031 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_noGroup =
                                  ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 2037 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_rename =
                                  ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 2043 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 2049 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOtSyns =
                                  ({-# LINE 248 "./src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 2055 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              ( _hdIextendedNTs,_hdIinhMap',_hdIppA,_hdIppAI,_hdIppCata,_hdIppD,_hdIppDI,_hdIppL,_hdIppLI,_hdIppNtL,_hdIppR,_hdIppSF,_hdIppW,_hdIsynMap') =
                                  hd_ _hdOderivs _hdOext _hdOinhMap _hdOnewAtts _hdOnewNTs _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOsynMap _hdOtSyns
                              ( _tlIextendedNTs,_tlIinhMap',_tlIppA,_tlIppAI,_tlIppCata,_tlIppD,_tlIppDI,_tlIppL,_tlIppLI,_tlIppNtL,_tlIppR,_tlIppSF,_tlIppW,_tlIsynMap') =
                                  tl_ _tlOderivs _tlOext _tlOinhMap _tlOnewAtts _tlOnewNTs _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOsynMap _tlOtSyns
                          in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))))
sem_Nonterminals_Nil :: T_Nonterminals
sem_Nonterminals_Nil =
    (T_Nonterminals (\ _lhsIderivs
                       _lhsIext
                       _lhsIinhMap
                       _lhsInewAtts
                       _lhsInewNTs
                       _lhsInewProds
                       _lhsIo_noGroup
                       _lhsIo_rename
                       _lhsIsynMap
                       _lhsItSyns ->
                         (let _lhsOextendedNTs :: (Set NontermIdent)
                              _lhsOinhMap' :: (Map Identifier Attributes)
                              _lhsOppA :: PP_Doc
                              _lhsOppAI :: ([PP_Doc])
                              _lhsOppCata :: PP_Doc
                              _lhsOppD :: PP_Doc
                              _lhsOppDI :: ([PP_Doc])
                              _lhsOppL :: PP_Doc
                              _lhsOppLI :: ([PP_Doc])
                              _lhsOppNtL :: ([(PP_Doc, Attributes)])
                              _lhsOppR :: PP_Doc
                              _lhsOppSF :: PP_Doc
                              _lhsOppW :: PP_Doc
                              _lhsOsynMap' :: (Map Identifier Attributes)
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 105, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 105 "./src-ag/AG2AspectAG.ag" #-}
                                   Set.empty
                                   {-# LINE 2092 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "./src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2098 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 321, column 64)
                              _lhsOppA =
                                  ({-# LINE 321 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2104 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 335, column 42)
                              _lhsOppAI =
                                  ({-# LINE 335 "./src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2110 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 732, column 67)
                              _lhsOppCata =
                                  ({-# LINE 732 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2116 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 205, column 52)
                              _lhsOppD =
                                  ({-# LINE 205 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2122 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 205, column 86)
                              _lhsOppDI =
                                  ({-# LINE 205 "./src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2128 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                              _lhsOppL =
                                  ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2134 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                              _lhsOppLI =
                                  ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2140 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 403, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 403 "./src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2146 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                              _lhsOppR =
                                  ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2152 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 773, column 66)
                              _lhsOppSF =
                                  ({-# LINE 773 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2158 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/AG2AspectAG.ag"(line 843, column 42)
                              _lhsOppW =
                                  ({-# LINE 843 "./src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2164 "dist/build/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "./src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "./src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2170 "dist/build/AG2AspectAG.hs" #-}
                                   )
                          in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Pattern 
         info                 : (Identifier, Identifier)
   alternatives:
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (( Pattern,((Identifier, Identifier))))
data Inh_Pattern = Inh_Pattern {}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: Pattern,info_Syn_Pattern :: ((Identifier, Identifier))}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern) =
    (let ( _lhsOcopy,_lhsOinfo) = sem
     in  (Syn_Pattern _lhsOcopy _lhsOinfo))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    -- "./src-ag/AG2AspectAG.ag"(line 383, column 25)
                    _lhsOinfo =
                        ({-# LINE 383 "./src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Constr undefined!!"
                         {-# LINE 2239 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 2245 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2251 "dist/build/AG2AspectAG.hs" #-}
                         )
                    ( _patsIcopy) =
                        pats_
                in  ( _lhsOcopy,_lhsOinfo)))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    -- "./src-ag/AG2AspectAG.ag"(line 384, column 25)
                    _lhsOinfo =
                        ({-# LINE 384 "./src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Product undefined!!"
                         {-# LINE 2267 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 2273 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2279 "dist/build/AG2AspectAG.hs" #-}
                         )
                    ( _patsIcopy) =
                        pats_
                in  ( _lhsOcopy,_lhsOinfo)))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIinfo :: ((Identifier, Identifier))
                    -- "./src-ag/AG2AspectAG.ag"(line 382, column 25)
                    _lhsOinfo =
                        ({-# LINE 382 "./src-ag/AG2AspectAG.ag" #-}
                         (field_, attr_)
                         {-# LINE 2297 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 2303 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2309 "dist/build/AG2AspectAG.hs" #-}
                         )
                    ( _patIcopy,_patIinfo) =
                        pat_
                in  ( _lhsOcopy,_lhsOinfo)))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (let _lhsOcopy :: Pattern
                    _lhsOinfo :: ((Identifier, Identifier))
                    _patIcopy :: Pattern
                    _patIinfo :: ((Identifier, Identifier))
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 2325 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2331 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOinfo =
                        ({-# LINE 380 "./src-ag/AG2AspectAG.ag" #-}
                         _patIinfo
                         {-# LINE 2337 "dist/build/AG2AspectAG.hs" #-}
                         )
                    ( _patIcopy,_patIinfo) =
                        pat_
                in  ( _lhsOcopy,_lhsOinfo)))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern
                    -- "./src-ag/AG2AspectAG.ag"(line 385, column 25)
                    _lhsOinfo =
                        ({-# LINE 385 "./src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Underscore undefined!!"
                         {-# LINE 2351 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 2357 "dist/build/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2363 "dist/build/AG2AspectAG.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOinfo)))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : Patterns 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (( Patterns))
data Inh_Patterns = Inh_Patterns {}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: Patterns}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns) =
    (let ( _lhsOcopy) = sem
     in  (Syn_Patterns _lhsOcopy))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (let _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIinfo :: ((Identifier, Identifier))
                     _tlIcopy :: Patterns
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 2408 "dist/build/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2414 "dist/build/AG2AspectAG.hs" #-}
                          )
                     ( _hdIcopy,_hdIinfo) =
                         hd_
                     ( _tlIcopy) =
                         tl_
                 in  ( _lhsOcopy)))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (let _lhsOcopy :: Patterns
                     -- self rule
                     _copy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 2428 "dist/build/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "./src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2434 "dist/build/AG2AspectAG.hs" #-}
                          )
                 in  ( _lhsOcopy)))
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inh                  :  Attributes 
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newNT                : Bool
         newProds             :  Map.Map ConstructorIdent FieldMap 
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         syn                  :  Attributes 
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         hasMoreProds         :  Bool 
         ppA                  : PP_Doc
         ppCata               : PP_Doc
         ppD                  : PP_Doc
         ppDI                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
         ppRA                 : [PP_Doc]
         ppSF                 : PP_Doc
         ppSPF                : PP_Doc
         prdInh               : Attributes
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         child macro          : {MaybeMacro}
         visit 0:
            local ppProd      : _
            local prodName    : _
            local conName     : _
            local newProd     : _
            local ppR         : _
            local ppRA        : _
-}
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro) =
    (sem_Production_Production _con _params _constraints (sem_Children _children) (sem_Rules _rules) (sem_TypeSigs _typeSigs) _macro)
-- semantic domain
newtype T_Production = T_Production ((Maybe String) ->
                                     ( Attributes ) ->
                                     (Map Identifier Attributes) ->
                                     ([String]) ->
                                     ( Attributes ) ->
                                     Bool ->
                                     ( Map.Map ConstructorIdent FieldMap ) ->
                                     ([String]) ->
                                     Bool ->
                                     PP_Doc ->
                                     ( Attributes ) ->
                                     (Map Identifier Attributes) ->
                                     ([String]) ->
                                     ( ( Bool ),PP_Doc,PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,Attributes))
data Inh_Production = Inh_Production {ext_Inh_Production :: (Maybe String),inh_Inh_Production :: ( Attributes ),inhMap_Inh_Production :: (Map Identifier Attributes),inhNoGroup_Inh_Production :: ([String]),newAtts_Inh_Production :: ( Attributes ),newNT_Inh_Production :: Bool,newProds_Inh_Production :: ( Map.Map ConstructorIdent FieldMap ),o_noGroup_Inh_Production :: ([String]),o_rename_Inh_Production :: Bool,ppNt_Inh_Production :: PP_Doc,syn_Inh_Production :: ( Attributes ),synMap_Inh_Production :: (Map Identifier Attributes),synNoGroup_Inh_Production :: ([String])}
data Syn_Production = Syn_Production {hasMoreProds_Syn_Production :: ( Bool ),ppA_Syn_Production :: PP_Doc,ppCata_Syn_Production :: PP_Doc,ppD_Syn_Production :: PP_Doc,ppDI_Syn_Production :: ([PP_Doc]),ppL_Syn_Production :: PP_Doc,ppLI_Syn_Production :: ([PP_Doc]),ppR_Syn_Production :: PP_Doc,ppRA_Syn_Production :: ([PP_Doc]),ppSF_Syn_Production :: PP_Doc,ppSPF_Syn_Production :: PP_Doc,prdInh_Syn_Production :: Attributes}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production (T_Production sem) (Inh_Production _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh) = sem _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup
     in  (Syn_Production _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children ->
                             T_Rules ->
                             T_TypeSigs ->
                             MaybeMacro ->
                             T_Production
sem_Production_Production con_ params_ constraints_ (T_Children children_) (T_Rules rules_) (T_TypeSigs typeSigs_) macro_ =
    (T_Production (\ _lhsIext
                     _lhsIinh
                     _lhsIinhMap
                     _lhsIinhNoGroup
                     _lhsInewAtts
                     _lhsInewNT
                     _lhsInewProds
                     _lhsIo_noGroup
                     _lhsIo_rename
                     _lhsIppNt
                     _lhsIsyn
                     _lhsIsynMap
                     _lhsIsynNoGroup ->
                       (let _lhsOhasMoreProds :: ( Bool )
                            _childrenOppProd :: PP_Doc
                            _rulesOppProd :: PP_Doc
                            _lhsOppD :: PP_Doc
                            _lhsOppL :: PP_Doc
                            _lhsOppLI :: ([PP_Doc])
                            _lhsOppA :: PP_Doc
                            _lhsOppCata :: PP_Doc
                            _lhsOppSF :: PP_Doc
                            _lhsOppSPF :: PP_Doc
                            _lhsOppDI :: ([PP_Doc])
                            _lhsOppR :: PP_Doc
                            _lhsOppRA :: ([PP_Doc])
                            _lhsOprdInh :: Attributes
                            _childrenOext :: (Maybe String)
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOinhNoGroup :: ([String])
                            _childrenOnewAtts :: ( Attributes )
                            _childrenOo_noGroup :: ([String])
                            _childrenOo_rename :: Bool
                            _childrenOppNt :: PP_Doc
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _childrenOsynNoGroup :: ([String])
                            _rulesOext :: (Maybe String)
                            _rulesOinhNoGroup :: ([String])
                            _rulesOnewAtts :: ( Attributes )
                            _rulesOnewProd :: Bool
                            _rulesOo_noGroup :: ([String])
                            _rulesOppNt :: PP_Doc
                            _rulesOsynNoGroup :: ([String])
                            _childrenIidCL :: ([(Identifier,Type)])
                            _childrenIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                            _childrenIppDL :: ([PP_Doc])
                            _childrenIppL :: PP_Doc
                            _childrenIppLI :: ([PP_Doc])
                            _childrenIppR :: PP_Doc
                            _childrenIprdInh :: Attributes
                            _rulesIlocals :: ([Identifier])
                            _rulesIppRL :: ([ PPRule ])
                            -- "./src-ag/AG2AspectAG.ag"(line 103, column 29)
                            _lhsOhasMoreProds =
                                ({-# LINE 103 "./src-ag/AG2AspectAG.ag" #-}
                                 not $ Map.member con_ _lhsInewProds
                                 {-# LINE 2577 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 176, column 25)
                            _ppProd =
                                ({-# LINE 176 "./src-ag/AG2AspectAG.ag" #-}
                                 pp con_
                                 {-# LINE 2583 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 177, column 25)
                            _prodName =
                                ({-# LINE 177 "./src-ag/AG2AspectAG.ag" #-}
                                 ppName [_lhsIppNt, _ppProd    ]
                                 {-# LINE 2589 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 178, column 25)
                            _conName =
                                ({-# LINE 178 "./src-ag/AG2AspectAG.ag" #-}
                                 if _lhsIo_rename
                                 then _prodName
                                 else _ppProd
                                 {-# LINE 2597 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 195, column 25)
                            _childrenOppProd =
                                ({-# LINE 195 "./src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2603 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 196, column 25)
                            _rulesOppProd =
                                ({-# LINE 196 "./src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2609 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 228, column 25)
                            _lhsOppD =
                                ({-# LINE 228 "./src-ag/AG2AspectAG.ag" #-}
                                 _conName     >|< ppListSep " {" "}" ", " _childrenIppDL
                                 {-# LINE 2615 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 275, column 25)
                            _lhsOppL =
                                ({-# LINE 275 "./src-ag/AG2AspectAG.ag" #-}
                                 if (Map.member con_ _lhsInewProds)
                                   then _childrenIppL
                                   else empty
                                 {-# LINE 2623 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 279, column 25)
                            _lhsOppLI =
                                ({-# LINE 279 "./src-ag/AG2AspectAG.ag" #-}
                                 if (not $ Map.member con_ _lhsInewProds)
                                   then _childrenIppLI
                                   else []
                                 {-# LINE 2631 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 332, column 25)
                            _lhsOppA =
                                ({-# LINE 332 "./src-ag/AG2AspectAG.ag" #-}
                                 defLocalAtts _prodName     (length _rulesIlocals) 1 $ sort _rulesIlocals
                                 {-# LINE 2637 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 428, column 25)
                            _newProd =
                                ({-# LINE 428 "./src-ag/AG2AspectAG.ag" #-}
                                 Map.member con_ _lhsInewProds
                                 {-# LINE 2643 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 429, column 41)
                            (_ppR,_ppRA) =
                                ({-# LINE 429 "./src-ag/AG2AspectAG.ag" #-}
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
                                 {-# LINE 2666 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 739, column 25)
                            _lhsOppCata =
                                ({-# LINE 739 "./src-ag/AG2AspectAG.ag" #-}
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
                                 {-# LINE 2684 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 803, column 25)
                            _lhsOppSF =
                                ({-# LINE 803 "./src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppPattern = case (show con_) of
                                                   "Cons"    -> ppParams (ppListSep "" "" " : ")
                                                   "Nil"     -> pp "[]"
                                                   otherwise -> _conName     >|< " " >|< (ppParams ppSpaced)
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "sem_" >|< _lhsIppNt >|< " (" >|< ppPattern >|< ") = sem_" >|< _prodName     >|<
                                      " (" >|< map (fst . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2697 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- "./src-ag/AG2AspectAG.ag"(line 815, column 25)
                            _lhsOppSPF =
                                ({-# LINE 815 "./src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "sem_" >|< _lhsIppNt >|< "_" >|< con_ >#< ppParams ppSpaced >|< " = semP_" >|< _prodName     >|<
                                      " (" >|< map (snd . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2706 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "./src-ag/AG2AspectAG.ag"(line 205, column 86)
                            _lhsOppDI =
                                ({-# LINE 205 "./src-ag/AG2AspectAG.ag" #-}
                                 []
                                 {-# LINE 2712 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                            _lhsOppR =
                                ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                                 _ppR
                                 {-# LINE 2718 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "./src-ag/AG2AspectAG.ag"(line 419, column 43)
                            _lhsOppRA =
                                ({-# LINE 419 "./src-ag/AG2AspectAG.ag" #-}
                                 _ppRA
                                 {-# LINE 2724 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "./src-ag/AG2AspectAG.ag"(line 65, column 57)
                            _lhsOprdInh =
                                ({-# LINE 65 "./src-ag/AG2AspectAG.ag" #-}
                                 _childrenIprdInh
                                 {-# LINE 2730 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOext =
                                ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2736 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 2742 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhNoGroup =
                                ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2748 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOnewAtts =
                                ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsInewAtts
                                 {-# LINE 2754 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_noGroup =
                                ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_noGroup
                                 {-# LINE 2760 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_rename =
                                ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_rename
                                 {-# LINE 2766 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOppNt =
                                ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2772 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 2778 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynNoGroup =
                                ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2784 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOext =
                                ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2790 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinhNoGroup =
                                ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2796 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOnewAtts =
                                ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsInewAtts
                                 {-# LINE 2802 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOnewProd =
                                ({-# LINE 412 "./src-ag/AG2AspectAG.ag" #-}
                                 _newProd
                                 {-# LINE 2808 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_noGroup =
                                ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_noGroup
                                 {-# LINE 2814 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOppNt =
                                ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2820 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsynNoGroup =
                                ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2826 "dist/build/AG2AspectAG.hs" #-}
                                 )
                            ( _childrenIidCL,_childrenIppCSF,_childrenIppDL,_childrenIppL,_childrenIppLI,_childrenIppR,_childrenIprdInh) =
                                children_ _childrenOext _childrenOinhMap _childrenOinhNoGroup _childrenOnewAtts _childrenOo_noGroup _childrenOo_rename _childrenOppNt _childrenOppProd _childrenOsynMap _childrenOsynNoGroup
                            ( _rulesIlocals,_rulesIppRL) =
                                rules_ _rulesOext _rulesOinhNoGroup _rulesOnewAtts _rulesOnewProd _rulesOo_noGroup _rulesOppNt _rulesOppProd _rulesOsynNoGroup
                        in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))))
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inh                  :  Attributes 
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newNT                : Bool
         newProds             :  Map.Map ConstructorIdent FieldMap 
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         syn                  :  Attributes 
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         hasMoreProds         :  Bool 
         ppA                  : PP_Doc
         ppCata               : PP_Doc
         ppDL                 : [PP_Doc]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
         ppRA                 : [PP_Doc]
         ppSF                 : PP_Doc
         ppSPF                : PP_Doc
         prdInh               : Attributes
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
      alternative Nil:
-}
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
newtype T_Productions = T_Productions ((Maybe String) ->
                                       ( Attributes ) ->
                                       (Map Identifier Attributes) ->
                                       ([String]) ->
                                       ( Attributes ) ->
                                       Bool ->
                                       ( Map.Map ConstructorIdent FieldMap ) ->
                                       ([String]) ->
                                       Bool ->
                                       PP_Doc ->
                                       ( Attributes ) ->
                                       (Map Identifier Attributes) ->
                                       ([String]) ->
                                       ( ( Bool ),PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,Attributes))
data Inh_Productions = Inh_Productions {ext_Inh_Productions :: (Maybe String),inh_Inh_Productions :: ( Attributes ),inhMap_Inh_Productions :: (Map Identifier Attributes),inhNoGroup_Inh_Productions :: ([String]),newAtts_Inh_Productions :: ( Attributes ),newNT_Inh_Productions :: Bool,newProds_Inh_Productions :: ( Map.Map ConstructorIdent FieldMap ),o_noGroup_Inh_Productions :: ([String]),o_rename_Inh_Productions :: Bool,ppNt_Inh_Productions :: PP_Doc,syn_Inh_Productions :: ( Attributes ),synMap_Inh_Productions :: (Map Identifier Attributes),synNoGroup_Inh_Productions :: ([String])}
data Syn_Productions = Syn_Productions {hasMoreProds_Syn_Productions :: ( Bool ),ppA_Syn_Productions :: PP_Doc,ppCata_Syn_Productions :: PP_Doc,ppDL_Syn_Productions :: ([PP_Doc]),ppL_Syn_Productions :: PP_Doc,ppLI_Syn_Productions :: ([PP_Doc]),ppR_Syn_Productions :: PP_Doc,ppRA_Syn_Productions :: ([PP_Doc]),ppSF_Syn_Productions :: PP_Doc,ppSPF_Syn_Productions :: PP_Doc,prdInh_Syn_Productions :: Attributes}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions (T_Productions sem) (Inh_Productions _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup) =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh) = sem _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup
     in  (Syn_Productions _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppDL _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons (T_Production hd_) (T_Productions tl_) =
    (T_Productions (\ _lhsIext
                      _lhsIinh
                      _lhsIinhMap
                      _lhsIinhNoGroup
                      _lhsInewAtts
                      _lhsInewNT
                      _lhsInewProds
                      _lhsIo_noGroup
                      _lhsIo_rename
                      _lhsIppNt
                      _lhsIsyn
                      _lhsIsynMap
                      _lhsIsynNoGroup ->
                        (let _hdOinhNoGroup :: ([String])
                             _lhsOppDL :: ([PP_Doc])
                             _lhsOhasMoreProds :: ( Bool )
                             _lhsOppA :: PP_Doc
                             _lhsOppCata :: PP_Doc
                             _lhsOppL :: PP_Doc
                             _lhsOppLI :: ([PP_Doc])
                             _lhsOppR :: PP_Doc
                             _lhsOppRA :: ([PP_Doc])
                             _lhsOppSF :: PP_Doc
                             _lhsOppSPF :: PP_Doc
                             _lhsOprdInh :: Attributes
                             _hdOext :: (Maybe String)
                             _hdOinh :: ( Attributes )
                             _hdOinhMap :: (Map Identifier Attributes)
                             _hdOnewAtts :: ( Attributes )
                             _hdOnewNT :: Bool
                             _hdOnewProds :: ( Map.Map ConstructorIdent FieldMap )
                             _hdOo_noGroup :: ([String])
                             _hdOo_rename :: Bool
                             _hdOppNt :: PP_Doc
                             _hdOsyn :: ( Attributes )
                             _hdOsynMap :: (Map Identifier Attributes)
                             _hdOsynNoGroup :: ([String])
                             _tlOext :: (Maybe String)
                             _tlOinh :: ( Attributes )
                             _tlOinhMap :: (Map Identifier Attributes)
                             _tlOinhNoGroup :: ([String])
                             _tlOnewAtts :: ( Attributes )
                             _tlOnewNT :: Bool
                             _tlOnewProds :: ( Map.Map ConstructorIdent FieldMap )
                             _tlOo_noGroup :: ([String])
                             _tlOo_rename :: Bool
                             _tlOppNt :: PP_Doc
                             _tlOsyn :: ( Attributes )
                             _tlOsynMap :: (Map Identifier Attributes)
                             _tlOsynNoGroup :: ([String])
                             _hdIhasMoreProds :: ( Bool )
                             _hdIppA :: PP_Doc
                             _hdIppCata :: PP_Doc
                             _hdIppD :: PP_Doc
                             _hdIppDI :: ([PP_Doc])
                             _hdIppL :: PP_Doc
                             _hdIppLI :: ([PP_Doc])
                             _hdIppR :: PP_Doc
                             _hdIppRA :: ([PP_Doc])
                             _hdIppSF :: PP_Doc
                             _hdIppSPF :: PP_Doc
                             _hdIprdInh :: Attributes
                             _tlIhasMoreProds :: ( Bool )
                             _tlIppA :: PP_Doc
                             _tlIppCata :: PP_Doc
                             _tlIppDL :: ([PP_Doc])
                             _tlIppL :: PP_Doc
                             _tlIppLI :: ([PP_Doc])
                             _tlIppR :: PP_Doc
                             _tlIppRA :: ([PP_Doc])
                             _tlIppSF :: PP_Doc
                             _tlIppSPF :: PP_Doc
                             _tlIprdInh :: Attributes
                             -- "./src-ag/AG2AspectAG.ag"(line 62, column 11)
                             _hdOinhNoGroup =
                                 ({-# LINE 62 "./src-ag/AG2AspectAG.ag" #-}
                                  filter (flip Map.member _hdIprdInh . identifier) _lhsIinhNoGroup
                                  {-# LINE 2977 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- "./src-ag/AG2AspectAG.ag"(line 234, column 33)
                             _lhsOppDL =
                                 ({-# LINE 234 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppD : _tlIppDL
                                  {-# LINE 2983 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 101, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 101 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIhasMoreProds  ||  _tlIhasMoreProds
                                  {-# LINE 2989 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 321, column 64)
                             _lhsOppA =
                                 ({-# LINE 321 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppA >-< _tlIppA
                                  {-# LINE 2995 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 732, column 67)
                             _lhsOppCata =
                                 ({-# LINE 732 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppCata >-< _tlIppCata
                                  {-# LINE 3001 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                             _lhsOppL =
                                 ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppL >-< _tlIppL
                                  {-# LINE 3007 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                             _lhsOppLI =
                                 ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppLI ++ _tlIppLI
                                  {-# LINE 3013 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                             _lhsOppR =
                                 ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppR >-< _tlIppR
                                  {-# LINE 3019 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 419, column 43)
                             _lhsOppRA =
                                 ({-# LINE 419 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppRA ++ _tlIppRA
                                  {-# LINE 3025 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 773, column 66)
                             _lhsOppSF =
                                 ({-# LINE 773 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSF >-< _tlIppSF
                                  {-# LINE 3031 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 774, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 774 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSPF >-< _tlIppSPF
                                  {-# LINE 3037 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 65, column 57)
                             _lhsOprdInh =
                                 ({-# LINE 65 "./src-ag/AG2AspectAG.ag" #-}
                                  _hdIprdInh `Map.union` _tlIprdInh
                                  {-# LINE 3043 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOext =
                                 ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 3049 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 763 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 3055 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 3061 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewAtts =
                                 ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 3067 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewNT =
                                 ({-# LINE 411 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 3073 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewProds =
                                 ({-# LINE 86 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 3079 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_noGroup =
                                 ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 3085 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_rename =
                                 ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 3091 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOppNt =
                                 ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 3097 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 763 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 3103 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 3109 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynNoGroup =
                                 ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 3115 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOext =
                                 ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 3121 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 763 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 3127 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 3133 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhNoGroup =
                                 ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinhNoGroup
                                  {-# LINE 3139 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewAtts =
                                 ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 3145 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewNT =
                                 ({-# LINE 411 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 3151 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewProds =
                                 ({-# LINE 86 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 3157 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_noGroup =
                                 ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 3163 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_rename =
                                 ({-# LINE 41 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 3169 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOppNt =
                                 ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 3175 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 763 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 3181 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "./src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 3187 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynNoGroup =
                                 ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 3193 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             ( _hdIhasMoreProds,_hdIppA,_hdIppCata,_hdIppD,_hdIppDI,_hdIppL,_hdIppLI,_hdIppR,_hdIppRA,_hdIppSF,_hdIppSPF,_hdIprdInh) =
                                 hd_ _hdOext _hdOinh _hdOinhMap _hdOinhNoGroup _hdOnewAtts _hdOnewNT _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOppNt _hdOsyn _hdOsynMap _hdOsynNoGroup
                             ( _tlIhasMoreProds,_tlIppA,_tlIppCata,_tlIppDL,_tlIppL,_tlIppLI,_tlIppR,_tlIppRA,_tlIppSF,_tlIppSPF,_tlIprdInh) =
                                 tl_ _tlOext _tlOinh _tlOinhMap _tlOinhNoGroup _tlOnewAtts _tlOnewNT _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOppNt _tlOsyn _tlOsynMap _tlOsynNoGroup
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (T_Productions (\ _lhsIext
                      _lhsIinh
                      _lhsIinhMap
                      _lhsIinhNoGroup
                      _lhsInewAtts
                      _lhsInewNT
                      _lhsInewProds
                      _lhsIo_noGroup
                      _lhsIo_rename
                      _lhsIppNt
                      _lhsIsyn
                      _lhsIsynMap
                      _lhsIsynNoGroup ->
                        (let _lhsOppDL :: ([PP_Doc])
                             _lhsOhasMoreProds :: ( Bool )
                             _lhsOppA :: PP_Doc
                             _lhsOppCata :: PP_Doc
                             _lhsOppL :: PP_Doc
                             _lhsOppLI :: ([PP_Doc])
                             _lhsOppR :: PP_Doc
                             _lhsOppRA :: ([PP_Doc])
                             _lhsOppSF :: PP_Doc
                             _lhsOppSPF :: PP_Doc
                             _lhsOprdInh :: Attributes
                             -- "./src-ag/AG2AspectAG.ag"(line 235, column 33)
                             _lhsOppDL =
                                 ({-# LINE 235 "./src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 3230 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 101, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 101 "./src-ag/AG2AspectAG.ag" #-}
                                  False
                                  {-# LINE 3236 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 321, column 64)
                             _lhsOppA =
                                 ({-# LINE 321 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3242 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 732, column 67)
                             _lhsOppCata =
                                 ({-# LINE 732 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3248 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 79)
                             _lhsOppL =
                                 ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3254 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 259, column 112)
                             _lhsOppLI =
                                 ({-# LINE 259 "./src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 3260 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 418, column 79)
                             _lhsOppR =
                                 ({-# LINE 418 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3266 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 419, column 43)
                             _lhsOppRA =
                                 ({-# LINE 419 "./src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 3272 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 773, column 66)
                             _lhsOppSF =
                                 ({-# LINE 773 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3278 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 774, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 774 "./src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3284 "dist/build/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "./src-ag/AG2AspectAG.ag"(line 65, column 57)
                             _lhsOprdInh =
                                 ({-# LINE 65 "./src-ag/AG2AspectAG.ag" #-}
                                  Map.empty
                                  {-# LINE 3290 "dist/build/AG2AspectAG.hs" #-}
                                  )
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppDL,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))))
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newProd              : Bool
         o_noGroup            : [String]
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synNoGroup           : [String]
      synthesized attributes:
         locals               : [Identifier]
         ppRL                 : [ PPRule ]
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : Expression 
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
-}
-- cata
sem_Rule :: Rule ->
            T_Rule
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager) =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern) (sem_Expression _rhs) _owrt _origin _explicit _pure _identity _mbError _eager)
-- semantic domain
newtype T_Rule = T_Rule ((Maybe String) ->
                         ([String]) ->
                         ( Attributes ) ->
                         Bool ->
                         ([String]) ->
                         PP_Doc ->
                         PP_Doc ->
                         ([String]) ->
                         ( ([Identifier]),([ PPRule ])))
data Inh_Rule = Inh_Rule {ext_Inh_Rule :: (Maybe String),inhNoGroup_Inh_Rule :: ([String]),newAtts_Inh_Rule :: ( Attributes ),newProd_Inh_Rule :: Bool,o_noGroup_Inh_Rule :: ([String]),ppNt_Inh_Rule :: PP_Doc,ppProd_Inh_Rule :: PP_Doc,synNoGroup_Inh_Rule :: ([String])}
data Syn_Rule = Syn_Rule {locals_Syn_Rule :: ([Identifier]),ppRL_Syn_Rule :: ([ PPRule ])}
wrap_Rule :: T_Rule ->
             Inh_Rule ->
             Syn_Rule
wrap_Rule (T_Rule sem) (Inh_Rule _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup
     in  (Syn_Rule _lhsOlocals _lhsOppRL))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern ->
                 T_Expression ->
                 Bool ->
                 String ->
                 Bool ->
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule
sem_Rule_Rule mbName_ (T_Pattern pattern_) (T_Expression rhs_) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_ =
    (T_Rule (\ _lhsIext
               _lhsIinhNoGroup
               _lhsInewAtts
               _lhsInewProd
               _lhsIo_noGroup
               _lhsIppNt
               _lhsIppProd
               _lhsIsynNoGroup ->
                 (let _lhsOlocals :: ([Identifier])
                      _lhsOppRL :: ([ PPRule ])
                      _rhsOppNt :: PP_Doc
                      _rhsOppProd :: PP_Doc
                      _patternIcopy :: Pattern
                      _patternIinfo :: ((Identifier, Identifier))
                      _rhsIppRE :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
                      -- "./src-ag/AG2AspectAG.ag"(line 375, column 25)
                      _lhsOlocals =
                          ({-# LINE 375 "./src-ag/AG2AspectAG.ag" #-}
                           if (show (fst _patternIinfo) == "loc")
                            then [ snd _patternIinfo ]
                            else [ ]
                           {-# LINE 3377 "dist/build/AG2AspectAG.hs" #-}
                           )
                      -- "./src-ag/AG2AspectAG.ag"(line 472, column 33)
                      _lhsOppRL =
                          ({-# LINE 472 "./src-ag/AG2AspectAG.ag" #-}
                           if (not explicit_ &&  not _lhsInewProd && not (Map.member (snd _patternIinfo) _lhsInewAtts) )
                           then []
                           else [ ppRule _patternIinfo owrt_ (defRule _lhsIppNt _patternIinfo _lhsIo_noGroup _rhsIppRE) ]
                           {-# LINE 3385 "dist/build/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppNt =
                          ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                           _lhsIppNt
                           {-# LINE 3391 "dist/build/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppProd =
                          ({-# LINE 192 "./src-ag/AG2AspectAG.ag" #-}
                           _lhsIppProd
                           {-# LINE 3397 "dist/build/AG2AspectAG.hs" #-}
                           )
                      ( _patternIcopy,_patternIinfo) =
                          pattern_
                      ( _rhsIppRE) =
                          rhs_ _rhsOppNt _rhsOppProd
                  in  ( _lhsOlocals,_lhsOppRL))))
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newProd              : Bool
         o_noGroup            : [String]
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synNoGroup           : [String]
      synthesized attributes:
         locals               : [Identifier]
         ppRL                 : [ PPRule ]
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
-}
-- cata
sem_Rules :: Rules ->
             T_Rules
sem_Rules list =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list))
-- semantic domain
newtype T_Rules = T_Rules ((Maybe String) ->
                           ([String]) ->
                           ( Attributes ) ->
                           Bool ->
                           ([String]) ->
                           PP_Doc ->
                           PP_Doc ->
                           ([String]) ->
                           ( ([Identifier]),([ PPRule ])))
data Inh_Rules = Inh_Rules {ext_Inh_Rules :: (Maybe String),inhNoGroup_Inh_Rules :: ([String]),newAtts_Inh_Rules :: ( Attributes ),newProd_Inh_Rules :: Bool,o_noGroup_Inh_Rules :: ([String]),ppNt_Inh_Rules :: PP_Doc,ppProd_Inh_Rules :: PP_Doc,synNoGroup_Inh_Rules :: ([String])}
data Syn_Rules = Syn_Rules {locals_Syn_Rules :: ([Identifier]),ppRL_Syn_Rules :: ([ PPRule ])}
wrap_Rules :: T_Rules ->
              Inh_Rules ->
              Syn_Rules
wrap_Rules (T_Rules sem) (Inh_Rules _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup) =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsInewAtts _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup
     in  (Syn_Rules _lhsOlocals _lhsOppRL))
sem_Rules_Cons :: T_Rule ->
                  T_Rules ->
                  T_Rules
sem_Rules_Cons (T_Rule hd_) (T_Rules tl_) =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsInewAtts
                _lhsInewProd
                _lhsIo_noGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       _hdOext :: (Maybe String)
                       _hdOinhNoGroup :: ([String])
                       _hdOnewAtts :: ( Attributes )
                       _hdOnewProd :: Bool
                       _hdOo_noGroup :: ([String])
                       _hdOppNt :: PP_Doc
                       _hdOppProd :: PP_Doc
                       _hdOsynNoGroup :: ([String])
                       _tlOext :: (Maybe String)
                       _tlOinhNoGroup :: ([String])
                       _tlOnewAtts :: ( Attributes )
                       _tlOnewProd :: Bool
                       _tlOo_noGroup :: ([String])
                       _tlOppNt :: PP_Doc
                       _tlOppProd :: PP_Doc
                       _tlOsynNoGroup :: ([String])
                       _hdIlocals :: ([Identifier])
                       _hdIppRL :: ([ PPRule ])
                       _tlIlocals :: ([Identifier])
                       _tlIppRL :: ([ PPRule ])
                       -- "./src-ag/AG2AspectAG.ag"(line 468, column 33)
                       _lhsOppRL =
                           ({-# LINE 468 "./src-ag/AG2AspectAG.ag" #-}
                            _hdIppRL ++ _tlIppRL
                            {-# LINE 3486 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- use rule "./src-ag/AG2AspectAG.ag"(line 371, column 30)
                       _lhsOlocals =
                           ({-# LINE 371 "./src-ag/AG2AspectAG.ag" #-}
                            _hdIlocals ++ _tlIlocals
                            {-# LINE 3492 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOext =
                           ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 3498 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinhNoGroup =
                           ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 3504 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnewAtts =
                           ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsInewAtts
                            {-# LINE 3510 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnewProd =
                           ({-# LINE 412 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsInewProd
                            {-# LINE 3516 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_noGroup =
                           ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIo_noGroup
                            {-# LINE 3522 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppNt =
                           ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 3528 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppProd =
                           ({-# LINE 192 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3534 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsynNoGroup =
                           ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3540 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOext =
                           ({-# LINE 119 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 3546 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinhNoGroup =
                           ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 3552 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnewAtts =
                           ({-# LINE 78 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsInewAtts
                            {-# LINE 3558 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnewProd =
                           ({-# LINE 412 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsInewProd
                            {-# LINE 3564 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_noGroup =
                           ({-# LINE 45 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIo_noGroup
                            {-# LINE 3570 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppNt =
                           ({-# LINE 187 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 3576 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppProd =
                           ({-# LINE 192 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3582 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsynNoGroup =
                           ({-# LINE 55 "./src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3588 "dist/build/AG2AspectAG.hs" #-}
                            )
                       ( _hdIlocals,_hdIppRL) =
                           hd_ _hdOext _hdOinhNoGroup _hdOnewAtts _hdOnewProd _hdOo_noGroup _hdOppNt _hdOppProd _hdOsynNoGroup
                       ( _tlIlocals,_tlIppRL) =
                           tl_ _tlOext _tlOinhNoGroup _tlOnewAtts _tlOnewProd _tlOo_noGroup _tlOppNt _tlOppProd _tlOsynNoGroup
                   in  ( _lhsOlocals,_lhsOppRL))))
sem_Rules_Nil :: T_Rules
sem_Rules_Nil =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsInewAtts
                _lhsInewProd
                _lhsIo_noGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       -- "./src-ag/AG2AspectAG.ag"(line 469, column 33)
                       _lhsOppRL =
                           ({-# LINE 469 "./src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3611 "dist/build/AG2AspectAG.hs" #-}
                            )
                       -- use rule "./src-ag/AG2AspectAG.ag"(line 371, column 30)
                       _lhsOlocals =
                           ({-# LINE 371 "./src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3617 "dist/build/AG2AspectAG.hs" #-}
                            )
                   in  ( _lhsOlocals,_lhsOppRL))))
-- TypeSig -----------------------------------------------------
{-
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
-- cata
sem_TypeSig :: TypeSig ->
               T_TypeSig
sem_TypeSig (TypeSig _name _tp) =
    (sem_TypeSig_TypeSig _name _tp)
-- semantic domain
newtype T_TypeSig = T_TypeSig (( ))
data Inh_TypeSig = Inh_TypeSig {}
data Syn_TypeSig = Syn_TypeSig {}
wrap_TypeSig :: T_TypeSig ->
                Inh_TypeSig ->
                Syn_TypeSig
wrap_TypeSig (T_TypeSig sem) (Inh_TypeSig) =
    (let ( ) = sem
     in  (Syn_TypeSig))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig
sem_TypeSig_TypeSig name_ tp_ =
    (T_TypeSig (let
                in  ( )))
-- TypeSigs ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
-- cata
sem_TypeSigs :: TypeSigs ->
                T_TypeSigs
sem_TypeSigs list =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list))
-- semantic domain
newtype T_TypeSigs = T_TypeSigs (( ))
data Inh_TypeSigs = Inh_TypeSigs {}
data Syn_TypeSigs = Syn_TypeSigs {}
wrap_TypeSigs :: T_TypeSigs ->
                 Inh_TypeSigs ->
                 Syn_TypeSigs
wrap_TypeSigs (T_TypeSigs sem) (Inh_TypeSigs) =
    (let ( ) = sem
     in  (Syn_TypeSigs))
sem_TypeSigs_Cons :: T_TypeSig ->
                     T_TypeSigs ->
                     T_TypeSigs
sem_TypeSigs_Cons (T_TypeSig hd_) (T_TypeSigs tl_) =
    (T_TypeSigs (let
                 in  ( )))
sem_TypeSigs_Nil :: T_TypeSigs
sem_TypeSigs_Nil =
    (T_TypeSigs (let
                 in  ( )))