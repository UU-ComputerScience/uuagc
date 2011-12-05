

-- UUAGC 0.9.39.1.0 (src-ag/AG2AspectAG.ag)
module AG2AspectAG where
{-# LINE 8 "src-ag/AG2AspectAG.ag" #-}

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

{-# LINE 24 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 36 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 43 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 49 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 55 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}
{-# LINE 27 "src-ag/AG2AspectAG.ag" #-}

pragmaAspectAG =  pp  "{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction , TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}"

{-# LINE 60 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 32 "src-ag/AG2AspectAG.ag" #-}

ppName l = ppListSep "" "" "_" l
{-# LINE 65 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 69 "src-ag/AG2AspectAG.ag" #-}

type FieldMap  = [(Identifier, Type)]
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap)
{-# LINE 71 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 335 "src-ag/AG2AspectAG.ag" #-}

filterAtts newAtts = filter (\att -> Map.member (identifier att) newAtts)
filterNotAtts newAtts = filter (\att -> not (Map.member (identifier att) newAtts))

defAtt  att = "data " >|< attTName att >|< "; " >|< attName att >|< " = proxy :: Proxy " >|< attTName att
attName att = pp $ "att_" ++ att
attTName att = pp $ "Att_" ++ att


defAttRec  recPref ppNt atts noGroup =
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

{-# LINE 100 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 390 "src-ag/AG2AspectAG.ag" #-}

ntsList att ppNtL = "nts_" ++ att ++ " = " >|<  ppListSep "" "" " .*. " ((map fst ppNtL) ++ [pp "hNil"])

filterNts att = filter ( Map.member (identifier att) . snd )
{-# LINE 107 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 448 "src-ag/AG2AspectAG.ag" #-}

data PPRule = PPRule Identifier Identifier Bool ([(Identifier,Type)] -> [Identifier] -> PP_Doc)

ppRule (field,attr) owrt def = PPRule field attr owrt def
ruleField (PPRule field  _     _     _  ) = field
ruleAttr  (PPRule _      attr  _     _  ) = attr
ruleOwrt  (PPRule _      _     owrt  _  ) = owrt
ruleDef   (PPRule _      _     _     def) = def

{-# LINE 119 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 487 "src-ag/AG2AspectAG.ag" #-}


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
                                  in  (pos, if (elem (getName attr) noGroup )
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

{-# LINE 344 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 714 "src-ag/AG2AspectAG.ag" #-}

ppMacro (Macro con children) = "( atts_" >|< show con >|< ", " >|<  ppListSep "" "" " <.> " ppChildren  >|<")"
                where   ppChildren = map  ppChild children
                        ppChild (RuleChild  ch n) = chName ch >|< " ==> " >|< ppMacro n
                        ppChild (ChildChild ch n) = chName ch >|< " --> " >|< n
                        ppChild (ValueChild ch n) = chName ch >|< " ~~> " >|< n
                        chName ch = ppName [pp "ch", pp ch, pp con]
{-# LINE 354 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 747 "src-ag/AG2AspectAG.ag" #-}

ppNoGroupAtts syn noGroup = let synatts = Map.keys $ Map.filterWithKey (\att _ -> elem (getName att) noGroup) syn
                            in  map (flip (>|<) "_inh") noGroup ++  map (flip (>|<) "_syn") synatts

ruleName att prodName = ppName [att,prodName]

elemNT a b = False
{-# LINE 364 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 790 "src-ag/AG2AspectAG.ag" #-}

attTypes atts = map (\(a,t) -> "(HCons (LVPair (Proxy Att_" >|< a >|< ") " >|< ppShow t >|< ") ") $ Map.toAscList atts
{-# LINE 369 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 844 "src-ag/AG2AspectAG.ag" #-}

attVars atts = map (\(a,_) -> "_" >|< a >|< " ") $ Map.toAscList atts
attFields atts noGroup ppNt =
     let ng = map (\(a,_) -> attName (getName a) >|< " .=. _" >|< a >|< " .*. ") $ Map.toAscList noGroup
         g  = ppCommas $ map (\(a,_) -> ppName [pp a, pp "InhG",ppNt]  >|< "= _" >|< a) $ Map.toAscList $ Map.difference atts noGroup
     in "(" >|< ng >|< "att_inh .=. " >|< ppName [pp "InhG", ppNt] >|< " { " >|< g >|< " } .*. emptyRecord)"
{-# LINE 378 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
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
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _kind )  =
    (sem_Child_Child _name _tp _kind )
-- semantic domain
newtype T_Child  = T_Child ((Maybe String) ->
                            (Map Identifier Attributes) ->
                            ([String]) ->
                            ([String]) ->
                            Bool ->
                            PP_Doc ->
                            PP_Doc ->
                            (Map Identifier Attributes) ->
                            ([String]) ->
                            ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),PP_Doc,([PP_Doc]),PP_Doc,Attributes))
data Inh_Child  = Inh_Child {ext_Inh_Child :: (Maybe String),inhMap_Inh_Child :: (Map Identifier Attributes),inhNoGroup_Inh_Child :: ([String]),o_noGroup_Inh_Child :: ([String]),o_rename_Inh_Child :: Bool,ppNt_Inh_Child :: PP_Doc,ppProd_Inh_Child :: PP_Doc,synMap_Inh_Child :: (Map Identifier Attributes),synNoGroup_Inh_Child :: ([String])}
data Syn_Child  = Syn_Child {idCL_Syn_Child :: ([(Identifier,Type)]),ppCSF_Syn_Child :: ([(Identifier,(PP_Doc,PP_Doc))]),ppL_Syn_Child :: PP_Doc,ppLI_Syn_Child :: ([PP_Doc]),ppR_Syn_Child :: PP_Doc,prdInh_Syn_Child :: Attributes}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup )  =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh) = sem _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup 
     in  (Syn_Child _lhsOidCL _lhsOppCSF _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh ))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child 
sem_Child_Child name_ tp_ kind_  =
    (T_Child (\ _lhsIext
                _lhsIinhMap
                _lhsIinhNoGroup
                _lhsIo_noGroup
                _lhsIo_rename
                _lhsIppNt
                _lhsIppProd
                _lhsIsynMap
                _lhsIsynNoGroup ->
                  (let _lhsOprdInh :: Attributes
                       _lhsOppL :: PP_Doc
                       _lhsOppLI :: ([PP_Doc])
                       _lhsOppR :: PP_Doc
                       _lhsOidCL :: ([(Identifier,Type)])
                       _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                       -- "src-ag/AG2AspectAG.ag"(line 66, column 12)
                       _lhsOprdInh =
                           ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                            _inh
                            {-# LINE 462 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 181, column 25)
                       _ppCh =
                           ({-# LINE 181 "src-ag/AG2AspectAG.ag" #-}
                            pp name_
                            {-# LINE 468 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 182, column 25)
                       _ppTCh =
                           ({-# LINE 182 "src-ag/AG2AspectAG.ag" #-}
                            ppShow tp_
                            {-# LINE 474 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 183, column 25)
                       _chName =
                           ({-# LINE 183 "src-ag/AG2AspectAG.ag" #-}
                            ppName [_ppCh    , _lhsIppNt, _lhsIppProd]
                            {-# LINE 480 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 283, column 25)
                       _chLabel =
                           ({-# LINE 283 "src-ag/AG2AspectAG.ag" #-}
                            "ch_" >|< _chName
                            {-# LINE 486 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 284, column 25)
                       _chTLabel =
                           ({-# LINE 284 "src-ag/AG2AspectAG.ag" #-}
                            "Ch_" >|< _chName
                            {-# LINE 492 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 285, column 25)
                       _lhsOppL =
                           ({-# LINE 285 "src-ag/AG2AspectAG.ag" #-}
                            "data " >|< _chTLabel     >|< "; " >|< _chLabel     >|< pp " = proxy :: " >|<
                            case kind_ of
                             ChildSyntax    ->  "Proxy " >|< "(" >|< _chTLabel     >|< ", " >|< _ppTCh     >|< ")"
                             _              ->  "SemType " >|< _ppTCh     >|< pp " nt =>  Proxy " >|<
                                                "(" >|< _chTLabel     >|< ", nt)"
                            {-# LINE 502 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 291, column 25)
                       _lhsOppLI =
                           ({-# LINE 291 "src-ag/AG2AspectAG.ag" #-}
                            [ _chLabel    , _chTLabel     ]
                            {-# LINE 508 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 444, column 25)
                       _lhsOppR =
                           ({-# LINE 444 "src-ag/AG2AspectAG.ag" #-}
                            let chName = ppListSep "" "" "_" [pp name_, _lhsIppNt, _lhsIppProd]
                            in  pp name_ >|< " <- at ch_" >|< chName
                            {-# LINE 515 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 482, column 25)
                       _lhsOidCL =
                           ({-# LINE 482 "src-ag/AG2AspectAG.ag" #-}
                            [ (name_, removeDeforested tp_ ) ]
                            {-# LINE 521 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 819, column 25)
                       _lhsOppCSF =
                           ({-# LINE 819 "src-ag/AG2AspectAG.ag" #-}
                            let
                                 semC   = if (isNonterminal tp_)
                                           then "sem_" >|< ppShow tp_ >|<  " _" >|< name_
                                           else "sem_Lit _" >|< name_
                            in   case kind_ of
                                      ChildSyntax ->  [(name_, (  _chLabel     >|< " .=. (" >|< semC >|< ") .*. "
                                                              ,  _chLabel     >|< " .=. _" >|< name_ >|< " .*. "))]
                                      _           ->  []
                            {-# LINE 534 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 19, column 11)
                       _chnt =
                           ({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier t
                            {-# LINE 543 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 23, column 11)
                       _inh =
                           ({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                            {-# LINE 549 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/DistChildAttr.ag"(line 24, column 11)
                       _syn =
                           ({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                            Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                            {-# LINE 555 "src-ag/AG2AspectAG.hs" #-}
                            )
                   in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhMap               : Map Identifier Attributes
         inhNoGroup           : [String]
         o_noGroup            : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synMap               : Map Identifier Attributes
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
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
sem_Children :: Children  ->
                T_Children 
sem_Children list  =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list) )
-- semantic domain
newtype T_Children  = T_Children ((Maybe String) ->
                                  (Map Identifier Attributes) ->
                                  ([String]) ->
                                  ([String]) ->
                                  Bool ->
                                  PP_Doc ->
                                  PP_Doc ->
                                  (Map Identifier Attributes) ->
                                  ([String]) ->
                                  ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),PP_Doc,([PP_Doc]),PP_Doc,Attributes))
data Inh_Children  = Inh_Children {ext_Inh_Children :: (Maybe String),inhMap_Inh_Children :: (Map Identifier Attributes),inhNoGroup_Inh_Children :: ([String]),o_noGroup_Inh_Children :: ([String]),o_rename_Inh_Children :: Bool,ppNt_Inh_Children :: PP_Doc,ppProd_Inh_Children :: PP_Doc,synMap_Inh_Children :: (Map Identifier Attributes),synNoGroup_Inh_Children :: ([String])}
data Syn_Children  = Syn_Children {idCL_Syn_Children :: ([(Identifier,Type)]),ppCSF_Syn_Children :: ([(Identifier,(PP_Doc,PP_Doc))]),ppL_Syn_Children :: PP_Doc,ppLI_Syn_Children :: ([PP_Doc]),ppR_Syn_Children :: PP_Doc,prdInh_Syn_Children :: Attributes}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup )  =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh) = sem _lhsIext _lhsIinhMap _lhsIinhNoGroup _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynMap _lhsIsynNoGroup 
     in  (Syn_Children _lhsOidCL _lhsOppCSF _lhsOppL _lhsOppLI _lhsOppR _lhsOprdInh ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIext
                   _lhsIinhMap
                   _lhsIinhNoGroup
                   _lhsIo_noGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynMap
                   _lhsIsynNoGroup ->
                     (let _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          _lhsOprdInh :: Attributes
                          _hdOext :: (Maybe String)
                          _hdOinhMap :: (Map Identifier Attributes)
                          _hdOinhNoGroup :: ([String])
                          _hdOo_noGroup :: ([String])
                          _hdOo_rename :: Bool
                          _hdOppNt :: PP_Doc
                          _hdOppProd :: PP_Doc
                          _hdOsynMap :: (Map Identifier Attributes)
                          _hdOsynNoGroup :: ([String])
                          _tlOext :: (Maybe String)
                          _tlOinhMap :: (Map Identifier Attributes)
                          _tlOinhNoGroup :: ([String])
                          _tlOo_noGroup :: ([String])
                          _tlOo_rename :: Bool
                          _tlOppNt :: PP_Doc
                          _tlOppProd :: PP_Doc
                          _tlOsynMap :: (Map Identifier Attributes)
                          _tlOsynNoGroup :: ([String])
                          _hdIidCL :: ([(Identifier,Type)])
                          _hdIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _hdIppL :: PP_Doc
                          _hdIppLI :: ([PP_Doc])
                          _hdIppR :: PP_Doc
                          _hdIprdInh :: Attributes
                          _tlIidCL :: ([(Identifier,Type)])
                          _tlIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _tlIppL :: PP_Doc
                          _tlIppLI :: ([PP_Doc])
                          _tlIppR :: PP_Doc
                          _tlIprdInh :: Attributes
                          -- use rule "src-ag/AG2AspectAG.ag"(line 480, column 31)
                          _lhsOidCL =
                              ({-# LINE 480 "src-ag/AG2AspectAG.ag" #-}
                               _hdIidCL ++ _tlIidCL
                               {-# LINE 661 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 815, column 34)
                          _lhsOppCSF =
                              ({-# LINE 815 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppCSF ++ _tlIppCSF
                               {-# LINE 667 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                          _lhsOppL =
                              ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppL >-< _tlIppL
                               {-# LINE 673 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                          _lhsOppLI =
                              ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppLI ++ _tlIppLI
                               {-# LINE 679 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                          _lhsOppR =
                              ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppR >-< _tlIppR
                               {-# LINE 685 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 64, column 57)
                          _lhsOprdInh =
                              ({-# LINE 64 "src-ag/AG2AspectAG.ag" #-}
                               _hdIprdInh `Map.union` _tlIprdInh
                               {-# LINE 691 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOext =
                              ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 697 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 703 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhNoGroup =
                              ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 709 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_noGroup =
                              ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_noGroup
                               {-# LINE 715 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_rename =
                              ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 721 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppNt =
                              ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 727 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppProd =
                              ({-# LINE 191 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 733 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 739 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynNoGroup =
                              ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 745 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOext =
                              ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 751 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIinhMap
                               {-# LINE 757 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhNoGroup =
                              ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 763 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_noGroup =
                              ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_noGroup
                               {-# LINE 769 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_rename =
                              ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 775 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppNt =
                              ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 781 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppProd =
                              ({-# LINE 191 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 787 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynMap =
                              ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 793 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynNoGroup =
                              ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 799 "src-ag/AG2AspectAG.hs" #-}
                               )
                          ( _hdIidCL,_hdIppCSF,_hdIppL,_hdIppLI,_hdIppR,_hdIprdInh) =
                              hd_ _hdOext _hdOinhMap _hdOinhNoGroup _hdOo_noGroup _hdOo_rename _hdOppNt _hdOppProd _hdOsynMap _hdOsynNoGroup 
                          ( _tlIidCL,_tlIppCSF,_tlIppL,_tlIppLI,_tlIppR,_tlIprdInh) =
                              tl_ _tlOext _tlOinhMap _tlOinhNoGroup _tlOo_noGroup _tlOo_rename _tlOppNt _tlOppProd _tlOsynMap _tlOsynNoGroup 
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIext
                   _lhsIinhMap
                   _lhsIinhNoGroup
                   _lhsIo_noGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynMap
                   _lhsIsynNoGroup ->
                     (let _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          _lhsOprdInh :: Attributes
                          -- use rule "src-ag/AG2AspectAG.ag"(line 480, column 31)
                          _lhsOidCL =
                              ({-# LINE 480 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 827 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 815, column 34)
                          _lhsOppCSF =
                              ({-# LINE 815 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 833 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                          _lhsOppL =
                              ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 839 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                          _lhsOppLI =
                              ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 845 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                          _lhsOppR =
                              ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 851 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 64, column 57)
                          _lhsOprdInh =
                              ({-# LINE 64 "src-ag/AG2AspectAG.ag" #-}
                               Map.empty
                               {-# LINE 857 "src-ag/AG2AspectAG.hs" #-}
                               )
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOprdInh))) )
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
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression _pos _tks )  =
    (sem_Expression_Expression _pos _tks )
-- semantic domain
newtype T_Expression  = T_Expression (PP_Doc ->
                                      PP_Doc ->
                                      ( ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)))
data Inh_Expression  = Inh_Expression {ppNt_Inh_Expression :: PP_Doc,ppProd_Inh_Expression :: PP_Doc}
data Syn_Expression  = Syn_Expression {ppRE_Syn_Expression :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression (T_Expression sem ) (Inh_Expression _lhsIppNt _lhsIppProd )  =
    (let ( _lhsOppRE) = sem _lhsIppNt _lhsIppProd 
     in  (Syn_Expression _lhsOppRE ))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression 
sem_Expression_Expression pos_ tks_  =
    (T_Expression (\ _lhsIppNt
                     _lhsIppProd ->
                       (let _lhsOppRE :: ([String] -> Identifier -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
                            -- "src-ag/AG2AspectAG.ag"(line 477, column 25)
                            _lhsOppRE =
                                ({-# LINE 477 "src-ag/AG2AspectAG.ag" #-}
                                 rhsRule _lhsIppNt _lhsIppProd tks_
                                 {-# LINE 901 "src-ag/AG2AspectAG.hs" #-}
                                 )
                        in  ( _lhsOppRE))) )
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
            local ppNtL       : _
            local ppR         : _
-}
-- cata
sem_Grammar :: Grammar  ->
               T_Grammar 
sem_Grammar (Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )  =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts ) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap )
-- semantic domain
newtype T_Grammar  = T_Grammar (((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))) ->
                                (Maybe String) ->
                                Options ->
                                ( PP_Doc,PP_Doc))
data Inh_Grammar  = Inh_Grammar {agi_Inh_Grammar :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))),ext_Inh_Grammar :: (Maybe String),options_Inh_Grammar :: Options}
data Syn_Grammar  = Syn_Grammar {imp_Syn_Grammar :: PP_Doc,pp_Syn_Grammar :: PP_Doc}
wrap_Grammar :: T_Grammar  ->
                Inh_Grammar  ->
                Syn_Grammar 
wrap_Grammar (T_Grammar sem ) (Inh_Grammar _lhsIagi _lhsIext _lhsIoptions )  =
    (let ( _lhsOimp,_lhsOpp) = sem _lhsIagi _lhsIext _lhsIoptions 
     in  (Syn_Grammar _lhsOimp _lhsOpp ))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals  ->
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
sem_Grammar_Grammar typeSyns_ useMap_ derivings_ wrappers_ (T_Nonterminals nonts_ ) pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_  =
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
                         -- "src-ag/AG2AspectAG.ag"(line 42, column 14)
                         _nontsOo_rename =
                             ({-# LINE 42 "src-ag/AG2AspectAG.ag" #-}
                              rename    _lhsIoptions
                              {-# LINE 1006 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 46, column 14)
                         _o_noGroup =
                             ({-# LINE 46 "src-ag/AG2AspectAG.ag" #-}
                              sort $ noGroup    _lhsIoptions
                              {-# LINE 1012 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 47, column 14)
                         _nontsOo_noGroup =
                             ({-# LINE 47 "src-ag/AG2AspectAG.ag" #-}
                              _o_noGroup
                              {-# LINE 1018 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 79, column 23)
                         _newAtts =
                             ({-# LINE 79 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (_,_,atts) -> ( Map.unions . (\(a,b) -> a++b) . unzip . Map.elems) atts
                              {-# LINE 1025 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 81, column 23)
                         _nontsOnewAtts =
                             ({-# LINE 81 "src-ag/AG2AspectAG.ag" #-}
                              _newAtts
                              {-# LINE 1031 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 87, column 23)
                         _newProds =
                             ({-# LINE 87 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                     (_,prods,_) -> prods
                              {-# LINE 1038 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 89, column 23)
                         _nontsOnewProds =
                             ({-# LINE 89 "src-ag/AG2AspectAG.ag" #-}
                              _newProds
                              {-# LINE 1044 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 111, column 23)
                         _nontsOnewNTs =
                             ({-# LINE 111 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (newNTs,_,_) -> Set.difference newNTs _nontsIextendedNTs
                              {-# LINE 1051 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 126, column 25)
                         _lhsOimp =
                             ({-# LINE 126 "src-ag/AG2AspectAG.ag" #-}
                              "import Language.Grammars.AspectAG" >-<
                              "import Language.Grammars.AspectAG.Derive" >-<
                              "import Data.HList.Label4" >-<
                              "import Data.HList.TypeEqGeneric1" >-<
                              "import Data.HList.TypeCastGeneric1" >-<
                              maybe empty ("import qualified" >#<) _lhsIext >-<
                              maybe empty (\ext -> "import" >#< ext >#< ppListSep "(" ")" "," (_nontsIppDI ++ _nontsIppLI ++ _ppAI    )) _lhsIext
                              {-# LINE 1063 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 139, column 25)
                         _lhsOpp =
                             ({-# LINE 139 "src-ag/AG2AspectAG.ag" #-}
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
                              {-# LINE 1086 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 201, column 25)
                         _nontsOderivs =
                             ({-# LINE 201 "src-ag/AG2AspectAG.ag" #-}
                              derivings_
                              {-# LINE 1092 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 249, column 34)
                         _nontsOtSyns =
                             ({-# LINE 249 "src-ag/AG2AspectAG.ag" #-}
                              typeSyns_
                              {-# LINE 1098 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 298, column 25)
                         _ppA =
                             ({-# LINE 298 "src-ag/AG2AspectAG.ag" #-}
                              vlist (map defAtt (filterAtts _newAtts     _o_noGroup    )) >-<
                              defAtt "loc" >-<
                              (case _lhsIext of
                                Nothing    ->  defAtt "inh" >-< defAtt "syn"
                                otherwise  ->  empty) >-<
                              _nontsIppA
                              {-# LINE 1109 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 305, column 25)
                         _ppAI =
                             ({-# LINE 305 "src-ag/AG2AspectAG.ag" #-}
                              let atts =  filterNotAtts _newAtts     _o_noGroup
                              in  (foldr (\a as -> attName a : as) [] atts) ++
                                  (foldr (\a as -> attTName a : as) [] atts) ++
                                  (case _lhsIext of
                                    Nothing    ->  []
                                    otherwise  ->  [ attName "inh", attName "syn", attTName "inh", attTName "syn" ]) ++
                                  _nontsIppAI
                              {-# LINE 1121 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 385, column 25)
                         _ppNtL =
                             ({-# LINE 385 "src-ag/AG2AspectAG.ag" #-}
                              _nontsIppNtL
                              {-# LINE 1127 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 386, column 25)
                         _ppR =
                             ({-# LINE 386 "src-ag/AG2AspectAG.ag" #-}
                              ntsList "group" _ppNtL      >-<
                              vlist (map (\att -> ntsList att (filterNts att _ppNtL    )) (filterAtts _newAtts _o_noGroup    ))  >-<
                              _nontsIppR
                              {-# LINE 1135 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 15, column 13)
                         _nontsOinhMap =
                             ({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                              _nontsIinhMap'
                              {-# LINE 1141 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/DistChildAttr.ag"(line 16, column 13)
                         _nontsOsynMap =
                             ({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                              _nontsIsynMap'
                              {-# LINE 1147 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOext =
                             ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                              _lhsIext
                              {-# LINE 1153 "src-ag/AG2AspectAG.hs" #-}
                              )
                         ( _nontsIextendedNTs,_nontsIinhMap',_nontsIppA,_nontsIppAI,_nontsIppCata,_nontsIppD,_nontsIppDI,_nontsIppL,_nontsIppLI,_nontsIppNtL,_nontsIppR,_nontsIppSF,_nontsIppW,_nontsIsynMap') =
                             nonts_ _nontsOderivs _nontsOext _nontsOinhMap _nontsOnewAtts _nontsOnewNTs _nontsOnewProds _nontsOo_noGroup _nontsOo_rename _nontsOsynMap _nontsOtSyns 
                     in  ( _lhsOimp,_lhsOpp))) )
-- HsToken -----------------------------------------------------
{-
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken  ->
               T_HsToken 
sem_HsToken (AGField _field _attr _pos _rdesc )  =
    (sem_HsToken_AGField _field _attr _pos _rdesc )
sem_HsToken (AGLocal _var _pos _rdesc )  =
    (sem_HsToken_AGLocal _var _pos _rdesc )
sem_HsToken (CharToken _value _pos )  =
    (sem_HsToken_CharToken _value _pos )
sem_HsToken (Err _mesg _pos )  =
    (sem_HsToken_Err _mesg _pos )
sem_HsToken (HsToken _value _pos )  =
    (sem_HsToken_HsToken _value _pos )
sem_HsToken (StrToken _value _pos )  =
    (sem_HsToken_StrToken _value _pos )
-- semantic domain
newtype T_HsToken  = T_HsToken (( ))
data Inh_HsToken  = Inh_HsToken {}
data Syn_HsToken  = Syn_HsToken {}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken (T_HsToken sem ) (Inh_HsToken )  =
    (let ( ) = sem 
     in  (Syn_HsToken ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField field_ attr_ pos_ rdesc_  =
    (T_HsToken (let 
                in  ( )) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal var_ pos_ rdesc_  =
    (T_HsToken (let 
                in  ( )) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken value_ pos_  =
    (T_HsToken (let 
                in  ( )) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err mesg_ pos_  =
    (T_HsToken (let 
                in  ( )) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken value_ pos_  =
    (T_HsToken (let 
                in  ( )) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken value_ pos_  =
    (T_HsToken (let 
                in  ( )) )
-- HsTokens ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
-- cata
sem_HsTokens :: HsTokens  ->
                T_HsTokens 
sem_HsTokens list  =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list) )
-- semantic domain
newtype T_HsTokens  = T_HsTokens (( ))
data Inh_HsTokens  = Inh_HsTokens {}
data Syn_HsTokens  = Syn_HsTokens {}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens (T_HsTokens sem ) (Inh_HsTokens )  =
    (let ( ) = sem 
     in  (Syn_HsTokens ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons (T_HsToken hd_ ) (T_HsTokens tl_ )  =
    (T_HsTokens (let 
                 in  ( )) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (let 
                 in  ( )) )
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot  ->
                    T_HsTokensRoot 
sem_HsTokensRoot (HsTokensRoot _tokens )  =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens ) )
-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot (( ))
data Inh_HsTokensRoot  = Inh_HsTokensRoot {}
data Syn_HsTokensRoot  = Syn_HsTokensRoot {}
wrap_HsTokensRoot :: T_HsTokensRoot  ->
                     Inh_HsTokensRoot  ->
                     Syn_HsTokensRoot 
wrap_HsTokensRoot (T_HsTokensRoot sem ) (Inh_HsTokensRoot )  =
    (let ( ) = sem 
     in  (Syn_HsTokensRoot ))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  ->
                                 T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_ )  =
    (T_HsTokensRoot (let 
                     in  ( )) )
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
sem_Nonterminal :: Nonterminal  ->
                   T_Nonterminal 
sem_Nonterminal (Nonterminal _nt _params _inh _syn _prods )  =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods ) )
-- semantic domain
newtype T_Nonterminal  = T_Nonterminal (Derivings ->
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
data Inh_Nonterminal  = Inh_Nonterminal {derivs_Inh_Nonterminal :: Derivings,ext_Inh_Nonterminal :: (Maybe String),inhMap_Inh_Nonterminal :: (Map Identifier Attributes),newAtts_Inh_Nonterminal :: ( Attributes ),newNTs_Inh_Nonterminal :: (Set NontermIdent),newProds_Inh_Nonterminal :: ( DataTypes ),o_noGroup_Inh_Nonterminal :: ([String]),o_rename_Inh_Nonterminal :: Bool,synMap_Inh_Nonterminal :: (Map Identifier Attributes),tSyns_Inh_Nonterminal :: TypeSyns}
data Syn_Nonterminal  = Syn_Nonterminal {extendedNTs_Syn_Nonterminal :: (Set NontermIdent),inhMap'_Syn_Nonterminal :: (Map Identifier Attributes),ppA_Syn_Nonterminal :: PP_Doc,ppAI_Syn_Nonterminal :: ([PP_Doc]),ppCata_Syn_Nonterminal :: PP_Doc,ppD_Syn_Nonterminal :: PP_Doc,ppDI_Syn_Nonterminal :: ([PP_Doc]),ppL_Syn_Nonterminal :: PP_Doc,ppLI_Syn_Nonterminal :: ([PP_Doc]),ppNtL_Syn_Nonterminal :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminal :: PP_Doc,ppSF_Syn_Nonterminal :: PP_Doc,ppW_Syn_Nonterminal :: PP_Doc,synMap'_Syn_Nonterminal :: (Map Identifier Attributes)}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns )  =
    (let ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap') = sem _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns 
     in  (Syn_Nonterminal _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap' ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
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
                             _prodsIppL :: PP_Doc
                             _prodsIppLI :: ([PP_Doc])
                             _prodsIppR :: PP_Doc
                             _prodsIppRA :: ([PP_Doc])
                             _prodsIppSF :: PP_Doc
                             _prodsIppSPF :: PP_Doc
                             _prodsIprdInh :: Attributes
                             -- "src-ag/AG2AspectAG.ag"(line 50, column 18)
                             _inhNoGroup =
                                 ({-# LINE 50 "src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) _prodsIprdInh
                                  {-# LINE 1431 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 51, column 18)
                             _synNoGroup =
                                 ({-# LINE 51 "src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) syn_
                                  {-# LINE 1437 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 56, column 18)
                             _prodsOinhNoGroup =
                                 ({-# LINE 56 "src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _inhNoGroup
                                  {-# LINE 1443 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 57, column 18)
                             _prodsOsynNoGroup =
                                 ({-# LINE 57 "src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _synNoGroup
                                  {-# LINE 1449 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 93, column 17)
                             _prodsOnewProds =
                                 ({-# LINE 93 "src-ag/AG2AspectAG.ag" #-}
                                  case Map.lookup nt_ _lhsInewProds of
                                         Just prds -> prds
                                         Nothing   -> Map.empty
                                  {-# LINE 1457 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 106, column 31)
                             _lhsOextendedNTs =
                                 ({-# LINE 106 "src-ag/AG2AspectAG.ag" #-}
                                  if _prodsIhasMoreProds
                                  then Set.singleton nt_
                                  else Set.empty
                                  {-# LINE 1465 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 172, column 25)
                             _ppNt =
                                 ({-# LINE 172 "src-ag/AG2AspectAG.ag" #-}
                                  pp nt_
                                  {-# LINE 1471 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 189, column 25)
                             _prodsOppNt =
                                 ({-# LINE 189 "src-ag/AG2AspectAG.ag" #-}
                                  _ppNt
                                  {-# LINE 1477 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 207, column 25)
                             _lhsOppD =
                                 ({-# LINE 207 "src-ag/AG2AspectAG.ag" #-}
                                  if (Set.member nt_ _lhsInewNTs)
                                  then  case (lookup nt_ _lhsItSyns) of
                                                 Nothing ->  "data " >|< _ppNt
                                                 Just tp ->  "type " >|< _ppNt     >|< " = " >|< ppShow tp
                                  else  empty
                                  {-# LINE 1487 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 220, column 25)
                             _lhsOppDI =
                                 ({-# LINE 220 "src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then  [ _ppNt     ]
                                  else  [ ]
                                  {-# LINE 1495 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 260, column 25)
                             _ntLabel =
                                 ({-# LINE 260 "src-ag/AG2AspectAG.ag" #-}
                                  "nt_" >|< _ppNt
                                  {-# LINE 1501 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 262, column 25)
                             _lhsOppL =
                                 ({-# LINE 262 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then _ntLabel     >|< " = proxy :: Proxy " >|< _ppNt
                                    else empty)  >-<
                                  _prodsIppL
                                  {-# LINE 1510 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 267, column 25)
                             _lhsOppLI =
                                 ({-# LINE 267 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (not $ Set.member nt_ _lhsInewNTs)
                                    then [ _ntLabel     ]
                                    else [ ])  ++
                                  _prodsIppLI
                                  {-# LINE 1519 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 318, column 25)
                             _lhsOppA =
                                 ({-# LINE 318 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then   defAttRec (pp "InhG") _ppNt     inh_ _inhNoGroup     >-<
                                           defAttRec (pp "SynG") _ppNt     syn_ _synNoGroup
                                    else   empty) >-<
                                  _prodsIppA
                                  {-# LINE 1529 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 331, column 25)
                             _lhsOppAI =
                                 ({-# LINE 331 "src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then [ ppName [(pp "InhG"), _ppNt     ] >#< pp "(..)", ppName [(pp "SynG"), _ppNt     ] >#< pp "(..)" ]
                                  else [ ]
                                  {-# LINE 1537 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 399, column 25)
                             _lhsOppNtL =
                                 ({-# LINE 399 "src-ag/AG2AspectAG.ag" #-}
                                  [ ("nt_" >|< nt_, Map.union inh_ syn_) ]
                                  {-# LINE 1543 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 408, column 25)
                             _prodsOnewNT =
                                 ({-# LINE 408 "src-ag/AG2AspectAG.ag" #-}
                                  Set.member nt_ _lhsInewNTs
                                  {-# LINE 1549 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 418, column 25)
                             _lhsOppR =
                                 ({-# LINE 418 "src-ag/AG2AspectAG.ag" #-}
                                  pp "----" >|< pp nt_ >-< _prodsIppR
                                  {-# LINE 1555 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 728, column 25)
                             _lhsOppCata =
                                 ({-# LINE 728 "src-ag/AG2AspectAG.ag" #-}
                                  "----" >|< _ppNt     >-< _prodsIppCata
                                  {-# LINE 1561 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 759, column 25)
                             _prodsOsyn =
                                 ({-# LINE 759 "src-ag/AG2AspectAG.ag" #-}
                                  syn_
                                  {-# LINE 1567 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 760, column 25)
                             _prodsOinh =
                                 ({-# LINE 760 "src-ag/AG2AspectAG.ag" #-}
                                  inh_
                                  {-# LINE 1573 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 771, column 25)
                             _lhsOppSF =
                                 ({-# LINE 771 "src-ag/AG2AspectAG.ag" #-}
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
                                           "instance SemType T_" >|< _ppNt     >|< " " >|< _ppNt     >-<
                                           "-- sem_" >|< _ppNt     >|< " :: " >|< _ppNt     >|< " -> T_" >|<  _ppNt     >-<
                                           _prodsIppSPF
                                  {-# LINE 1594 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 839, column 25)
                             _lhsOppW =
                                 ({-# LINE 839 "src-ag/AG2AspectAG.ag" #-}
                                  ppName [pp "wrap", _ppNt    ] >|< " sem " >|< attVars inh_ >|< " = " >-<
                                  "   sem " >|< attFields inh_ _inhNoGroup     _ppNt
                                  {-# LINE 1601 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 7, column 18)
                             _lhsOinhMap' =
                                 ({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ inh_
                                  {-# LINE 1607 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/DistChildAttr.ag"(line 8, column 18)
                             _lhsOsynMap' =
                                 ({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                                  Map.singleton nt_ syn_
                                  {-# LINE 1613 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOext =
                                 ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 1619 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 1625 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOnewAtts =
                                 ({-# LINE 77 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 1631 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_noGroup =
                                 ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 1637 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_rename =
                                 ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 1643 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 1649 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             ( _prodsIhasMoreProds,_prodsIppA,_prodsIppCata,_prodsIppL,_prodsIppLI,_prodsIppR,_prodsIppRA,_prodsIppSF,_prodsIppSPF,_prodsIprdInh) =
                                 prods_ _prodsOext _prodsOinh _prodsOinhMap _prodsOinhNoGroup _prodsOnewAtts _prodsOnewNT _prodsOnewProds _prodsOo_noGroup _prodsOo_rename _prodsOppNt _prodsOsyn _prodsOsynMap _prodsOsynNoGroup 
                         in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))) )
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
sem_Nonterminals :: Nonterminals  ->
                    T_Nonterminals 
sem_Nonterminals list  =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list) )
-- semantic domain
newtype T_Nonterminals  = T_Nonterminals (Derivings ->
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
data Inh_Nonterminals  = Inh_Nonterminals {derivs_Inh_Nonterminals :: Derivings,ext_Inh_Nonterminals :: (Maybe String),inhMap_Inh_Nonterminals :: (Map Identifier Attributes),newAtts_Inh_Nonterminals :: ( Attributes ),newNTs_Inh_Nonterminals :: (Set NontermIdent),newProds_Inh_Nonterminals :: ( DataTypes ),o_noGroup_Inh_Nonterminals :: ([String]),o_rename_Inh_Nonterminals :: Bool,synMap_Inh_Nonterminals :: (Map Identifier Attributes),tSyns_Inh_Nonterminals :: TypeSyns}
data Syn_Nonterminals  = Syn_Nonterminals {extendedNTs_Syn_Nonterminals :: (Set NontermIdent),inhMap'_Syn_Nonterminals :: (Map Identifier Attributes),ppA_Syn_Nonterminals :: PP_Doc,ppAI_Syn_Nonterminals :: ([PP_Doc]),ppCata_Syn_Nonterminals :: PP_Doc,ppD_Syn_Nonterminals :: PP_Doc,ppDI_Syn_Nonterminals :: ([PP_Doc]),ppL_Syn_Nonterminals :: PP_Doc,ppLI_Syn_Nonterminals :: ([PP_Doc]),ppNtL_Syn_Nonterminals :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminals :: PP_Doc,ppSF_Syn_Nonterminals :: PP_Doc,ppW_Syn_Nonterminals :: PP_Doc,synMap'_Syn_Nonterminals :: (Map Identifier Attributes)}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns )  =
    (let ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap') = sem _lhsIderivs _lhsIext _lhsIinhMap _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIsynMap _lhsItSyns 
     in  (Syn_Nonterminals _lhsOextendedNTs _lhsOinhMap' _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW _lhsOsynMap' ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
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
                              -- use rule "src-ag/AG2AspectAG.ag"(line 104, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 104 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIextendedNTs `Set.union` _tlIextendedNTs
                                   {-# LINE 1794 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIinhMap' `Map.union` _tlIinhMap'
                                   {-# LINE 1800 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 315, column 64)
                              _lhsOppA =
                                  ({-# LINE 315 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppA >-< _tlIppA
                                   {-# LINE 1806 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 328, column 42)
                              _lhsOppAI =
                                  ({-# LINE 328 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppAI ++ _tlIppAI
                                   {-# LINE 1812 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 725, column 67)
                              _lhsOppCata =
                                  ({-# LINE 725 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppCata >-< _tlIppCata
                                   {-# LINE 1818 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 204, column 41)
                              _lhsOppD =
                                  ({-# LINE 204 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppD >-< _tlIppD
                                   {-# LINE 1824 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 204, column 75)
                              _lhsOppDI =
                                  ({-# LINE 204 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppDI ++ _tlIppDI
                                   {-# LINE 1830 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                              _lhsOppL =
                                  ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppL >-< _tlIppL
                                   {-# LINE 1836 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                              _lhsOppLI =
                                  ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppLI ++ _tlIppLI
                                   {-# LINE 1842 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 396, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 396 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppNtL ++ _tlIppNtL
                                   {-# LINE 1848 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                              _lhsOppR =
                                  ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppR >-< _tlIppR
                                   {-# LINE 1854 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 766, column 66)
                              _lhsOppSF =
                                  ({-# LINE 766 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppSF >-< _tlIppSF
                                   {-# LINE 1860 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 836, column 42)
                              _lhsOppW =
                                  ({-# LINE 836 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppW >-< _tlIppW
                                   {-# LINE 1866 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   _hdIsynMap' `Map.union` _tlIsynMap'
                                   {-# LINE 1872 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOderivs =
                                  ({-# LINE 198 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 1878 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOext =
                                  ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 1884 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1890 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewAtts =
                                  ({-# LINE 77 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 1896 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewNTs =
                                  ({-# LINE 98 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 1902 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewProds =
                                  ({-# LINE 84 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 1908 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_noGroup =
                                  ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 1914 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_rename =
                                  ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1920 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1926 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOtSyns =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 1932 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOderivs =
                                  ({-# LINE 198 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 1938 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOext =
                                  ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 1944 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOinhMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 1950 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewAtts =
                                  ({-# LINE 77 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 1956 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewNTs =
                                  ({-# LINE 98 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 1962 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewProds =
                                  ({-# LINE 84 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 1968 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_noGroup =
                                  ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 1974 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_rename =
                                  ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1980 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOsynMap =
                                  ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIsynMap
                                   {-# LINE 1986 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOtSyns =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 1992 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              ( _hdIextendedNTs,_hdIinhMap',_hdIppA,_hdIppAI,_hdIppCata,_hdIppD,_hdIppDI,_hdIppL,_hdIppLI,_hdIppNtL,_hdIppR,_hdIppSF,_hdIppW,_hdIsynMap') =
                                  hd_ _hdOderivs _hdOext _hdOinhMap _hdOnewAtts _hdOnewNTs _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOsynMap _hdOtSyns 
                              ( _tlIextendedNTs,_tlIinhMap',_tlIppA,_tlIppAI,_tlIppCata,_tlIppD,_tlIppDI,_tlIppL,_tlIppLI,_tlIppNtL,_tlIppR,_tlIppSF,_tlIppW,_tlIsynMap') =
                                  tl_ _tlOderivs _tlOext _tlOinhMap _tlOnewAtts _tlOnewNTs _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOsynMap _tlOtSyns 
                          in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
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
                              -- use rule "src-ag/AG2AspectAG.ag"(line 104, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 104 "src-ag/AG2AspectAG.ag" #-}
                                   Set.empty
                                   {-# LINE 2029 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOinhMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2035 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 315, column 64)
                              _lhsOppA =
                                  ({-# LINE 315 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2041 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 328, column 42)
                              _lhsOppAI =
                                  ({-# LINE 328 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2047 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 725, column 67)
                              _lhsOppCata =
                                  ({-# LINE 725 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2053 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 204, column 41)
                              _lhsOppD =
                                  ({-# LINE 204 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2059 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 204, column 75)
                              _lhsOppDI =
                                  ({-# LINE 204 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2065 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                              _lhsOppL =
                                  ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2071 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                              _lhsOppLI =
                                  ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2077 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 396, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 396 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 2083 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                              _lhsOppR =
                                  ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2089 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 766, column 66)
                              _lhsOppSF =
                                  ({-# LINE 766 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2095 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 836, column 42)
                              _lhsOppW =
                                  ({-# LINE 836 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 2101 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/DistChildAttr.ag"(line 4, column 53)
                              _lhsOsynMap' =
                                  ({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                   Map.empty
                                   {-# LINE 2107 "src-ag/AG2AspectAG.hs" #-}
                                   )
                          in  ( _lhsOextendedNTs,_lhsOinhMap',_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW,_lhsOsynMap'))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         info                 : (Identifier, Identifier)
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Alias _field _attr _pat )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) )
sem_Pattern (Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern (Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern (Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern (Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( Pattern ,((Identifier, Identifier))))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: Pattern ,info_Syn_Pattern :: ((Identifier, Identifier))}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOinfo) = sem 
     in  (Syn_Pattern _lhsOcopy _lhsOinfo ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIinfo :: ((Identifier, Identifier))
                    -- "src-ag/AG2AspectAG.ag"(line 375, column 25)
                    _lhsOinfo =
                        ({-# LINE 375 "src-ag/AG2AspectAG.ag" #-}
                         (field_, attr_)
                         {-# LINE 2178 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 2184 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2190 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patIcopy,_patIinfo) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    -- "src-ag/AG2AspectAG.ag"(line 376, column 25)
                    _lhsOinfo =
                        ({-# LINE 376 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Constr undefined!!"
                         {-# LINE 2206 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 2212 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2218 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patsIcopy) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOcopy :: Pattern 
                    _lhsOinfo :: ((Identifier, Identifier))
                    _patIcopy :: Pattern 
                    _patIinfo :: ((Identifier, Identifier))
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 2234 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2240 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOinfo =
                        ({-# LINE 373 "src-ag/AG2AspectAG.ag" #-}
                         _patIinfo
                         {-# LINE 2246 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patIcopy,_patIinfo) =
                        pat_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    -- "src-ag/AG2AspectAG.ag"(line 377, column 25)
                    _lhsOinfo =
                        ({-# LINE 377 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Product undefined!!"
                         {-# LINE 2262 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 2268 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2274 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patsIcopy) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    -- "src-ag/AG2AspectAG.ag"(line 378, column 25)
                    _lhsOinfo =
                        ({-# LINE 378 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Underscore undefined!!"
                         {-# LINE 2288 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 2294 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2300 "src-ag/AG2AspectAG.hs" #-}
                         )
                in  ( _lhsOcopy,_lhsOinfo)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
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
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns ))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: Patterns }
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy) = sem 
     in  (Syn_Patterns _lhsOcopy ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOcopy :: Patterns 
                     _hdIcopy :: Pattern 
                     _hdIinfo :: ((Identifier, Identifier))
                     _tlIcopy :: Patterns 
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 2345 "src-ag/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2351 "src-ag/AG2AspectAG.hs" #-}
                          )
                     ( _hdIcopy,_hdIinfo) =
                         hd_ 
                     ( _tlIcopy) =
                         tl_ 
                 in  ( _lhsOcopy)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOcopy :: Patterns 
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 2365 "src-ag/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2371 "src-ag/AG2AspectAG.hs" #-}
                          )
                 in  ( _lhsOcopy)) )
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
            local _tup1       : _
            local ppR         : _
            local ppRA        : _
-}
-- cata
sem_Production :: Production  ->
                  T_Production 
sem_Production (Production _con _params _constraints _children _rules _typeSigs _macro )  =
    (sem_Production_Production _con _params _constraints (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) _macro )
-- semantic domain
newtype T_Production  = T_Production ((Maybe String) ->
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
                                      ( ( Bool ),PP_Doc,PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,Attributes))
data Inh_Production  = Inh_Production {ext_Inh_Production :: (Maybe String),inh_Inh_Production :: ( Attributes ),inhMap_Inh_Production :: (Map Identifier Attributes),inhNoGroup_Inh_Production :: ([String]),newAtts_Inh_Production :: ( Attributes ),newNT_Inh_Production :: Bool,newProds_Inh_Production :: ( Map.Map ConstructorIdent FieldMap ),o_noGroup_Inh_Production :: ([String]),o_rename_Inh_Production :: Bool,ppNt_Inh_Production :: PP_Doc,syn_Inh_Production :: ( Attributes ),synMap_Inh_Production :: (Map Identifier Attributes),synNoGroup_Inh_Production :: ([String])}
data Syn_Production  = Syn_Production {hasMoreProds_Syn_Production :: ( Bool ),ppA_Syn_Production :: PP_Doc,ppCata_Syn_Production :: PP_Doc,ppL_Syn_Production :: PP_Doc,ppLI_Syn_Production :: ([PP_Doc]),ppR_Syn_Production :: PP_Doc,ppRA_Syn_Production :: ([PP_Doc]),ppSF_Syn_Production :: PP_Doc,ppSPF_Syn_Production :: PP_Doc,prdInh_Syn_Production :: Attributes}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup )  =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh) = sem _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup 
     in  (Syn_Production _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh ))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             MaybeMacro ->
                             T_Production 
sem_Production_Production con_ params_ constraints_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ ) macro_  =
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
                            _lhsOppL :: PP_Doc
                            _lhsOppLI :: ([PP_Doc])
                            _lhsOppA :: PP_Doc
                            _lhsOppCata :: PP_Doc
                            _lhsOppSF :: PP_Doc
                            _lhsOppSPF :: PP_Doc
                            _lhsOppR :: PP_Doc
                            _lhsOppRA :: ([PP_Doc])
                            _lhsOprdInh :: Attributes
                            _childrenOext :: (Maybe String)
                            _childrenOinhMap :: (Map Identifier Attributes)
                            _childrenOinhNoGroup :: ([String])
                            _childrenOo_noGroup :: ([String])
                            _childrenOo_rename :: Bool
                            _childrenOppNt :: PP_Doc
                            _childrenOsynMap :: (Map Identifier Attributes)
                            _childrenOsynNoGroup :: ([String])
                            _rulesOext :: (Maybe String)
                            _rulesOinhNoGroup :: ([String])
                            _rulesOnewProd :: Bool
                            _rulesOo_noGroup :: ([String])
                            _rulesOppNt :: PP_Doc
                            _rulesOsynNoGroup :: ([String])
                            _childrenIidCL :: ([(Identifier,Type)])
                            _childrenIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                            _childrenIppL :: PP_Doc
                            _childrenIppLI :: ([PP_Doc])
                            _childrenIppR :: PP_Doc
                            _childrenIprdInh :: Attributes
                            _rulesIlocals :: ([Identifier])
                            _rulesIppRL :: ([ PPRule ])
                            -- "src-ag/AG2AspectAG.ag"(line 102, column 29)
                            _lhsOhasMoreProds =
                                ({-# LINE 102 "src-ag/AG2AspectAG.ag" #-}
                                 not $ Map.member con_ _lhsInewProds
                                 {-# LINE 2508 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 175, column 25)
                            _ppProd =
                                ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                                 pp con_
                                 {-# LINE 2514 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 176, column 25)
                            _prodName =
                                ({-# LINE 176 "src-ag/AG2AspectAG.ag" #-}
                                 ppName [_lhsIppNt, _ppProd    ]
                                 {-# LINE 2520 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 177, column 25)
                            _conName =
                                ({-# LINE 177 "src-ag/AG2AspectAG.ag" #-}
                                 if _lhsIo_rename
                                 then _prodName
                                 else _ppProd
                                 {-# LINE 2528 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 194, column 25)
                            _childrenOppProd =
                                ({-# LINE 194 "src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2534 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 195, column 25)
                            _rulesOppProd =
                                ({-# LINE 195 "src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2540 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 273, column 25)
                            _lhsOppL =
                                ({-# LINE 273 "src-ag/AG2AspectAG.ag" #-}
                                 if (Map.member con_ _lhsInewProds)
                                   then _childrenIppL
                                   else empty
                                 {-# LINE 2548 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 277, column 25)
                            _lhsOppLI =
                                ({-# LINE 277 "src-ag/AG2AspectAG.ag" #-}
                                 if (not $ Map.member con_ _lhsInewProds)
                                   then _childrenIppLI
                                   else []
                                 {-# LINE 2556 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 325, column 25)
                            _lhsOppA =
                                ({-# LINE 325 "src-ag/AG2AspectAG.ag" #-}
                                 defLocalAtts _prodName     (length _rulesIlocals) 1 $ sort _rulesIlocals
                                 {-# LINE 2562 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 421, column 25)
                            _newProd =
                                ({-# LINE 421 "src-ag/AG2AspectAG.ag" #-}
                                 Map.member con_ _lhsInewProds
                                 {-# LINE 2568 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 422, column 41)
                            __tup1 =
                                ({-# LINE 422 "src-ag/AG2AspectAG.ag" #-}
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
                                 {-# LINE 2591 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 422, column 41)
                            (_ppR,_) =
                                ({-# LINE 422 "src-ag/AG2AspectAG.ag" #-}
                                 __tup1
                                 {-# LINE 2597 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 422, column 41)
                            (_,_ppRA) =
                                ({-# LINE 422 "src-ag/AG2AspectAG.ag" #-}
                                 __tup1
                                 {-# LINE 2603 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 732, column 25)
                            _lhsOppCata =
                                ({-# LINE 732 "src-ag/AG2AspectAG.ag" #-}
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
                                 {-# LINE 2621 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 796, column 25)
                            _lhsOppSF =
                                ({-# LINE 796 "src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppPattern = case (show con_) of
                                                   "Cons"    -> ppParams (ppListSep "" "" " : ")
                                                   "Nil"     -> pp "[]"
                                                   otherwise -> _conName     >|< " " >|< (ppParams ppSpaced)
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "sem_" >|< _lhsIppNt >|< " (" >|< ppPattern >|< ") = sem_" >|< _prodName     >|<
                                      " (" >|< map (fst . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2634 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 808, column 25)
                            _lhsOppSPF =
                                ({-# LINE 808 "src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "sem_" >|< _lhsIppNt >|< "_" >|< con_ >#< ppParams ppSpaced >|< " = semP_" >|< _prodName     >|<
                                      " (" >|< map (snd . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2643 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                            _lhsOppR =
                                ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                                 _ppR
                                 {-# LINE 2649 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "src-ag/AG2AspectAG.ag"(line 412, column 43)
                            _lhsOppRA =
                                ({-# LINE 412 "src-ag/AG2AspectAG.ag" #-}
                                 _ppRA
                                 {-# LINE 2655 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "src-ag/AG2AspectAG.ag"(line 64, column 57)
                            _lhsOprdInh =
                                ({-# LINE 64 "src-ag/AG2AspectAG.ag" #-}
                                 _childrenIprdInh
                                 {-# LINE 2661 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOext =
                                ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2667 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 2673 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhNoGroup =
                                ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2679 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_noGroup =
                                ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_noGroup
                                 {-# LINE 2685 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_rename =
                                ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_rename
                                 {-# LINE 2691 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOppNt =
                                ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2697 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynMap =
                                ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIsynMap
                                 {-# LINE 2703 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynNoGroup =
                                ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2709 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOext =
                                ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2715 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinhNoGroup =
                                ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2721 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (from local)
                            _rulesOnewProd =
                                ({-# LINE 405 "src-ag/AG2AspectAG.ag" #-}
                                 _newProd
                                 {-# LINE 2727 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOo_noGroup =
                                ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_noGroup
                                 {-# LINE 2733 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOppNt =
                                ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2739 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsynNoGroup =
                                ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2745 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            ( _childrenIidCL,_childrenIppCSF,_childrenIppL,_childrenIppLI,_childrenIppR,_childrenIprdInh) =
                                children_ _childrenOext _childrenOinhMap _childrenOinhNoGroup _childrenOo_noGroup _childrenOo_rename _childrenOppNt _childrenOppProd _childrenOsynMap _childrenOsynNoGroup 
                            ( _rulesIlocals,_rulesIppRL) =
                                rules_ _rulesOext _rulesOinhNoGroup _rulesOnewProd _rulesOo_noGroup _rulesOppNt _rulesOppProd _rulesOsynNoGroup 
                        in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))) )
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
sem_Productions :: Productions  ->
                   T_Productions 
sem_Productions list  =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list) )
-- semantic domain
newtype T_Productions  = T_Productions ((Maybe String) ->
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
                                        ( ( Bool ),PP_Doc,PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,Attributes))
data Inh_Productions  = Inh_Productions {ext_Inh_Productions :: (Maybe String),inh_Inh_Productions :: ( Attributes ),inhMap_Inh_Productions :: (Map Identifier Attributes),inhNoGroup_Inh_Productions :: ([String]),newAtts_Inh_Productions :: ( Attributes ),newNT_Inh_Productions :: Bool,newProds_Inh_Productions :: ( Map.Map ConstructorIdent FieldMap ),o_noGroup_Inh_Productions :: ([String]),o_rename_Inh_Productions :: Bool,ppNt_Inh_Productions :: PP_Doc,syn_Inh_Productions :: ( Attributes ),synMap_Inh_Productions :: (Map Identifier Attributes),synNoGroup_Inh_Productions :: ([String])}
data Syn_Productions  = Syn_Productions {hasMoreProds_Syn_Productions :: ( Bool ),ppA_Syn_Productions :: PP_Doc,ppCata_Syn_Productions :: PP_Doc,ppL_Syn_Productions :: PP_Doc,ppLI_Syn_Productions :: ([PP_Doc]),ppR_Syn_Productions :: PP_Doc,ppRA_Syn_Productions :: ([PP_Doc]),ppSF_Syn_Productions :: PP_Doc,ppSPF_Syn_Productions :: PP_Doc,prdInh_Syn_Productions :: Attributes}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup )  =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh) = sem _lhsIext _lhsIinh _lhsIinhMap _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynMap _lhsIsynNoGroup 
     in  (Syn_Productions _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF _lhsOprdInh ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
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
                             _tlIppL :: PP_Doc
                             _tlIppLI :: ([PP_Doc])
                             _tlIppR :: PP_Doc
                             _tlIppRA :: ([PP_Doc])
                             _tlIppSF :: PP_Doc
                             _tlIppSPF :: PP_Doc
                             _tlIprdInh :: Attributes
                             -- "src-ag/AG2AspectAG.ag"(line 61, column 11)
                             _hdOinhNoGroup =
                                 ({-# LINE 61 "src-ag/AG2AspectAG.ag" #-}
                                  filter (flip Map.member _hdIprdInh . identifier) _lhsIinhNoGroup
                                  {-# LINE 2891 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 100, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 100 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIhasMoreProds  ||  _tlIhasMoreProds
                                  {-# LINE 2897 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 315, column 64)
                             _lhsOppA =
                                 ({-# LINE 315 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppA >-< _tlIppA
                                  {-# LINE 2903 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 725, column 67)
                             _lhsOppCata =
                                 ({-# LINE 725 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppCata >-< _tlIppCata
                                  {-# LINE 2909 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                             _lhsOppL =
                                 ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppL >-< _tlIppL
                                  {-# LINE 2915 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                             _lhsOppLI =
                                 ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppLI ++ _tlIppLI
                                  {-# LINE 2921 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                             _lhsOppR =
                                 ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppR >-< _tlIppR
                                  {-# LINE 2927 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 412, column 43)
                             _lhsOppRA =
                                 ({-# LINE 412 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppRA ++ _tlIppRA
                                  {-# LINE 2933 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 766, column 66)
                             _lhsOppSF =
                                 ({-# LINE 766 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSF >-< _tlIppSF
                                  {-# LINE 2939 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 767, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 767 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSPF >-< _tlIppSPF
                                  {-# LINE 2945 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 64, column 57)
                             _lhsOprdInh =
                                 ({-# LINE 64 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIprdInh `Map.union` _tlIprdInh
                                  {-# LINE 2951 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOext =
                                 ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 2957 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 756 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2963 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 2969 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewAtts =
                                 ({-# LINE 77 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 2975 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewNT =
                                 ({-# LINE 404 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 2981 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewProds =
                                 ({-# LINE 85 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 2987 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_noGroup =
                                 ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 2993 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_rename =
                                 ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 2999 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOppNt =
                                 ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 3005 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 756 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 3011 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 3017 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynNoGroup =
                                 ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 3023 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOext =
                                 ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 3029 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 756 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 3035 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIinhMap
                                  {-# LINE 3041 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhNoGroup =
                                 ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinhNoGroup
                                  {-# LINE 3047 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewAtts =
                                 ({-# LINE 77 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 3053 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewNT =
                                 ({-# LINE 404 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 3059 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewProds =
                                 ({-# LINE 85 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 3065 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_noGroup =
                                 ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_noGroup
                                  {-# LINE 3071 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_rename =
                                 ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 3077 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOppNt =
                                 ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 3083 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 756 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 3089 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynMap =
                                 ({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                  _lhsIsynMap
                                  {-# LINE 3095 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynNoGroup =
                                 ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 3101 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             ( _hdIhasMoreProds,_hdIppA,_hdIppCata,_hdIppL,_hdIppLI,_hdIppR,_hdIppRA,_hdIppSF,_hdIppSPF,_hdIprdInh) =
                                 hd_ _hdOext _hdOinh _hdOinhMap _hdOinhNoGroup _hdOnewAtts _hdOnewNT _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOppNt _hdOsyn _hdOsynMap _hdOsynNoGroup 
                             ( _tlIhasMoreProds,_tlIppA,_tlIppCata,_tlIppL,_tlIppLI,_tlIppR,_tlIppRA,_tlIppSF,_tlIppSPF,_tlIprdInh) =
                                 tl_ _tlOext _tlOinh _tlOinhMap _tlOinhNoGroup _tlOnewAtts _tlOnewNT _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOppNt _tlOsyn _tlOsynMap _tlOsynNoGroup 
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
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
                        (let _lhsOhasMoreProds :: ( Bool )
                             _lhsOppA :: PP_Doc
                             _lhsOppCata :: PP_Doc
                             _lhsOppL :: PP_Doc
                             _lhsOppLI :: ([PP_Doc])
                             _lhsOppR :: PP_Doc
                             _lhsOppRA :: ([PP_Doc])
                             _lhsOppSF :: PP_Doc
                             _lhsOppSPF :: PP_Doc
                             _lhsOprdInh :: Attributes
                             -- use rule "src-ag/AG2AspectAG.ag"(line 100, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 100 "src-ag/AG2AspectAG.ag" #-}
                                  False
                                  {-# LINE 3137 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 315, column 64)
                             _lhsOppA =
                                 ({-# LINE 315 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3143 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 725, column 67)
                             _lhsOppCata =
                                 ({-# LINE 725 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3149 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 79)
                             _lhsOppL =
                                 ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3155 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 257, column 112)
                             _lhsOppLI =
                                 ({-# LINE 257 "src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 3161 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 411, column 79)
                             _lhsOppR =
                                 ({-# LINE 411 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3167 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 412, column 43)
                             _lhsOppRA =
                                 ({-# LINE 412 "src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 3173 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 766, column 66)
                             _lhsOppSF =
                                 ({-# LINE 766 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3179 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 767, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 767 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 3185 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 64, column 57)
                             _lhsOprdInh =
                                 ({-# LINE 64 "src-ag/AG2AspectAG.ag" #-}
                                  Map.empty
                                  {-# LINE 3191 "src-ag/AG2AspectAG.hs" #-}
                                  )
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF,_lhsOprdInh))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
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
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit _pure _identity _mbError _eager )
-- semantic domain
newtype T_Rule  = T_Rule ((Maybe String) ->
                          ([String]) ->
                          Bool ->
                          ([String]) ->
                          PP_Doc ->
                          PP_Doc ->
                          ([String]) ->
                          ( ([Identifier]),([ PPRule ])))
data Inh_Rule  = Inh_Rule {ext_Inh_Rule :: (Maybe String),inhNoGroup_Inh_Rule :: ([String]),newProd_Inh_Rule :: Bool,o_noGroup_Inh_Rule :: ([String]),ppNt_Inh_Rule :: PP_Doc,ppProd_Inh_Rule :: PP_Doc,synNoGroup_Inh_Rule :: ([String])}
data Syn_Rule  = Syn_Rule {locals_Syn_Rule :: ([Identifier]),ppRL_Syn_Rule :: ([ PPRule ])}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIext _lhsIinhNoGroup _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Rule _lhsOlocals _lhsOppRL ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_  =
    (T_Rule (\ _lhsIext
               _lhsIinhNoGroup
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
                      -- "src-ag/AG2AspectAG.ag"(line 368, column 25)
                      _lhsOlocals =
                          ({-# LINE 368 "src-ag/AG2AspectAG.ag" #-}
                           if (show (fst _patternIinfo) == "loc")
                            then [ snd _patternIinfo ]
                            else [ ]
                           {-# LINE 3275 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- "src-ag/AG2AspectAG.ag"(line 465, column 33)
                      _lhsOppRL =
                          ({-# LINE 465 "src-ag/AG2AspectAG.ag" #-}
                           if (not explicit_ &&  not _lhsInewProd)
                           then []
                           else [ ppRule _patternIinfo owrt_ (defRule _lhsIppNt _patternIinfo _lhsIo_noGroup _rhsIppRE) ]
                           {-# LINE 3283 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppNt =
                          ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                           _lhsIppNt
                           {-# LINE 3289 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppProd =
                          ({-# LINE 191 "src-ag/AG2AspectAG.ag" #-}
                           _lhsIppProd
                           {-# LINE 3295 "src-ag/AG2AspectAG.hs" #-}
                           )
                      ( _patternIcopy,_patternIinfo) =
                          pattern_ 
                      ( _rhsIppRE) =
                          rhs_ _rhsOppNt _rhsOppProd 
                  in  ( _lhsOlocals,_lhsOppRL))) )
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
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
sem_Rules :: Rules  ->
             T_Rules 
sem_Rules list  =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list) )
-- semantic domain
newtype T_Rules  = T_Rules ((Maybe String) ->
                            ([String]) ->
                            Bool ->
                            ([String]) ->
                            PP_Doc ->
                            PP_Doc ->
                            ([String]) ->
                            ( ([Identifier]),([ PPRule ])))
data Inh_Rules  = Inh_Rules {ext_Inh_Rules :: (Maybe String),inhNoGroup_Inh_Rules :: ([String]),newProd_Inh_Rules :: Bool,o_noGroup_Inh_Rules :: ([String]),ppNt_Inh_Rules :: PP_Doc,ppProd_Inh_Rules :: PP_Doc,synNoGroup_Inh_Rules :: ([String])}
data Syn_Rules  = Syn_Rules {locals_Syn_Rules :: ([Identifier]),ppRL_Syn_Rules :: ([ PPRule ])}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIext _lhsIinhNoGroup _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsInewProd _lhsIo_noGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Rules _lhsOlocals _lhsOppRL ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsInewProd
                _lhsIo_noGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       _hdOext :: (Maybe String)
                       _hdOinhNoGroup :: ([String])
                       _hdOnewProd :: Bool
                       _hdOo_noGroup :: ([String])
                       _hdOppNt :: PP_Doc
                       _hdOppProd :: PP_Doc
                       _hdOsynNoGroup :: ([String])
                       _tlOext :: (Maybe String)
                       _tlOinhNoGroup :: ([String])
                       _tlOnewProd :: Bool
                       _tlOo_noGroup :: ([String])
                       _tlOppNt :: PP_Doc
                       _tlOppProd :: PP_Doc
                       _tlOsynNoGroup :: ([String])
                       _hdIlocals :: ([Identifier])
                       _hdIppRL :: ([ PPRule ])
                       _tlIlocals :: ([Identifier])
                       _tlIppRL :: ([ PPRule ])
                       -- "src-ag/AG2AspectAG.ag"(line 461, column 33)
                       _lhsOppRL =
                           ({-# LINE 461 "src-ag/AG2AspectAG.ag" #-}
                            _hdIppRL ++ _tlIppRL
                            {-# LINE 3379 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- use rule "src-ag/AG2AspectAG.ag"(line 364, column 30)
                       _lhsOlocals =
                           ({-# LINE 364 "src-ag/AG2AspectAG.ag" #-}
                            _hdIlocals ++ _tlIlocals
                            {-# LINE 3385 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOext =
                           ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 3391 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinhNoGroup =
                           ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 3397 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOnewProd =
                           ({-# LINE 405 "src-ag/AG2AspectAG.ag" #-}
                            _lhsInewProd
                            {-# LINE 3403 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOo_noGroup =
                           ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIo_noGroup
                            {-# LINE 3409 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppNt =
                           ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 3415 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppProd =
                           ({-# LINE 191 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3421 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsynNoGroup =
                           ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3427 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOext =
                           ({-# LINE 118 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 3433 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinhNoGroup =
                           ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 3439 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOnewProd =
                           ({-# LINE 405 "src-ag/AG2AspectAG.ag" #-}
                            _lhsInewProd
                            {-# LINE 3445 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOo_noGroup =
                           ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIo_noGroup
                            {-# LINE 3451 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppNt =
                           ({-# LINE 186 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 3457 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppProd =
                           ({-# LINE 191 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3463 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsynNoGroup =
                           ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3469 "src-ag/AG2AspectAG.hs" #-}
                            )
                       ( _hdIlocals,_hdIppRL) =
                           hd_ _hdOext _hdOinhNoGroup _hdOnewProd _hdOo_noGroup _hdOppNt _hdOppProd _hdOsynNoGroup 
                       ( _tlIlocals,_tlIppRL) =
                           tl_ _tlOext _tlOinhNoGroup _tlOnewProd _tlOo_noGroup _tlOppNt _tlOppProd _tlOsynNoGroup 
                   in  ( _lhsOlocals,_lhsOppRL))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsInewProd
                _lhsIo_noGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       -- "src-ag/AG2AspectAG.ag"(line 462, column 33)
                       _lhsOppRL =
                           ({-# LINE 462 "src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3491 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- use rule "src-ag/AG2AspectAG.ag"(line 364, column 30)
                       _lhsOlocals =
                           ({-# LINE 364 "src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3497 "src-ag/AG2AspectAG.hs" #-}
                            )
                   in  ( _lhsOlocals,_lhsOppRL))) )
-- TypeSig -----------------------------------------------------
{-
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
-}
-- cata
sem_TypeSig :: TypeSig  ->
               T_TypeSig 
sem_TypeSig (TypeSig _name _tp )  =
    (sem_TypeSig_TypeSig _name _tp )
-- semantic domain
newtype T_TypeSig  = T_TypeSig (( ))
data Inh_TypeSig  = Inh_TypeSig {}
data Syn_TypeSig  = Syn_TypeSig {}
wrap_TypeSig :: T_TypeSig  ->
                Inh_TypeSig  ->
                Syn_TypeSig 
wrap_TypeSig (T_TypeSig sem ) (Inh_TypeSig )  =
    (let ( ) = sem 
     in  (Syn_TypeSig ))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig 
sem_TypeSig_TypeSig name_ tp_  =
    (T_TypeSig (let 
                in  ( )) )
-- TypeSigs ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
      alternative Nil:
-}
-- cata
sem_TypeSigs :: TypeSigs  ->
                T_TypeSigs 
sem_TypeSigs list  =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list) )
-- semantic domain
newtype T_TypeSigs  = T_TypeSigs (( ))
data Inh_TypeSigs  = Inh_TypeSigs {}
data Syn_TypeSigs  = Syn_TypeSigs {}
wrap_TypeSigs :: T_TypeSigs  ->
                 Inh_TypeSigs  ->
                 Syn_TypeSigs 
wrap_TypeSigs (T_TypeSigs sem ) (Inh_TypeSigs )  =
    (let ( ) = sem 
     in  (Syn_TypeSigs ))
sem_TypeSigs_Cons :: T_TypeSig  ->
                     T_TypeSigs  ->
                     T_TypeSigs 
sem_TypeSigs_Cons (T_TypeSig hd_ ) (T_TypeSigs tl_ )  =
    (T_TypeSigs (let 
                 in  ( )) )
sem_TypeSigs_Nil :: T_TypeSigs 
sem_TypeSigs_Nil  =
    (T_TypeSigs (let 
                 in  ( )) )