

-- UUAGC 0.9.39.0.0 (src-ag/AG2AspectAG.ag)
module AG2AspectAG where
{-# LINE 7 "src-ag/AG2AspectAG.ag" #-}

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
{-# LINE 23 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
{-# LINE 33 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 40 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 46 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 52 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}
{-# LINE 25 "src-ag/AG2AspectAG.ag" #-}

pragmaAspectAG =  pp  "{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction , TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}"

{-# LINE 57 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 30 "src-ag/AG2AspectAG.ag" #-}

ppName l = ppListSep "" "" "_" l 
{-# LINE 62 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 58 "src-ag/AG2AspectAG.ag" #-}

type FieldMap  = [(Identifier, Type)] 
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap) 
{-# LINE 68 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 324 "src-ag/AG2AspectAG.ag" #-}

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

{-# LINE 97 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 379 "src-ag/AG2AspectAG.ag" #-}

ntsList att ppNtL = "nts_" ++ att ++ " = " >|<  ppListSep "" "" " .*. " ((map fst ppNtL) ++ [pp "hNil"])

filterNts att = filter ( Map.member (identifier att) . snd )
{-# LINE 104 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 439 "src-ag/AG2AspectAG.ag" #-}

data PPRule = PPRule Identifier Identifier Bool ([(Identifier,Type)] -> [Identifier] -> PP_Doc)

ppRule (field,attr) owrt def = PPRule field attr owrt def
ruleField (PPRule field  _     _     _  ) = field
ruleAttr  (PPRule _      attr  _     _  ) = attr
ruleOwrt  (PPRule _      _     owrt  _  ) = owrt
ruleDef   (PPRule _      _     _     def) = def

{-# LINE 116 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 474 "src-ag/AG2AspectAG.ag" #-}


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
                                     indent 6  (ppListSep "(" ")" "," $ mapGRuleDefs ((== "loc") . show)  rules inhNoGroup synNoGroup chids locals)
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
 
defRule ppNt (field,att) inhNoGroup synNoGroup rhs = \chids locals -> 
                                     let ppAtt = if (elem (getName att) inhNoGroup || elem (getName att) synNoGroup) 
                                                  then empty
                                                  else case (show field) of
                                                        "lhs"     -> att >|< "_" >|< pp "SynG" >|< pp "_" >|< ppNt  >|< " = "
                                                        "loc"     -> empty
                                                        "inst"    -> empty
                                                        otherwise -> att >|< "_" >|< pp "InhG" >|< pp "_" >|< 
                                                                     (maybe (error $ "lhs field " ++ show field ++" is not a child") 
                                                                            ppShow (lookup field chids))  
                                                                     >|< " = "
                                     in  ppAtt >|< (rhs field inhNoGroup synNoGroup  chids locals)

         
rhsRule ppNt ppProd tks field inhNoGroup synNoGroup  chids locals  =  vlist . lines2PP . (map (token2PP ppNt ppProd field chids locals inhNoGroup synNoGroup )) $ tks


lines2PP [] = []
lines2PP xs = map line2PP . shiftLeft . getLines $ xs


token2PP ppNt ppProd field chids locals inhNoGroup synNoGroup  tk
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
                                  in  (pos, if (elem (getName attr) inhNoGroup || elem (getName attr) synNoGroup )
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

{-# LINE 336 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 717 "src-ag/AG2AspectAG.ag" #-}

ppNoGroupAtts syn noGroup = let synatts = Map.keys $ Map.filterWithKey (\att _ -> elem (getName att) noGroup) syn 
                            in  map (flip (>|<) "_inh") noGroup ++  map (flip (>|<) "_syn") synatts

ruleName att prodName = ppName [att,prodName] 

elemNT a b = False 
{-# LINE 346 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 760 "src-ag/AG2AspectAG.ag" #-}

attTypes atts = map (\(a,t) -> "(HCons (LVPair (Proxy Att_" >|< a >|< ") " >|< ppShow t >|< ") ") $ Map.toAscList atts
{-# LINE 351 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}

{-# LINE 814 "src-ag/AG2AspectAG.ag" #-}

attVars atts = map (\(a,_) -> "_" >|< a >|< " ") $ Map.toAscList atts
attFields atts noGroup ppNt = 
     let ng = map (\(a,_) -> attName (getName a) >|< " .=. _" >|< a >|< " .*. ") $ Map.toAscList noGroup
         g  = ppCommas $ map (\(a,_) -> ppName [pp a, pp "InhG",ppNt]  >|< "= _" >|< a) $ Map.toAscList $ Map.difference atts noGroup
     in "(" >|< ng >|< "att_inh .=. " >|< ppName [pp "InhG", ppNt] >|< " { " >|< g >|< " } .*. emptyRecord)"
{-# LINE 360 "dist/build/uuagc/uuagc-tmp/AG2AspectAG.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child virtual        : {Maybe (Maybe Type)}
         visit 0:
            local ppCh        : _
            local ppTCh       : _
            local chName      : _
            local chLabel     : _
            local chTLabel    : _
-}
-- cata
sem_Child :: Child  ->
             T_Child 
sem_Child (Child _name _tp _inh _syn _virtual )  =
    (sem_Child_Child _name _tp _inh _syn _virtual )
-- semantic domain
newtype T_Child  = T_Child ((Maybe String) ->
                            ([String]) ->
                            Bool ->
                            PP_Doc ->
                            PP_Doc ->
                            ([String]) ->
                            ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),PP_Doc,([PP_Doc]),PP_Doc))
data Inh_Child  = Inh_Child {ext_Inh_Child :: (Maybe String),inhNoGroup_Inh_Child :: ([String]),o_rename_Inh_Child :: Bool,ppNt_Inh_Child :: PP_Doc,ppProd_Inh_Child :: PP_Doc,synNoGroup_Inh_Child :: ([String])}
data Syn_Child  = Syn_Child {idCL_Syn_Child :: ([(Identifier,Type)]),ppCSF_Syn_Child :: ([(Identifier,(PP_Doc,PP_Doc))]),ppL_Syn_Child :: PP_Doc,ppLI_Syn_Child :: ([PP_Doc]),ppR_Syn_Child :: PP_Doc}
wrap_Child :: T_Child  ->
              Inh_Child  ->
              Syn_Child 
wrap_Child (T_Child sem ) (Inh_Child _lhsIext _lhsIinhNoGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR) = sem _lhsIext _lhsIinhNoGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Child _lhsOidCL _lhsOppCSF _lhsOppL _lhsOppLI _lhsOppR ))
sem_Child_Child :: Identifier ->
                   Type ->
                   Attributes ->
                   Attributes ->
                   (Maybe (Maybe Type)) ->
                   T_Child 
sem_Child_Child name_ tp_ inh_ syn_ virtual_  =
    (T_Child (\ _lhsIext
                _lhsIinhNoGroup
                _lhsIo_rename
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppL :: PP_Doc
                       _lhsOppLI :: ([PP_Doc])
                       _lhsOppR :: PP_Doc
                       _lhsOidCL :: ([(Identifier,Type)])
                       _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                       -- "src-ag/AG2AspectAG.ag"(line 170, column 25)
                       _ppCh =
                           ({-# LINE 170 "src-ag/AG2AspectAG.ag" #-}
                            pp name_
                            {-# LINE 434 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 171, column 25)
                       _ppTCh =
                           ({-# LINE 171 "src-ag/AG2AspectAG.ag" #-}
                            ppShow tp_
                            {-# LINE 440 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 172, column 25)
                       _chName =
                           ({-# LINE 172 "src-ag/AG2AspectAG.ag" #-}
                            ppName [_ppCh    , _lhsIppNt, _lhsIppProd]
                            {-# LINE 446 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 272, column 25)
                       _chLabel =
                           ({-# LINE 272 "src-ag/AG2AspectAG.ag" #-}
                            "ch_" >|< _chName
                            {-# LINE 452 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 273, column 25)
                       _chTLabel =
                           ({-# LINE 273 "src-ag/AG2AspectAG.ag" #-}
                            "Ch_" >|< _chName
                            {-# LINE 458 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 274, column 25)
                       _lhsOppL =
                           ({-# LINE 274 "src-ag/AG2AspectAG.ag" #-}
                            "data " >|< _chTLabel     >|< "; " >|< _chLabel     >|< pp " = proxy :: " >|<
                            case virtual_ of
                             Nothing    ->  "Proxy " >|< "(" >|< _chTLabel     >|< ", " >|< _ppTCh     >|< ")"
                             otherwise  ->  "SemType " >|< _ppTCh     >|< pp " nt =>  Proxy " >|<
                                            "(" >|< _chTLabel     >|< ", nt)"
                            {-# LINE 468 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 280, column 25)
                       _lhsOppLI =
                           ({-# LINE 280 "src-ag/AG2AspectAG.ag" #-}
                            [ _chLabel    , _chTLabel     ]
                            {-# LINE 474 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 435, column 25)
                       _lhsOppR =
                           ({-# LINE 435 "src-ag/AG2AspectAG.ag" #-}
                            let chName = ppListSep "" "" "_" [pp name_, _lhsIppNt, _lhsIppProd]
                            in  pp name_ >|< " <- at ch_" >|< chName
                            {-# LINE 481 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 466, column 25)
                       _lhsOidCL =
                           ({-# LINE 466 "src-ag/AG2AspectAG.ag" #-}
                            [ (name_, case (stripPrefix "T_" (show tp_)) of
                                        Nothing -> tp_
                                        Just t  -> NT (Ident t undefined) []
                            ) ]
                            {-# LINE 490 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- "src-ag/AG2AspectAG.ag"(line 789, column 25)
                       _lhsOppCSF =
                           ({-# LINE 789 "src-ag/AG2AspectAG.ag" #-}
                            let
                                 semC   = if (isNonterminal tp_)
                                           then "sem_" >|< ppShow tp_ >|<  " _" >|< name_
                                           else "sem_Lit _" >|< name_
                            in   case virtual_ of
                                      Nothing    ->  [(name_, (  _chLabel     >|< " .=. (" >|< semC >|< ") .*. "
                                                              ,  _chLabel     >|< " .=. _" >|< name_ >|< " .*. "))]
                                      otherwise  ->  []
                            {-# LINE 503 "src-ag/AG2AspectAG.hs" #-}
                            )
                   in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR))) )
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
         o_rename             : Bool
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
         synNoGroup           : [String]
      synthesized attributes:
         idCL                 : [(Identifier,Type)]
         ppCSF                : [(Identifier,(PP_Doc,PP_Doc))]
         ppL                  : PP_Doc
         ppLI                 : [PP_Doc]
         ppR                  : PP_Doc
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
                                  ([String]) ->
                                  Bool ->
                                  PP_Doc ->
                                  PP_Doc ->
                                  ([String]) ->
                                  ( ([(Identifier,Type)]),([(Identifier,(PP_Doc,PP_Doc))]),PP_Doc,([PP_Doc]),PP_Doc))
data Inh_Children  = Inh_Children {ext_Inh_Children :: (Maybe String),inhNoGroup_Inh_Children :: ([String]),o_rename_Inh_Children :: Bool,ppNt_Inh_Children :: PP_Doc,ppProd_Inh_Children :: PP_Doc,synNoGroup_Inh_Children :: ([String])}
data Syn_Children  = Syn_Children {idCL_Syn_Children :: ([(Identifier,Type)]),ppCSF_Syn_Children :: ([(Identifier,(PP_Doc,PP_Doc))]),ppL_Syn_Children :: PP_Doc,ppLI_Syn_Children :: ([PP_Doc]),ppR_Syn_Children :: PP_Doc}
wrap_Children :: T_Children  ->
                 Inh_Children  ->
                 Syn_Children 
wrap_Children (T_Children sem ) (Inh_Children _lhsIext _lhsIinhNoGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR) = sem _lhsIext _lhsIinhNoGroup _lhsIo_rename _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Children _lhsOidCL _lhsOppCSF _lhsOppL _lhsOppLI _lhsOppR ))
sem_Children_Cons :: T_Child  ->
                     T_Children  ->
                     T_Children 
sem_Children_Cons (T_Child hd_ ) (T_Children tl_ )  =
    (T_Children (\ _lhsIext
                   _lhsIinhNoGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynNoGroup ->
                     (let _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          _hdOext :: (Maybe String)
                          _hdOinhNoGroup :: ([String])
                          _hdOo_rename :: Bool
                          _hdOppNt :: PP_Doc
                          _hdOppProd :: PP_Doc
                          _hdOsynNoGroup :: ([String])
                          _tlOext :: (Maybe String)
                          _tlOinhNoGroup :: ([String])
                          _tlOo_rename :: Bool
                          _tlOppNt :: PP_Doc
                          _tlOppProd :: PP_Doc
                          _tlOsynNoGroup :: ([String])
                          _hdIidCL :: ([(Identifier,Type)])
                          _hdIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _hdIppL :: PP_Doc
                          _hdIppLI :: ([PP_Doc])
                          _hdIppR :: PP_Doc
                          _tlIidCL :: ([(Identifier,Type)])
                          _tlIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _tlIppL :: PP_Doc
                          _tlIppLI :: ([PP_Doc])
                          _tlIppR :: PP_Doc
                          -- use rule "src-ag/AG2AspectAG.ag"(line 464, column 31)
                          _lhsOidCL =
                              ({-# LINE 464 "src-ag/AG2AspectAG.ag" #-}
                               _hdIidCL ++ _tlIidCL
                               {-# LINE 590 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 785, column 34)
                          _lhsOppCSF =
                              ({-# LINE 785 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppCSF ++ _tlIppCSF
                               {-# LINE 596 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                          _lhsOppL =
                              ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppL >-< _tlIppL
                               {-# LINE 602 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                          _lhsOppLI =
                              ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppLI ++ _tlIppLI
                               {-# LINE 608 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                          _lhsOppR =
                              ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                               _hdIppR >-< _tlIppR
                               {-# LINE 614 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOext =
                              ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 620 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOinhNoGroup =
                              ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 626 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOo_rename =
                              ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 632 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppNt =
                              ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 638 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOppProd =
                              ({-# LINE 180 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 644 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOsynNoGroup =
                              ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 650 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOext =
                              ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIext
                               {-# LINE 656 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOinhNoGroup =
                              ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIinhNoGroup
                               {-# LINE 662 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOo_rename =
                              ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIo_rename
                               {-# LINE 668 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppNt =
                              ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppNt
                               {-# LINE 674 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOppProd =
                              ({-# LINE 180 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIppProd
                               {-# LINE 680 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOsynNoGroup =
                              ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                               _lhsIsynNoGroup
                               {-# LINE 686 "src-ag/AG2AspectAG.hs" #-}
                               )
                          ( _hdIidCL,_hdIppCSF,_hdIppL,_hdIppLI,_hdIppR) =
                              hd_ _hdOext _hdOinhNoGroup _hdOo_rename _hdOppNt _hdOppProd _hdOsynNoGroup 
                          ( _tlIidCL,_tlIppCSF,_tlIppL,_tlIppLI,_tlIppR) =
                              tl_ _tlOext _tlOinhNoGroup _tlOo_rename _tlOppNt _tlOppProd _tlOsynNoGroup 
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR))) )
sem_Children_Nil :: T_Children 
sem_Children_Nil  =
    (T_Children (\ _lhsIext
                   _lhsIinhNoGroup
                   _lhsIo_rename
                   _lhsIppNt
                   _lhsIppProd
                   _lhsIsynNoGroup ->
                     (let _lhsOidCL :: ([(Identifier,Type)])
                          _lhsOppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                          _lhsOppL :: PP_Doc
                          _lhsOppLI :: ([PP_Doc])
                          _lhsOppR :: PP_Doc
                          -- use rule "src-ag/AG2AspectAG.ag"(line 464, column 31)
                          _lhsOidCL =
                              ({-# LINE 464 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 710 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 785, column 34)
                          _lhsOppCSF =
                              ({-# LINE 785 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 716 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                          _lhsOppL =
                              ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 722 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                          _lhsOppLI =
                              ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                               []
                               {-# LINE 728 "src-ag/AG2AspectAG.hs" #-}
                               )
                          -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                          _lhsOppR =
                              ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                               empty
                               {-# LINE 734 "src-ag/AG2AspectAG.hs" #-}
                               )
                      in  ( _lhsOidCL,_lhsOppCSF,_lhsOppL,_lhsOppLI,_lhsOppR))) )
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ppNt                 : PP_Doc
         ppProd               : PP_Doc
      synthesized attribute:
         ppRE                 : Identifier -> [String] -> [String] -> [(Identifier,Type)] -> [Identifier] -> PP_Doc
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
                                      ( (Identifier -> [String] -> [String] -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)))
data Inh_Expression  = Inh_Expression {ppNt_Inh_Expression :: PP_Doc,ppProd_Inh_Expression :: PP_Doc}
data Syn_Expression  = Syn_Expression {ppRE_Syn_Expression :: (Identifier -> [String] -> [String] -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)}
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
                       (let _lhsOppRE :: (Identifier -> [String] -> [String] -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
                            -- "src-ag/AG2AspectAG.ag"(line 460, column 25)
                            _lhsOppRE =
                                ({-# LINE 460 "src-ag/AG2AspectAG.ag" #-}
                                 rhsRule _lhsIppNt _lhsIppProd tks_
                                 {-# LINE 778 "src-ag/AG2AspectAG.hs" #-}
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
                         _nontsOext :: (Maybe String)
                         _nontsIextendedNTs :: (Set NontermIdent)
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
                         -- "src-ag/AG2AspectAG.ag"(line 40, column 14)
                         _nontsOo_rename =
                             ({-# LINE 40 "src-ag/AG2AspectAG.ag" #-}
                              rename    _lhsIoptions
                              {-# LINE 879 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 44, column 14)
                         _o_noGroup =
                             ({-# LINE 44 "src-ag/AG2AspectAG.ag" #-}
                              sort $ noGroup    _lhsIoptions
                              {-# LINE 885 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 45, column 14)
                         _nontsOo_noGroup =
                             ({-# LINE 45 "src-ag/AG2AspectAG.ag" #-}
                              _o_noGroup
                              {-# LINE 891 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 68, column 23)
                         _newAtts =
                             ({-# LINE 68 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (_,_,atts) -> ( Map.unions . (\(a,b) -> a++b) . unzip . Map.elems) atts
                              {-# LINE 898 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 70, column 23)
                         _nontsOnewAtts =
                             ({-# LINE 70 "src-ag/AG2AspectAG.ag" #-}
                              _newAtts
                              {-# LINE 904 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 76, column 23)
                         _newProds =
                             ({-# LINE 76 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                     (_,prods,_) -> prods
                              {-# LINE 911 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 78, column 23)
                         _nontsOnewProds =
                             ({-# LINE 78 "src-ag/AG2AspectAG.ag" #-}
                              _newProds
                              {-# LINE 917 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 100, column 23)
                         _nontsOnewNTs =
                             ({-# LINE 100 "src-ag/AG2AspectAG.ag" #-}
                              case _lhsIagi of
                                      (newNTs,_,_) -> Set.difference newNTs _nontsIextendedNTs
                              {-# LINE 924 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 115, column 25)
                         _lhsOimp =
                             ({-# LINE 115 "src-ag/AG2AspectAG.ag" #-}
                              "import Language.Grammars.AspectAG" >-<
                              "import Language.Grammars.AspectAG.Derive" >-<
                              "import Data.HList.Label4" >-<
                              "import Data.HList.TypeEqGeneric1" >-<
                              "import Data.HList.TypeCastGeneric1" >-<
                              maybe empty ("import qualified" >#<) _lhsIext >-<
                              maybe empty (\ext -> "import" >#< ext >#< ppListSep "(" ")" "," (_nontsIppDI ++ _nontsIppLI ++ _ppAI    )) _lhsIext
                              {-# LINE 936 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 128, column 25)
                         _lhsOpp =
                             ({-# LINE 128 "src-ag/AG2AspectAG.ag" #-}
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
                              {-# LINE 959 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 190, column 25)
                         _nontsOderivs =
                             ({-# LINE 190 "src-ag/AG2AspectAG.ag" #-}
                              derivings_
                              {-# LINE 965 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 238, column 34)
                         _nontsOtSyns =
                             ({-# LINE 238 "src-ag/AG2AspectAG.ag" #-}
                              typeSyns_
                              {-# LINE 971 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 287, column 25)
                         _ppA =
                             ({-# LINE 287 "src-ag/AG2AspectAG.ag" #-}
                              vlist (map defAtt (filterAtts _newAtts     _o_noGroup    )) >-<
                              defAtt "loc" >-<
                              (case _lhsIext of
                                Nothing    ->  defAtt "inh" >-< defAtt "syn"
                                otherwise  ->  empty) >-<
                              _nontsIppA
                              {-# LINE 982 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 294, column 25)
                         _ppAI =
                             ({-# LINE 294 "src-ag/AG2AspectAG.ag" #-}
                              let atts =  filterNotAtts _newAtts     _o_noGroup
                              in  (foldr (\a as -> attName a : as) [] atts) ++
                                  (foldr (\a as -> attTName a : as) [] atts) ++
                                  (case _lhsIext of
                                    Nothing    ->  []
                                    otherwise  ->  [ attName "inh", attName "syn", attTName "inh", attTName "syn" ]) ++
                                  _nontsIppAI
                              {-# LINE 994 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 374, column 25)
                         _ppNtL =
                             ({-# LINE 374 "src-ag/AG2AspectAG.ag" #-}
                              _nontsIppNtL
                              {-# LINE 1000 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- "src-ag/AG2AspectAG.ag"(line 375, column 25)
                         _ppR =
                             ({-# LINE 375 "src-ag/AG2AspectAG.ag" #-}
                              ntsList "group" _ppNtL      >-<
                              vlist (map (\att -> ntsList att (filterNts att _ppNtL    )) (filterAtts _newAtts _o_noGroup    ))  >-<
                              _nontsIppR
                              {-# LINE 1008 "src-ag/AG2AspectAG.hs" #-}
                              )
                         -- copy rule (down)
                         _nontsOext =
                             ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                              _lhsIext
                              {-# LINE 1014 "src-ag/AG2AspectAG.hs" #-}
                              )
                         ( _nontsIextendedNTs,_nontsIppA,_nontsIppAI,_nontsIppCata,_nontsIppD,_nontsIppDI,_nontsIppL,_nontsIppLI,_nontsIppNtL,_nontsIppR,_nontsIppSF,_nontsIppW) =
                             nonts_ _nontsOderivs _nontsOext _nontsOnewAtts _nontsOnewNTs _nontsOnewProds _nontsOo_noGroup _nontsOo_rename _nontsOtSyns 
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
         newAtts              :  Attributes 
         newNTs               : Set NontermIdent
         newProds             :  DataTypes 
         o_noGroup            : [String]
         o_rename             : Bool
         tSyns                : TypeSyns
      synthesized attributes:
         extendedNTs          : Set NontermIdent
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
                                        ( Attributes ) ->
                                        (Set NontermIdent) ->
                                        ( DataTypes ) ->
                                        ([String]) ->
                                        Bool ->
                                        TypeSyns ->
                                        ( (Set NontermIdent),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),([(PP_Doc, Attributes)]),PP_Doc,PP_Doc,PP_Doc))
data Inh_Nonterminal  = Inh_Nonterminal {derivs_Inh_Nonterminal :: Derivings,ext_Inh_Nonterminal :: (Maybe String),newAtts_Inh_Nonterminal :: ( Attributes ),newNTs_Inh_Nonterminal :: (Set NontermIdent),newProds_Inh_Nonterminal :: ( DataTypes ),o_noGroup_Inh_Nonterminal :: ([String]),o_rename_Inh_Nonterminal :: Bool,tSyns_Inh_Nonterminal :: TypeSyns}
data Syn_Nonterminal  = Syn_Nonterminal {extendedNTs_Syn_Nonterminal :: (Set NontermIdent),ppA_Syn_Nonterminal :: PP_Doc,ppAI_Syn_Nonterminal :: ([PP_Doc]),ppCata_Syn_Nonterminal :: PP_Doc,ppD_Syn_Nonterminal :: PP_Doc,ppDI_Syn_Nonterminal :: ([PP_Doc]),ppL_Syn_Nonterminal :: PP_Doc,ppLI_Syn_Nonterminal :: ([PP_Doc]),ppNtL_Syn_Nonterminal :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminal :: PP_Doc,ppSF_Syn_Nonterminal :: PP_Doc,ppW_Syn_Nonterminal :: PP_Doc}
wrap_Nonterminal :: T_Nonterminal  ->
                    Inh_Nonterminal  ->
                    Syn_Nonterminal 
wrap_Nonterminal (T_Nonterminal sem ) (Inh_Nonterminal _lhsIderivs _lhsIext _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsItSyns )  =
    (let ( _lhsOextendedNTs,_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW) = sem _lhsIderivs _lhsIext _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsItSyns 
     in  (Syn_Nonterminal _lhsOextendedNTs _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW ))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions  ->
                               T_Nonterminal 
sem_Nonterminal_Nonterminal nt_ params_ inh_ syn_ (T_Productions prods_ )  =
    (T_Nonterminal (\ _lhsIderivs
                      _lhsIext
                      _lhsInewAtts
                      _lhsInewNTs
                      _lhsInewProds
                      _lhsIo_noGroup
                      _lhsIo_rename
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
                             _prodsOext :: (Maybe String)
                             _prodsOnewAtts :: ( Attributes )
                             _prodsOo_rename :: Bool
                             _prodsIhasMoreProds :: ( Bool )
                             _prodsIppA :: PP_Doc
                             _prodsIppCata :: PP_Doc
                             _prodsIppL :: PP_Doc
                             _prodsIppLI :: ([PP_Doc])
                             _prodsIppR :: PP_Doc
                             _prodsIppRA :: ([PP_Doc])
                             _prodsIppSF :: PP_Doc
                             _prodsIppSPF :: PP_Doc
                             -- "src-ag/AG2AspectAG.ag"(line 48, column 18)
                             _inhNoGroup =
                                 ({-# LINE 48 "src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) inh_
                                  {-# LINE 1278 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 49, column 18)
                             _synNoGroup =
                                 ({-# LINE 49 "src-ag/AG2AspectAG.ag" #-}
                                  Map.filterWithKey (\att _ -> elem (getName att) _lhsIo_noGroup) syn_
                                  {-# LINE 1284 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 54, column 18)
                             _prodsOinhNoGroup =
                                 ({-# LINE 54 "src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _inhNoGroup
                                  {-# LINE 1290 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 55, column 18)
                             _prodsOsynNoGroup =
                                 ({-# LINE 55 "src-ag/AG2AspectAG.ag" #-}
                                  map show $ Map.keys _synNoGroup
                                  {-# LINE 1296 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 82, column 17)
                             _prodsOnewProds =
                                 ({-# LINE 82 "src-ag/AG2AspectAG.ag" #-}
                                  case Map.lookup nt_ _lhsInewProds of
                                         Just prds -> prds
                                         Nothing   -> Map.empty
                                  {-# LINE 1304 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 95, column 31)
                             _lhsOextendedNTs =
                                 ({-# LINE 95 "src-ag/AG2AspectAG.ag" #-}
                                  if _prodsIhasMoreProds
                                  then Set.singleton nt_
                                  else Set.empty
                                  {-# LINE 1312 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 161, column 25)
                             _ppNt =
                                 ({-# LINE 161 "src-ag/AG2AspectAG.ag" #-}
                                  pp nt_
                                  {-# LINE 1318 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 178, column 25)
                             _prodsOppNt =
                                 ({-# LINE 178 "src-ag/AG2AspectAG.ag" #-}
                                  _ppNt
                                  {-# LINE 1324 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 196, column 25)
                             _lhsOppD =
                                 ({-# LINE 196 "src-ag/AG2AspectAG.ag" #-}
                                  if (Set.member nt_ _lhsInewNTs)
                                  then  case (lookup nt_ _lhsItSyns) of
                                                 Nothing ->  "data " >|< _ppNt
                                                 Just tp ->  "type " >|< _ppNt     >|< " = " >|< ppShow tp
                                  else  empty
                                  {-# LINE 1334 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 209, column 25)
                             _lhsOppDI =
                                 ({-# LINE 209 "src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then  [ _ppNt     ]
                                  else  [ ]
                                  {-# LINE 1342 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 249, column 25)
                             _ntLabel =
                                 ({-# LINE 249 "src-ag/AG2AspectAG.ag" #-}
                                  "nt_" >|< _ppNt
                                  {-# LINE 1348 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 251, column 25)
                             _lhsOppL =
                                 ({-# LINE 251 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then _ntLabel     >|< " = proxy :: Proxy " >|< _ppNt
                                    else empty)  >-<
                                  _prodsIppL
                                  {-# LINE 1357 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 256, column 25)
                             _lhsOppLI =
                                 ({-# LINE 256 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (not $ Set.member nt_ _lhsInewNTs)
                                    then [ _ntLabel     ]
                                    else [ ])  ++
                                  _prodsIppLI
                                  {-# LINE 1366 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 307, column 25)
                             _lhsOppA =
                                 ({-# LINE 307 "src-ag/AG2AspectAG.ag" #-}
                                  ( if (Set.member nt_ _lhsInewNTs)
                                    then   defAttRec (pp "InhG") _ppNt     inh_ _inhNoGroup     >-<
                                           defAttRec (pp "SynG") _ppNt     syn_ _synNoGroup
                                    else   empty) >-<
                                  _prodsIppA
                                  {-# LINE 1376 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 320, column 25)
                             _lhsOppAI =
                                 ({-# LINE 320 "src-ag/AG2AspectAG.ag" #-}
                                  if (not $ Set.member nt_ _lhsInewNTs)
                                  then [ ppName [(pp "InhG"), _ppNt     ] >#< pp "(..)", ppName [(pp "SynG"), _ppNt     ] >#< pp "(..)" ]
                                  else [ ]
                                  {-# LINE 1384 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 388, column 25)
                             _lhsOppNtL =
                                 ({-# LINE 388 "src-ag/AG2AspectAG.ag" #-}
                                  [ ("nt_" >|< nt_, Map.union inh_ syn_) ]
                                  {-# LINE 1390 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 399, column 25)
                             _prodsOnewNT =
                                 ({-# LINE 399 "src-ag/AG2AspectAG.ag" #-}
                                  Set.member nt_ _lhsInewNTs
                                  {-# LINE 1396 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 409, column 25)
                             _lhsOppR =
                                 ({-# LINE 409 "src-ag/AG2AspectAG.ag" #-}
                                  pp "----" >|< pp nt_ >-< _prodsIppR
                                  {-# LINE 1402 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 703, column 25)
                             _lhsOppCata =
                                 ({-# LINE 703 "src-ag/AG2AspectAG.ag" #-}
                                  "----" >|< _ppNt     >-< _prodsIppCata
                                  {-# LINE 1408 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 729, column 25)
                             _prodsOsyn =
                                 ({-# LINE 729 "src-ag/AG2AspectAG.ag" #-}
                                  syn_
                                  {-# LINE 1414 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 730, column 25)
                             _prodsOinh =
                                 ({-# LINE 730 "src-ag/AG2AspectAG.ag" #-}
                                  inh_
                                  {-# LINE 1420 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 741, column 25)
                             _lhsOppSF =
                                 ({-# LINE 741 "src-ag/AG2AspectAG.ag" #-}
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
                                  {-# LINE 1441 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- "src-ag/AG2AspectAG.ag"(line 809, column 25)
                             _lhsOppW =
                                 ({-# LINE 809 "src-ag/AG2AspectAG.ag" #-}
                                  ppName [pp "wrap", _ppNt    ] >|< " sem " >|< attVars inh_ >|< " = " >-<
                                  "   sem " >|< attFields inh_ _inhNoGroup     _ppNt
                                  {-# LINE 1448 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOext =
                                 ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 1454 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOnewAtts =
                                 ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 1460 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _prodsOo_rename =
                                 ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 1466 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             ( _prodsIhasMoreProds,_prodsIppA,_prodsIppCata,_prodsIppL,_prodsIppLI,_prodsIppR,_prodsIppRA,_prodsIppSF,_prodsIppSPF) =
                                 prods_ _prodsOext _prodsOinh _prodsOinhNoGroup _prodsOnewAtts _prodsOnewNT _prodsOnewProds _prodsOo_rename _prodsOppNt _prodsOsyn _prodsOsynNoGroup 
                         in  ( _lhsOextendedNTs,_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW))) )
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         derivs               : Derivings
         ext                  : Maybe String
         newAtts              :  Attributes 
         newNTs               : Set NontermIdent
         newProds             :  DataTypes 
         o_noGroup            : [String]
         o_rename             : Bool
         tSyns                : TypeSyns
      synthesized attributes:
         extendedNTs          : Set NontermIdent
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
                                          ( Attributes ) ->
                                          (Set NontermIdent) ->
                                          ( DataTypes ) ->
                                          ([String]) ->
                                          Bool ->
                                          TypeSyns ->
                                          ( (Set NontermIdent),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),([(PP_Doc, Attributes)]),PP_Doc,PP_Doc,PP_Doc))
data Inh_Nonterminals  = Inh_Nonterminals {derivs_Inh_Nonterminals :: Derivings,ext_Inh_Nonterminals :: (Maybe String),newAtts_Inh_Nonterminals :: ( Attributes ),newNTs_Inh_Nonterminals :: (Set NontermIdent),newProds_Inh_Nonterminals :: ( DataTypes ),o_noGroup_Inh_Nonterminals :: ([String]),o_rename_Inh_Nonterminals :: Bool,tSyns_Inh_Nonterminals :: TypeSyns}
data Syn_Nonterminals  = Syn_Nonterminals {extendedNTs_Syn_Nonterminals :: (Set NontermIdent),ppA_Syn_Nonterminals :: PP_Doc,ppAI_Syn_Nonterminals :: ([PP_Doc]),ppCata_Syn_Nonterminals :: PP_Doc,ppD_Syn_Nonterminals :: PP_Doc,ppDI_Syn_Nonterminals :: ([PP_Doc]),ppL_Syn_Nonterminals :: PP_Doc,ppLI_Syn_Nonterminals :: ([PP_Doc]),ppNtL_Syn_Nonterminals :: ([(PP_Doc, Attributes)]),ppR_Syn_Nonterminals :: PP_Doc,ppSF_Syn_Nonterminals :: PP_Doc,ppW_Syn_Nonterminals :: PP_Doc}
wrap_Nonterminals :: T_Nonterminals  ->
                     Inh_Nonterminals  ->
                     Syn_Nonterminals 
wrap_Nonterminals (T_Nonterminals sem ) (Inh_Nonterminals _lhsIderivs _lhsIext _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsItSyns )  =
    (let ( _lhsOextendedNTs,_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW) = sem _lhsIderivs _lhsIext _lhsInewAtts _lhsInewNTs _lhsInewProds _lhsIo_noGroup _lhsIo_rename _lhsItSyns 
     in  (Syn_Nonterminals _lhsOextendedNTs _lhsOppA _lhsOppAI _lhsOppCata _lhsOppD _lhsOppDI _lhsOppL _lhsOppLI _lhsOppNtL _lhsOppR _lhsOppSF _lhsOppW ))
sem_Nonterminals_Cons :: T_Nonterminal  ->
                         T_Nonterminals  ->
                         T_Nonterminals 
sem_Nonterminals_Cons (T_Nonterminal hd_ ) (T_Nonterminals tl_ )  =
    (T_Nonterminals (\ _lhsIderivs
                       _lhsIext
                       _lhsInewAtts
                       _lhsInewNTs
                       _lhsInewProds
                       _lhsIo_noGroup
                       _lhsIo_rename
                       _lhsItSyns ->
                         (let _lhsOextendedNTs :: (Set NontermIdent)
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
                              _hdOderivs :: Derivings
                              _hdOext :: (Maybe String)
                              _hdOnewAtts :: ( Attributes )
                              _hdOnewNTs :: (Set NontermIdent)
                              _hdOnewProds :: ( DataTypes )
                              _hdOo_noGroup :: ([String])
                              _hdOo_rename :: Bool
                              _hdOtSyns :: TypeSyns
                              _tlOderivs :: Derivings
                              _tlOext :: (Maybe String)
                              _tlOnewAtts :: ( Attributes )
                              _tlOnewNTs :: (Set NontermIdent)
                              _tlOnewProds :: ( DataTypes )
                              _tlOo_noGroup :: ([String])
                              _tlOo_rename :: Bool
                              _tlOtSyns :: TypeSyns
                              _hdIextendedNTs :: (Set NontermIdent)
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
                              _tlIextendedNTs :: (Set NontermIdent)
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
                              -- use rule "src-ag/AG2AspectAG.ag"(line 93, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 93 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIextendedNTs `Set.union` _tlIextendedNTs
                                   {-# LINE 1593 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 304, column 64)
                              _lhsOppA =
                                  ({-# LINE 304 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppA >-< _tlIppA
                                   {-# LINE 1599 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 317, column 42)
                              _lhsOppAI =
                                  ({-# LINE 317 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppAI ++ _tlIppAI
                                   {-# LINE 1605 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 700, column 67)
                              _lhsOppCata =
                                  ({-# LINE 700 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppCata >-< _tlIppCata
                                   {-# LINE 1611 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 193, column 41)
                              _lhsOppD =
                                  ({-# LINE 193 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppD >-< _tlIppD
                                   {-# LINE 1617 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 193, column 75)
                              _lhsOppDI =
                                  ({-# LINE 193 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppDI ++ _tlIppDI
                                   {-# LINE 1623 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                              _lhsOppL =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppL >-< _tlIppL
                                   {-# LINE 1629 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                              _lhsOppLI =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppLI ++ _tlIppLI
                                   {-# LINE 1635 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 385, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 385 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppNtL ++ _tlIppNtL
                                   {-# LINE 1641 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                              _lhsOppR =
                                  ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppR >-< _tlIppR
                                   {-# LINE 1647 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 736, column 66)
                              _lhsOppSF =
                                  ({-# LINE 736 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppSF >-< _tlIppSF
                                   {-# LINE 1653 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 806, column 42)
                              _lhsOppW =
                                  ({-# LINE 806 "src-ag/AG2AspectAG.ag" #-}
                                   _hdIppW >-< _tlIppW
                                   {-# LINE 1659 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOderivs =
                                  ({-# LINE 187 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 1665 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOext =
                                  ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 1671 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewAtts =
                                  ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 1677 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewNTs =
                                  ({-# LINE 87 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 1683 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOnewProds =
                                  ({-# LINE 73 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 1689 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_noGroup =
                                  ({-# LINE 42 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 1695 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOo_rename =
                                  ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1701 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _hdOtSyns =
                                  ({-# LINE 235 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 1707 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOderivs =
                                  ({-# LINE 187 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIderivs
                                   {-# LINE 1713 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOext =
                                  ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIext
                                   {-# LINE 1719 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewAtts =
                                  ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewAtts
                                   {-# LINE 1725 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewNTs =
                                  ({-# LINE 87 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewNTs
                                   {-# LINE 1731 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOnewProds =
                                  ({-# LINE 73 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsInewProds
                                   {-# LINE 1737 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_noGroup =
                                  ({-# LINE 42 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_noGroup
                                   {-# LINE 1743 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOo_rename =
                                  ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsIo_rename
                                   {-# LINE 1749 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- copy rule (down)
                              _tlOtSyns =
                                  ({-# LINE 235 "src-ag/AG2AspectAG.ag" #-}
                                   _lhsItSyns
                                   {-# LINE 1755 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              ( _hdIextendedNTs,_hdIppA,_hdIppAI,_hdIppCata,_hdIppD,_hdIppDI,_hdIppL,_hdIppLI,_hdIppNtL,_hdIppR,_hdIppSF,_hdIppW) =
                                  hd_ _hdOderivs _hdOext _hdOnewAtts _hdOnewNTs _hdOnewProds _hdOo_noGroup _hdOo_rename _hdOtSyns 
                              ( _tlIextendedNTs,_tlIppA,_tlIppAI,_tlIppCata,_tlIppD,_tlIppDI,_tlIppL,_tlIppLI,_tlIppNtL,_tlIppR,_tlIppSF,_tlIppW) =
                                  tl_ _tlOderivs _tlOext _tlOnewAtts _tlOnewNTs _tlOnewProds _tlOo_noGroup _tlOo_rename _tlOtSyns 
                          in  ( _lhsOextendedNTs,_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW))) )
sem_Nonterminals_Nil :: T_Nonterminals 
sem_Nonterminals_Nil  =
    (T_Nonterminals (\ _lhsIderivs
                       _lhsIext
                       _lhsInewAtts
                       _lhsInewNTs
                       _lhsInewProds
                       _lhsIo_noGroup
                       _lhsIo_rename
                       _lhsItSyns ->
                         (let _lhsOextendedNTs :: (Set NontermIdent)
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
                              -- use rule "src-ag/AG2AspectAG.ag"(line 93, column 52)
                              _lhsOextendedNTs =
                                  ({-# LINE 93 "src-ag/AG2AspectAG.ag" #-}
                                   Set.empty
                                   {-# LINE 1788 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 304, column 64)
                              _lhsOppA =
                                  ({-# LINE 304 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1794 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 317, column 42)
                              _lhsOppAI =
                                  ({-# LINE 317 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 1800 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 700, column 67)
                              _lhsOppCata =
                                  ({-# LINE 700 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1806 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 193, column 41)
                              _lhsOppD =
                                  ({-# LINE 193 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1812 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 193, column 75)
                              _lhsOppDI =
                                  ({-# LINE 193 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 1818 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                              _lhsOppL =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1824 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                              _lhsOppLI =
                                  ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 1830 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 385, column 44)
                              _lhsOppNtL =
                                  ({-# LINE 385 "src-ag/AG2AspectAG.ag" #-}
                                   []
                                   {-# LINE 1836 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                              _lhsOppR =
                                  ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1842 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 736, column 66)
                              _lhsOppSF =
                                  ({-# LINE 736 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1848 "src-ag/AG2AspectAG.hs" #-}
                                   )
                              -- use rule "src-ag/AG2AspectAG.ag"(line 806, column 42)
                              _lhsOppW =
                                  ({-# LINE 806 "src-ag/AG2AspectAG.ag" #-}
                                   empty
                                   {-# LINE 1854 "src-ag/AG2AspectAG.hs" #-}
                                   )
                          in  ( _lhsOextendedNTs,_lhsOppA,_lhsOppAI,_lhsOppCata,_lhsOppD,_lhsOppDI,_lhsOppL,_lhsOppLI,_lhsOppNtL,_lhsOppR,_lhsOppSF,_lhsOppW))) )
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
         child parts          : Patterns 
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
sem_Pattern (Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
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
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    _patIcopy :: Pattern 
                    _patIinfo :: ((Identifier, Identifier))
                    _partsIcopy :: Patterns 
                    -- "src-ag/AG2AspectAG.ag"(line 364, column 25)
                    _lhsOinfo =
                        ({-# LINE 364 "src-ag/AG2AspectAG.ag" #-}
                         (field_, attr_)
                         {-# LINE 1928 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy _partsIcopy
                         {-# LINE 1934 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1940 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patIcopy,_patIinfo) =
                        pat_ 
                    ( _partsIcopy) =
                        parts_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    _patsIcopy :: Patterns 
                    -- "src-ag/AG2AspectAG.ag"(line 365, column 25)
                    _lhsOinfo =
                        ({-# LINE 365 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Constr undefined!!"
                         {-# LINE 1958 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 1964 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1970 "src-ag/AG2AspectAG.hs" #-}
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
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 1986 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 1992 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- copy rule (up)
                    _lhsOinfo =
                        ({-# LINE 362 "src-ag/AG2AspectAG.ag" #-}
                         _patIinfo
                         {-# LINE 1998 "src-ag/AG2AspectAG.hs" #-}
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
                    -- "src-ag/AG2AspectAG.ag"(line 366, column 25)
                    _lhsOinfo =
                        ({-# LINE 366 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Product undefined!!"
                         {-# LINE 2014 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 2020 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2026 "src-ag/AG2AspectAG.hs" #-}
                         )
                    ( _patsIcopy) =
                        pats_ 
                in  ( _lhsOcopy,_lhsOinfo)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOinfo :: ((Identifier, Identifier))
                    _lhsOcopy :: Pattern 
                    -- "src-ag/AG2AspectAG.ag"(line 367, column 25)
                    _lhsOinfo =
                        ({-# LINE 367 "src-ag/AG2AspectAG.ag" #-}
                         error "Pattern Underscore undefined!!"
                         {-# LINE 2040 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 2046 "src-ag/AG2AspectAG.hs" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 23 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 2052 "src-ag/AG2AspectAG.hs" #-}
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
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 2097 "src-ag/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2103 "src-ag/AG2AspectAG.hs" #-}
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
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 2117 "src-ag/AG2AspectAG.hs" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 23 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 2123 "src-ag/AG2AspectAG.hs" #-}
                          )
                 in  ( _lhsOcopy)) )
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inh                  :  Attributes 
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newNT                : Bool
         newProds             :  Map.Map ConstructorIdent FieldMap 
         o_rename             : Bool
         ppNt                 : PP_Doc
         syn                  :  Attributes 
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
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
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
sem_Production (Production _con _children _rules _typeSigs )  =
    (sem_Production_Production _con (sem_Children _children ) (sem_Rules _rules ) (sem_TypeSigs _typeSigs ) )
-- semantic domain
newtype T_Production  = T_Production ((Maybe String) ->
                                      ( Attributes ) ->
                                      ([String]) ->
                                      ( Attributes ) ->
                                      Bool ->
                                      ( Map.Map ConstructorIdent FieldMap ) ->
                                      Bool ->
                                      PP_Doc ->
                                      ( Attributes ) ->
                                      ([String]) ->
                                      ( ( Bool ),PP_Doc,PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc))
data Inh_Production  = Inh_Production {ext_Inh_Production :: (Maybe String),inh_Inh_Production :: ( Attributes ),inhNoGroup_Inh_Production :: ([String]),newAtts_Inh_Production :: ( Attributes ),newNT_Inh_Production :: Bool,newProds_Inh_Production :: ( Map.Map ConstructorIdent FieldMap ),o_rename_Inh_Production :: Bool,ppNt_Inh_Production :: PP_Doc,syn_Inh_Production :: ( Attributes ),synNoGroup_Inh_Production :: ([String])}
data Syn_Production  = Syn_Production {hasMoreProds_Syn_Production :: ( Bool ),ppA_Syn_Production :: PP_Doc,ppCata_Syn_Production :: PP_Doc,ppL_Syn_Production :: PP_Doc,ppLI_Syn_Production :: ([PP_Doc]),ppR_Syn_Production :: PP_Doc,ppRA_Syn_Production :: ([PP_Doc]),ppSF_Syn_Production :: PP_Doc,ppSPF_Syn_Production :: PP_Doc}
wrap_Production :: T_Production  ->
                   Inh_Production  ->
                   Syn_Production 
wrap_Production (T_Production sem ) (Inh_Production _lhsIext _lhsIinh _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynNoGroup )  =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF) = sem _lhsIext _lhsIinh _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynNoGroup 
     in  (Syn_Production _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF ))
sem_Production_Production :: ConstructorIdent ->
                             T_Children  ->
                             T_Rules  ->
                             T_TypeSigs  ->
                             T_Production 
sem_Production_Production con_ (T_Children children_ ) (T_Rules rules_ ) (T_TypeSigs typeSigs_ )  =
    (T_Production (\ _lhsIext
                     _lhsIinh
                     _lhsIinhNoGroup
                     _lhsInewAtts
                     _lhsInewNT
                     _lhsInewProds
                     _lhsIo_rename
                     _lhsIppNt
                     _lhsIsyn
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
                            _childrenOext :: (Maybe String)
                            _childrenOinhNoGroup :: ([String])
                            _childrenOo_rename :: Bool
                            _childrenOppNt :: PP_Doc
                            _childrenOsynNoGroup :: ([String])
                            _rulesOext :: (Maybe String)
                            _rulesOinhNoGroup :: ([String])
                            _rulesOppNt :: PP_Doc
                            _rulesOsynNoGroup :: ([String])
                            _childrenIidCL :: ([(Identifier,Type)])
                            _childrenIppCSF :: ([(Identifier,(PP_Doc,PP_Doc))])
                            _childrenIppL :: PP_Doc
                            _childrenIppLI :: ([PP_Doc])
                            _childrenIppR :: PP_Doc
                            _rulesIlocals :: ([Identifier])
                            _rulesIppRL :: ([ PPRule ])
                            -- "src-ag/AG2AspectAG.ag"(line 91, column 29)
                            _lhsOhasMoreProds =
                                ({-# LINE 91 "src-ag/AG2AspectAG.ag" #-}
                                 not $ Map.member con_ _lhsInewProds
                                 {-# LINE 2237 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 164, column 25)
                            _ppProd =
                                ({-# LINE 164 "src-ag/AG2AspectAG.ag" #-}
                                 pp con_
                                 {-# LINE 2243 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 165, column 25)
                            _prodName =
                                ({-# LINE 165 "src-ag/AG2AspectAG.ag" #-}
                                 ppName [_lhsIppNt, _ppProd    ]
                                 {-# LINE 2249 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 166, column 25)
                            _conName =
                                ({-# LINE 166 "src-ag/AG2AspectAG.ag" #-}
                                 if _lhsIo_rename
                                 then _prodName
                                 else _ppProd
                                 {-# LINE 2257 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 183, column 25)
                            _childrenOppProd =
                                ({-# LINE 183 "src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2263 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 184, column 25)
                            _rulesOppProd =
                                ({-# LINE 184 "src-ag/AG2AspectAG.ag" #-}
                                 _ppProd
                                 {-# LINE 2269 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 262, column 25)
                            _lhsOppL =
                                ({-# LINE 262 "src-ag/AG2AspectAG.ag" #-}
                                 if (Map.member con_ _lhsInewProds)
                                   then _childrenIppL
                                   else empty
                                 {-# LINE 2277 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 266, column 25)
                            _lhsOppLI =
                                ({-# LINE 266 "src-ag/AG2AspectAG.ag" #-}
                                 if (not $ Map.member con_ _lhsInewProds)
                                   then _childrenIppLI
                                   else []
                                 {-# LINE 2285 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 314, column 25)
                            _lhsOppA =
                                ({-# LINE 314 "src-ag/AG2AspectAG.ag" #-}
                                 defLocalAtts _prodName     (length _rulesIlocals) 1 $ sort _rulesIlocals
                                 {-# LINE 2291 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 412, column 25)
                            _newProd =
                                ({-# LINE 412 "src-ag/AG2AspectAG.ag" #-}
                                 Map.member con_ _lhsInewProds
                                 {-# LINE 2297 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 413, column 41)
                            __tup1 =
                                ({-# LINE 413 "src-ag/AG2AspectAG.ag" #-}
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
                                 {-# LINE 2320 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 413, column 41)
                            (_ppR,_) =
                                ({-# LINE 413 "src-ag/AG2AspectAG.ag" #-}
                                 __tup1
                                 {-# LINE 2326 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 413, column 41)
                            (_,_ppRA) =
                                ({-# LINE 413 "src-ag/AG2AspectAG.ag" #-}
                                 __tup1
                                 {-# LINE 2332 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 707, column 25)
                            _lhsOppCata =
                                ({-# LINE 707 "src-ag/AG2AspectAG.ag" #-}
                                 let  extend = maybe  []
                                                      (  \ext ->  if (_lhsInewNT || (not _lhsInewNT && _newProd    ))
                                                                  then []
                                                                  else [ ext >|< ".atts_" >|< _prodName     ])
                                                      _lhsIext
                                 in   "atts_" >|< _prodName     >|< " = " >|<
                                                                    ppListSep "" "" " `ext` "
                                                                    (_ppRA     ++ extend ) >-<
                                      "sem_" >|< _prodName     >|< pp " = knit atts_" >|< _prodName
                                 {-# LINE 2346 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 766, column 25)
                            _lhsOppSF =
                                ({-# LINE 766 "src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppPattern = case (show con_) of
                                                   "Cons"    -> ppParams (ppListSep "" "" " : ")
                                                   "Nil"     -> pp "[]"
                                                   otherwise -> _conName     >|< " " >|< (ppParams ppSpaced)
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "sem_" >|< _lhsIppNt >|< " (" >|< ppPattern >|< ") = sem_" >|< _prodName     >|<
                                      " (" >|< map (fst . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2359 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- "src-ag/AG2AspectAG.ag"(line 778, column 25)
                            _lhsOppSPF =
                                ({-# LINE 778 "src-ag/AG2AspectAG.ag" #-}
                                 let  chi = _childrenIppCSF
                                      ppParams f =   f $ map (((>|<) (pp "_")) . fst) chi
                                 in   "semP_" >|< _lhsIppNt >|< "_" >|< con_ >#< ppParams ppSpaced >|< " = sem_" >|< _prodName     >|<
                                      " (" >|< map (snd . snd) chi >|< "emptyRecord)"
                                 {-# LINE 2368 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                            _lhsOppR =
                                ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                                 _ppR
                                 {-# LINE 2374 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- use rule "src-ag/AG2AspectAG.ag"(line 403, column 43)
                            _lhsOppRA =
                                ({-# LINE 403 "src-ag/AG2AspectAG.ag" #-}
                                 _ppRA
                                 {-# LINE 2380 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOext =
                                ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2386 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOinhNoGroup =
                                ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2392 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOo_rename =
                                ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIo_rename
                                 {-# LINE 2398 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOppNt =
                                ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2404 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _childrenOsynNoGroup =
                                ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2410 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOext =
                                ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIext
                                 {-# LINE 2416 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOinhNoGroup =
                                ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIinhNoGroup
                                 {-# LINE 2422 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOppNt =
                                ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIppNt
                                 {-# LINE 2428 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            -- copy rule (down)
                            _rulesOsynNoGroup =
                                ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                 _lhsIsynNoGroup
                                 {-# LINE 2434 "src-ag/AG2AspectAG.hs" #-}
                                 )
                            ( _childrenIidCL,_childrenIppCSF,_childrenIppL,_childrenIppLI,_childrenIppR) =
                                children_ _childrenOext _childrenOinhNoGroup _childrenOo_rename _childrenOppNt _childrenOppProd _childrenOsynNoGroup 
                            ( _rulesIlocals,_rulesIppRL) =
                                rules_ _rulesOext _rulesOinhNoGroup _rulesOppNt _rulesOppProd _rulesOsynNoGroup 
                        in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF))) )
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inh                  :  Attributes 
         inhNoGroup           : [String]
         newAtts              :  Attributes 
         newNT                : Bool
         newProds             :  Map.Map ConstructorIdent FieldMap 
         o_rename             : Bool
         ppNt                 : PP_Doc
         syn                  :  Attributes 
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
                                        ([String]) ->
                                        ( Attributes ) ->
                                        Bool ->
                                        ( Map.Map ConstructorIdent FieldMap ) ->
                                        Bool ->
                                        PP_Doc ->
                                        ( Attributes ) ->
                                        ([String]) ->
                                        ( ( Bool ),PP_Doc,PP_Doc,PP_Doc,([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc))
data Inh_Productions  = Inh_Productions {ext_Inh_Productions :: (Maybe String),inh_Inh_Productions :: ( Attributes ),inhNoGroup_Inh_Productions :: ([String]),newAtts_Inh_Productions :: ( Attributes ),newNT_Inh_Productions :: Bool,newProds_Inh_Productions :: ( Map.Map ConstructorIdent FieldMap ),o_rename_Inh_Productions :: Bool,ppNt_Inh_Productions :: PP_Doc,syn_Inh_Productions :: ( Attributes ),synNoGroup_Inh_Productions :: ([String])}
data Syn_Productions  = Syn_Productions {hasMoreProds_Syn_Productions :: ( Bool ),ppA_Syn_Productions :: PP_Doc,ppCata_Syn_Productions :: PP_Doc,ppL_Syn_Productions :: PP_Doc,ppLI_Syn_Productions :: ([PP_Doc]),ppR_Syn_Productions :: PP_Doc,ppRA_Syn_Productions :: ([PP_Doc]),ppSF_Syn_Productions :: PP_Doc,ppSPF_Syn_Productions :: PP_Doc}
wrap_Productions :: T_Productions  ->
                    Inh_Productions  ->
                    Syn_Productions 
wrap_Productions (T_Productions sem ) (Inh_Productions _lhsIext _lhsIinh _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynNoGroup )  =
    (let ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF) = sem _lhsIext _lhsIinh _lhsIinhNoGroup _lhsInewAtts _lhsInewNT _lhsInewProds _lhsIo_rename _lhsIppNt _lhsIsyn _lhsIsynNoGroup 
     in  (Syn_Productions _lhsOhasMoreProds _lhsOppA _lhsOppCata _lhsOppL _lhsOppLI _lhsOppR _lhsOppRA _lhsOppSF _lhsOppSPF ))
sem_Productions_Cons :: T_Production  ->
                        T_Productions  ->
                        T_Productions 
sem_Productions_Cons (T_Production hd_ ) (T_Productions tl_ )  =
    (T_Productions (\ _lhsIext
                      _lhsIinh
                      _lhsIinhNoGroup
                      _lhsInewAtts
                      _lhsInewNT
                      _lhsInewProds
                      _lhsIo_rename
                      _lhsIppNt
                      _lhsIsyn
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
                             _hdOext :: (Maybe String)
                             _hdOinh :: ( Attributes )
                             _hdOinhNoGroup :: ([String])
                             _hdOnewAtts :: ( Attributes )
                             _hdOnewNT :: Bool
                             _hdOnewProds :: ( Map.Map ConstructorIdent FieldMap )
                             _hdOo_rename :: Bool
                             _hdOppNt :: PP_Doc
                             _hdOsyn :: ( Attributes )
                             _hdOsynNoGroup :: ([String])
                             _tlOext :: (Maybe String)
                             _tlOinh :: ( Attributes )
                             _tlOinhNoGroup :: ([String])
                             _tlOnewAtts :: ( Attributes )
                             _tlOnewNT :: Bool
                             _tlOnewProds :: ( Map.Map ConstructorIdent FieldMap )
                             _tlOo_rename :: Bool
                             _tlOppNt :: PP_Doc
                             _tlOsyn :: ( Attributes )
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
                             _tlIhasMoreProds :: ( Bool )
                             _tlIppA :: PP_Doc
                             _tlIppCata :: PP_Doc
                             _tlIppL :: PP_Doc
                             _tlIppLI :: ([PP_Doc])
                             _tlIppR :: PP_Doc
                             _tlIppRA :: ([PP_Doc])
                             _tlIppSF :: PP_Doc
                             _tlIppSPF :: PP_Doc
                             -- use rule "src-ag/AG2AspectAG.ag"(line 89, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 89 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIhasMoreProds  ||  _tlIhasMoreProds
                                  {-# LINE 2561 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 304, column 64)
                             _lhsOppA =
                                 ({-# LINE 304 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppA >-< _tlIppA
                                  {-# LINE 2567 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 700, column 67)
                             _lhsOppCata =
                                 ({-# LINE 700 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppCata >-< _tlIppCata
                                  {-# LINE 2573 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                             _lhsOppL =
                                 ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppL >-< _tlIppL
                                  {-# LINE 2579 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                             _lhsOppLI =
                                 ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppLI ++ _tlIppLI
                                  {-# LINE 2585 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                             _lhsOppR =
                                 ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppR >-< _tlIppR
                                  {-# LINE 2591 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 403, column 43)
                             _lhsOppRA =
                                 ({-# LINE 403 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppRA ++ _tlIppRA
                                  {-# LINE 2597 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 736, column 66)
                             _lhsOppSF =
                                 ({-# LINE 736 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSF >-< _tlIppSF
                                  {-# LINE 2603 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 737, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 737 "src-ag/AG2AspectAG.ag" #-}
                                  _hdIppSPF >-< _tlIppSPF
                                  {-# LINE 2609 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOext =
                                 ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 2615 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinh =
                                 ({-# LINE 726 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2621 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOinhNoGroup =
                                 ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinhNoGroup
                                  {-# LINE 2627 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewAtts =
                                 ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 2633 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewNT =
                                 ({-# LINE 396 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 2639 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOnewProds =
                                 ({-# LINE 74 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 2645 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOo_rename =
                                 ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 2651 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOppNt =
                                 ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 2657 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsyn =
                                 ({-# LINE 726 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2663 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOsynNoGroup =
                                 ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 2669 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOext =
                                 ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIext
                                  {-# LINE 2675 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinh =
                                 ({-# LINE 726 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinh
                                  {-# LINE 2681 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOinhNoGroup =
                                 ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIinhNoGroup
                                  {-# LINE 2687 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewAtts =
                                 ({-# LINE 66 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewAtts
                                  {-# LINE 2693 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewNT =
                                 ({-# LINE 396 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewNT
                                  {-# LINE 2699 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOnewProds =
                                 ({-# LINE 74 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsInewProds
                                  {-# LINE 2705 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOo_rename =
                                 ({-# LINE 38 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIo_rename
                                  {-# LINE 2711 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOppNt =
                                 ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIppNt
                                  {-# LINE 2717 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsyn =
                                 ({-# LINE 726 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsyn
                                  {-# LINE 2723 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOsynNoGroup =
                                 ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                                  _lhsIsynNoGroup
                                  {-# LINE 2729 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             ( _hdIhasMoreProds,_hdIppA,_hdIppCata,_hdIppL,_hdIppLI,_hdIppR,_hdIppRA,_hdIppSF,_hdIppSPF) =
                                 hd_ _hdOext _hdOinh _hdOinhNoGroup _hdOnewAtts _hdOnewNT _hdOnewProds _hdOo_rename _hdOppNt _hdOsyn _hdOsynNoGroup 
                             ( _tlIhasMoreProds,_tlIppA,_tlIppCata,_tlIppL,_tlIppLI,_tlIppR,_tlIppRA,_tlIppSF,_tlIppSPF) =
                                 tl_ _tlOext _tlOinh _tlOinhNoGroup _tlOnewAtts _tlOnewNT _tlOnewProds _tlOo_rename _tlOppNt _tlOsyn _tlOsynNoGroup 
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF))) )
sem_Productions_Nil :: T_Productions 
sem_Productions_Nil  =
    (T_Productions (\ _lhsIext
                      _lhsIinh
                      _lhsIinhNoGroup
                      _lhsInewAtts
                      _lhsInewNT
                      _lhsInewProds
                      _lhsIo_rename
                      _lhsIppNt
                      _lhsIsyn
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
                             -- use rule "src-ag/AG2AspectAG.ag"(line 89, column 51)
                             _lhsOhasMoreProds =
                                 ({-# LINE 89 "src-ag/AG2AspectAG.ag" #-}
                                  False
                                  {-# LINE 2761 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 304, column 64)
                             _lhsOppA =
                                 ({-# LINE 304 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2767 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 700, column 67)
                             _lhsOppCata =
                                 ({-# LINE 700 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2773 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 79)
                             _lhsOppL =
                                 ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2779 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 246, column 112)
                             _lhsOppLI =
                                 ({-# LINE 246 "src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 2785 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 402, column 79)
                             _lhsOppR =
                                 ({-# LINE 402 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2791 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 403, column 43)
                             _lhsOppRA =
                                 ({-# LINE 403 "src-ag/AG2AspectAG.ag" #-}
                                  []
                                  {-# LINE 2797 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 736, column 66)
                             _lhsOppSF =
                                 ({-# LINE 736 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2803 "src-ag/AG2AspectAG.hs" #-}
                                  )
                             -- use rule "src-ag/AG2AspectAG.ag"(line 737, column 42)
                             _lhsOppSPF =
                                 ({-# LINE 737 "src-ag/AG2AspectAG.ag" #-}
                                  empty
                                  {-# LINE 2809 "src-ag/AG2AspectAG.hs" #-}
                                  )
                         in  ( _lhsOhasMoreProds,_lhsOppA,_lhsOppCata,_lhsOppL,_lhsOppLI,_lhsOppR,_lhsOppRA,_lhsOppSF,_lhsOppSPF))) )
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ext                  : Maybe String
         inhNoGroup           : [String]
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
-}
-- cata
sem_Rule :: Rule  ->
            T_Rule 
sem_Rule (Rule _mbName _pattern _rhs _owrt _origin _explicit )  =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern ) (sem_Expression _rhs ) _owrt _origin _explicit )
-- semantic domain
newtype T_Rule  = T_Rule ((Maybe String) ->
                          ([String]) ->
                          PP_Doc ->
                          PP_Doc ->
                          ([String]) ->
                          ( ([Identifier]),([ PPRule ])))
data Inh_Rule  = Inh_Rule {ext_Inh_Rule :: (Maybe String),inhNoGroup_Inh_Rule :: ([String]),ppNt_Inh_Rule :: PP_Doc,ppProd_Inh_Rule :: PP_Doc,synNoGroup_Inh_Rule :: ([String])}
data Syn_Rule  = Syn_Rule {locals_Syn_Rule :: ([Identifier]),ppRL_Syn_Rule :: ([ PPRule ])}
wrap_Rule :: T_Rule  ->
             Inh_Rule  ->
             Syn_Rule 
wrap_Rule (T_Rule sem ) (Inh_Rule _lhsIext _lhsIinhNoGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Rule _lhsOlocals _lhsOppRL ))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern  ->
                 T_Expression  ->
                 Bool ->
                 String ->
                 Bool ->
                 T_Rule 
sem_Rule_Rule mbName_ (T_Pattern pattern_ ) (T_Expression rhs_ ) owrt_ origin_ explicit_  =
    (T_Rule (\ _lhsIext
               _lhsIinhNoGroup
               _lhsIppNt
               _lhsIppProd
               _lhsIsynNoGroup ->
                 (let _lhsOlocals :: ([Identifier])
                      _lhsOppRL :: ([ PPRule ])
                      _rhsOppNt :: PP_Doc
                      _rhsOppProd :: PP_Doc
                      _patternIcopy :: Pattern 
                      _patternIinfo :: ((Identifier, Identifier))
                      _rhsIppRE :: (Identifier -> [String] -> [String] -> [(Identifier,Type)] -> [Identifier] -> PP_Doc)
                      -- "src-ag/AG2AspectAG.ag"(line 357, column 25)
                      _lhsOlocals =
                          ({-# LINE 357 "src-ag/AG2AspectAG.ag" #-}
                           if (show (fst _patternIinfo) == "loc")
                            then [ snd _patternIinfo ]
                            else [ ]
                           {-# LINE 2879 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- "src-ag/AG2AspectAG.ag"(line 456, column 33)
                      _lhsOppRL =
                          ({-# LINE 456 "src-ag/AG2AspectAG.ag" #-}
                           [ ppRule _patternIinfo owrt_ (defRule _lhsIppNt _patternIinfo _lhsIinhNoGroup _lhsIsynNoGroup _rhsIppRE) ]
                           {-# LINE 2885 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppNt =
                          ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                           _lhsIppNt
                           {-# LINE 2891 "src-ag/AG2AspectAG.hs" #-}
                           )
                      -- copy rule (down)
                      _rhsOppProd =
                          ({-# LINE 180 "src-ag/AG2AspectAG.ag" #-}
                           _lhsIppProd
                           {-# LINE 2897 "src-ag/AG2AspectAG.hs" #-}
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
                            PP_Doc ->
                            PP_Doc ->
                            ([String]) ->
                            ( ([Identifier]),([ PPRule ])))
data Inh_Rules  = Inh_Rules {ext_Inh_Rules :: (Maybe String),inhNoGroup_Inh_Rules :: ([String]),ppNt_Inh_Rules :: PP_Doc,ppProd_Inh_Rules :: PP_Doc,synNoGroup_Inh_Rules :: ([String])}
data Syn_Rules  = Syn_Rules {locals_Syn_Rules :: ([Identifier]),ppRL_Syn_Rules :: ([ PPRule ])}
wrap_Rules :: T_Rules  ->
              Inh_Rules  ->
              Syn_Rules 
wrap_Rules (T_Rules sem ) (Inh_Rules _lhsIext _lhsIinhNoGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup )  =
    (let ( _lhsOlocals,_lhsOppRL) = sem _lhsIext _lhsIinhNoGroup _lhsIppNt _lhsIppProd _lhsIsynNoGroup 
     in  (Syn_Rules _lhsOlocals _lhsOppRL ))
sem_Rules_Cons :: T_Rule  ->
                  T_Rules  ->
                  T_Rules 
sem_Rules_Cons (T_Rule hd_ ) (T_Rules tl_ )  =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       _hdOext :: (Maybe String)
                       _hdOinhNoGroup :: ([String])
                       _hdOppNt :: PP_Doc
                       _hdOppProd :: PP_Doc
                       _hdOsynNoGroup :: ([String])
                       _tlOext :: (Maybe String)
                       _tlOinhNoGroup :: ([String])
                       _tlOppNt :: PP_Doc
                       _tlOppProd :: PP_Doc
                       _tlOsynNoGroup :: ([String])
                       _hdIlocals :: ([Identifier])
                       _hdIppRL :: ([ PPRule ])
                       _tlIlocals :: ([Identifier])
                       _tlIppRL :: ([ PPRule ])
                       -- "src-ag/AG2AspectAG.ag"(line 452, column 33)
                       _lhsOppRL =
                           ({-# LINE 452 "src-ag/AG2AspectAG.ag" #-}
                            _hdIppRL ++ _tlIppRL
                            {-# LINE 2971 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- use rule "src-ag/AG2AspectAG.ag"(line 353, column 30)
                       _lhsOlocals =
                           ({-# LINE 353 "src-ag/AG2AspectAG.ag" #-}
                            _hdIlocals ++ _tlIlocals
                            {-# LINE 2977 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOext =
                           ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 2983 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOinhNoGroup =
                           ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 2989 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppNt =
                           ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 2995 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOppProd =
                           ({-# LINE 180 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3001 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _hdOsynNoGroup =
                           ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3007 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOext =
                           ({-# LINE 107 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIext
                            {-# LINE 3013 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOinhNoGroup =
                           ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIinhNoGroup
                            {-# LINE 3019 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppNt =
                           ({-# LINE 175 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppNt
                            {-# LINE 3025 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOppProd =
                           ({-# LINE 180 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIppProd
                            {-# LINE 3031 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- copy rule (down)
                       _tlOsynNoGroup =
                           ({-# LINE 52 "src-ag/AG2AspectAG.ag" #-}
                            _lhsIsynNoGroup
                            {-# LINE 3037 "src-ag/AG2AspectAG.hs" #-}
                            )
                       ( _hdIlocals,_hdIppRL) =
                           hd_ _hdOext _hdOinhNoGroup _hdOppNt _hdOppProd _hdOsynNoGroup 
                       ( _tlIlocals,_tlIppRL) =
                           tl_ _tlOext _tlOinhNoGroup _tlOppNt _tlOppProd _tlOsynNoGroup 
                   in  ( _lhsOlocals,_lhsOppRL))) )
sem_Rules_Nil :: T_Rules 
sem_Rules_Nil  =
    (T_Rules (\ _lhsIext
                _lhsIinhNoGroup
                _lhsIppNt
                _lhsIppProd
                _lhsIsynNoGroup ->
                  (let _lhsOppRL :: ([ PPRule ])
                       _lhsOlocals :: ([Identifier])
                       -- "src-ag/AG2AspectAG.ag"(line 453, column 33)
                       _lhsOppRL =
                           ({-# LINE 453 "src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3057 "src-ag/AG2AspectAG.hs" #-}
                            )
                       -- use rule "src-ag/AG2AspectAG.ag"(line 353, column 30)
                       _lhsOlocals =
                           ({-# LINE 353 "src-ag/AG2AspectAG.ag" #-}
                            []
                            {-# LINE 3063 "src-ag/AG2AspectAG.hs" #-}
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