module Parser
where
import Data.Maybe
import UU.Parsing
import UU.Parsing.Machine(RealParser(..),RealRecogn(..),anaDynE,mkPR)
import ConcreteSyntax
import CommonTypes
import Patterns
import UU.Pretty(text,PP_Doc,empty,(>-<))
import TokenDef
import Data.List (intersperse)
import Data.Char
import Scanner (Input(..),scanLit,input)
import Data.List
import Expression
import Macro --marcos
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.GenToken
import UU.Scanner.GenTokenParser
import UU.Scanner.Position
import UU.Scanner.TokenShow()
import System.Directory
import System.FilePath
import HsTokenScanner
import Options


type AGParser = AnaParser Input  Pair Token Pos

pIdentifier, pIdentifierU :: AGParser Identifier
pIdentifierU = uncurry Ident <$> pConidPos
pIdentifier   = uncurry Ident <$> pVaridPos


parseAG :: Options -> [FilePath] -> String -> IO (AG,[Message Token Pos])
parseAG opts searchPath file
              = do (es,_,_,_,mesg) <- parseFile False opts searchPath file
                   return (AG es, mesg)

--marcos
parseAGI :: Options -> [FilePath] -> String -> IO (AG, Maybe String)
parseAGI opts searchPath file
              = do (es,_,_,ext,_) <- parseFile True opts searchPath file
                   return (AG es, ext)


depsAG :: Options -> [FilePath] -> String -> IO ([String], [Message Token Pos])
depsAG opts searchPath file
  = do (_,_,fs,_,mesgs) <- parseFile False opts searchPath file
       return (fs, mesgs)

-- marcos: added the parameter 'agi' and the 'ext' part
parseFile :: Bool -> Options -> [FilePath] -> String -> IO  ([Elem],[String],[String], Maybe String,[Message Token Pos ])
parseFile agi opts searchPath file
 = do txt <- readFile file
      let litMode = ".lag" `isSuffixOf` file
          (files,text) = if litMode then scanLit txt
                         else ([],txt)
          tokens       = input opts (initPos file) text

          steps = parse (pElemsFiles agi) tokens
          stop (_,fs,_,_,_) = null fs
          cont (es,fs,allfs,ext,msg)
            = do files <- mapM (resolveFile searchPath) fs
                 res <- mapM (parseFile agi opts searchPath) files
                 let (ess,fss,allfss,_, msgs) = unzip5 res
                 return (es ++ concat ess, concat fss, concat allfss ++ allfs, ext, msg ++ concat msgs)
      let (Pair (es,fls,ext) _ ,mesg) = evalStepsMessages steps
      let allfs = files ++ fls
      loopp stop cont (es,allfs,allfs, ext,mesg)
 where

    --
    -- Option dependent AG Parsers inlined here
    -- to have access to the opts
    -- while retaining sharing
    --

    pElemsFiles :: Bool -> AGParser ([Elem],[String],Maybe String)
    pElemsFiles agi = pFoldr (($),([],[],Nothing)) pElem'
       where pElem' =  addElem <$> pElem
                   <|> pINCLUDE *> (addInc <$> pStringPos)
                   <|> pEXTENDS *> (addExt <$> pStringPos)
             addElem e      (es,fs,ext) = (e:es,   fs, ext)
             addInc  (fn,_) (es,fs,ext) = (  es,fn:fs, ext)
             addExt  (fn,_) (es,fs,ext) = if agi then (es,fs, Just fn) else (es,fn:fs, ext) --marcos

    pCodescrapL = (\(ValToken _ str pos) -> (str, pos))<$>
                        parseScrapL <?> "a code block"

    parseScrapL :: AGParser Token
    parseScrapL
               = let p acc =  (\k (Input pos str next) ->
                               let (sc,rest) = case next of
                                     Just (t@(ValToken TkTextln _ _), rs) -> (t,rs)
                                     _ -> let (tok,p2,inp2) = codescrapL pos str
                                          in (tok, input opts p2 inp2)
                                   steps   = k ( rest)
                               in  (val (acc sc)  steps)
                          )
                 in anaDynE  (mkPR (P (p  ), R (p (const id))))

    pElems :: AGParser Elems
    pElems = pList_ng pElem

    pComplexType =  List   <$> pBracks pTypeEncapsulated
               <|> Maybe  <$ pMAYBE <*> pType
               <|> Either <$ pEITHER <*> pType <*> pType
               <|> Map    <$ pMAP <*> pTypePrimitive <*> pType
               <|> IntMap <$ pINTMAP <*> pType
               <|> tuple  <$> pParens (pListSep pComma field)
      where field = (,) <$> ((Just <$> pIdentifier <* pTypeColon) `opt` Nothing) <*> pTypeEncapsulated
            tuple xs = Tuple [(fromMaybe (Ident ("x"++show n) noPos) f, t)
                             | (n,(f,t)) <- zip [1..] xs
                             ]

    pElem :: AGParser Elem
    pElem
         =  Data <$> pDATA
                 <*> pOptClassContext
                 <*> pNontSet
                 <*> pList pIdentifier
                 <*> pOptAttrs
                 <*> pAlts
                 <*> pSucceed False
        <|> Attr <$> pATTR
                 <*> pOptClassContext
                 <*> pNontSet
                 <*> pOptQuantifiers
                 <*> pAttrs
        <|> Type <$> pTYPE
                 <*> pOptClassContext
                 <*> pIdentifierU
                 <*> pList pIdentifier
                 <*  pEquals
                 <*> pComplexType
        <|> Sem  <$> pSEM
                 <*> pOptClassContext
                 <*> pNontSet
                 <*> pOptAttrs
                 <*> pOptQuantifiers
                 <*> pSemAlts
        <|> Set  <$> pSET
                 <*> pIdentifierU
                 <*> (   False <$ pEquals
                     <|> True  <$ pColon
                     )
                 <*> pNontSet
        <|> Deriving
                 <$> pDERIVING
                 <*> pNontSet
                 <*  pColon
                 <*> pListSep pComma pIdentifierU
        <|> Wrapper
                 <$> pWRAPPER
                 <*> pNontSet
        <|> Nocatas
                 <$> pNOCATAS
                 <*> pNontSet
        <|> Pragma
                 <$> pPRAGMA
                 <*> pNames
        <|> Module
                 <$> pMODULE
                 <*> pCodescrap'
                 <*> pCodescrap'
                 <*> pCodescrap'
        <|> codeBlock <$> (pIdentifier <|> pSucceed (Ident "" noPos)) <*> ((Just <$ pATTACH <*> pIdentifierU) <|> pSucceed Nothing) <*> pCodeBlock <?> "a statement"
              where codeBlock nm mbNt (txt,pos) = Txt pos nm mbNt (lines txt)


    pAttrs :: AGParser Attrs
    pAttrs
        = Attrs <$> pOBrackPos <*> (concat <$> pList pInhAttrNames <?> "inherited attribute declarations")
                                  <* pBar    <*> (concat <$> pList pAttrNames <?> "chained attribute declarations"  )
                                  <* pBar    <*> (concat <$> pList pAttrNames <?> "synthesised attribute declarations"  )
                   <*  pCBrack
           <|> (\ds -> Attrs (fst $ head ds) [n | (_,(nms,_,_)) <- ds, n <- nms] [n | (_,(_,nms,_)) <- ds, n <- nms] [n | (_,(_,_,nms)) <- ds, n <- nms]) <$> pList1 pSingleAttrDefs

    pSingleAttrDefs :: AGParser (Pos, (AttrNames, AttrNames, AttrNames))
    pSingleAttrDefs
      =    (\p is -> (p, (is,[],[]))) <$> pINH <*> pList1Sep pComma pSingleInhAttrDef
      <|>  (\p is -> (p, ([],[],is))) <$> pSYN <*> pList1Sep pComma pSingleSynAttrDef
      <|>  (\p is -> (p, ([],is,[]))) <$> pCHN <*> pList1Sep pComma pSingleChnAttrDef

    pSingleInhAttrDef :: AGParser (Identifier,Type,(String,String,String))
    pSingleInhAttrDef
      = (\v tp -> (v,tp,("","",""))) <$> pIdentifier <* pTypeColon <*> pType <?> "inh attribute declaration"

    pSingleSynAttrDef :: AGParser (Identifier,Type,(String,String,String))
    pSingleSynAttrDef
      = (\v u tp -> (v,tp,u)) <$> pIdentifier <*> pUse <* pTypeColon <*> pType <?> "syn attribute declaration"

    pSingleChnAttrDef :: AGParser (Identifier,Type,(String,String,String))
    pSingleChnAttrDef
      = (\v tp -> (v,tp,("","",""))) <$> pIdentifier <* pTypeColon <*> pType <?> "chn attribute declaration"

    pOptAttrs :: AGParser Attrs
    pOptAttrs = pAttrs `opt` Attrs noPos [] [] []

    pInhAttrNames :: AGParser AttrNames
    pInhAttrNames
                   = (\vs tp -> map (\v -> (v,tp,("","",""))) vs)
                      <$> pIdentifiers <*  pTypeColon <*> pType <?> "attribute declarations"

    pAttrNames :: AGParser AttrNames
    pAttrNames
             = (\vs use tp -> map (\v -> (v,tp,use)) vs)
                 <$> pIdentifiers <*> pUse <* pTypeColon <*> pType <?> "attribute declarations"

    pAlt :: AGParser Alt
    pAlt =  Alt <$> pBar <*> pSimpleConstructorSet <*> pFields <*> pMaybeMacro <?> "a datatype alternative" --marcos

    pAlts :: AGParser Alts
    pAlts =  pList_ng pAlt <?> "datatype alternatives"

    pFields :: AGParser Fields
    pFields = concat <$> pList_ng pField <?> "fields"

    pField :: AGParser Fields
    pField
           =  (\nms tp -> map (flip (,) tp) nms)
              <$> pIdentifiers <* pTypeColon <*> pType
              <|> (\s -> [(Ident (mklower (getName s)) (getPos s) ,NT s [])]) <$> pIdentifierU

    pSemAlt :: AGParser SemAlt
    pSemAlt = SemAlt <$> pBar <*> pConstructorSet <*> pSemDefs <?> "SEM alternative"

    pSemAlts :: AGParser SemAlts
    pSemAlts =  pList pSemAlt <?> "SEM alternatives"

    pSemDef :: AGParser [SemDef]
    pSemDef
          =   (\x y fs -> map (\f -> f x y) fs) <$> pMaybeRuleName <*> pFieldIdentifier <*> pList1 pAttrDef
          <|>                            pLOC              *> pList1 pLocDecl
          <|>                            pINST             *> pList1 pInstDecl
          <|>  pSEMPRAGMA *> pList1 (SemPragma <$> pNames)
          <|> (\n e -> [AugmentDef n e]) <$ pAugmentToken <*> pIdentifier <* pAssign <*> pExpr
          <|> (\n e -> [AroundDef n e]) <$ pAROUND <*> pIdentifier <* pAssign <*> pExpr
          <|> (\a b -> [AttrOrderBefore a [b]]) <$> pList1 pAttrOrIdent <* pSmaller <*> pAttrOrIdent
          <|> (\sources target nt expr -> [MergeDef target nt sources expr]) <$ pMERGE <*> (pList1_ng pIdentifier <* pAS <|> pSucceed []) <*> pIdentifier <* pTypeColon <*> pIdentifierU <* pAssign <*> pExpr
          <|> (\mbNm pat (owrt,pos,pur) exp -> [Def pos mbNm (pat ()) exp owrt pur]) <$> pMaybeRuleName <*> pPattern (const <$> pAttr) <*> pRuleSym <*> pExpr

    pMaybeRuleName :: AGParser (Maybe Identifier)
    pMaybeRuleName
      =   (Just <$> pIdentifier <* pColon <?> "rule name")
      <|> pSucceed Nothing

    pAttrDef :: AGParser (Maybe Identifier -> Identifier -> SemDef)
    pAttrDef
      = (\pat (owrt,pos,pur) exp mbNm fld -> Def pos mbNm (pat fld) exp owrt pur)
               <$ pDot <*> pattern <*> pRuleSym <*> pExpr
      where pattern =  pPattern pVar
                   <|> (\ir a fld -> ir $ Alias fld a (Underscore noPos) []) <$> ((Irrefutable <$ pTilde) `opt` id) <*> pIdentifier

    pLocDecl :: AGParser SemDef
    pLocDecl = pDot <**> (pIdentifier <**> (pTypeColon <**> (   (\(tp,pos) _ ident _  -> TypeDef pos ident tp) <$> pLocType
                                                            <|> (\ref _ ident _ -> UniqueDef ident ref) <$ pUNIQUEREF <*> pIdentifier )))


    pInstDecl :: AGParser SemDef
    pInstDecl = (\ident tp -> TypeDef (getPos ident) ident tp)
                  <$ pDot <*> pIdentifier <* pTypeColon <*> pTypeNt

    pSemDefs :: AGParser SemDefs
    pSemDefs =  concat <$> pList_ng pSemDef <?> "attribute rules"

    pExpr :: AGParser Expression
    pExpr = (\(str,pos) ->  Expression pos (lexTokens pos str)) <$> pCodescrapL <?> "an expression"

    pTypeColon :: AGParser Pos
    pTypeColon
      = if doubleColons opts
        then pDoubleColon
        else pColon

    --marcos
    pMaybeMacro :: AGParser MaybeMacro
    pMaybeMacro  =  Just <$ pDoubleArrow <*>  pMacro
                <|> pSucceed Nothing

    pMacro :: AGParser Macro
    pMacro
          =  Macro <$> pIdentifierU
                   <*> pList1 pMacroChild
                   <?> "macro"

    pMacroChild :: AGParser MacroChild
    pMacroChild
          =  (pIdentifier <* pEquals) <**>
             (flip RuleChild   <$> pMacro       <|>
              flip ChildChild  <$> pIdentifier  <|>
              flip ValueChild  <$> pCodescrap' )

    --
    -- End of AG Parser
    --

resolveFile :: [FilePath] -> FilePath -> IO FilePath
resolveFile path fname = search (path ++ ["."])
 where search (p:ps) = do let filename = joinPath [p, fname]
                          fExists <- doesFileExist filename
                          if fExists
                            then return filename
                            else search ps
       search []     = error ("File: " ++ show fname ++ " not found in search path: " ++ show (concat (intersperse ";" (path ++ ["."]))) )

pathSeparator = "/"

evalStepsMessages :: (Eq s, Show s, Show p) => Steps a s p -> (a,[Message s p])
evalStepsMessages steps = case steps of
  OkVal v        rest -> let (arg,ms) = evalStepsMessages rest
                         in (v arg,ms)
  Ok             rest -> evalStepsMessages rest
  Cost  _        rest -> evalStepsMessages rest
  StRepair _ msg rest -> let (v,ms) = evalStepsMessages rest
                         in (v, msg:ms)
  Best _   rest   _   -> evalStepsMessages rest
  NoMoreSteps v       -> (v,[])

loopp ::(a->Bool) -> (a->IO a) -> a ->  IO a
loopp pred cont x | pred x = return x
                  | otherwise = do x' <- cont x
                                   loopp pred cont x'

codescrapL p []                 = (valueToken TkTextln "" p,p,[])
codescrapL p (x:xs) | isSpace x = (updPos'  x p)  codescrapL xs
                    | otherwise = let refcol = column p
                                      (p',sc,rest) = scrapL refcol p  (x:xs)
                                  in (valueToken TkTextln sc p,p',rest)

scrapL ref p (x:xs) | isSpace x || column p >= ref =
                          let (p'',sc,inp) = updPos'  x p (scrapL ref)  xs
                          in  (p'',x:sc,inp)
                    | otherwise       =(p,[],x:xs)
scrapL ref p []     = (p,[],[])

pNontSet = set0
  where set0 = pChainr (Intersect <$ pIntersect) set1
        set1 = pChainl (Difference <$ pMinus) set2
        set2 = pChainr (pSucceed Union) set3
        set3 = pIdentifierU <**> opt (flip Path <$ pArrow <*> pIdentifierU) NamedSet
            <|> All <$ pStar
            <|> pParens set0

pNames :: AGParser [Identifier]
pNames = pList1 pIdentifier




-- Insertion is expensive for pCodeBlock in order to prevent infinite inserts.
pCodeBlock ::  AGParser (String,Pos)
pCodeBlock   = pCostValToken 90 TkTextln "" <?> "a code block"

pOptClassContext :: AGParser ClassContext
pOptClassContext
  =   pClassContext <* pDoubleArrow
  <|> pSucceed []

pClassContext :: AGParser ClassContext
pClassContext
  = pListSep pComma ((,) <$> pIdentifierU <*> pList pTypeHaskellAnyAsString)

pOptQuantifiers :: AGParser [String]
pOptQuantifiers = (return <$ pDoubleColon <*> pCodescrap') `opt` []

pTypeNt :: AGParser Type
pTypeNt
  =   ((\nt -> NT nt []) <$> pIdentifierU <?> "nonterminal name (no brackets)")
  <|> (pParens (NT <$> pIdentifierU <*> pList pTypeHaskellAnyAsString) <?> "nonterminal name with parameters (using parenthesis)")

pTypeHaskellAnyAsString :: AGParser String
pTypeHaskellAnyAsString
  =   getName <$> pIdentifier
  <|> getName <$> pIdentifierU
  <|> pCodescrap' <?> "a type"

-- if the type is within some kind of parentheses or brackets (then we allow lowercase identifiers as well)
pTypeEncapsulated :: AGParser Type
pTypeEncapsulated
  =   pParens pTypeEncapsulated
  <|> NT <$> pIdentifierU <*> pList pTypeHaskellAnyAsString
  <|> (Haskell . getName) <$> pIdentifier
  <|> pTypePrimitive

pTypePrimitive :: AGParser Type
pTypePrimitive
  = Haskell <$> pCodescrap'  <?> "a type"

pType :: AGParser Type
pType =  pTypeNt
     <|> pTypePrimitive

pIdentifiers :: AGParser [Identifier]
pIdentifiers =  pList1Sep pComma pIdentifier <?> "lowercase identifiers"

pUse :: AGParser (String,String,String)
pUse = (  (\u x y->(x,y,show u)) <$> pUSE <*> pCodescrap'  <*> pCodescrap') `opt` ("","","") <?> "USE declaration"

mklower :: String -> String
mklower (x:xs) = toLower x : xs
mklower []     = []

pSimpleConstructorSet :: AGParser ConstructorSet
pSimpleConstructorSet =  CName <$> pIdentifierU
                     <|> CAll  <$  pStar
                     <|> pParens pConstructorSet

pConstructorSet :: AGParser ConstructorSet
pConstructorSet =  pChainl (CDifference <$ pMinus) term2
  where term2 =  pChainr (pSucceed CUnion) term1
        term1 =  CName <$> pIdentifierU
             <|> CAll  <$  pStar

pFieldIdentifier :: AGParser Identifier
pFieldIdentifier =  pIdentifier
                <|> Ident "lhs"  <$> pLHS
                <|> Ident "loc"  <$> pLOC
                <|> Ident "inst" <$> pINST

pAugmentToken :: AGParser ()
pAugmentToken = () <$ (pAUGMENT <|> pPlus)

pAttr = (,) <$> pFieldIdentifier <* pDot <*> pIdentifier

pAttrOrIdent
  = OccAttr <$> pFieldIdentifier <* pDot <*> pIdentifier
  <|> OccRule <$> pIdentifier

nl2sp :: Char -> Char
nl2sp '\n' = ' '
nl2sp '\r' = ' '
nl2sp x = x

pLocType :: AGParser (Type, Pos)
pLocType = (\u -> (Haskell $ getName u, getPos u)) <$> pIdentifierU
       <|> (\(s,p) -> (Haskell s,p)) <$> pCodescrap  <?> "a type"

pVar :: AGParser (Identifier -> (Identifier, Identifier))
pVar = (\att fld -> (fld,att)) <$> pIdentifier

pAssign :: AGParser Bool
pAssign =  False <$ pReserved "="
       <|> True  <$ pReserved ":="

pRuleSym :: AGParser (Bool, Pos, Bool)
pRuleSym  =     (\p -> (False, p, True)) <$> pReserved "="
            <|> (\p -> (True,  p, True)) <$> pReserved ":="
            <|> (\p -> (False, p, True)) <$> pReserved "<-"

pPattern :: AGParser (a -> (Identifier,Identifier)) -> AGParser (a -> Pattern)
pPattern pvar = pPattern2 where
  pPattern0 =  (\i pats a -> Constr i (map ($ a) pats))
               <$> pIdentifierU <*> pList  pPattern1
               <|> pPattern1 <?> "a pattern"
  pPattern1 =  pvariable
           <|> pPattern2
  pvariable = (\ir var pat a -> case var a of (fld,att) -> ir $ Alias fld att (pat a) [])
           <$> ((Irrefutable <$ pTilde) `opt` id) <*> pvar <*> ((pAt *> pPattern1) `opt` const (Underscore noPos))
  pPattern2 = (mkTuple <$> pOParenPos <*> pListSep pComma pPattern0 <* pCParen )
          <|> (const . Underscore) <$> pUScore <?> "a pattern"
    where mkTuple _ [x] a = x a
          mkTuple p xs  a = Product p (map ($ a) xs)

pCostSym' c t = pCostSym c t t

pCodescrap' ::  AGParser String
pCodescrap' = fst <$> pCodescrap

pCodescrap ::  AGParser (String,Pos)
pCodescrap   = pCodeBlock

pSEM, pATTR, pDATA, pUSE, pLOC,pINCLUDE, pTYPE, pEquals, pColonEquals, pTilde,
      pEXTENDS, --marcos
      pBar, pColon, pLHS,pINST,pSET,pDERIVING,pMinus,pIntersect,pDoubleArrow,pArrow,
      pDot, pUScore, pEXT,pAt,pStar, pSmaller, pWRAPPER, pNOCATAS, pPRAGMA, pMAYBE, pEITHER, pMAP, pINTMAP,
      pMODULE, pATTACH, pUNIQUEREF, pINH, pSYN, pAUGMENT, pPlus, pAROUND, pSEMPRAGMA, pMERGE, pAS
      :: AGParser Pos
pSET         = pCostReserved 90 "SET"     <?> "SET"
pDERIVING    = pCostReserved 90 "DERIVING"<?> "DERIVING"
pWRAPPER     = pCostReserved 90 "WRAPPER" <?> "WRAPPER"
pNOCATAS     = pCostReserved 90 "NOCATAS" <?> "NOCATAS"
pPRAGMA      = pCostReserved 90 "PRAGMA"  <?> "PRAGMA"
pSEMPRAGMA   = pCostReserved 90 "SEMPRAGMA" <?> "SEMPRAGMA"
pATTACH      = pCostReserved 90 "ATTACH"  <?> "ATTACH"
pDATA        = pCostReserved 90 "DATA"    <?> "DATA"
pEXT         = pCostReserved 90 "EXT"     <?> "EXT"
pATTR        = pCostReserved 90 "ATTR"    <?> "ATTR"
pSEM         = pCostReserved 90 "SEM"     <?> "SEM"
pINCLUDE     = pCostReserved 90 "INCLUDE" <?> "INCLUDE"
pEXTENDS     = pCostReserved 90 "EXTENDS" <?> "EXTENDS" --marcos
pTYPE        = pCostReserved 90 "TYPE"    <?> "TYPE"
pINH         = pCostReserved 90 "INH"     <?> "INH"
pSYN         = pCostReserved 90 "SYN"     <?> "SYN"
pCHN         = pCostReserved 90 "CHN"     <?> "CHN"
pMAYBE       = pCostReserved 5  "MAYBE"   <?> "MAYBE"
pEITHER      = pCostReserved 5  "EITHER"  <?> "EITHER"
pMAP         = pCostReserved 5  "MAP"     <?> "MAP"
pINTMAP      = pCostReserved 5  "INTMAP"  <?> "INTMAP"
pUSE         = pCostReserved 5  "USE"     <?> "USE"
pLOC         = pCostReserved 5  "loc"     <?> "loc"
pLHS         = pCostReserved 5  "lhs"     <?> "loc"
pINST        = pCostReserved 5  "inst"    <?> "inst"
pAt          = pCostReserved 5  "@"       <?> "@"
pDot         = pCostReserved 5  "."       <?> "."
pUScore      = pCostReserved 5  "_"       <?> "_"
pColon       = pCostReserved 5  ":"       <?> ":"
pDoubleColon = pCostReserved 5  "::"      <?> "::"
pEquals      = pCostReserved 5  "="       <?> "="
pColonEquals = pCostReserved 5  ":="      <?> ":="
pTilde       = pCostReserved 5  "~"       <?> "~"
pPlus        = pCostReserved 5  "+"       <?> "+"
pBar         = pCostReserved 5  "|"       <?> "|"
pIntersect   = pCostReserved 5  "/\\"     <?> "/\\"
pMinus       = pCostReserved 5  "-"       <?> "-"
pDoubleArrow = pCostReserved 5  "=>"      <?> "=>"
pArrow       = pCostReserved 5  "->"      <?> "->"
pStar        = pCostReserved 5  "*"       <?> "*"
pSmaller     = pCostReserved 5  "<"       <?> "<"
pMODULE      = pCostReserved 5  "MODULE"  <?> "MODULE"
pUNIQUEREF   = pCostReserved 5  "UNIQUEREF" <?> "UNIQUEREF"
pAUGMENT     = pCostReserved 5  "AUGMENT" <?> "AUGMENT"
pAROUND      = pCostReserved 5  "AROUND" <?> "AROUND"
pMERGE       = pCostReserved 5  "MERGE" <?> "MERGE"
pAS          = pCostReserved 5  "AS" <?> "AS"
