module Parser --(parseAG)
where
import Data.Maybe
import UU.Parsing
import UU.Parsing.Machine(RealParser(..),RealRecogn(..),anaDynE,mkPR)
import ConcreteSyntax
import CommonTypes
import Patterns
import UU.Pretty(text,PP_Doc,empty,(>-<))
import TokenDef
import List (intersperse)
import Char
import Scanner (Input(..),scanLit,input)
import List
import Expression
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.GenToken
import UU.Scanner.GenTokenParser
import UU.Scanner.Position
import UU.Scanner.TokenShow()
import System.Directory


type AGParser = AnaParser Input  Pair Token Pos

pIdentifier, pIdentifierU :: AGParser Identifier
pIdentifierU = uncurry Ident <$> pConidPos
pIdentifier   = uncurry Ident <$> pVaridPos


parseAG :: [FilePath] -> String -> IO (AG,[Message Token Pos])
parseAG  searchPath file 
              = do (es,_,mesg) <- parseFile searchPath file
                   return (AG es, mesg)

parseFile :: [FilePath] -> String -> IO  ([Elem],[String],[Message Token Pos ])
parseFile searchPath file 
               = do txt <- readFile file
                    let litMode = ".lag" `isSuffixOf` file
                        (files,text) = if litMode then scanLit txt
                                       else ([],txt)
                        tokens       = input (initPos file) text

                        steps = parse pElemsFiles tokens
                        stop (_,fs,_) = null fs
                        cont (es,fs,msg) = do files <- mapM (resolveFile searchPath) fs
                                              res <- mapM (parseFile searchPath) files
                                              let (ess,fss,msgs) = unzip3 res
                                              return (es ++ concat ess, concat fss, msg ++ concat msgs)
                    let (Pair (es,fls) _ ,mesg) = evalStepsMessages steps
                    loopp stop cont (es,files++fls,mesg)

resolveFile :: [FilePath] -> FilePath -> IO FilePath
resolveFile path fname = search (path ++ ["."])                                                  
 where search (p:ps) = do dExists <- doesDirectoryExist p
                          if dExists
                             then do let filename = p++pathSeparator++fname
                                     fExists <- doesFileExist filename
                                     if fExists
                                        then return filename
                                        else search ps      
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
                                  
pElemsFiles :: AGParser ([Elem],[String])
pElemsFiles = pFoldr (($),([],[])) pElem'
   where pElem' =  addElem <$> pElem
               <|> pINCLUDE *> (addInc <$> pStringPos)
         addElem e      (es,fs) = (e:es,   fs)
         addInc  (fn,_) (es,fs) = (  es,fn:fs)

pCodescrapL = (\(ValToken _ str pos) -> (str, pos))<$>
            parseScrapL  <?> "a code block"

parseScrapL :: AGParser Token
parseScrapL = let p acc =  (\k (Input pos str next) ->
                            let (sc,rest) = case next of
                                  Just (t@(ValToken TkTextln _ _), rs) -> (t,rs)
                                  _ -> let (tok,p2,inp2) = codescrapL pos str
                                       in (tok, input p2 inp2)
                                steps   = k ( rest)
                            in  (val (acc sc)  steps)
                       )
              in anaDynE  (mkPR (P (p  ), R (p (const id))))

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

pNames :: AGParser [Name]
pNames = pList1 pIdentifier

pAG  :: AGParser AG
pAG  = AG <$> pElems

pElems :: AGParser Elems
pElems = pList_ng pElem

pComplexType =  List <$> pBracks pType 
            <|> Maybe <$ pMAYBE <*> pType
            <|> tuple <$> pParens (pListSep pComma field)
 where field = (,) <$> ((Just <$> pIdentifier <* pColon) `opt` Nothing) <*> pType
       tuple xs = Tuple [(fromMaybe (Ident ("x"++show n) noPos) f, t) 
                        | (n,(f,t)) <- zip [1..] xs
                        ]
pElem :: AGParser Elem
pElem =  Data <$> pDATA
              <*> pNontSet
              <*> pOptAttrs
              <*> pAlts
             <*> pSucceed False
     <|> Attr <$> pATTR
              <*> pNontSet
              <*> pAttrs
     <|> Type <$> pTYPE
              <*> pIdentifierU
              <*  pEquals
              <*> pComplexType
     <|> Sem  <$> pSEM
              <*> pNontSet
              <*> pOptAttrs
              <*> pSemAlts
     <|> Set  <$> pSET
              <*> pIdentifierU
              <*  pEquals
              <*> pNontSet
     <|> Deriving
              <$> pDERIVING
              <*> pNontSet
              <*  pColon
              <*> pListSep pComma pIdentifierU
     <|> Wrapper
              <$> pWRAPPER
              <*> pNontSet
     <|> Pragma
              <$> pPRAGMA
              <*> pNames
   <|> codeBlock <$> (pIdentifier <|> pSucceed (Ident "" noPos)) <*> pCodeBlock <?> "a statement"
           where codeBlock nm (txt,pos) = Txt pos nm (lines txt)

-- Insertion is expensive for pCodeBlock in order to prevent infinite inserts.
pCodeBlock ::  AGParser (String,Pos)
pCodeBlock   = pCostValToken 90 TkTextln "" <?> "a code block"


pAttrs :: AGParser Attrs
pAttrs = Attrs <$> pOBrackPos <*> (concat <$> pList pInhAttrNames <?> "inherited attribute declarations")
                              <* pBar    <*> (concat <$> pList pAttrNames <?> "chained attribute declarations"  )
                              <* pBar    <*> (concat <$> pList pAttrNames <?> "synthesised attribute declarations"  )
               <*  pCBrack

pOptAttrs :: AGParser Attrs
pOptAttrs = pAttrs `opt` Attrs noPos [] [] []

pType :: AGParser Type
pType =  NT <$> pIdentifierU
     <|> Haskell <$> pCodescrap'  <?> "a type"


pInhAttrNames :: AGParser AttrNames
pInhAttrNames   = (\vs tp -> map (\v -> (v,tp,("","",""))) vs)
                  <$> pIdentifiers <*  pColon <*> pType <?> "attribute declarations"

pIdentifiers :: AGParser [Identifier]
pIdentifiers =  pList1Sep pComma pIdentifier <?> "lowercase identifiers"


pAttrNames :: AGParser AttrNames
pAttrNames = (\vs use tp -> map (\v -> (v,tp,use)) vs)
             <$> pIdentifiers <*> pUse <* pColon <*> pType <?> "attribute declarations"

pUse :: AGParser (String,String,String)
pUse = (  (\u x y->(x,y,show u)) <$> pUSE <*> pCodescrap'  <*> pCodescrap')` opt` ("","","") <?> "USE declaration"

pAlt :: AGParser Alt
pAlt =  Alt <$> pBar <*> pSimpleConstructorSet <*> pFields <?> "a datatype alternative"

pAlts :: AGParser Alts
pAlts =  pList_ng pAlt <?> "datatype alternatives"

pFields    :: AGParser Fields
pFields    = concat <$> pList_ng pField <?> "fields"

pField     :: AGParser Fields
pField     =  (\nms tp -> map (flip (,) tp) nms)
           <$> pIdentifiers <* pColon <*> pType
           <|> (\s -> [(Ident (mklower (getName s)) (getPos s) ,NT s)]) <$> pIdentifierU

mklower :: String -> String
mklower (x:xs) = toLower x : xs
mklower []     = []

pSemAlt :: AGParser SemAlt
pSemAlt  = SemAlt
          <$> pBar <*> pConstructorSet <*> pSemDefs <?> "SEM alternative"

pSimpleConstructorSet :: AGParser ConstructorSet
pSimpleConstructorSet =  CName <$> pIdentifierU
                     <|> CAll  <$  pStar
                     <|> pParens pConstructorSet

pConstructorSet :: AGParser ConstructorSet
pConstructorSet =  pChainl (CDifference <$ pMinus) term2
  where term2 =  pChainr (pSucceed CUnion) term1
        term1 =  CName <$> pIdentifierU
             <|> CAll  <$  pStar
               

pSemAlts :: AGParser SemAlts
pSemAlts =  pList pSemAlt <?> "SEM alternatives"

pFieldIdentifier =  pIdentifier 
                <|> Ident "lhs"  <$> pLHS 
                <|> Ident "loc"  <$> pLOC
                <|> Ident "inst" <$> pINST

pSemDef :: AGParser [SemDef]
pSemDef = (\x fs -> map ($ x) fs)<$> pFieldIdentifier <*> pList1 pAttrDef
      <|>                            pLOC              *> pList1 pLocDecl
      <|>                            pINST             *> pList1 pInstDecl
      <|>  pSEMPRAGMA *> pList1 (SemPragma <$> pNames)
      <|> (\a b -> [AttrOrderBefore a b]) <$> pAttr <* pSmaller <*> pAttr
      <|> (\pat owrt exp -> [Def (pat ()) exp owrt]) <$> pPattern (const <$> pAttr) <*> pAssign <*> pExpr
 
pAttr = (,) <$> pFieldIdentifier <* pDot <*> pIdentifier
 
pAttrDef :: AGParser (Name -> SemDef)
pAttrDef = (\pat owrt exp fld -> Def (pat fld) exp owrt)
           <$ pDot <*> pattern <*> pAssign <*> pExpr
  where pattern =  pPattern pVar
               <|> (\a fld -> Alias fld a (Underscore noPos) []) <$> pIdentifier


nl2sp :: Char -> Char
nl2sp '\n' = ' '
nl2sp '\r' = ' '
nl2sp x = x

pLocDecl :: AGParser SemDef
pLocDecl =   (\ident tp -> TypeDef ident tp)
              <$ pDot <*> pIdentifier <* pColon <*> pLocType

pLocType = (Haskell . getName) <$> pIdentifierU
       <|> Haskell <$> pCodescrap'  <?> "a type"

pInstDecl :: AGParser SemDef
pInstDecl = (\ident tp -> TypeDef ident tp)
             <$ pDot <*> pIdentifier <* pColon <*> (NT <$> pIdentifierU)

pSemDefs :: AGParser SemDefs
pSemDefs =  concat <$> pList_ng pSemDef  <?> "attribute rules"

pVar :: AGParser (Name -> (Name, Name))
pVar = (\att fld -> (fld,att)) <$> pIdentifier

 
pExpr :: AGParser Expression
pExpr = (\(str,pos) ->  Expression pos str) <$> ( pCodescrapL  ) <?> "an expression"

pAssign :: AGParser Bool
pAssign =  False <$ pReserved "="
       <|> True  <$ pReserved ":="
       
pAttrDefs :: AGParser (Name -> [SemDef])
pAttrDefs = (\fs field -> map ($ field) fs) <$> pList1 pAttrDef  <?> "attribute definitions"

pPattern :: AGParser (a -> (Name,Name)) -> AGParser (a -> Pattern)
pPattern pvar = pPattern2 where
  pPattern0 =  (\i pats a -> Constr i (map ($ a) pats))
               <$> pIdentifierU <*> pList  pPattern1
               <|> pPattern1 <?> "a pattern"
  pPattern1 =  pvariable 
           <|> pPattern2
  pvariable = (\var pat a -> case var a of (fld,att) -> Alias fld att (pat a) []) 
           <$> pvar <*> ((pAt *> pPattern1) `opt` const (Underscore noPos)) 
  pPattern2 = (mkTuple <$> pOParenPos <*> pListSep pComma pPattern0 <* pCParen )
          <|> (const . Underscore) <$> pUScore <?> "a pattern"
    where mkTuple _ [x] a = x a
          mkTuple p xs  a = Product p (map ($ a) xs)

pCostSym' c t = pCostSym c t t

pCodescrap' ::  AGParser String
pCodescrap' = fst <$> pCodescrap

pCodescrap ::  AGParser (String,Pos)
pCodescrap   = pCodeBlock

pSEM, pATTR, pDATA, pUSE, pLOC,pINCLUDE, pTYPE, pEquals, pColonEquals,
      pBar, pColon, pLHS,pINST,pSET,pDERIVING,pMinus,pIntersect,pArrow,
      pDot, pUScore, pEXT,pAt,pStar, pSmaller, pWRAPPER, pMAYBE
      :: AGParser  Pos
pSET         = pCostReserved 90 "SET"     <?> "SET"
pDERIVING    = pCostReserved 90 "DERIVING"<?> "DERIVING"
pWRAPPER     = pCostReserved 90 "WRAPPER" <?> "WRAPPER"
pPRAGMA      = pCostReserved 90 "PRAGMA"  <?> "PRAGMA"
pSEMPRAGMA   = pCostReserved 90 "SEMPRAGMA" <?> "SEMPRAGMA"
pDATA        = pCostReserved 90 "DATA"    <?> "DATA"
pEXT         = pCostReserved 90 "EXT"     <?> "EXT"
pATTR        = pCostReserved 90 "ATTR"    <?> "ATTR"
pSEM         = pCostReserved 90 "SEM"     <?> "SEM"
pINCLUDE     = pCostReserved 90 "INCLUDE" <?> "INCLUDE"
pTYPE        = pCostReserved 90 "TYPE"    <?> "TYPE"
pMAYBE       = pCostReserved 5  "MAYBE"   <?> "MAYBE"
pUSE         = pCostReserved 5  "USE"     <?> "USE"
pLOC         = pCostReserved 5  "loc"     <?> "loc"
pLHS         = pCostReserved 5  "lhs"     <?> "loc"
pINST        = pCostReserved 5  "inst"    <?> "inst"
pAt          = pCostReserved 5  "@"       <?> "@"
pDot         = pCostReserved 5  "."       <?> "."
pUScore      = pCostReserved 5  "_"       <?> "_"
pColon       = pCostReserved 5  ":"       <?> ":"
pEquals      = pCostReserved 5  "="       <?> "="
pColonEquals = pCostReserved 5  ":="      <?> ":="
pBar         = pCostReserved 5  "|"       <?> "|"
pIntersect   = pCostReserved 5  "/\\"     <?> "/\\"
pMinus       = pCostReserved 5  "-"       <?> "-"
pArrow       = pCostReserved 5  "->"      <?> "->"
pStar        = pCostReserved 5  "*"       <?> "*"
pSmaller     = pCostReserved 5  "<"       <?> "<"
