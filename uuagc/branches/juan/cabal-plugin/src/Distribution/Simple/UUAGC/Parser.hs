{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Distribution.Simple.UUAGC.Parser(parserAG,
                                        scanner,
                                        parseIOAction,
                                        parseClassAG,
                                        parseOptionAG) where

import UU.Parsing
import UU.Scanner
import Distribution.Simple.UUAGC.AbsSyn
import Distribution.Simple.UUAGC.Options
import System.IO.Unsafe(unsafeInterleaveIO)
import System.IO(hPutStr,stderr)
import Control.Exception

uFlags = [odata, ostrictdata, ostrictwrap, ocatas, osemfuns, osignatures
         ,onewtypes, opretty
         ,owrappers, orename, omodcopy, onest, osyntaxmacro, overbose
         ,ohelp, ocycle, oversion, ovisit, oseq, ounbox, obangpats
         ,ocase, ostrictcase, ostrictercase, olocalcps, osplitsems
         ,owerrors, owignore, odumpgrammar, odumpcgrammar, ogentraces
         ,ogenusetraces, ogencostcentres, ogenlinepragmas, osepsemmods
         ,ogenfiledeps, ogenvisage, ogenattrlist, olckeywords
         ,odoublecolons ]

uabsFlags = [UData, UStrictData, UStrictWData, UCatas, USemFuns, USignatures
            ,UNewTypes, UPretty
            ,UWrappers, URename, UModCopy, UNest, USyntaxMacro, UVerbose
            ,UHelp, UCycle, UVersion, UVisit, USeq, UUnbox, UBangPats
            ,UCase, UStrictCase, UStricterCase, ULocalCPS, USplitSems
            ,UWErrors, UWIgnore, UDumpGrammar, UDumpCGrammar, UGenTraces
            ,UGenUseTraces, UGenCostCentres, UGenLinePragmas, USepSemMods
            ,UGenFileDeps, UGenVisage, UGenAttrList, ULCKeyWords
            ,UDoubleColons ]

gFlags = [(oall, [odata, ocatas, osemfuns, osignatures, opretty, orename])
         ,(ooptimize, [ovisit,ocase])
         ,(ohaskellsyntax, [olckeywords, odoublecolons,ogenlinepragmas])
         ]

gabsFlags = [UAll, UOptimize, UHaskellSyntax]


aFlags = [omodule, ooutput, osearch, oprefix, owmax, oforceirrefutable]

ugFlags = uFlags ++ (map (fst) gFlags)

ugabsFlags = uabsFlags ++ gabsFlags

kwtxt = uFlags ++ (map fst gFlags) ++ aFlags ++ ["file", "options", "class", "with"]
kwotxt = ["=",":","..","."]
sctxt  = "..,"
octxt = "=:.,"

posTxt :: Pos
posTxt = Pos 0 0 ""

puFlag :: UUAGCOption -> String -> Parser Token UUAGCOption
puFlag opt sopt = opt <$ pKey sopt

pugFlags :: [Parser Token UUAGCOption]
pugFlags = zipWith puFlag ugabsFlags ugFlags

pModule :: Parser Token UUAGCOption
pModule =  UModuleDefault <$ pKey omodule
       <|> UModule <$> (pKey omodule *> pString)

pOutput :: Parser Token UUAGCOption
pOutput = UOutput <$> (pKey ooutput *> pString)

pSearch :: Parser Token UUAGCOption
pSearch = USearchPath <$> (pKey osearch *> pString)

pPrefix :: Parser Token UUAGCOption
pPrefix = UPrefix <$> (pKey oprefix *> pString)

pWmax :: Parser Token UUAGCOption
pWmax = f <$> (pKey owmax *> pInteger)
    where f x = UWMax (read x)

pForceIrrefutable :: Parser Token UUAGCOption
pForceIrrefutable = UForceIrrefutable <$> (pKey oforceirrefutable *> pString)

pAllFlags = pugFlags ++ [pModule,pOutput,pSearch,pPrefix,pWmax,pForceIrrefutable]

pAnyFlag = pAny id pAllFlags

pSep :: Parser Token String
pSep = pKey ":" <|> pKey "="

pFileClasses :: Parser Token [String]
pFileClasses = pKey "with" *> (pCommas pString)
             <|> pSucceed []

pLiftOptions f n = f <$> (pKey n *> pSep *> pString)
                <*> (pKey "options" *> pSep *> pCommas pAnyFlag)

pAGFileOption :: Parser Token AGFileOption
pAGFileOption = AGFileOption <$> (pKey "file" *> pSep *> pString) 
                <*> pFileClasses
                <*> (pKey "options" *> pSep *> pCommas pAnyFlag)

pAGOptionsClass :: Parser Token AGOptionsClass
pAGOptionsClass = pLiftOptions AGOptionsClass "class"

pAGFileOptions :: Parser Token AGFileOptions
pAGFileOptions = pList pAGFileOption

parserAG :: FilePath -> IO AGFileOptions
parserAG fp = do s <- readFile fp
                 parseIOAction action pAGFileOptions (scanner fp s)

liftParse p text = parseIOAction action p (scanner text text)

parseOptionAG :: String -> IO AGFileOption
parseOptionAG = liftParse pAGFileOption

parseClassAG :: String -> IO AGOptionsClass
parseClassAG = liftParse pAGOptionsClass

scanner     :: String -> String -> [Token]
scanner fn s = scan kwtxt kwotxt sctxt octxt (Pos 0 0 fn) s

action :: (Eq s, Show s, Show p) => Message s p -> IO ()
action m = hPutStr stderr (show m)

test :: (Show a) => Parser Token a -> [Token] -> IO ()
test p inp = do r <- parseIOAction action p inp
                print r

parseIOAction :: (Symbol s, InputState inp s p)
               => (Message s p -> IO ())
               -> AnaParser inp Pair s p a
               -> inp
               -> IO a
parseIOAction  showMessage p inp
 = do  (Pair v final) <- evalStepsIOAction showMessage (parse p inp)
       final `seq` return v -- in order to force the trailing error messages to be printed

evalStepsIOAction :: (Message s p -> IO ())
                  ->  Steps b s p
                  -> IO b
evalStepsIOAction showMessage = evalStepsIOAction' showMessage (-1)

evalStepsIOAction' :: (Message s p -> IO ())
                   -> Int
                   ->  Steps b s p
                   -> IO b
evalStepsIOAction' showMessage n (steps :: Steps b s p) = eval n steps
  where eval                      :: Int -> Steps a s p -> IO a
        eval 0 steps               = return (evalSteps steps)
        eval n steps = case steps of
          OkVal v        rest -> do arg <- unsafeInterleaveIO (eval n rest)
                                    return (v arg)
          Ok             rest -> eval n rest
          Cost  _        rest -> eval n rest
          StRepair _ msg rest -> do showMessage msg
                                    eval (n-1) rest
          Best _   rest   _   -> eval n rest
          NoMoreSteps v       -> return v


