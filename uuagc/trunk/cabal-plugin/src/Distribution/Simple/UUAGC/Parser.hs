{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Simple.UUAGC.Parser(parserAG,
                                        parserAG',
                                        scanner,
                                        parseIOAction,
                                        parseClassAG,
                                        parseOptionAG) where

import UU.Parsing
import UU.Scanner
import Distribution.Simple.UUAGC.AbsSyn
import Options
import System.Console.GetOpt
import System.IO.Unsafe(unsafeInterleaveIO)
import System.IO(hPutStr,stderr)

data ParserError = DefParserError String
                 deriving (Show, Eq, Read)

uFlags :: [String]
uFlags = concat [ filter (not . null) x | Option _ x _ _ <- options]

kwtxt = uFlags ++ ["file", "options", "class", "with"]
kwotxt = ["=",":","..","."]
sctxt  = "..,"
octxt = "=:.,"

posTxt :: Pos
posTxt = Pos 0 0 ""

puFlag :: OptDescr (Options -> Options) -> Parser Token (Options -> Options)
puFlag (Option _ []  _            _) = pFail
puFlag (Option _ kws (NoArg f)    _) = pAny (\kw -> const f <$> pKey kw) kws
puFlag (Option _ kws (ReqArg f _) _) = pAny (\kw -> f <$ pKey kw <*> pString) kws
puFlag (Option _ kws (OptArg f _) _) = pAny (\kw -> const (f Nothing) <$> pKey kw
                                                    <|> f . Just <$ pKey kw <*> pString) kws

pugFlags :: [Parser Token (Options -> Options)]
pugFlags = map puFlag options

pAnyFlag = pAny id pugFlags

pSep :: Parser Token String
pSep = pKey ":" <|> pKey "="

pFileClasses :: Parser Token [String]
pFileClasses = pKey "with" *> (pCommas pString)
             <|> pSucceed []

pAGFileOption :: Parser Token AGFileOption
pAGFileOption = (\f cl opt -> AGFileOption f cl (constructOptions opt))
                <$> (pKey "file" *> pSep *> pString)
                <*> pFileClasses
                <*> (pKey "options" *> pSep *> pCommas pAnyFlag)

pAGOptionsClass :: Parser Token AGOptionsClass
pAGOptionsClass = (\c opt -> AGOptionsClass c (constructOptions opt))
                  <$> (pKey "class" *> pSep *> pString)
                  <*> (pKey "options" *> pSep *> pCommas pAnyFlag)

pAGFileOptions :: Parser Token AGFileOptions
pAGFileOptions = pList pAGFileOption

parserAG :: FilePath -> IO AGFileOptions
parserAG fp = do s <- readFile fp
                 parseIOAction action pAGFileOptions (scanner fp s)

parserAG' :: FilePath -> IO (Either ParserError AGFileOptions)
parserAG' fp = do s <- readFile fp
                  let steps = parse pAGFileOptions (scanner fp s)
                  let (Pair res _, mesg) = evalStepsMessages steps
                  if null mesg
                     then return $ Right res
                     else do let err = foldr (++) [] $ map message2error mesg
                             return (Left $ DefParserError err)

message2error :: Message Token (Maybe Token) -> String
message2error (Msg e p a) = "Expecting: " ++ (show e) ++ " at " ++ action
    where action = case a of
                     Insert s -> " Inserting: " ++ (show s)
                     Delete s -> " Deleting: " ++ (show s)
                     Other s  -> s

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


evalStepsMessages :: (Eq s, Show s, Show p) => Steps a s p -> (a,[Message s p])
evalStepsMessages steps = case steps of
     OkVal v             rest -> let (arg, ms) = evalStepsMessages rest
                                 in (v arg, ms)
     Ok                  rest -> evalStepsMessages rest
     Cost _              rest -> evalStepsMessages rest
     StRepair _    msg   rest -> let (v, ms) = evalStepsMessages rest
                                 in (v, msg:ms)
     Best _        rest  _    -> evalStepsMessages rest
     NoMoreSteps v            -> (v,[])
