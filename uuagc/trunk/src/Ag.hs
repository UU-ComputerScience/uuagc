module Main where

import System                        (getArgs, getProgName, exitFailure)
import System.Console.GetOpt         (usageInfo)
import List                          (isSuffixOf)
import Monad                         (zipWithM_)

import qualified Data.Map as Map (elems, partitionWithKey, unionWith)
import qualified UU.DData.Seq as Seq ((<>),toList)
import qualified UU.Pretty           (PP_Doc, render, disp)

import UU.Parsing                    (Message(..), Action(..))
import UU.Scanner.Position           (Pos)
import UU.Scanner.Token              (Token)

import qualified Transform          as Pass1 (sem_AG     ,  wrap_AG     ,  Syn_AG      (..), Inh_AG      (..))
import qualified DefaultRules       as Pass2 (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified Order              as Pass3 (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified GenerateCode       as Pass4 (sem_CGrammar, wrap_CGrammar, Syn_CGrammar(..), Inh_CGrammar(..))
import qualified PrintCode          as Pass5 (sem_Program,  wrap_Program,  Syn_Program (..), Inh_Program (..))
import qualified PrintErrorMessages as PrErr (sem_Errors ,  wrap_Errors ,  Syn_Errors  (..), Inh_Errors  (..), isError)

import qualified AbstractSyntaxDump as GrammarDump (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified CodeSyntaxDump as CGrammarDump (sem_CGrammar,  wrap_CGrammar,  Syn_CGrammar (..), Inh_CGrammar (..))

import Options
import Version       (banner)
import Parser        (parseAG)
import ErrorMessages (Error(ParserError), Errors)
import CommonTypes   (Blocks)


main :: IO ()
main        
 = do args     <- getArgs
      progName <- getProgName
      
      let usageheader = "Usage info:\n " ++ progName ++ " options file ...\n\nList of options:"
          (flags,files,errs) = getOptions args
          
      if showVersion flags
       then putStrLn banner
       else if null files || showHelp flags || (not.null) errs
       then mapM_ putStrLn (usageInfo usageheader options : errs)
       else zipWithM_ (compile flags) files (outputFiles flags++repeat "")


compile :: Options -> String -> String -> IO ()
compile flags input output
 = do (output0,parseErrors) <- parseAG (searchPath flags) (inputFile input)

      let output1  = Pass1.wrap_AG              (Pass1.sem_AG                                 output0 ) Pass1.Inh_AG       {Pass1.options_Inh_AG       = flags}
          flags'   = Pass1.pragmas_Syn_AG       output1 $ flags
          grammar1 = Pass1.output_Syn_AG        output1
          output2  = Pass2.wrap_Grammar         (Pass2.sem_Grammar grammar1                           ) Pass2.Inh_Grammar  {Pass2.options_Inh_Grammar  = flags'}
          grammar2 = Pass2.output_Syn_Grammar   output2
          output3  = Pass3.wrap_Grammar         (Pass3.sem_Grammar grammar2                           ) Pass3.Inh_Grammar  {Pass3.options_Inh_Grammar  = flags'}
          grammar3 = Pass3.output_Syn_Grammar   output3
          output4  = Pass4.wrap_CGrammar        (Pass4.sem_CGrammar(Pass3.output_Syn_Grammar  output3)) Pass4.Inh_CGrammar {Pass4.options_Inh_CGrammar = flags'}
          output5  = Pass5.wrap_Program         (Pass5.sem_Program (Pass4.output_Syn_CGrammar output4)) Pass5.Inh_Program  {Pass5.options_Inh_Program  = flags'}
          output6  = PrErr.wrap_Errors          (PrErr.sem_Errors                       errorsToReport) PrErr.Inh_Errors   {PrErr.options_Inh_Errors   = flags'} 

          dump1    = GrammarDump.wrap_Grammar   (GrammarDump.sem_Grammar grammar1                     ) GrammarDump.Inh_Grammar
          dump2    = GrammarDump.wrap_Grammar   (GrammarDump.sem_Grammar grammar2                     ) GrammarDump.Inh_Grammar
          dump3    = CGrammarDump.wrap_CGrammar (CGrammarDump.sem_CGrammar grammar3                   ) CGrammarDump.Inh_CGrammar

          errorList        = map message2error parseErrors
                             ++ Seq.toList (      Pass1.errors_Syn_AG       output1
                                           Seq.<> Pass2.errors_Syn_Grammar  output2
                                           Seq.<> Pass3.errors_Syn_Grammar  output3
                                           Seq.<> Pass4.errors_Syn_CGrammar output4
                                           )
                                           
          fatalErrorList = filter PrErr.isError errorList
          
          errorsToReport = if wignore flags'
                            then fatalErrorList
                            else errorList
                            
          errorsToStopOn = if werrors flags'
                            then errorList
                            else fatalErrorList               
          
      putStr . formatErrors $ PrErr.pp_Syn_Errors output6
     
      if not (null fatalErrorList) 
       then exitFailure
       else do let outputfile = if null output then outputFile input else output
                   blocks1                    = (Pass1.blocks_Syn_AG output1) {-SM `Map.unionWith (++)` (Pass3.blocks_Syn_Grammar output3)-}
                   (pragmaBlocks, blocks2)    = Map.partitionWithKey (\k _->k=="optpragmas") blocks1
                   (importBlocks, textBlocks) = Map.partitionWithKey (\k _->k=="imports"   ) blocks2
                   optionsGHC = option (unbox flags') "-fglasgow-exts" ++ option (bangpats flags') "-fbang-patterns"
                   option True s  = [s]
                   option False _ = []
                   optionsLine | null optionsGHC = ""
                               | otherwise       = "{-# OPTIONS_GHC " ++ unwords optionsGHC ++ " #-}\n"
                   
                   
                                      
               writeFile  outputfile . unlines . concat . Map.elems $ pragmaBlocks
               appendFile outputfile                                $ optionsLine
               appendFile outputfile                                $ take 70 ("-- UUAGC " ++ drop 50 banner ++ " (" ++ input) ++ ")\n"
               appendFile outputfile                                $ moduleHeader flags' input
               appendFile outputfile . unlines . concat . Map.elems $ importBlocks
               appendFile outputfile . unlines . concat . Map.elems $ textBlocks
               appendFile outputfile . formatProg                   $ Pass5.output_Syn_Program output5
               appendFile outputfile                                $ if dumpgrammar flags'
                                                                      then ("{- Dump of grammar without default rules\n" ++)
                                                                           $ UU.Pretty.disp (GrammarDump.pp_Syn_Grammar dump1) 5000
                                                                           $ ("\n-}\n" ++)
                                                                           $ ("{- Dump of grammar with default rules\n" ++)
                                                                           $ UU.Pretty.disp (GrammarDump.pp_Syn_Grammar dump2) 5000
                                                                           $ ("\n-}\n" ++)
                                                                           $ ""
                                                                      else ""
               appendFile outputfile                                $ if dumpcgrammar flags'
                                                                      then ( "{- Dump of cgrammar\n" ++)
                                                                           $ UU.Pretty.disp (CGrammarDump.pp_Syn_CGrammar dump3) 5000
                                                                           $ ("\n-}\n" ++)
                                                                           $ ""
                                                                      else ""
               --putStrLn ("\n" ++ outputfile ++ " generated")
               if not (null errorsToStopOn) then exitFailure else return ()


formatProg :: [UU.Pretty.PP_Doc] -> String
formatProg pps = foldr (.) 
                       id
                       (map (\d -> (UU.Pretty.disp d 5000) . ( '\n':) ) pps)
                       ""

formatErrors :: UU.Pretty.PP_Doc -> String
formatErrors pp = UU.Pretty.disp pp 5000 ""


message2error :: Message Token Pos -> Error
message2error (Msg expect pos action) = ParserError pos (show expect) actionString
 where actionString 
        =  case action 
           of Insert s -> "inserting: " ++ show s

              Delete s -> "deleting: "  ++ show s

              Other ms -> ms


moduleHeader :: Options -> String -> String
moduleHeader flags input
 = case moduleName flags 
   of Name nm -> genMod nm
      Default -> genMod (defaultModuleName input)
      NoName  -> ""
   where genMod x = "module " ++ x ++ " where\n"

inputFile :: String -> String
inputFile name 
 = if ".ag" `isSuffixOf` name || ".lag" `isSuffixOf` name
   then name
   else name ++ ".ag"

outputFile :: String -> String
outputFile name 
 = defaultModuleName name ++ ".hs"

defaultModuleName :: String -> String
defaultModuleName name 
 = if ".ag" `isSuffixOf` name
   then take (length name - 3) name
   else if ".lag" `isSuffixOf` name
   then take (length name - 4) name
   else name
