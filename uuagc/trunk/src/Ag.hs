-- Todo: we should make a nicer pipeline. Perhaps use Atze's "compile run" combinators.
module Ag (uuagcLib, uuagcExe,compile) where

import System.Environment            (getArgs, getProgName)
import System.Console.GetOpt         (usageInfo)
import Data.List                     (partition)
import Control.Monad                 (zipWithM_)
import Data.Maybe
import System.FilePath

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq ((><),null)
import Data.Foldable(toList)
import Pretty
import PPUtil

import UU.Parsing                    (Message(..), Action(..))
import UU.Scanner.Position           (Pos, line, file)
import UU.Scanner.Token              (Token)

import qualified Transform          as Pass1  (sem_AG     ,  wrap_AG     ,  Syn_AG      (..), Inh_AG      (..))
import qualified Desugar            as Pass1a (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified DefaultRules       as Pass2  (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified ResolveLocals      as Pass2a (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified Order              as Pass3  (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified KWOrder            as Pass3a (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified GenerateCode       as Pass4  (sem_CGrammar, wrap_CGrammar, Syn_CGrammar(..), Inh_CGrammar(..))
import qualified PrintVisitCode     as Pass4a (sem_CGrammar, wrap_CGrammar, Syn_CGrammar(..), Inh_CGrammar(..))
import qualified ExecutionPlan2Hs   as Pass4b (sem_ExecutionPlan, wrap_ExecutionPlan, Syn_ExecutionPlan(..), Inh_ExecutionPlan(..), warrenFlagsPP)
import qualified ExecutionPlan2Caml as Pass4c (sem_ExecutionPlan, wrap_ExecutionPlan, Syn_ExecutionPlan(..), Inh_ExecutionPlan(..))
import qualified PrintCode          as Pass5  (sem_Program,  wrap_Program,  Syn_Program (..), Inh_Program (..))
import qualified PrintOcamlCode     as Pass5a (sem_Program,  wrap_Program,  Syn_Program (..), Inh_Program (..))
import qualified PrintErrorMessages as PrErr  (sem_Errors ,  wrap_Errors ,  Syn_Errors  (..), Inh_Errors  (..), isError)
import qualified TfmToVisage        as PassV  (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))

import qualified AbstractSyntaxDump as GrammarDump (sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..))
import qualified CodeSyntaxDump as CGrammarDump (sem_CGrammar,  wrap_CGrammar,  Syn_CGrammar (..), Inh_CGrammar (..))
import qualified Visage as VisageDump (sem_VisageGrammar, wrap_VisageGrammar, Syn_VisageGrammar(..), Inh_VisageGrammar(..))
import qualified AG2AspectAG as AspectAGDump (pragmaAspectAG, sem_Grammar,  wrap_Grammar,  Syn_Grammar (..), Inh_Grammar (..)) --marcos

import Options
import Version       (banner)
import Parser        (parseAG, depsAG, parseAGI)
import ErrorMessages (Error(ParserError))
import CommonTypes
import ATermWrite

-- Library version
import System.Exit (ExitCode(..), exitWith)

uuagcLib :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagcLib args fileP
  = do let (flags,_,errs) = getOptions args
       if showVersion flags || showHelp flags
         then do putStrLn "Cannot display help or version in library mode."
                 return (ExitFailure 1, [])
         else if (not.null) errs
              then do putStrLn "One or more errors occured:"
                      mapM_ putStrLn errs
                      return (ExitFailure 2, [])
              else if genFileDeps flags
                   then do deps <- getDeps flags [fileP]
                           return (ExitSuccess, deps)
                   else do compile flags fileP (head $ outputFiles flags++repeat "")
                           return (ExitSuccess, [])

-- Executable version
uuagcExe :: IO ()
uuagcExe
 = do args     <- getArgs
      progName <- getProgName

      let usageheader = "Usage info:\n " ++ progName ++ " options file ...\n\nList of options:"
          (flags,files,errs) = getOptions args

      if showVersion flags
        then putStrLn banner
        else if showHelp flags
             then putStrLn (usageInfo usageheader options)
             else if null files || (not.null) errs
                  then do mapM_ putStrLn (usageInfo usageheader options : errs)
                          exitWith (ExitFailure 2)
                  else if genFileDeps flags
                       then reportDeps flags files
                       else zipWithM_ (compile flags) files (outputFiles flags++repeat "")


compile :: Options -> FilePath -> FilePath -> IO ()
compile flags input output
 = do (output0,parseErrors) <- parseAG flags (searchPath flags) input
      irrefutableMap <- readIrrefutableMap flags
      let printStr  = outputStr flags
          failWith  = failWithCode flags
          inputfile = maybe input id (mainFilename flags)
      let output1   = Pass1.wrap_AG              (Pass1.sem_AG                                 output0 ) Pass1.Inh_AG       {Pass1.options_Inh_AG       = flags}
          flags'    = condDisableOptimizations (Pass1.pragmas_Syn_AG output1 flags)
          grammar1  = Pass1.output_Syn_AG        output1
          output1a  = Pass1a.wrap_Grammar        (Pass1a.sem_Grammar grammar1                          ) Pass1a.Inh_Grammar {Pass1a.options_Inh_Grammar = flags', Pass1a.forcedIrrefutables_Inh_Grammar = irrefutableMap, Pass1a.mainName_Inh_Grammar = mainName }
          grammar1a =Pass1a.output_Syn_Grammar   output1a
          output2   = Pass2.wrap_Grammar         (Pass2.sem_Grammar grammar1a                          ) Pass2.Inh_Grammar  {Pass2.options_Inh_Grammar  = flags'}
          grammar2  = Pass2.output_Syn_Grammar   output2
          outputV   = PassV.wrap_Grammar         (PassV.sem_Grammar grammar2                           ) PassV.Inh_Grammar  {}
          grammarV  = PassV.visage_Syn_Grammar   outputV
          output2a  = Pass2a.wrap_Grammar        (Pass2a.sem_Grammar grammar2                          ) Pass2a.Inh_Grammar {Pass2a.options_Inh_Grammar = flags'}
          grammar2a = Pass2a.output_Syn_Grammar  output2a
          output3   = Pass3.wrap_Grammar         (Pass3.sem_Grammar grammar2a                          ) Pass3.Inh_Grammar  {Pass3.options_Inh_Grammar  = flags'}
          grammar3  = Pass3.output_Syn_Grammar   output3
          output3a  = Pass3a.wrap_Grammar        (Pass3a.sem_Grammar grammar2a                         ) Pass3a.Inh_Grammar  {Pass3a.options_Inh_Grammar  = flags'}
          grammar3a = Pass3a.output_Syn_Grammar  output3a
          output4   = Pass4.wrap_CGrammar        (Pass4.sem_CGrammar(Pass3.output_Syn_Grammar  output3)) Pass4.Inh_CGrammar {Pass4.options_Inh_CGrammar = flags'}
          output4a  = Pass4a.wrap_CGrammar       (Pass4a.sem_CGrammar(Pass3.output_Syn_Grammar output3)) Pass4a.Inh_CGrammar {Pass4a.options_Inh_CGrammar = flags'}
          output4b  = Pass4b.wrap_ExecutionPlan  (Pass4b.sem_ExecutionPlan grammar3a) Pass4b.Inh_ExecutionPlan {Pass4b.options_Inh_ExecutionPlan = flags', Pass4b.inhmap_Inh_ExecutionPlan = Pass3a.inhmap_Syn_Grammar output3a, Pass4b.synmap_Inh_ExecutionPlan = Pass3a.synmap_Syn_Grammar output3a, Pass4b.pragmaBlocks_Inh_ExecutionPlan = pragmaBlocksTxt, Pass4b.importBlocks_Inh_ExecutionPlan = importBlocksTxt, Pass4b.textBlocks_Inh_ExecutionPlan = textBlocksDoc, Pass4b.moduleHeader_Inh_ExecutionPlan = mkModuleHeader $ Pass1.moduleDecl_Syn_AG output1, Pass4b.mainName_Inh_ExecutionPlan = mkMainName mainName $ Pass1.moduleDecl_Syn_AG output1, Pass4b.mainFile_Inh_ExecutionPlan = mainFile, Pass4b.textBlockMap_Inh_ExecutionPlan = textBlockMap, Pass4b.mainBlocksDoc_Inh_ExecutionPlan = mainBlocksDoc,Pass4b.localAttrTypes_Inh_ExecutionPlan = Pass3a.localSigMap_Syn_Grammar output3a}
          output4c  = Pass4c.wrap_ExecutionPlan  (Pass4c.sem_ExecutionPlan grammar3a) Pass4c.Inh_ExecutionPlan {Pass4c.options_Inh_ExecutionPlan = flags', Pass4c.inhmap_Inh_ExecutionPlan = Pass3a.inhmap_Syn_Grammar output3a, Pass4c.synmap_Inh_ExecutionPlan = Pass3a.synmap_Syn_Grammar output3a, Pass4c.mainName_Inh_ExecutionPlan = mkMainName mainName $ Pass1.moduleDecl_Syn_AG output1, Pass4c.mainFile_Inh_ExecutionPlan = mainFile, Pass4c.localAttrTypes_Inh_ExecutionPlan = Pass3a.localSigMap_Syn_Grammar output3a}
          output5   = Pass5.wrap_Program         (Pass5.sem_Program (Pass4.output_Syn_CGrammar output4)) Pass5.Inh_Program  {Pass5.options_Inh_Program  = flags', Pass5.pragmaBlocks_Inh_Program = pragmaBlocksTxt, Pass5.importBlocks_Inh_Program = importBlocksTxt, Pass5.textBlocks_Inh_Program = textBlocksDoc, Pass5.textBlockMap_Inh_Program = textBlockMap, Pass5.mainBlocksDoc_Inh_Program = mainBlocksDoc, Pass5.optionsLine_Inh_Program = optionsLine, Pass5.mainFile_Inh_Program = mainFile, Pass5.moduleHeader_Inh_Program = mkModuleHeader $ Pass1.moduleDecl_Syn_AG output1, Pass5.mainName_Inh_Program = mkMainName mainName $ Pass1.moduleDecl_Syn_AG output1}
          output5a  = Pass5a.wrap_Program        (Pass5a.sem_Program (Pass4.output_Syn_CGrammar output4)) Pass5a.Inh_Program { Pass5a.options_Inh_Program  = flags', Pass5a.textBlockMap_Inh_Program = textBlockMap }
          output6   = PrErr.wrap_Errors          (PrErr.sem_Errors                       errorsToReport) PrErr.Inh_Errors   {PrErr.options_Inh_Errors   = flags', PrErr.dups_Inh_Errors = [] }

          dump1    = GrammarDump.wrap_Grammar   (GrammarDump.sem_Grammar grammar1                     ) GrammarDump.Inh_Grammar
          dump2    = GrammarDump.wrap_Grammar   (GrammarDump.sem_Grammar grammar2                     ) GrammarDump.Inh_Grammar
          dump3    = CGrammarDump.wrap_CGrammar (CGrammarDump.sem_CGrammar grammar3                   ) CGrammarDump.Inh_CGrammar


          outputVisage = VisageDump.wrap_VisageGrammar (VisageDump.sem_VisageGrammar grammarV) VisageDump.Inh_VisageGrammar
          aterm        = VisageDump.aterm_Syn_VisageGrammar outputVisage

          parseErrorList   = map message2error (parseErrors)
          mainErrors       = toList ( Pass1.errors_Syn_AG       output1
                               Seq.>< Pass1a.errors_Syn_Grammar output1a
                               Seq.>< Pass2.errors_Syn_Grammar  output2
                               Seq.>< Pass2a.errors_Syn_Grammar output2a)
          furtherErrors    = if kennedyWarren flags'
                             then let errs3a = Pass3a.errors_Syn_Grammar output3a
                                  in if Seq.null errs3a
                                     then if ocaml flags' 
                                          then toList ( Pass4c.errors_Syn_ExecutionPlan output4c )
                                          else toList ( Pass4b.errors_Syn_ExecutionPlan output4b )
                                     else toList errs3a
                             else toList ( Pass3.errors_Syn_Grammar  output3
                                  Seq.>< Pass4.errors_Syn_CGrammar output4)

          errorList        = if null parseErrorList
                             then mainErrors
                                  ++ if null (filter (PrErr.isError flags') mainErrors)
                                     then furtherErrors
                                     else []
                             else [head parseErrorList]

          fatalErrorList = filter (PrErr.isError flags') errorList

          allErrors = if wignore flags'
                      then fatalErrorList
                      else errorsToFront flags' errorList

          errorsToReport = take (wmaxerrs flags') allErrors

          errorsToStopOn = if werrors flags'
                            then errorList
                            else fatalErrorList

          blocks1                    = (Pass1.blocks_Syn_AG output1) {-SM `Map.unionWith (++)` (Pass3.blocks_Syn_Grammar output3)-}
          (pragmaBlocks, blocks2)    = Map.partitionWithKey (\(k, at) _->k==BlockPragma && at == Nothing) blocks1
          (importBlocks, textBlocks) = Map.partitionWithKey (\(k, at) _->k==BlockImport && at == Nothing) blocks2

          importBlocksTxt = vlist_sep "" . map addLocationPragma . concat . Map.elems $ importBlocks
          textBlocksDoc   = vlist_sep "" . map addLocationPragma . Map.findWithDefault [] (BlockOther, Nothing) $ textBlocks
          mainBlocksDoc   = vlist_sep "" . map addLocationPragma . Map.findWithDefault [] (BlockMain, Nothing) $ textBlocks
          dataBlocksDoc   = vlist_sep "" . map addLocationPragma . Map.findWithDefault [] (BlockData, Nothing) $ textBlocks
          recBlocksDoc    = vlist_sep "" . map addLocationPragma . Map.findWithDefault [] (BlockRec, Nothing) $ textBlocks
          pragmaBlocksTxt = unlines . concat . map fst  . concat . Map.elems $ pragmaBlocks
          textBlockMap    = Map.map (vlist_sep "" . map addLocationPragma) . Map.filterWithKey (\(_, at) _ -> at /= Nothing) $ textBlocks

          outputfile = if null output then outputFile flags' inputfile else output
          mainFile | null output = outputFile flags' inputfile
                   | otherwise   = output
          mainName = dropExtension $ takeFileName inputfile

          addLocationPragma :: ([String], Pos) -> PP_Doc
          addLocationPragma (strs, p)
            | genLinePragmas flags' =
                ppLinePragma flags' (line p) (file p) >-< vlist (map pp strs)
                >-< ppWithLineNr (\l -> ppLinePragma flags' (l+1) outputfile)
            | otherwise = vlist (map pp strs)

          optionsGHC = option (unbox flags') "-fglasgow-exts" ++ option (bangpats flags') "-XBangPatterns"
          option True s  = [s]
          option False _ = []
          optionsLine | null optionsGHC = ""
                      | otherwise       = "{-# OPTIONS_GHC " ++ unwords optionsGHC ++ " #-}"

          nrOfErrorsToReport = length $ filter (PrErr.isError flags') errorsToReport
          nrOfWarningsToReport = length $ filter (not.(PrErr.isError flags')) errorsToReport
          totalNrOfErrors = length $ filter (PrErr.isError flags') allErrors
          totalNrOfWarnings = length $ filter (not.(PrErr.isError flags')) allErrors
          additionalErrors = totalNrOfErrors - nrOfErrorsToReport
          additionalWarnings = totalNrOfWarnings - nrOfWarningsToReport
          pluralS n = if n == 1 then "" else "s"

      (outAgi, ext) <-  --marcos
                     if genAspectAG flags'
                     then parseAGI flags (searchPath flags) (agiFile input)
                     else return (undefined, undefined)

      let ext' = case ext of
                        Nothing -> Nothing
                        Just e  -> Just (remAgi e)

          outAgi1   = Pass1.wrap_AG              (Pass1.sem_AG               outAgi ) Pass1.Inh_AG             {Pass1.options_Inh_AG       = flags'}
          agi       = Pass1.agi_Syn_AG           outAgi1
          aspectAG  = AspectAGDump.wrap_Grammar (AspectAGDump.sem_Grammar grammar2  ) AspectAGDump.Inh_Grammar { AspectAGDump.options_Inh_Grammar  = flags'
                                                                                                               , AspectAGDump.agi_Inh_Grammar      = agi
                                                                                                               , AspectAGDump.ext_Inh_Grammar      = ext' } --marcos

      printStr . formatErrors $ PrErr.pp_Syn_Errors output6

      if additionalErrors > 0
       then printStr $ "\nPlus " ++ show additionalErrors ++ " more error" ++ pluralS additionalErrors ++
                     if additionalWarnings > 0
                     then " and " ++ show additionalWarnings ++ " more warning" ++ pluralS additionalWarnings ++ ".\n"
                     else ".\n"
       else if additionalWarnings > 0
            then printStr $ "\nPlus " ++ show additionalWarnings ++ " more warning" ++ pluralS additionalWarnings ++ ".\n"
            else return ()

      if not (null errorsToStopOn)  -- note: this may already run quite a part of the compilation...
       then failWith 1
       else
        do
           if genvisage flags'
            then writeFile (outputfile++".visage") (writeATerm aterm)
            else return ()

           if genAttributeList flags'
            then writeAttributeList (outputfile++".attrs") (Pass1a.allAttributes_Syn_Grammar output1a)
            else return ()

           if sepSemMods flags'
            then do -- alternative module gen
                    if kennedyWarren flags'
                      then if ocaml flags' 
                           then error "sepsemmods is not implemented for the ocaml output generation"
                           else Pass4b.genIO_Syn_ExecutionPlan output4b
                      else Pass5.genIO_Syn_Program output5
                    if not (null errorsToStopOn) then failWith 1 else return ()
            else do -- conventional module gen
                    let doc
                         | visitorsOutput flags'
                            = vlist [ pp_braces importBlocksTxt
                                    , pp_braces textBlocksDoc
                                    , vlist $ Pass4a.output_Syn_CGrammar output4a
                                    ]
                         -- marcos AspectAG gen
                         | genAspectAG flags'
                            = vlist [ AspectAGDump.pragmaAspectAG
                                    , pp optionsLine
                                    , pp pragmaBlocksTxt
                                    , pp $ take 70 ("-- UUAGC2AspectAG " ++ drop 50 banner ++ " (" ++ input) ++ ")"
                                    , pp $ if isNothing $ Pass1.moduleDecl_Syn_AG output1
                                           then moduleHeader flags' mainName ext'
                                           else mkModuleHeader (Pass1.moduleDecl_Syn_AG output1) mainName "" "" False
                                    , pp importBlocksTxt
                                    , AspectAGDump.imp_Syn_Grammar aspectAG
                                    , pp "\n\n{-- AspectAG Code --}\n\n"
                                    , AspectAGDump.pp_Syn_Grammar aspectAG
                                    , dataBlocksDoc
                                    , mainBlocksDoc
                                    , textBlocksDoc
                                    , if dumpgrammar flags'
                                      then vlist [ pp "{- Dump of AGI"
                                                 , pp (show agi)
                                                 , pp "-}"
                                                 , pp "{- Dump of grammar with default rules"
                                                 , GrammarDump.pp_Syn_Grammar dump2
                                                 , pp "-}"
                                                 ]
                                      else empty]
                         | kennedyWarren flags'
                            = if ocaml flags'
                              then vlist
                                    [ text "(* generated by UUAG from" >#< mainFile >#< "*)"
                                    , pp pragmaBlocksTxt
                                    , text "(* module imports *)"
                                    , pp importBlocksTxt
                                    , Pass4c.modules_Syn_ExecutionPlan output4c
                                    , text ""
                                    , text "(* generated data types *)"
                                    , text "module Data__ = struct"
                                    , indent 2 $ vlist
                                      [ text "type __generated_by_uuagc__ = Generated_by_uuagc__"
                                      , Pass4c.datas_Syn_ExecutionPlan output4c
                                      ]
                                    , text "end"
                                    , text "open Data__"
                                    , text ""
                                    , text "(* embedded data types *)"
                                    , dataBlocksDoc
                                    , text ""
                                    , text "(* embedded utilty functions *)"
                                    , textBlocksDoc
                                    , text "(* generated evaluationcode *)"
                                    , text "module Code__ = struct"
                                    , indent 2 $ vlist
                                      [ text "let rec __generated_by_uuagc__ = Generated_by_uuagc__"
                                      , Pass4c.code_Syn_ExecutionPlan output4c
                                      , recBlocksDoc
                                      ]
                                    , text "end"
                                    , text "open Code__"
                                    , text ""
                                    , text "(* main code *)"
                                    , mainBlocksDoc
                                    ]
                              else vlist
                                    [ Pass4b.warrenFlagsPP flags'
                                    , pp pragmaBlocksTxt
                                    , pp $ if isNothing $ Pass1.moduleDecl_Syn_AG output1
                                           then moduleHeader flags' mainName Nothing
                                           else mkModuleHeader (Pass1.moduleDecl_Syn_AG output1) mainName "" "" False
                                    , pp importBlocksTxt
                                    , ( if tupleAsDummyToken flags'
                                          then empty
                                          else pp "import GHC.Prim"  -- need it to pass State#
                                      )
                                    , if parallelInvoke flags'
                                      then vlist [ pp $ "import qualified System.IO.Unsafe(unsafePerformIO)"
                                                 , pp $ "import System.IO(IO)"
                                                 , pp $ "import Control.Concurrent(newEmptyMVar,forkIO,putMVar,takeMVar)"]
                                      else vlist [ pp $ "import Control.Monad.Identity (Identity)"
                                                 , pp $ "import qualified Control.Monad.Identity" ]
                                    , dataBlocksDoc
                                    , mainBlocksDoc
                                    , textBlocksDoc
                                    , recBlocksDoc
                                    --, pp $ "{-"
                                    --, Pass3a.depgraphs_Syn_Grammar output3a
                                    --, Pass3a.visitgraph_Syn_Grammar output3a
                                    --, pp $ "-}"
                                    , Pass4b.output_Syn_ExecutionPlan output4b
                                    , if dumpgrammar flags'
                                      then vlist [ pp "{- Dump of grammar with default rules"
                                                 , GrammarDump.pp_Syn_Grammar dump2
                                                 , pp "-}"
                                                 ]
                                      else empty]
                         | otherwise
                            = vlist [ vlist ( if not (ocaml flags')
                                              then [ pp optionsLine
                                                   , pp pragmaBlocksTxt
                                                   , pp $ take 70 ("-- UUAGC " ++ drop 50 banner ++ " (" ++ input) ++ ")"
                                                   , pp $ if isNothing $ Pass1.moduleDecl_Syn_AG output1
                                                          then moduleHeader flags' mainName Nothing
                                                          else mkModuleHeader (Pass1.moduleDecl_Syn_AG output1) mainName "" "" False
                                                   ]
                                              else []
                                            )
                                    , pp importBlocksTxt
                                    , dataBlocksDoc
                                    , mainBlocksDoc
                                    , textBlocksDoc
                                    , vlist $ if not (ocaml flags')
                                              then Pass5.output_Syn_Program  output5
                                              else Pass5a.output_Syn_Program output5a
                                    , if dumpgrammar flags'
                                      then vlist [ pp "{- Dump of grammar without default rules"
                                                 , GrammarDump.pp_Syn_Grammar dump1
                                                 , pp "-}"
                                                 , pp "{- Dump of grammar with default rules"
                                                 , GrammarDump.pp_Syn_Grammar dump2
                                                 , pp "-}"
                                                 ]
                                      else empty
                                    , if dumpcgrammar flags'
                                      then vlist [ pp "{- Dump of cgrammar"
                                                 , CGrammarDump.pp_Syn_CGrammar dump3
                                                 , pp "-}"
                                                 ]
                                      else empty
                                    ]

                    let docTxt = disp doc 50000 ""
                    writeFile outputfile docTxt
                    -- HACK: write statistics
                    let nAuto = Pass3.nAutoRules_Syn_Grammar output3
                        nExpl = Pass3.nExplicitRules_Syn_Grammar output3
                        line' = inputfile ++ "," ++ show nAuto ++ "," ++ show nExpl ++ "\r\n"
                    case statsFile flags' of
                      Nothing -> return ()
                      Just f  -> appendFile f line'
                    if not (null errorsToStopOn) then failWith 1 else return ()



formatErrors :: PP_Doc -> String
formatErrors doc = disp doc 5000 ""


message2error :: Message Token Pos -> Error
message2error (Msg expect pos action) = ParserError pos (show expect) actionString
 where actionString
        =  case action
           of Insert s -> "inserting: " ++ show s

              Delete s -> "deleting: "  ++ show s

              Other ms -> ms

errorsToFront :: Options -> [Error] -> [Error]
errorsToFront flags mesgs = errs ++ warnings
  where (errs,warnings) = partition (PrErr.isError flags) mesgs

moduleHeader :: Options -> String -> Maybe String -> String
moduleHeader flags input export
 = case moduleName flags
   of Name nm -> genMod nm
      Default -> genMod (defaultModuleName input)
      NoName  -> ""
   where genMod x = "module " ++ x ++ genExp export x ++ " where"
         genExp Nothing _ = ""
         genExp (Just e) x = "(module " ++ x ++ ", module " ++ e ++ ")"

--marcos
agiFile :: String -> String
agiFile name = replaceExtension name "agi"

remAgi :: String -> String
remAgi = dropExtension

outputFile :: Options -> String -> String
outputFile opts name
  | ocaml opts = replaceExtension name "ml"
  | otherwise  = replaceExtension name "hs"

defaultModuleName :: String -> String
defaultModuleName = dropExtension

mkMainName :: String -> Maybe (String, String,String) -> String
mkMainName defaultName Nothing
  = defaultName
mkMainName _ (Just (name, _, _))
  = name

mkModuleHeader :: Maybe (String,String,String) -> String -> String -> String -> Bool -> String
mkModuleHeader Nothing defaultName suffix _ _
  = "module " ++ defaultName ++ suffix ++ " where"
mkModuleHeader (Just (name, exports, imports)) _ suffix addExports replaceExports
  = "module " ++ name ++ suffix ++ ex ++ " where\n" ++ imports ++ "\n"
  where
    ex  = if null exports || (replaceExports && null addExports)
          then ""
          else if null addExports
               then "(" ++ exports ++ ")"
               else if replaceExports
                    then "(" ++ addExports ++ ")"
                    else "(" ++ exports ++ "," ++ addExports ++ ")"

reportDeps :: Options -> [String] -> IO ()
reportDeps flags files
  = do deps <- getDeps flags files
       mapM_ putStrLn deps

getDeps :: Options -> [String] -> IO [String]
getDeps flags files
  = do results <- mapM (depsAG flags (searchPath flags)) files
       let (fs, mesgs) = foldr comb ([],[]) results
       let errs = take (min 1 (wmaxerrs flags)) (map message2error mesgs)
       let ppErrs = PrErr.wrap_Errors (PrErr.sem_Errors errs) PrErr.Inh_Errors {PrErr.options_Inh_Errors = flags, PrErr.dups_Inh_Errors = []}
       if null errs
        then return fs
        else do putStr . formatErrors $ PrErr.pp_Syn_Errors ppErrs
                failWithCode flags 1
                return []
  where
    comb :: ([a],[b]) -> ([a], [b]) -> ([a], [b])
    comb (fs, mesgs) (fsr, mesgsr)
      = (fs ++ fsr, mesgs ++ mesgsr)


writeAttributeList :: String -> AttrMap -> IO ()
writeAttributeList fileP mp
  = writeFile fileP s
  where
    s = show $ map (\(x,y) -> (show x, y)) $ Map.toList $ Map.map (map (\(x,y) -> (show x, y)) . Map.toList . Map.map (map (\(x,y) -> (show x, show y)) . Set.toList)) $ mp

readIrrefutableMap :: Options -> IO AttrMap
readIrrefutableMap flags
  = case forceIrrefutables flags of
      Just fileP -> do s <- readFile fileP
                       seq (length s) (return ())
                       let lists :: [(String,[(String,[(String, String)])])]
                           lists = read s
                       return $ Map.fromList [ (identifier n, Map.fromList [(identifier c, Set.fromList [ (identifier fld, identifier attr) | (fld,attr) <- ss ]) | (c,ss) <- cs ]) | (n,cs) <- lists ]
      Nothing   -> return Map.empty


