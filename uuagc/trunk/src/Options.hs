module Options where

import System.Console.GetOpt
import CommonTypes
import Data.Set(Set)
import qualified Data.Set as Set

options     :: [OptDescr (Options -> Options)]
options     =  [ Option ['m']     []                (NoArg (moduleOpt Nothing)) "generate default module header"
               , Option []        ["module"]        (OptArg moduleOpt "name")   "generate module header, specify module name"
               , Option ['d']     ["data"]          (NoArg dataOpt)             "generate data type definition"
               , Option []        ["datarecords"]   (NoArg dataRecOpt)          "generate record data types"
               , Option []        ["strictdata"]    (NoArg strictDataOpt)       "generate strict data fields (when data is generated)"
               , Option []        ["strictwrap"]    (NoArg strictWrapOpt)       "generate strict wrap fields for WRAPPER generated data"
               , Option ['c']     ["catas"]         (NoArg cataOpt)             "generate catamorphisms"
               , Option ['f']     ["semfuns"]       (NoArg semfunsOpt)          "generate semantic functions"
               , Option ['s']     ["signatures"]    (NoArg signaturesOpt)       "generate signatures for semantic functions"
               , Option []        ["newtypes"]      (NoArg newtypesOpt)         "use newtypes instead of type synonyms"
               , Option ['p']     ["pretty"]        (NoArg prettyOpt)           "generate pretty printed list of attributes"
               , Option ['w']     ["wrappers"]      (NoArg wrappersOpt)         "generate wappers for semantic domains"
               , Option ['r']     ["rename"]        (NoArg renameOpt)           "rename data constructors"
               , Option []        ["modcopy"]       (NoArg modcopyOpt)          "use modified copy rule"
               , Option []        ["nest"]          (NoArg nestOpt)             "use nested tuples"
               , Option []        ["syntaxmacro"]   (NoArg smacroOpt)           "experimental: generate syntax macro code (using knit catas)"
               , Option ['o']     ["output"]        (ReqArg outputOpt "file")   "specify output file"
               , Option ['v']     ["verbose"]       (NoArg verboseOpt)          "verbose error message format"
               , Option ['h','?'] ["help"]          (NoArg helpOpt)             "get (this) usage information"
               , Option ['a']     ["all"]           (NoArg allOpt)             ("do everything (-" ++ allc ++ ")")
               , Option ['P']     [""]              (ReqArg searchPathOpt "search path") ("specify seach path")
               , Option []        ["prefix"]        (ReqArg prefixOpt "prefix") "set prefix for semantic functions"
               , Option []        ["self"]          (NoArg selfOpt)             "generate self attribute"
               , Option []        ["cycle"]         (NoArg cycleOpt)            "check for cyclic definitions"
               , Option []        ["version"]       (NoArg versionOpt)          "get version information"
               , Option ['O']     ["optimize"]      (NoArg optimizeOpt)         "optimize generated code (--visit --case)"
               , Option []        ["visit"]         (NoArg visitOpt)            "try generating visit functions"
               , Option []        ["seq"]           (NoArg seqOpt)              "force evaluation using function seq (visit functions only)"
               , Option []        ["unbox"]         (NoArg unboxOpt)            "use unboxed tuples"
               , Option []        ["bangpats"]      (NoArg bangpatsOpt)         "use bang patterns (visit functions only)"
               , Option []        ["case"]          (NoArg casesOpt)            "Use nested cases instead of let (visit functions only)"
               , Option []        ["strictcase"]    (NoArg strictCasesOpt)      "Force evaluation of the scrutinee of cases (in generated code, visit functions only)"
               , Option []        ["strictercase"]  (NoArg stricterCasesOpt)      "Force evaluation of all variables bound by a case statement (in generated code)"
               , Option []        ["strictsem"]     (NoArg strictSemOpt)        "Force evaluation of sem-function arguments (in generated code)"
               , Option []        ["localcps"]      (NoArg localCpsOpt)         "Apply a local CPS transformation (in generated code, visit functions only)"
               , Option []        ["splitsems"] (NoArg splitSemsOpt)    "Split semantic functions into smaller pieces"
               , Option []        ["Werrors"]       (NoArg werrorsOpt)          "Turn warnings into fatal errors"
               , Option []        ["Wignore"]       (NoArg wignoreOpt)          "Ignore warnings"
               , Option []        ["Wmax"]          (ReqArg wmaxErrsOpt "<max errs reported>") "Sets the maximum number of errors that are reported"
               , Option []        ["dumpgrammar"]   (NoArg dumpgrammarOpt)      "Dump internal grammar representation (in generated code)"
               , Option []        ["dumpcgrammar"]  (NoArg dumpcgrammarOpt)      "Dump internal cgrammar representation (in generated code)"
               , Option []        ["gentraces"]     (NoArg genTracesOpt)        "Generate trace expressions (in generated code)"
               , Option []        ["genusetraces"]  (NoArg genUseTracesOpt)     "Generate trace expressions at attribute use sites (in generated code)"
               , Option []        ["gencostcentres"] (NoArg genCostCentresOpt)  "Generate cost centre pragmas (in generated code)"
               , Option []        ["genlinepragmas"] (NoArg genLinePragmasOpt)  "Generate GHC LINE pragmas (in generated code)"
               , Option []        ["sepsemmods"]    (NoArg sepSemModsOpt)       "Generate separate modules for semantic functions (in generated code)"
               , Option ['M']     ["genfiledeps"] (NoArg genFileDepsOpt) "Generate a list of dependencies on the input AG files"
               , Option []        ["genvisage"] (NoArg genVisageOpt)  "Generate output for the AG visualizer Visage"
               , Option []        ["aspectag"]   (NoArg genAspectAGOpt)  "Generate AspectAG file"
               , Option []        ["nogroup"]       (ReqArg noGroupOpt "attributes")   "specify the attributes that won't be grouped in AspectAG"
               , Option []        ["extends"]       (ReqArg extendsOpt "module")   "specify a module to be extended"
               , Option []        ["genattrlist"] (NoArg genAttrListOpt) "Generate a list of all explicitly defined attributes (outside irrefutable patterns)"
               , Option []        ["forceirrefutable"] (OptArg forceIrrefutableOpt "file") "Force a set of explicitly defined attributes to be irrefutable, specify file containing the attribute set"
               , Option []        ["uniquedispenser"] (ReqArg uniqueDispenserOpt "name") "The Haskell function to call in the generated code"
               , Option []        ["lckeywords"]      (NoArg lcKeywordsOpt) "Use lowercase keywords (sem, attr) instead of the uppercase ones (SEM, ATTR)"
               , Option []        ["doublecolons"]    (NoArg doubleColonsOpt) "Use double colons for type signatures instead of single colons"
               , Option ['H']     ["haskellsyntax"]   (NoArg haskellSyntaxOpt) "Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)"
               , Option []        ["monadic"]         (NoArg monadicOpt) "Experimental: generate monadic code"
               , Option []        ["ocaml"]           (NoArg ocamlOpt) "Experimental: generate Ocaml code"
               , Option []        ["breadthfirst"]      (NoArg breadthfirstOpt) "Experimental: generate breadth-first code"
               , Option []        ["breadthfirst-strict"] (NoArg breadthfirstStrictOpt) "Experimental: outermost breadth-first evaluator is strict instead of lazy"
               , Option []        ["visitcode"]        (NoArg visitorsOutputOpt) "Experimental: generate visitors code"
               , Option []        ["kennedywarren"]      (NoArg kennedyWarrenOpt) "Experimental: use Kennedy-Warren's algorithm for ordering"
               , Option []        ["statistics"]      (ReqArg statisticsOpt "FILE to append to") "Append statistics to FILE"
               , Option []        ["checkParseRhs"]         (NoArg parseHsRhsOpt) "Parse RHS of rules with Haskell parser"
               , Option []        ["checkParseTys"]         (NoArg parseHsTpOpt) "Parse types of attrs with Haskell parser"
               , Option []        ["checkParseBlocks"]         (NoArg parseHsBlockOpt) "Parse blocks with Haskell parser"
               , Option []        ["checkParseHaskell"]  (NoArg parseHsOpt) "Parse Haskell code (recognizer)"
               , Option []        ["nocatas"]           (ReqArg nocatasOpt "list of nonterms") "Nonterminals not to generate catas for"
               , Option []        ["nooptimize"]         (NoArg noOptimizeOpt) "Disable optimizations"
               , Option []        ["parallel"]           (NoArg parallelOpt) "Generate a parallel evaluator (if possible)"
               ]

allc = "dcfsprm"

data Options = Options{ moduleName :: ModuleHeader
                      , dataTypes :: Bool
                      , dataRecords :: Bool
                      , strictData :: Bool
                      , strictWrap :: Bool
                      , folds :: Bool
                      , semfuns :: Bool
                      , typeSigs :: Bool
                      , attrInfo :: Bool
                      , rename :: Bool
                      , wrappers :: Bool
                      , modcopy :: Bool
                      , newtypes :: Bool
                      , nest :: Bool
                      , smacro :: Bool
                      , outputFiles :: [String]
                      , searchPath :: [String]
                      , verbose :: Bool
                      , prefix :: String
                      , withSelf :: Bool
                      , withCycle :: Bool
                      , showHelp :: Bool
                      , showVersion :: Bool
                      , visit :: Bool
                      , withSeq :: Bool
                      , unbox :: Bool
                      , bangpats :: Bool
                      , cases :: Bool
                      , strictCases :: Bool
                      , stricterCases :: Bool
                      , strictSems :: Bool
                      , localCps :: Bool
                      , splitSems :: Bool
                      , werrors :: Bool
                      , wignore :: Bool
                      , wmaxerrs :: Int
                      , dumpgrammar :: Bool
                      , dumpcgrammar :: Bool
                      , genTraces :: Bool
                      , genUseTraces :: Bool
                      , genCostCentres :: Bool
                      , sepSemMods :: Bool
                      , genFileDeps :: Bool
                      , genLinePragmas :: Bool
                      , genvisage :: Bool
                      , genAspectAG :: Bool
                      , noGroup :: [String]
                      , extends :: Maybe String
                      , genAttributeList :: Bool
                      , forceIrrefutables :: Maybe String
                      , uniqueDispenser :: String
                      , lcKeywords :: Bool
                      , doubleColons :: Bool
                      , monadic :: Bool
                      , ocaml :: Bool
                      , visitorsOutput :: Bool
                      , statsFile :: Maybe String
                      , breadthFirst :: Bool
                      , breadthFirstStrict :: Bool
                      , checkParseRhs :: Bool
                      , checkParseTy :: Bool
                      , checkParseBlock :: Bool
                      , nocatas :: Set NontermIdent
                      , kennedyWarren :: Bool
                      , noOptimizations :: Bool
                      , parallelInvoke :: Bool
                      } deriving Show
noOptions = Options { moduleName    = NoName
                    , dataTypes     = False
                    , dataRecords   = False
                    , strictData    = False
                    , strictWrap    = False
                    , folds         = False
                    , semfuns       = False
                    , typeSigs      = False
                    , attrInfo      = False
                    , rename        = False
                    , wrappers      = False
                    , modcopy       = False
                    , newtypes      = False
                    , nest          = False
                    , smacro        = False
                    , outputFiles   = []
                    , searchPath    = []
                    , verbose       = False
                    , showHelp      = False
                    , showVersion   = False
                    , prefix        = "sem_"
                    , withSelf      = False
                    , withCycle     = False
                    , visit         = False
                    , withSeq       = False
                    , unbox         = False
                    , bangpats      = False
                    , cases         = False
                    , strictCases   = False
                    , stricterCases = False
                    , strictSems    = False
                    , localCps      = False
                    , splitSems     = False
                    , werrors       = False
                    , wignore       = False
                    , wmaxerrs      = 99999
                    , dumpgrammar   = False
                    , dumpcgrammar  = False
                    , genTraces     = False
                    , genUseTraces  = False
                    , genCostCentres = False
                    , sepSemMods     = False
                    , genFileDeps    = False
                    , genLinePragmas = False
                    , genvisage      = False
                    , genAspectAG    = False
                    , noGroup        = []
                    , extends        = Nothing
                    , genAttributeList = False
                    , forceIrrefutables = Nothing
                    , uniqueDispenser = "nextUnique"
                    , lcKeywords      = False
                    , doubleColons    = False
                    , monadic         = False
                    , ocaml           = False
                    , visitorsOutput  = False
                    , statsFile       = Nothing
                    , breadthFirst     = False
                    , breadthFirstStrict = False
                    , checkParseRhs = False
                    , checkParseTy  = False
                    , checkParseBlock = False
                    , nocatas         = Set.empty
                    , kennedyWarren   = False
                    , noOptimizations = False
                    , parallelInvoke  = False
                    }

moduleOpt  nm   opts = opts{moduleName   = maybe Default Name nm}
dataOpt         opts = opts{dataTypes    = True}
dataRecOpt      opts = opts{dataRecords  = True}
strictDataOpt   opts = opts{strictData   = True}
strictWrapOpt   opts = opts{strictWrap   = True}
cataOpt         opts = opts{folds        = True}
semfunsOpt      opts = opts{semfuns      = True}
signaturesOpt   opts = opts{typeSigs     = True}
prettyOpt       opts = opts{attrInfo     = True}
renameOpt       opts = opts{rename       = True}
wrappersOpt     opts = opts{wrappers     = True}
modcopyOpt      opts = opts{modcopy      = True}
newtypesOpt     opts = opts{newtypes     = True}
nestOpt         opts = opts{nest         = True}
smacroOpt       opts = opts{smacro       = True}
verboseOpt      opts = opts{verbose      = True}
helpOpt         opts = opts{showHelp     = True}
versionOpt      opts = opts{showVersion  = True}
prefixOpt pre   opts = opts{prefix       = pre }
selfOpt         opts = opts{withSelf     = True}
cycleOpt        opts = opts{withCycle    = True}
visitOpt        opts = opts{visit        = True, withCycle = True}
seqOpt          opts = opts{withSeq      = True}
unboxOpt        opts = opts{unbox        = True}
bangpatsOpt     opts = opts{bangpats     = True}
casesOpt        opts = opts{cases        = True}
strictCasesOpt  opts = opts{strictCases  = True}
stricterCasesOpt opts = opts{strictCases = True, stricterCases = True}
strictSemOpt    opts = opts{strictSems   = True}
localCpsOpt     opts = opts{localCps     = True}
splitSemsOpt    opts = opts{splitSems    = True}
werrorsOpt      opts = opts{werrors      = True}
wignoreOpt      opts = opts{wignore      = True}
wmaxErrsOpt n   opts = opts{wmaxerrs     = read n}
dumpgrammarOpt  opts = opts{dumpgrammar  = True}
dumpcgrammarOpt opts = opts{dumpcgrammar = True}
genTracesOpt    opts = opts{genTraces    = True}
genUseTracesOpt opts = opts{genUseTraces = True}
genCostCentresOpt opts = opts{genCostCentres = True}
sepSemModsOpt opts = opts{sepSemMods = True}
genFileDepsOpt opts = opts{genFileDeps = True}
genLinePragmasOpt opts = opts{genLinePragmas = True}
genVisageOpt opts = opts{genvisage = True }
genAspectAGOpt opts = opts{genAspectAG = True}
noGroupOpt  att  opts = opts{noGroup  = extract att  ++ noGroup opts}
  where extract s = case dropWhile isSeparator s of
                                "" -> []
                                s' -> w : extract s''
                                      where (w, s'') =
                                             break isSeparator  s'
        isSeparator x = x == ':'

extendsOpt  m  opts = opts{extends  = Just m }

genAttrListOpt opts = opts { genAttributeList = True }
forceIrrefutableOpt mbNm opts = opts { forceIrrefutables = mbNm }
uniqueDispenserOpt nm opts = opts { uniqueDispenser = nm }
lcKeywordsOpt opts = opts { lcKeywords = True }
doubleColonsOpt opts = opts { doubleColons = True }
haskellSyntaxOpt = lcKeywordsOpt . doubleColonsOpt . genLinePragmasOpt
monadicOpt opts = opts { monadic = True }
parallelOpt opts = opts { parallelInvoke = True }
ocamlOpt opts = opts { ocaml = True }
visitorsOutputOpt opts = opts { visitorsOutput = True }
statisticsOpt nm opts = opts { statsFile = Just nm }
breadthfirstOpt opts = opts { breadthFirst = True }
breadthfirstStrictOpt opts = opts { breadthFirstStrict = True }
parseHsRhsOpt opts = opts { checkParseRhs = True }
parseHsTpOpt opts = opts { checkParseTy = True }
parseHsBlockOpt opts = opts { checkParseBlock = True }
parseHsOpt = parseHsRhsOpt . parseHsTpOpt . parseHsBlockOpt
kennedyWarrenOpt opts = opts { kennedyWarren = True }
noOptimizeOpt opts = opts { noOptimizations = True }
nocatasOpt str opts = opts { nocatas = set `Set.union` nocatas opts } where
  set = Set.fromList ids
  ids = map identifier lst
  lst = split str

  split str | null p   = []
            | otherwise = p : split ps
    where (p,ps) = break (== ',') str

outputOpt  file  opts = opts{outputFiles  = file : outputFiles opts}
searchPathOpt  path  opts = opts{searchPath  = extract path ++ searchPath opts}
  where extract xs = let (p,ps) = break (\x -> x == ';' || x == ':') xs
                     in if null p then [] else p : extract ps
allOpt = moduleOpt Nothing . dataOpt . cataOpt . semfunsOpt . signaturesOpt . prettyOpt . renameOpt . dataRecOpt
optimizeOpt   = visitOpt . casesOpt

condDisableOptimizations opts
  | noOptimizations opts =
      opts { strictData         = False
           , strictWrap         = False
           , withSeq            = False
           , unbox              = False
           , bangpats           = False
           , cases              = False
           , strictCases        = False
           , stricterCases      = False
           , strictSems         = False
           , localCps           = False
           , splitSems          = False
           , breadthFirstStrict = False
           }
  | otherwise = opts

getOptions args = let (flags,files,errors) = getOpt Permute options args
                      appliedOpts = foldl (flip ($)) noOptions flags
                      finOpts = condDisableOptimizations appliedOpts
                  in (finOpts,files,errors)

data ModuleHeader  = NoName
                   | Name String
                   | Default deriving Show
