module Options where

import System.Console.GetOpt
import Data.Set(Set)
import UU.Scanner.Position(Pos,noPos)
import Data.List(intercalate)
import qualified Data.Set as Set
import System.IO
import System.Exit

-- From CommonTypes
data Identifier   = Ident { getName::String, getPos::Pos }
type NontermIdent = Identifier
identifier :: String -> Identifier
identifier x      = Ident x noPos

instance Eq Identifier where
 Ident x _ == Ident y _ = x == y

instance Ord Identifier where
 compare (Ident x _) (Ident y _) = compare x y

instance Show Identifier where
  show ident = getName ident
  
-- Make options serializable
data MyOptDescr = MyOpt [Char] [String] (ArgDescr (Options -> Options)) (Options -> String -> [String]) String

fromMyOpt :: MyOptDescr -> OptDescr (Options -> Options)
fromMyOpt (MyOpt sh ln desc _ s) = Option sh ln desc s

noOpt :: Options -> String -> [String]
noOpt _ _ = []

boolOpt :: (Options -> Bool) -> Options -> String -> [String]
boolOpt get opt strArg = let oldVal = get noOptions
                             newVal = get opt
                         in  if   oldVal /= newVal
                             then [strArg]
                             else []

stringOpt :: (Options -> String) -> Options -> String -> [String]
stringOpt get opt strArg = let oldVal = get noOptions
                               newVal = get opt
                           in  if   oldVal /= newVal
                               then [strArg, newVal]
                               else []

mbStringOpt :: (Options -> Maybe String) -> Options -> String -> [String]
mbStringOpt get opts nm = maybe [] (\s -> [nm++"="++s]) (get opts)

serializeOption :: Options -> MyOptDescr -> [String]
serializeOption opt (MyOpt sh ln _ get _) = get opt strArg
  where
    strArg = if null sh
             then '-' : '-' : head ln
             else '-' : head sh : []

-- All options
allOptions :: [MyOptDescr]
allOptions =  
  [ MyOpt ['m']     []                (NoArg (moduleOpt Nothing)) noOpt                 "generate default module header"
  , MyOpt []        ["module"]        (OptArg moduleOpt "name")   moduleOptGet          "generate module header, specify module name"
  , MyOpt ['d']     ["data"]          (NoArg dataOpt)             (boolOpt dataTypes)   "generate data type definition"
  , MyOpt []        ["datarecords"]   (NoArg dataRecOpt)          (boolOpt dataRecords) "generate record data types"
  , MyOpt []        ["strictdata"]    (NoArg strictDataOpt)       (boolOpt strictData)  "generate strict data fields (when data is generated)"
  , MyOpt []        ["strictwrap"]    (NoArg strictWrapOpt)       (boolOpt strictWrap)  "generate strict wrap fields for WRAPPER generated data"
  , MyOpt ['c']     ["catas"]         (NoArg cataOpt)             (boolOpt folds)       "generate catamorphisms"
  , MyOpt ['f']     ["semfuns"]       (NoArg semfunsOpt)          (boolOpt semfuns)     "generate semantic functions"
  , MyOpt ['s']     ["signatures"]    (NoArg signaturesOpt)       (boolOpt typeSigs)    "generate signatures for semantic functions"
  , MyOpt []        ["newtypes"]      (NoArg newtypesOpt)         (boolOpt newtypes)    "use newtypes instead of type synonyms"
  , MyOpt ['p']     ["pretty"]        (NoArg prettyOpt)           (boolOpt attrInfo)    "generate pretty printed list of attributes"
  , MyOpt ['w']     ["wrappers"]      (NoArg wrappersOpt)         (boolOpt wrappers)    "generate wappers for semantic domains"
  , MyOpt ['r']     ["rename"]        (NoArg renameOpt)           (boolOpt rename)      "rename data constructors"
  , MyOpt []        ["modcopy"]       (NoArg modcopyOpt)          (boolOpt modcopy)     "use modified copy rule"
  , MyOpt []        ["nest"]          (NoArg nestOpt)             (boolOpt nest)        "use nested tuples"
  , MyOpt []        ["syntaxmacro"]   (NoArg smacroOpt)           (boolOpt smacro)      "experimental: generate syntax macro code (using knit catas)"
  , MyOpt ['o']     ["output"]        (ReqArg outputOpt "file")   outputOptGet          "specify output file"
  , MyOpt ['v']     ["verbose"]       (NoArg verboseOpt)          (boolOpt verbose)     "verbose error message format"
  , MyOpt ['h','?'] ["help"]          (NoArg helpOpt)             (boolOpt showHelp)    "get (this) usage information"
  , MyOpt ['a']     ["all"]           (NoArg allOpt)              noOpt                ("do everything (-" ++ allc ++ ")")
  , MyOpt ['P']     [""]              (ReqArg searchPathOpt "search path") searchPathOptGet ("specify seach path")
  , MyOpt []        ["prefix"]        (ReqArg prefixOpt "prefix") (stringOpt prefix)    "set prefix for semantic functions"
  , MyOpt []        ["self"]          (NoArg selfOpt)             (boolOpt withSelf)    "generate self attribute"
  , MyOpt []        ["cycle"]         (NoArg cycleOpt)            (boolOpt withCycle)   "check for cyclic definitions"
  , MyOpt []        ["version"]       (NoArg versionOpt)          (boolOpt showVersion) "get version information"
  , MyOpt ['O']     ["optimize"]      (NoArg optimizeOpt)         noOpt                 "optimize generated code (--visit --case)"
  , MyOpt []        ["visit"]         (NoArg visitOpt)            (boolOpt visit)       "try generating visit functions"
  , MyOpt []        ["seq"]           (NoArg seqOpt)              (boolOpt withSeq)     "force evaluation using function seq (visit functions only)"
  , MyOpt []        ["unbox"]         (NoArg unboxOpt)            (boolOpt unbox)       "use unboxed tuples"
  , MyOpt []        ["bangpats"]      (NoArg bangpatsOpt)         (boolOpt bangpats)    "use bang patterns (visit functions only)"
  , MyOpt []        ["case"]          (NoArg casesOpt)            (boolOpt cases)       "Use nested cases instead of let (visit functions only)"
  , MyOpt []        ["strictcase"]    (NoArg strictCasesOpt)      (boolOpt strictCases) "Force evaluation of the scrutinee of cases (in generated code, visit functions only)"
  , MyOpt []        ["strictercase"]  (NoArg stricterCasesOpt)    (boolOpt stricterCases) "Force evaluation of all variables bound by a case statement (in generated code)"
  , MyOpt []        ["strictsem"]     (NoArg strictSemOpt)        (boolOpt strictSems)  "Force evaluation of sem-function arguments (in generated code)"
  , MyOpt []        ["localcps"]      (NoArg localCpsOpt)         (boolOpt localCps)    "Apply a local CPS transformation (in generated code, visit functions only)"
  , MyOpt []        ["splitsems"]     (NoArg splitSemsOpt)        (boolOpt splitSems)   "Split semantic functions into smaller pieces"
  , MyOpt []        ["Werrors"]       (NoArg werrorsOpt)          (boolOpt werrors)     "Turn warnings into fatal errors"
  , MyOpt []        ["Wignore"]       (NoArg wignoreOpt)          (boolOpt wignore)     "Ignore warnings"
  , MyOpt []        ["Wmax"]          (ReqArg wmaxErrsOpt "<max errs reported>") wmaxErrsOptGet "Sets the maximum number of errors that are reported"
  , MyOpt []        ["dumpgrammar"]   (NoArg dumpgrammarOpt)      (boolOpt dumpgrammar) "Dump internal grammar representation (in generated code)"
  , MyOpt []        ["dumpcgrammar"]  (NoArg dumpcgrammarOpt)     (boolOpt dumpcgrammar)"Dump internal cgrammar representation (in generated code)"
  , MyOpt []        ["gentraces"]     (NoArg genTracesOpt)        (boolOpt genTraces)   "Generate trace expressions (in generated code)"
  , MyOpt []        ["genusetraces"]  (NoArg genUseTracesOpt)     (boolOpt genUseTraces)"Generate trace expressions at attribute use sites (in generated code)"
  , MyOpt []        ["gencostcentres"] (NoArg genCostCentresOpt)  (boolOpt genCostCentres) "Generate cost centre pragmas (in generated code)"
  , MyOpt []        ["genlinepragmas"] (NoArg genLinePragmasOpt)  (boolOpt genLinePragmas) "Generate GHC LINE pragmas (in generated code)"
  , MyOpt []        ["sepsemmods"]    (NoArg sepSemModsOpt)       (boolOpt sepSemMods)  "Generate separate modules for semantic functions (in generated code)"
  , MyOpt ['M']     ["genfiledeps"]   (NoArg genFileDepsOpt)      (boolOpt genFileDeps) "Generate a list of dependencies on the input AG files"
  , MyOpt []        ["genvisage"]     (NoArg genVisageOpt)        (boolOpt genvisage)   "Generate output for the AG visualizer Visage"
  , MyOpt []        ["aspectag"]      (NoArg genAspectAGOpt)      (boolOpt genAspectAG) "Generate AspectAG file"
  , MyOpt []        ["nogroup"]       (ReqArg noGroupOpt "attributes") noGroupOptGet    "specify the attributes that won't be grouped in AspectAG"
  , MyOpt []        ["extends"]       (ReqArg extendsOpt "module") (mbStringOpt extends)        "specify a module to be extended"
  , MyOpt []        ["genattrlist"]   (NoArg genAttrListOpt)      (boolOpt genAttributeList) "Generate a list of all explicitly defined attributes (outside irrefutable patterns)"
  , MyOpt []        ["forceirrefutable"] (OptArg forceIrrefutableOpt "file") (mbStringOpt forceIrrefutables) "Force a set of explicitly defined attributes to be irrefutable, specify file containing the attribute set"
  , MyOpt []        ["uniquedispenser"] (ReqArg uniqueDispenserOpt "name") (stringOpt uniqueDispenser) "The Haskell function to call in the generated code"
  , MyOpt []        ["lckeywords"]    (NoArg lcKeywordsOpt)       (boolOpt lcKeywords)  "Use lowercase keywords (sem, attr) instead of the uppercase ones (SEM, ATTR)"
  , MyOpt []        ["doublecolons"]  (NoArg doubleColonsOpt)     (boolOpt doubleColons)"Use double colons for type signatures instead of single colons"
  , MyOpt ['H']     ["haskellsyntax"] (NoArg haskellSyntaxOpt)    noOpt                 "Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)"
  , MyOpt []        ["reference"]     (NoArg referenceOpt)        (boolOpt reference)   "Use reference attributes"
  , MyOpt []        ["monadic"]       (NoArg monadicOpt)          (boolOpt monadic)     "Experimental: generate monadic code"
  , MyOpt []        ["ocaml"]         (NoArg ocamlOpt)            (boolOpt ocaml)       "Generate Ocaml code"
  , MyOpt []        ["cleanlang"]     (NoArg cleanOpt)            (boolOpt clean)       "Generate Clean code"
  , MyOpt []        ["breadthfirst"]  (NoArg breadthfirstOpt)     (boolOpt breadthFirst)"Experimental: generate breadth-first code"
  , MyOpt []        ["breadthfirst-strict"] (NoArg breadthfirstStrictOpt) (boolOpt breadthFirstStrict) "Experimental: outermost breadth-first evaluator is strict instead of lazy"
  , MyOpt []        ["visitcode"]     (NoArg visitorsOutputOpt)   (boolOpt visitorsOutput) "Experimental: generate visitors code"
  , MyOpt []        ["kennedywarren"] (NoArg kennedyWarrenOpt)    (boolOpt kennedyWarren) "Experimental: use Kennedy-Warren's algorithm for ordering"
  , MyOpt []        ["statistics"]    (ReqArg statisticsOpt "FILE to append to") (mbStringOpt statsFile) "Append statistics to FILE"
  , MyOpt []        ["checkParseRhs"]           (NoArg parseHsRhsOpt)              (boolOpt checkParseRhs)         "Parse RHS of rules with Haskell parser"
  , MyOpt []        ["checkParseTys"]           (NoArg parseHsTpOpt)               (boolOpt checkParseTy)          "Parse types of attrs with Haskell parser"
  , MyOpt []        ["checkParseBlocks"]        (NoArg parseHsBlockOpt)            (boolOpt checkParseBlock)       "Parse blocks with Haskell parser"
  , MyOpt []        ["checkParseHaskell"]       (NoArg parseHsOpt)                 noOpt                           "Parse Haskell code (recognizer)"
  , MyOpt []        ["nocatas"]                 (ReqArg nocatasOpt "list of nonterms") nocatasOptGet               "Nonterminals not to generate catas for"
  , MyOpt []        ["nooptimize"]              (NoArg noOptimizeOpt)              (boolOpt noOptimizations)       "Disable optimizations"
  , MyOpt []        ["parallel"]                (NoArg parallelOpt)                (boolOpt parallelInvoke)        "Generate a parallel evaluator (if possible)"
  , MyOpt []        ["monadicwrapper"]          (NoArg monadicWrappersOpt)         (boolOpt monadicWrappers)       "Generate monadic wrappers"
  , MyOpt []        ["helpinlining"]            (NoArg helpInliningOpt)            (boolOpt helpInlining)          "Generate inline directives for GHC"
  , MyOpt []        ["dummytokenvisit"]         (NoArg dummyTokenVisitOpt)         (boolOpt dummyTokenVisit)       "Add an additional dummy parameter to visit functions"
  , MyOpt []        ["tupleasdummytoken"]       (NoArg tupleAsDummyTokenOpt)       (boolOpt tupleAsDummyToken)     "Use conventional tuples as dummy parameter instead of a RealWorld state token"
  , MyOpt []        ["stateasdummytoken"]       (NoArg stateAsDummyTokenOpt)       noOpt                           "Use RealWorld state token as dummy parameter instead of conventional tuples (default)"
  , MyOpt []        ["strictdummytoken"]        (NoArg strictDummyTokenOpt)        (boolOpt strictDummyToken)      "Strictify the dummy token that makes states and rules functions"
  , MyOpt []        ["noperruletypesigs"]       (NoArg noPerRuleTypeSigsOpt)       (boolOpt noPerRuleTypeSigs)     "Do not generate type sigs for attrs passed to rules"
  , MyOpt []        ["noperstatetypesigs"]      (NoArg noPerStateTypeSigsOpt)      (boolOpt noPerStateTypeSigs)    "Do not generate type sigs for attrs saved in node states"
  , MyOpt []        ["noeagerblackholing"]      (NoArg noEagerBlackholingOpt)      (boolOpt noEagerBlackholing)    "Do not automatically add the eager blackholing feature for parallel programs"
  , MyOpt []        ["noperrulecostcentres"]    (NoArg noPerRuleCostCentresOpt)    (boolOpt noPerRuleCostCentres)  "Do not generate cost centres for rules"
  , MyOpt []        ["nopervisitcostcentres"]   (NoArg noPerVisitCostCentresOpt)   (boolOpt noPerVisitCostCentres) "Do not generate cost centres for visits"
  , MyOpt []        ["noinlinepragmas"]         (NoArg noInlinePragmasOpt)         (boolOpt noInlinePragmas)       "Definitely not generate inline directives"
  , MyOpt []        ["aggressiveinlinepragmas"] (NoArg aggressiveInlinePragmasOpt) (boolOpt aggressiveInlinePragmas) "Generate more aggressive inline directives"
  , MyOpt []        ["latehigherorderbinding"]  (NoArg lateHigherOrderBindingOpt)  (boolOpt lateHigherOrderBinding) "Generate an attribute and wrapper for late binding of higher-order attributes"
  , MyOpt []        ["noincludes"]              (NoArg noIncludesOpt)              (boolOpt noIncludes)             "Ignore include directives in .ag files"
  , MyOpt []        ["quiet"]                   (NoArg beQuietOpt)                 (boolOpt beQuiet)                "Dont print some compilation information"
  ]

-- For compatibility
options     :: [OptDescr (Options -> Options)]
options     = map fromMyOpt allOptions

allc :: String
allc = "dcfsprm"

data ModuleHeader  = NoName
                   | Name String
                   | Default deriving (Eq, Show)

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
                      , sepSemMods :: Bool
                      , allowSepSemMods :: Bool
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
                      , clean :: Bool
                      , visitorsOutput :: Bool
                      , statsFile :: Maybe String
                      , breadthFirst :: Bool
                      , breadthFirstStrict :: Bool
                      , checkParseRhs :: Bool
                      , checkParseTy :: Bool
                      , checkParseBlock :: Bool
                      , nocatas :: Set NontermIdent
                      , noOptimizations :: Bool
                      , reference :: Bool
                      , noIncludes :: Bool
                      , outputStr :: String -> IO ()
                      , failWithCode :: Int -> IO ()
                      , mainFilename :: Maybe String
                      , beQuiet :: Bool

                      -- KW code path
                      , kennedyWarren       :: Bool
                      , parallelInvoke      :: Bool
                      , tupleAsDummyToken   :: Bool  -- use the empty tuple as dummy token instead of State# RealWorld (Lambda State Hack GHC?)
                      , dummyTokenVisit     :: Bool  -- add a dummy argument/pass dummy extra token to visits (should not really have an effect ... Lambda State Hack GHC?)
                      , strictDummyToken    :: Bool  -- make the dummy token strict (to prevent its removal -- should not really have an effect)
                      , noPerRuleTypeSigs   :: Bool  -- do not print type signatures for attributes of rules
                      , noPerStateTypeSigs  :: Bool  -- do not print type signatures for attributes contained in the state
                      , noEagerBlackholing  :: Bool  -- disable the use of eager black holing in the parallel evaluator code
                      , lateHigherOrderBinding :: Bool  -- generate code to allow late binding of higher-order children semantics
                      , monadicWrappers        :: Bool

                      -- tracing
                      , genTraces :: Bool
                      , genUseTraces :: Bool
                      , genCostCentres :: Bool
                      , noPerRuleCostCentres :: Bool
                      , noPerVisitCostCentres :: Bool

                      -- inline pragma generation
                      , helpInlining :: Bool
                      , noInlinePragmas :: Bool
                      , aggressiveInlinePragmas :: Bool
                      } -- deriving (Eq, Show)

noOptions :: Options
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
                    , sepSemMods     = False
                    , allowSepSemMods = True
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
                    , clean           = False
                    , visitorsOutput  = False
                    , statsFile       = Nothing
                    , breadthFirst     = False
                    , breadthFirstStrict = False
                    , checkParseRhs = False
                    , checkParseTy  = False
                    , checkParseBlock = False
                    , nocatas         = Set.empty
                    , noOptimizations = False
                    , reference       = False
                    , noIncludes      = False
                    , outputStr       = hPutStr stderr
                    , failWithCode    = exitWith . ExitFailure
                    , mainFilename    = Nothing
                    , beQuiet         = False

                    -- defaults for the KW-code path
                    , kennedyWarren       = False
                    , parallelInvoke      = False
                    , tupleAsDummyToken   = True
                    , dummyTokenVisit     = False
                    , strictDummyToken    = False
                    , noPerRuleTypeSigs   = False
                    , noPerStateTypeSigs  = False
                    , noEagerBlackholing  = False
                    , lateHigherOrderBinding = False
                    , monadicWrappers        = False

                    -- defaults for tracing
                    , genTraces     = False
                    , genUseTraces  = False
                    , genCostCentres = False
                    , noPerRuleCostCentres  = False
                    , noPerVisitCostCentres = False

                    -- defaults for inline pragma generation
                    , helpInlining    = False
                    , noInlinePragmas = False
                    , aggressiveInlinePragmas = False
                    }

--Options -> String -> [String]
moduleOpt :: Maybe String -> Options -> Options
moduleOpt  nm   opts = opts{moduleName   = maybe Default Name nm}
moduleOptGet :: Options -> String -> [String]
moduleOptGet opts nm = case moduleName opts of
  NoName -> []
  Name s -> [nm++"="++s]
  Default -> [nm]

dataOpt, dataRecOpt, strictDataOpt, strictWrapOpt, cataOpt, semfunsOpt, signaturesOpt, prettyOpt,renameOpt, wrappersOpt, modcopyOpt, newtypesOpt, nestOpt, smacroOpt, verboseOpt, helpOpt, versionOpt, selfOpt, cycleOpt, visitOpt, seqOpt, unboxOpt, bangpatsOpt, casesOpt, strictCasesOpt, stricterCasesOpt, strictSemOpt, localCpsOpt, splitSemsOpt, werrorsOpt, wignoreOpt, dumpgrammarOpt, dumpcgrammarOpt, genTracesOpt, genUseTracesOpt, genCostCentresOpt, sepSemModsOpt, genFileDepsOpt, genLinePragmasOpt, genVisageOpt, genAspectAGOpt, dummyTokenVisitOpt, tupleAsDummyTokenOpt, stateAsDummyTokenOpt, strictDummyTokenOpt, noPerRuleTypeSigsOpt, noPerStateTypeSigsOpt, noEagerBlackholingOpt, noPerRuleCostCentresOpt, noPerVisitCostCentresOpt, helpInliningOpt, noInlinePragmasOpt, aggressiveInlinePragmasOpt, lateHigherOrderBindingOpt, monadicWrappersOpt, referenceOpt, genAttrListOpt, lcKeywordsOpt, doubleColonsOpt, haskellSyntaxOpt, monadicOpt, parallelOpt, ocamlOpt, cleanOpt, visitorsOutputOpt, breadthfirstOpt, breadthfirstStrictOpt, parseHsRhsOpt, parseHsTpOpt, parseHsBlockOpt, parseHsOpt, kennedyWarrenOpt, noOptimizeOpt, allOpt, optimizeOpt, noIncludesOpt, beQuietOpt, condDisableOptimizations :: Options -> Options

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
prefixOpt :: String -> Options -> Options
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
wmaxErrsOpt :: String -> Options -> Options
wmaxErrsOpt n   opts = opts{wmaxerrs     = read n}
wmaxErrsOptGet :: Options -> String -> [String]
wmaxErrsOptGet opts nm = if wmaxerrs opts /= wmaxerrs noOptions
                         then [nm,show (wmaxerrs opts)]
                         else []
dumpgrammarOpt  opts = opts{dumpgrammar  = True}
dumpcgrammarOpt opts = opts{dumpcgrammar = True}
genTracesOpt    opts = opts{genTraces    = True}
genUseTracesOpt opts = opts{genUseTraces = True}
genCostCentresOpt opts = opts{genCostCentres = True}
sepSemModsOpt opts = opts{sepSemMods = allowSepSemMods opts}
genFileDepsOpt opts = opts{genFileDeps = True}
genLinePragmasOpt opts = opts{genLinePragmas = True}
genVisageOpt opts = opts{genvisage = True }
genAspectAGOpt opts = opts{genAspectAG = True}

dummyTokenVisitOpt opts         = opts { dummyTokenVisit = True }
tupleAsDummyTokenOpt opts       = opts { tupleAsDummyToken = True }
stateAsDummyTokenOpt opts       = opts { tupleAsDummyToken = False }
strictDummyTokenOpt opts        = opts { strictDummyToken = True }
noPerRuleTypeSigsOpt opts       = opts { noPerRuleTypeSigs = True }
noPerStateTypeSigsOpt opts      = opts { noPerStateTypeSigs = True }
noEagerBlackholingOpt opts      = opts { noEagerBlackholing = True }
noPerRuleCostCentresOpt opts    = opts { noPerRuleCostCentres = True }
noPerVisitCostCentresOpt opts   = opts { noPerVisitCostCentres = True }
helpInliningOpt opts            = opts { helpInlining = True }
noInlinePragmasOpt opts         = opts { noInlinePragmas = True }
aggressiveInlinePragmasOpt opts = opts { aggressiveInlinePragmas = True }
lateHigherOrderBindingOpt opts  = opts { lateHigherOrderBinding = True }
monadicWrappersOpt opts         = opts { monadicWrappers = True }
referenceOpt opts               = opts { reference = True }

noGroupOpt :: String -> Options -> Options
noGroupOpt  att  opts = opts{noGroup  = wordsBy (== ':') att  ++ noGroup opts}
noGroupOptGet :: Options -> String -> [String]
noGroupOptGet opts nm = if null (noGroup opts)
                        then []
                        else [nm, intercalate ":" (noGroup opts)]
extendsOpt :: String -> Options -> Options
extendsOpt  m  opts = opts{extends  = Just m }

genAttrListOpt opts = opts { genAttributeList = True }
forceIrrefutableOpt :: Maybe String -> Options -> Options
forceIrrefutableOpt mbNm opts = opts { forceIrrefutables = mbNm }
uniqueDispenserOpt :: String -> Options -> Options
uniqueDispenserOpt nm opts = opts { uniqueDispenser = nm }
lcKeywordsOpt opts = opts { lcKeywords = True }
doubleColonsOpt opts = opts { doubleColons = True }
haskellSyntaxOpt = lcKeywordsOpt . doubleColonsOpt . genLinePragmasOpt
monadicOpt opts = opts { monadic = True }
parallelOpt opts = opts { parallelInvoke = True }
ocamlOpt opts = opts { ocaml = True, kennedyWarren = True, withCycle = True, visit = True }
cleanOpt opts = opts { clean = True } --TODO: More?
visitorsOutputOpt opts = opts { visitorsOutput = True }
statisticsOpt :: String -> Options -> Options
statisticsOpt nm opts = opts { statsFile = Just nm }
breadthfirstOpt opts = opts { breadthFirst = True }
breadthfirstStrictOpt opts = opts { breadthFirstStrict = True }
parseHsRhsOpt opts = opts { checkParseRhs = True }
parseHsTpOpt opts = opts { checkParseTy = True }
parseHsBlockOpt opts = opts { checkParseBlock = True }
parseHsOpt = parseHsRhsOpt . parseHsTpOpt . parseHsBlockOpt
kennedyWarrenOpt opts = opts { kennedyWarren = True }
noOptimizeOpt opts = opts { noOptimizations = True }
nocatasOpt :: String -> Options -> Options
nocatasOpt str opts = opts { nocatas = set `Set.union` nocatas opts } where
  set = Set.fromList ids
  ids = map identifier lst
  lst = wordsBy (== ',') str
nocatasOptGet :: Options -> String -> [String]
nocatasOptGet opts nm = if Set.null (nocatas opts)
                        then []
                        else [nm,intercalate "," . map getName . Set.toList . nocatas $ opts]
outputOpt :: String -> Options -> Options
outputOpt  file  opts = opts{outputFiles  = file : outputFiles opts}
outputOptGet :: Options -> String -> [String]
outputOptGet opts nm  = concat [ [nm, file] | file <- outputFiles opts]
searchPathOpt :: String -> Options -> Options
searchPathOpt  path  opts = opts{searchPath  = wordsBy (\x -> x == ';' || x == ':') path ++ searchPath opts}
searchPathOptGet :: Options -> String -> [String]
searchPathOptGet opts nm = if null (searchPath opts)
                           then []
                           else [nm, intercalate ":" (searchPath opts)]
allOpt = moduleOpt Nothing . dataOpt . cataOpt . semfunsOpt . signaturesOpt . prettyOpt . renameOpt . dataRecOpt
optimizeOpt   = visitOpt . casesOpt
noIncludesOpt opts = opts { noIncludes = True }
beQuietOpt opts = opts { beQuiet = True }

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
                
-- | Inverse of intercalate
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p = f
  where
    f s = let (x,xs) = break p s
          in  if null x then [] else x : f (drop 1 xs)
                
-- | Use all parsed options to generate real options
constructOptions :: [Options -> Options] -> Options
constructOptions = foldl (flip ($)) noOptions

-- | Create Options type from string arguments
getOptions :: [String] -> (Options,[String],[String])
getOptions args = let (flags,files,errors) = getOpt Permute options args
                      appliedOpts = constructOptions flags
                      finOpts = condDisableOptimizations appliedOpts
                  in (finOpts,files,errors)

-- | Convert options back to commandline string
optionsToString :: Options -> [String]
optionsToString opt = concatMap (serializeOption opt) allOptions

-- | Combine 2 sets of options
combineOptions :: Options -> Options -> Options
combineOptions o1 o2 = let str1      = optionsToString o1
                           str2      = optionsToString o2
                           (opt,_,_) = getOptions (str1 ++ str2)
                       in  opt
