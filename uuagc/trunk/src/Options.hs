module Options where

import System.Console.GetOpt

options     :: [OptDescr (Options -> Options)]
options     =  [ Option ['m']     []                (NoArg (moduleOpt Nothing)) "generate default module header"
               , Option []        ["module"]        (OptArg moduleOpt "name")   "generate module header, specify module name"
               , Option ['d']     ["data"]          (NoArg dataOpt)             "generate data type definition"
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
               , Option []        ["case"]          (NoArg casesOpt)            "Use nested cases instead of let (visit functions only)"
               , Option []        ["strictcase"]    (NoArg strictCasesOpt)      "Force evaluation of the scrutinee of cases (in generated code, visit functions only)"
               , Option []        ["localcps"]      (NoArg localCpsOpt)         "Apply a local CPS transformation (in generated code, visit functions only)"
               , Option []        ["Werrors"]       (NoArg werrorsOpt)          "Turn warnings into fatal errors"
               , Option []        ["dumpgrammar"]   (NoArg dumpgrammarOpt)      "Dump internal grammar representation (in generated code)"
               , Option []        ["dumpcgrammar"]  (NoArg dumpcgrammarOpt)      "Dump internal cgrammar representation (in generated code)"
               ]

allc = "dcfsprm"

data Options = Options{ moduleName :: ModuleHeader 
                      , dataTypes :: Bool
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
                      , cases :: Bool
                      , strictCases :: Bool
                      , localCps :: Bool
                      , werrors :: Bool
                      , dumpgrammar :: Bool
                      , dumpcgrammar :: Bool
                      } deriving Show
noOptions = Options { moduleName    = NoName
                    , dataTypes     = False
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
                    , cases         = False
                    , strictCases   = False
                    , localCps      = False
                    , werrors       = False
                    , dumpgrammar   = False
                    , dumpcgrammar  = False
                    }


moduleOpt  nm   opts = opts{moduleName   = maybe Default Name nm}            
dataOpt         opts = opts{dataTypes    = True}            
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
casesOpt        opts = opts{cases        = True}
strictCasesOpt  opts = opts{strictCases  = True}
localCpsOpt     opts = opts{localCps     = True}
werrorsOpt      opts = opts{werrors      = True}
dumpgrammarOpt  opts = opts{dumpgrammar  = True}
dumpcgrammarOpt opts = opts{dumpcgrammar = True}

outputOpt  file  opts = opts{outputFiles  = file : outputFiles opts}            
searchPathOpt  path  opts = opts{searchPath  = extract path ++ searchPath opts}            
  where extract xs = let (p,ps) = break (\x -> x == ';' || x == ':') xs
                     in if null p then [] else p : extract ps
allOpt = moduleOpt Nothing . dataOpt . cataOpt . semfunsOpt . signaturesOpt . prettyOpt . renameOpt
optimizeOpt   = visitOpt . casesOpt

getOptions args = let (flags,files,errors) = getOpt Permute options args
                  in (foldl (flip ($)) noOptions flags,files,errors)

data ModuleHeader  = NoName
                   | Name String
                   | Default deriving Show
