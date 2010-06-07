module Distribution.Simple.UUAGC.AbsSyn where

import Distribution.Simple.UUAGC.Options
import System.FilePath(normalise)

data AGFileOption = AGFileOption {filename :: String,
                                  fileClasses :: [String],
                                  opts :: UUAGCOptions}
     deriving (Show, Eq)

data AGOptionsClass = AGOptionsClass {className :: String, opts' :: UUAGCOptions}
     deriving (Show, Read, Eq)

type AGFileOptions = [AGFileOption]

data UUAGCOption = UModuleDefault
                 | UModule String
                 | UData
                 | UStrictData
                 | UStrictWData
                 | UCatas
                 | USemFuns
                 | USignatures
                 | UNewTypes
                 | UPretty
                 | UWrappers
                 | URename
                 | UModCopy
                 | UNest
                 | USyntaxMacro
                 | UOutput FilePath
                 | UVerbose
                 | UHelp -- ?
                 | UAll
                 | USearchPath FilePath
                 | UPrefix     String
                 | USelf
                 | UCycle
                 | UVersion -- ?
                 | UVisit
                 | USeq
                 | UUnbox -- UUnbox
                 | UBangPats
                 | UCase
                 | UStrictCase
                 | UStricterCase
                 | ULocalCPS
                 | USplitSems
                 | UWErrors
                 | UWIgnore
                 | UWMax       Int
                 | UDumpGrammar
                 | UDumpCGrammar
                 | UGenTraces
                 | UGenUseTraces
                 | UGenCostCentres
                 | UGenLinePragmas
                 | USepSemMods
                 | UGenFileDeps
                 | UGenVisage
                 | UGenAttrList
                 | UForceIrrefutable FilePath
                 | ULCKeyWords
                 | UOptimize
                 | UDoubleColons
                 | UHaskellSyntax
                   deriving (Eq, Read, Show)

type UUAGCOptions = [UUAGCOption]

defaultUUAGCOptions :: UUAGCOptions
defaultUUAGCOptions = [UData
                      ,UCatas
                      ,USemFuns
                      ,USignatures
                      ,UPretty
                      ,UWrappers
                      ,URename
                      ,UModuleDefault
                      ]

optionTxt = "--"
equalTxt  = "="

toLOp   s = optionTxt ++ s
toLEOpA s a = (toLOp s) ++ equalTxt ++ a

fromUUAGCOtoArgs :: UUAGCOption -> String
fromUUAGCOtoArgs (UModule s)            = toLEOpA omodule s
fromUUAGCOtoArgs UData                  = toLOp odata
fromUUAGCOtoArgs UStrictData            = toLOp ostrictdata
fromUUAGCOtoArgs UStrictWData           = toLOp ostrictwrap
fromUUAGCOtoArgs UCatas                 = toLOp ocatas
fromUUAGCOtoArgs USemFuns               = toLOp osemfuns
fromUUAGCOtoArgs USignatures            = toLOp osignatures
fromUUAGCOtoArgs UNewTypes              = toLOp onewtypes
fromUUAGCOtoArgs UPretty                = toLOp opretty
fromUUAGCOtoArgs UWrappers              = toLOp owrappers
fromUUAGCOtoArgs URename                = toLOp orename
fromUUAGCOtoArgs UModCopy               = toLOp omodcopy
fromUUAGCOtoArgs UNest                  = toLOp onest
fromUUAGCOtoArgs USyntaxMacro           = toLOp osyntaxmacro
fromUUAGCOtoArgs (UOutput fp)           = toLEOpA ooutput fp
fromUUAGCOtoArgs UVerbose               = toLOp overbose
fromUUAGCOtoArgs (USearchPath fp)       = toLEOpA "" fp
fromUUAGCOtoArgs (UPrefix p)            = toLEOpA oprefix p
fromUUAGCOtoArgs USelf                  = toLOp oself
fromUUAGCOtoArgs UCycle                 = toLOp ocycle
fromUUAGCOtoArgs UVersion               = toLOp oversion
fromUUAGCOtoArgs UVisit                 = toLOp ovisit
fromUUAGCOtoArgs USeq                   = toLOp oseq
fromUUAGCOtoArgs UUnbox                 = toLOp ounbox
fromUUAGCOtoArgs UBangPats              = toLOp obangpats
fromUUAGCOtoArgs UCase                  = toLOp ocase
fromUUAGCOtoArgs UStrictCase            = toLOp ostrictcase
fromUUAGCOtoArgs UStricterCase          = toLOp ostrictercase
fromUUAGCOtoArgs ULocalCPS              = toLOp olocalcps
fromUUAGCOtoArgs USplitSems             = toLOp osplitsems
fromUUAGCOtoArgs UWErrors               = toLOp owerrors
fromUUAGCOtoArgs UWIgnore               = toLOp owignore
fromUUAGCOtoArgs (UWMax i)              = toLEOpA owmax (show i)
fromUUAGCOtoArgs UDumpGrammar           = toLOp odumpgrammar
fromUUAGCOtoArgs UDumpCGrammar          = toLOp odumpcgrammar
fromUUAGCOtoArgs UGenTraces             = toLOp ogentraces
fromUUAGCOtoArgs UGenUseTraces          = toLOp ogenusetraces
fromUUAGCOtoArgs UGenCostCentres        = toLOp ogencostcentres
fromUUAGCOtoArgs UGenLinePragmas        = toLOp ogenlinepragmas
fromUUAGCOtoArgs USepSemMods            = toLOp osepsemmods
fromUUAGCOtoArgs UGenFileDeps           = toLOp ogenfiledeps
fromUUAGCOtoArgs UGenVisage             = toLOp ogenvisage
fromUUAGCOtoArgs UGenAttrList           = toLOp ogenattrlist
fromUUAGCOtoArgs (UForceIrrefutable fp) = toLEOpA oforceirrefutable fp
fromUUAGCOtoArgs UOptimize              = toLOp ooptimize
fromUUAGCOtoArgs UModuleDefault         = toLOp omodule

fromUUAGCOstoArgs :: UUAGCOptions -> [String]
fromUUAGCOstoArgs = map fromUUAGCOtoArgs

lookupFileOptions :: FilePath -> AGFileOptions -> UUAGCOptions
lookupFileOptions s = foldl f defaultUUAGCOptions
    where f e (AGFileOption s' classes opt)
              | s == (normalise s')  = opt
              | otherwise            = e

