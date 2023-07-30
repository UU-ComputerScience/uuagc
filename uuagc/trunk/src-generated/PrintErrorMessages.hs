{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrintErrorMessages where
{-# LINE 2 "src-ag/ErrorMessages.ag" #-}

import UU.Scanner.Position(Pos)
import Pretty
import CodeSyntax
import CommonTypes
{-# LINE 12 "src-generated/PrintErrorMessages.hs" #-}

{-# LINE 4 "src-ag/PrintErrorMessages.ag" #-}

import UU.Scanner.Position(Pos(..), noPos)
import ErrorMessages
import Data.List(mapAccumL)
import GrammarInfo
{-# LINE 20 "src-generated/PrintErrorMessages.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 11 "src-ag/PrintErrorMessages.ag" #-}

isError :: Options -> Error -> Bool
isError _ (ParserError     _ _ _    ) = True
isError _ (DupAlt          _ _ _    ) = False
isError _ (DupSynonym      _ _      ) = False
isError _ (DupSet          _ _      ) = False
isError _ (DupInhAttr      _ _ _    ) = True
isError _ (DupSynAttr      _ _ _    ) = True
isError _ (DupChild        _ _ _ _  ) = False
isError _ (DupRule         _ _ _ _ _) = True
isError _ (DupSig          _ _ _    ) = False
isError _ (UndefNont       _        ) = True
isError _ (UndefAlt        _ _      ) = True
isError _ (UndefChild      _ _ _    ) = True
isError _ (MissingRule     _ _ _ _  ) = False
isError _ (SuperfluousRule _ _ _ _  ) = False
isError _ (UndefLocal      _ _ _    ) = True
isError _ (ChildAsLocal    _ _ _    ) = False
isError _ (UndefAttr       _ _ _ _ _) = True
isError _ (CyclicSet       _        ) = True
isError _ (CustomError     w _ _    ) = not w
isError opts (LocalCirc       _ _ _ _ _) = cycleIsDangerous opts
isError opts (InstCirc        _ _ _ _ _) = cycleIsDangerous opts
isError opts (DirectCirc      _ _ _    ) = cycleIsDangerous opts
isError opts (InducedCirc     _ _ _    ) = cycleIsDangerous opts
isError _ (MissingTypeSig  _ _ _    ) = False
isError _ (MissingInstSig  _ _ _    ) = True
isError _ (DupUnique       _ _ _    ) = False
isError _ (MissingUnique   _ _      ) = True
isError _ (MissingSyn      _ _      ) = True
isError _ (MissingNamedRule _ _ _)    = True
isError _ (DupRuleName _ _ _)         = True
isError _ (HsParseError _ _)          = True
isError _ (Cyclic _ _ _)              = True
isError _ (IncompatibleVisitKind _ _ _ _) = True
isError _ (IncompatibleRuleKind _ _)      = True
isError _ (IncompatibleAttachKind _ _)    = True

cycleIsDangerous :: Options -> Bool
cycleIsDangerous opts
  = any ($ opts) [ wignore, bangpats, cases, strictCases, stricterCases, strictSems, withCycle ]
{-# LINE 65 "src-generated/PrintErrorMessages.hs" #-}

{-# LINE 539 "src-ag/PrintErrorMessages.ag" #-}

toWidth :: Int -> String -> String
toWidth n xs | k<n       = xs ++ replicate (n-k) ' '
             | otherwise = xs
               where k = length xs

showEdge :: ((Identifier,Identifier),[String],[String]) -> PP_Doc
showEdge ((inh,syn),_,_)
  = text ("inherited attribute " ++ toWidth 20 (getName inh) ++ " with synthesized attribute " ++  getName syn)

showEdgeLong :: ((Identifier,Identifier),[String],[String]) -> PP_Doc
showEdgeLong ((inh,syn),path1,path2)
  = text ("inherited attribute " ++ getName inh ++ " is needed for " ++  "synthesized attribute " ++ getName syn)
    >-< indent 4 (vlist (map text path2))
    >-< text "and back: "
    >-< indent 4 (vlist (map text path1))

attrText :: Identifier -> Identifier -> String
attrText inh syn
 =  if   inh == syn
    then "threaded attribute " ++ getName inh
    else "inherited attribute " ++ getName inh ++ " and synthesized attribute " ++getName syn

showLineNr :: Int -> String
showLineNr i | i==(-1) = "CR"
             | otherwise = show i

showAttrDef :: Identifier -> Identifier -> String
showAttrDef f a | f == _LHS  = "synthesized attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "inherited attribute " ++ getName a ++ " of field " ++ getName f

showAttrUse :: Identifier -> Identifier -> String
showAttrUse f a | f == _LHS  = "inherited attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "synthesized attribute " ++ getName a ++ " of field " ++ getName f

ppAttr :: Identifier -> Identifier -> PP_Doc
ppAttr f a = text (getName f++"."++getName a)
ppAttrUse :: Identifier -> Identifier -> PP_Doc
ppAttrUse f a = "@" >|< ppAttr f a
{-# LINE 111 "src-generated/PrintErrorMessages.hs" #-}

{-# LINE 585 "src-ag/PrintErrorMessages.ag" #-}

infixr 5 +#+
(+#+) :: String -> String -> String
(+#+) s t = s ++ " " ++ t

infixr 5 +.+
(+.+) :: Identifier -> Identifier -> String
(+.+) s t = getName s ++ "." ++ getName t

wfill :: [String] -> PP_Doc
wfill = fill . addSpaces. concat . map words
  where addSpaces (x:xs) = x:map addSpace xs
        addSpaces []     = []
        addSpace  [x]    | x `elem` ".,;:!?" = [x]
        addSpace  xs     = ' ':xs

ppError :: Bool           -- class of the error, True:error False:warning
        -> Pos      -- source position
        -> PP_Doc         -- error message
        -> PP_Doc         -- pattern
        -> PP_Doc         -- help, more info
        -> PP_Doc         -- action taken by AG
        -> Bool           -- verbose? show help and action?
        -> PP_Doc
ppError isErr pos mesg pat hlp act verb
  = let position = case pos of
                     Pos l c f | l >= 0    -> f >|< ":" >|< show l >|< ":" >|< show c
                               | otherwise -> pp "uuagc"
        tp      = if isErr then "error" else "warning"
        header  = position >|< ":" >#< tp >|< ":" >#< mesg
        pattern = "pattern  :" >#< pat
        help    = "help     :" >#< hlp
        action  = "action   :" >#< act
    in if verb
         then vlist [text "",header,pattern,help,action]
         else header

{-
-- old error reporting code
  = let
      cl = if isError then "ERROR" else "Warning"
      position   = case pos of
                         (Pos l c f) | l >= 0    -> f >|< ": line " >|< show l >|< ", column " >|< show c
                                     | otherwise -> empty
      header     = "*** UU.AG" >#< cl >#< position >#< "***"
      message    = "problem  :" >#< mesg
      pattern    = "pattern  :" >#< pat
      help       = "help     :" >#< hlp
      action     = "action   :" >#< act
    in
      if verbose
         then vlist [text "",header,message,pattern,help,action]
         else vlist [text "",header,message]
-}

showPos :: Identifier -> String
showPos = show . getPos

ppInterface :: Show a => a -> PP_Doc
ppInterface inter = wfill ["interface:", show inter]

{-# LINE 175 "src-generated/PrintErrorMessages.hs" #-}
-- Error -------------------------------------------------------
-- wrapper
data Inh_Error  = Inh_Error { options_Inh_Error :: (Options), verbose_Inh_Error :: (Bool) }
data Syn_Error  = Syn_Error { me_Syn_Error :: (Error), pp_Syn_Error :: (PP_Doc) }
{-# INLINABLE wrap_Error #-}
wrap_Error :: T_Error  -> Inh_Error  -> (Syn_Error )
wrap_Error (T_Error act) (Inh_Error _lhsIoptions _lhsIverbose) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg1 = T_Error_vIn1 _lhsIoptions _lhsIverbose
        (T_Error_vOut1 _lhsOme _lhsOpp) <- return (inv_Error_s2 sem arg1)
        return (Syn_Error _lhsOme _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Error #-}
sem_Error :: Error  -> T_Error 
sem_Error ( ParserError pos_ problem_ action_ ) = sem_Error_ParserError pos_ problem_ action_
sem_Error ( HsParseError pos_ msg_ ) = sem_Error_HsParseError pos_ msg_
sem_Error ( DupAlt nt_ con_ occ1_ ) = sem_Error_DupAlt nt_ con_ occ1_
sem_Error ( DupSynonym nt_ occ1_ ) = sem_Error_DupSynonym nt_ occ1_
sem_Error ( DupSet name_ occ1_ ) = sem_Error_DupSet name_ occ1_
sem_Error ( DupInhAttr nt_ attr_ occ1_ ) = sem_Error_DupInhAttr nt_ attr_ occ1_
sem_Error ( DupSynAttr nt_ attr_ occ1_ ) = sem_Error_DupSynAttr nt_ attr_ occ1_
sem_Error ( DupChild nt_ con_ name_ occ1_ ) = sem_Error_DupChild nt_ con_ name_ occ1_
sem_Error ( DupRule nt_ con_ field_ attr_ occ1_ ) = sem_Error_DupRule nt_ con_ field_ attr_ occ1_
sem_Error ( DupRuleName nt_ con_ nm_ ) = sem_Error_DupRuleName nt_ con_ nm_
sem_Error ( DupSig nt_ con_ attr_ ) = sem_Error_DupSig nt_ con_ attr_
sem_Error ( UndefNont nt_ ) = sem_Error_UndefNont nt_
sem_Error ( UndefAlt nt_ con_ ) = sem_Error_UndefAlt nt_ con_
sem_Error ( UndefChild nt_ con_ name_ ) = sem_Error_UndefChild nt_ con_ name_
sem_Error ( MissingRule nt_ con_ field_ attr_ ) = sem_Error_MissingRule nt_ con_ field_ attr_
sem_Error ( MissingNamedRule nt_ con_ name_ ) = sem_Error_MissingNamedRule nt_ con_ name_
sem_Error ( SuperfluousRule nt_ con_ field_ attr_ ) = sem_Error_SuperfluousRule nt_ con_ field_ attr_
sem_Error ( UndefLocal nt_ con_ var_ ) = sem_Error_UndefLocal nt_ con_ var_
sem_Error ( ChildAsLocal nt_ con_ var_ ) = sem_Error_ChildAsLocal nt_ con_ var_
sem_Error ( UndefAttr nt_ con_ field_ attr_ isOut_ ) = sem_Error_UndefAttr nt_ con_ field_ attr_ isOut_
sem_Error ( Cyclic nt_ mbCon_ verts_ ) = sem_Error_Cyclic nt_ mbCon_ verts_
sem_Error ( CyclicSet name_ ) = sem_Error_CyclicSet name_
sem_Error ( CustomError isWarning_ pos_ mesg_ ) = sem_Error_CustomError isWarning_ pos_ mesg_
sem_Error ( LocalCirc nt_ con_ attr_ o_visit_ path_ ) = sem_Error_LocalCirc nt_ con_ attr_ o_visit_ path_
sem_Error ( InstCirc nt_ con_ attr_ o_visit_ path_ ) = sem_Error_InstCirc nt_ con_ attr_ o_visit_ path_
sem_Error ( DirectCirc nt_ o_visit_ cyclic_ ) = sem_Error_DirectCirc nt_ o_visit_ cyclic_
sem_Error ( InducedCirc nt_ cinter_ cyclic_ ) = sem_Error_InducedCirc nt_ cinter_ cyclic_
sem_Error ( MissingTypeSig nt_ con_ attr_ ) = sem_Error_MissingTypeSig nt_ con_ attr_
sem_Error ( MissingInstSig nt_ con_ attr_ ) = sem_Error_MissingInstSig nt_ con_ attr_
sem_Error ( DupUnique nt_ con_ attr_ ) = sem_Error_DupUnique nt_ con_ attr_
sem_Error ( MissingUnique nt_ attr_ ) = sem_Error_MissingUnique nt_ attr_
sem_Error ( MissingSyn nt_ attr_ ) = sem_Error_MissingSyn nt_ attr_
sem_Error ( IncompatibleVisitKind child_ vis_ from_ to_ ) = sem_Error_IncompatibleVisitKind child_ vis_ from_ to_
sem_Error ( IncompatibleRuleKind rule_ kind_ ) = sem_Error_IncompatibleRuleKind rule_ kind_
sem_Error ( IncompatibleAttachKind child_ kind_ ) = sem_Error_IncompatibleAttachKind child_ kind_

-- semantic domain
newtype T_Error  = T_Error {
                           attach_T_Error :: Identity (T_Error_s2 )
                           }
newtype T_Error_s2  = C_Error_s2 {
                                 inv_Error_s2 :: (T_Error_v1 )
                                 }
data T_Error_s3  = C_Error_s3
type T_Error_v1  = (T_Error_vIn1 ) -> (T_Error_vOut1 )
data T_Error_vIn1  = T_Error_vIn1 (Options) (Bool)
data T_Error_vOut1  = T_Error_vOut1 (Error) (PP_Doc)
{-# NOINLINE sem_Error_ParserError #-}
sem_Error_ParserError :: (Pos) -> (String) -> (String) -> T_Error 
sem_Error_ParserError arg_pos_ arg_problem_ arg_action_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule0 _lhsIoptions _lhsIverbose _me arg_action_ arg_pos_ arg_problem_
         _me = rule1 arg_action_ arg_pos_ arg_problem_
         _lhsOme :: Error
         _lhsOme = rule2 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 78 "src-ag/PrintErrorMessages.ag" #-}
   rule0 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me action_ pos_ problem_ ->
                               {-# LINE 78 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg = text ("parser expecting " ++ problem_)
                                   pat  = text ""
                                   help = text ""
                                   act  = text action_
                                in ppError (isError _lhsIoptions _me) pos_ mesg pat help act _lhsIverbose
                               {-# LINE 264 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule1 #-}
   rule1 = \ action_ pos_ problem_ ->
     ParserError pos_ problem_ action_
   {-# INLINE rule2 #-}
   rule2 = \ _me ->
     _me
{-# NOINLINE sem_Error_HsParseError #-}
sem_Error_HsParseError :: (Pos) -> (String) -> T_Error 
sem_Error_HsParseError arg_pos_ arg_msg_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule3 _lhsIverbose arg_msg_ arg_pos_
         _me = rule4 arg_msg_ arg_pos_
         _lhsOme :: Error
         _lhsOme = rule5 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule3 #-}
   {-# LINE 84 "src-ag/PrintErrorMessages.ag" #-}
   rule3 = \ ((_lhsIverbose) :: Bool) msg_ pos_ ->
                               {-# LINE 84 "src-ag/PrintErrorMessages.ag" #-}
                               ppError True pos_ (text msg_) (text "") (text "") (text "Correct the syntax of the Haskell code.") _lhsIverbose
                               {-# LINE 291 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule4 #-}
   rule4 = \ msg_ pos_ ->
     HsParseError pos_ msg_
   {-# INLINE rule5 #-}
   rule5 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupAlt #-}
sem_Error_DupAlt :: (NontermIdent) -> (ConstructorIdent) -> (ConstructorIdent) -> T_Error 
sem_Error_DupAlt arg_nt_ arg_con_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule6 _lhsIoptions _lhsIverbose _me arg_con_ arg_nt_ arg_occ1_
         _me = rule7 arg_con_ arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule8 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule6 #-}
   {-# LINE 86 "src-ag/PrintErrorMessages.ag" #-}
   rule6 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ nt_ occ1_ ->
                               {-# LINE 86 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Repeated definition for alternative", getName con_
                                                 ,"of nonterminal", getName nt_, "."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Other definition:", (showPos con_),"."]
                                   pat =     "DATA" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< "...")
                                         >-< indent 2 ("|" >#< getName con_ >#< "...")
                                   help =  wfill ["The nonterminal",getName nt_,"has more than one alternative that"
                                                 ,"is labelled with the constructor name",getName con_,"."
                                                 ,"You should either rename or remove enough of them to make all"
                                                 ,"constructors of",getName nt_,"uniquely named."
                                                 ]
                                   act  = wfill [ "The first alternative of name",getName con_
                                                ,"you have given for nonterminal",getName nt_
                                                ,"is considered valid. All other alternatives have been discarded."
                                                ]
                               in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                               {-# LINE 335 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule7 #-}
   rule7 = \ con_ nt_ occ1_ ->
     DupAlt nt_ con_ occ1_
   {-# INLINE rule8 #-}
   rule8 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupSynonym #-}
sem_Error_DupSynonym :: (NontermIdent) -> (NontermIdent) -> T_Error 
sem_Error_DupSynonym arg_nt_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule9 _lhsIoptions _lhsIverbose _me arg_nt_ arg_occ1_
         _me = rule10 arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule11 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule9 #-}
   {-# LINE 108 "src-ag/PrintErrorMessages.ag" #-}
   rule9 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me nt_ occ1_ ->
                               {-# LINE 108 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Definition of type synonym", getName nt_, "clashes with another"
                                                 ,"type synonym."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Type synonym :"   , (showPos nt_),"."]
                                   pat =     "DATA" >#< getName nt_
                                         >-< indent 2 ("|" >#< "...")
                                         >-< "TYPE" >#< getName nt_ >#< "=" >#<  "..."
                                   help =  wfill ["A type synonym with name", getName  nt_
                                                 ,"has been given while there already is TYPE"
                                                 ,"definition with the same name."
                                                 ,"You should either rename or remove the type synonym."
                                                 ]
                                   act  = wfill [ "The clashing type synonym will be ignored."
                                                ]
                               in ppError (isError _lhsIoptions _me)  (getPos nt_) mesg pat help act _lhsIverbose
                               {-# LINE 377 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule10 #-}
   rule10 = \ nt_ occ1_ ->
     DupSynonym nt_ occ1_
   {-# INLINE rule11 #-}
   rule11 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupSet #-}
sem_Error_DupSet :: (NontermIdent) -> (NontermIdent) -> T_Error 
sem_Error_DupSet arg_name_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule12 _lhsIoptions _lhsIverbose _me arg_name_ arg_occ1_
         _me = rule13 arg_name_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule14 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule12 #-}
   {-# LINE 125 "src-ag/PrintErrorMessages.ag" #-}
   rule12 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me name_ occ1_ ->
                               {-# LINE 125 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Definition of nonterminal set", getName name_, "clashes with another"
                                                 ,"set, a type synonym or a data definition."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Set definition:"   , (showPos name_),"."]
                                   pat =     "SET" >#< getName name_ >#< "=" >#<  "..."
                                         >-< "SET" >#< getName name_ >#< "=" >#<  "..."
                                   help =  wfill ["A nonterminal set with name", getName  name_
                                                 ,"has been given while there already is a SET, DATA, or TYPE"
                                                 ,"definition with the same name."
                                                 ,"You should either rename or remove the nonterminal set."
                                                 ]
                                   act  = wfill [ "The clashing nonterminal set will be ignored."
                                                ]
                               in ppError (isError _lhsIoptions _me)  (getPos name_) mesg pat help act _lhsIverbose
                               {-# LINE 418 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule13 #-}
   rule13 = \ name_ occ1_ ->
     DupSet name_ occ1_
   {-# INLINE rule14 #-}
   rule14 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupInhAttr #-}
sem_Error_DupInhAttr :: (NontermIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_DupInhAttr arg_nt_ arg_attr_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule15 _lhsIoptions _lhsIverbose _me arg_attr_ arg_nt_ arg_occ1_
         _me = rule16 arg_attr_ arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule17 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule15 #-}
   {-# LINE 141 "src-ag/PrintErrorMessages.ag" #-}
   rule15 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ nt_ occ1_ ->
                               {-# LINE 141 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Repeated declaration of inherited attribute", getName attr_
                                                 , "of nonterminal", getName nt_, "."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Other definition:", (showPos attr_),"."]
                                   pat  = "ATTR" >#< getName nt_ >#< "[" >#< getName attr_ >|< ":...,"
                                                                 >#< getName attr_ >|< ":... | | ]"
                                   help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                                 ,"as an inherited (or chained) attribute for nonterminal"
                                                 ,getName nt_ , "more than once, with possibly different types."
                                                 ,"Delete all but one or rename them to make them unique."
                                                 ]
                                   act  = wfill ["One declaration with its corresponding type is considered valid."
                                                ,"All others have been discarded. The generated program will probably not run."
                                                ]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 460 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule16 #-}
   rule16 = \ attr_ nt_ occ1_ ->
     DupInhAttr nt_ attr_ occ1_
   {-# INLINE rule17 #-}
   rule17 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupSynAttr #-}
sem_Error_DupSynAttr :: (NontermIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_DupSynAttr arg_nt_ arg_attr_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule18 _lhsIoptions _lhsIverbose _me arg_attr_ arg_nt_ arg_occ1_
         _me = rule19 arg_attr_ arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule20 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule18 #-}
   {-# LINE 160 "src-ag/PrintErrorMessages.ag" #-}
   rule18 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ nt_ occ1_ ->
                               {-# LINE 160 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Repeated declaration of synthesized attribute", getName attr_
                                                 , "of nonterminal", getName nt_, "."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Other definition:", (showPos attr_),"."]
                                   pat  = "ATTR" >#< getName nt_ >#< "[ | |" >#< getName attr_ >|< ":...,"
                                                                     >#< getName attr_ >|< ":... ]"
                                   help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                                 ,"as a synthesized (or chained) attribute for nonterminal"
                                                 ,getName nt_ , "more than once, with possibly different types."
                                                 ,"Delete all but one or rename them to make them unique."
                                                 ]
                                   act  = wfill ["One declaration with its corresponding type is considered valid."
                                                ,"All others have been discarded. The generated program will probably not run."
                                                ]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 502 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule19 #-}
   rule19 = \ attr_ nt_ occ1_ ->
     DupSynAttr nt_ attr_ occ1_
   {-# INLINE rule20 #-}
   rule20 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupChild #-}
sem_Error_DupChild :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_DupChild arg_nt_ arg_con_ arg_name_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule21 _lhsIoptions _lhsIverbose _me arg_con_ arg_name_ arg_nt_ arg_occ1_
         _me = rule22 arg_con_ arg_name_ arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule23 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule21 #-}
   {-# LINE 179 "src-ag/PrintErrorMessages.ag" #-}
   rule21 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ name_ nt_ occ1_ ->
                               {-# LINE 179 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Repeated declaration for field", getName name_, "of alternative"
                                                 ,getName con_, "of nonterminal", getName nt_, "."
                                                 ] >-<
                                           wfill ["First definition:", (showPos occ1_),"."] >-<
                                           wfill ["Other definition:", (showPos name_),"."]
                                   pat   =   "DATA" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< (getName name_ >|< ":..." >-< getName name_ >|< ":..."))
                                   help =  wfill ["The alternative" ,getName con_ , "of nonterminal" ,getName nt_
                                                 ,"has more than one field that is named"
                                                 , getName name_ ++ ". Possibly they have different types."
                                                 ,"You should either rename or remove enough of them to make all fields of"
                                                 ,getName con_ , "for nonterminal " , getName nt_ , "uniquely named."
                                                 ]
                                   act  = wfill ["The last declaration with its corresponding type is considered valid."
                                                ,"All others have been discarded."
                                                ]
                               in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                               {-# LINE 545 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule22 #-}
   rule22 = \ con_ name_ nt_ occ1_ ->
     DupChild nt_ con_ name_ occ1_
   {-# INLINE rule23 #-}
   rule23 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupRule #-}
sem_Error_DupRule :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_DupRule arg_nt_ arg_con_ arg_field_ arg_attr_ arg_occ1_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule24 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_field_ arg_nt_ arg_occ1_
         _me = rule25 arg_attr_ arg_con_ arg_field_ arg_nt_ arg_occ1_
         _lhsOme :: Error
         _lhsOme = rule26 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule24 #-}
   {-# LINE 199 "src-ag/PrintErrorMessages.ag" #-}
   rule24 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ field_ nt_ occ1_ ->
                               {-# LINE 199 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more rules for"
                                                 ,showAttrDef field_ attr_,"."
                                                 ]  >-<
                                           wfill ["First rule:", (showPos occ1_),"."] >-<
                                           wfill ["Other rule:", (showPos attr_),"."]
                                   pat   =   "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                   help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                         ,", there is more than one rule for the" , showAttrDef field_ attr_
                                                         ,". You should either rename or remove enough of them to make all rules for alternative"
                                                         ,getName con_ , "of nonterminal " ,getName  nt_ , "uniquely named."
                                                         ]
                                   act  = wfill ["The last rule given is considered valid. All others have been discarded."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 586 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule25 #-}
   rule25 = \ attr_ con_ field_ nt_ occ1_ ->
     DupRule nt_ con_ field_ attr_ occ1_
   {-# INLINE rule26 #-}
   rule26 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupRuleName #-}
sem_Error_DupRuleName :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_DupRuleName arg_nt_ arg_con_ arg_nm_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule27 _lhsIoptions _lhsIverbose _me arg_con_ arg_nm_ arg_nt_
         _me = rule28 arg_con_ arg_nm_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule29 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule27 #-}
   {-# LINE 217 "src-ag/PrintErrorMessages.ag" #-}
   rule27 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ nm_ nt_ ->
                               {-# LINE 217 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more rule names for"
                                                 ,show nm_,"."
                                                 ]
                                   pat   =   "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< show nm_ >#< ": ... = ...")
                                         >-< indent 2 ("|" >#< getName con_ >#< show nm_ >#< ": ... = ...")
                                   help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                         ,", there is more than one rule name " , show nm_
                                                         ,". You should either rename or remove enough of them."
                                                         ]
                                   act  = wfill ["Compilation cannot continue."]
                               in ppError (isError _lhsIoptions _me) (getPos nm_) mesg pat help act _lhsIverbose
                               {-# LINE 624 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule28 #-}
   rule28 = \ con_ nm_ nt_ ->
     DupRuleName nt_ con_ nm_
   {-# INLINE rule29 #-}
   rule29 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupSig #-}
sem_Error_DupSig :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_DupSig arg_nt_ arg_con_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule30 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_
         _me = rule31 arg_attr_ arg_con_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule32 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule30 #-}
   {-# LINE 232 "src-ag/PrintErrorMessages.ag" #-}
   rule30 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ ->
                               {-# LINE 232 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more typesignatures for"
                                                 ,showAttrDef _LOC attr_,"."
                                                 ]  >-<
                                           wfill ["First signature:", (showPos attr_),"."]
                                   pat   =   "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                                   help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                         ,", there is more than one rule for the" , showAttrDef _LOC attr_
                                                         ,". You should remove enough of them to make all typesignatures for alternative"
                                                         ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                         ]
                                   act  = wfill ["The last typesignature given is considered valid. All others have been discarded."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 664 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule31 #-}
   rule31 = \ attr_ con_ nt_ ->
     DupSig nt_ con_ attr_
   {-# INLINE rule32 #-}
   rule32 = \ _me ->
     _me
{-# NOINLINE sem_Error_UndefNont #-}
sem_Error_UndefNont :: (NontermIdent) -> T_Error 
sem_Error_UndefNont arg_nt_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule33 _lhsIoptions _lhsIverbose _me arg_nt_
         _me = rule34 arg_nt_
         _lhsOme :: Error
         _lhsOme = rule35 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule33 #-}
   {-# LINE 249 "src-ag/PrintErrorMessages.ag" #-}
   rule33 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me nt_ ->
                               {-# LINE 249 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Nonterminal", getName nt_, "is not defined."
                                                 ]
                                   pat   = "DATA" >#< getName nt_ >#< "..."
                                   help =  wfill ["There are attributes and/or rules for nonterminal" , getName nt_  ,", but there is no definition"
                                                         , "for" ,getName  nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                         ]
                                   act  = wfill ["Everything regarding the unknown nonterminal has been ignored."]
                               in ppError (isError _lhsIoptions _me) (getPos nt_) mesg pat help act _lhsIverbose
                               {-# LINE 698 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule34 #-}
   rule34 = \ nt_ ->
     UndefNont nt_
   {-# INLINE rule35 #-}
   rule35 = \ _me ->
     _me
{-# NOINLINE sem_Error_UndefAlt #-}
sem_Error_UndefAlt :: (NontermIdent) -> (ConstructorIdent) -> T_Error 
sem_Error_UndefAlt arg_nt_ arg_con_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule36 _lhsIoptions _lhsIverbose _me arg_con_ arg_nt_
         _me = rule37 arg_con_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule38 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule36 #-}
   {-# LINE 259 "src-ag/PrintErrorMessages.ag" #-}
   rule36 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ nt_ ->
                               {-# LINE 259 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_, "is  not defined."
                                                 ]
                                   pat   =   "DATA" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< "...")
                                   help =  wfill ["There are rules for alternative", getName con_ , "of nonterminal" ,getName nt_
                                                         ,", but there is no definition for this alternative in the definitions of the"
                                                         ,"nonterminal" , getName nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                         ]
                                   act  = wfill ["All rules for the unknown alternative have been ignored."]
                               in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                               {-# LINE 734 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule37 #-}
   rule37 = \ con_ nt_ ->
     UndefAlt nt_ con_
   {-# INLINE rule38 #-}
   rule38 = \ _me ->
     _me
{-# NOINLINE sem_Error_UndefChild #-}
sem_Error_UndefChild :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_UndefChild arg_nt_ arg_con_ arg_name_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule39 _lhsIoptions _lhsIverbose _me arg_con_ arg_name_ arg_nt_
         _me = rule40 arg_con_ arg_name_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule41 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule39 #-}
   {-# LINE 271 "src-ag/PrintErrorMessages.ag" #-}
   rule39 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ name_ nt_ ->
                               {-# LINE 271 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_
                                                 , "does not have a nontrivial field named", getName name_ , "."
                                                 ]
                                   pat   =   "SEM" >#< nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr name_ (identifier "<attr>") >#< "= ...")
                                   help =  wfill ["There are rules that define or use attributes of field" , getName name_
                                                         ,"in alternative" , getName con_ , "of nonterminal" , getName nt_
                                                         ,", but there is no field with AG-type in the definition of the alternative."
                                                         ,"Maybe you misspelled it? Otherwise insert the field into the definition,"
                                                         ,"or change its type from an HS-type to an AG-type."
                                                         ]
                                   act  = wfill ["All rules for the unknown field have been ignored."]
                               in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                               {-# LINE 773 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule40 #-}
   rule40 = \ con_ name_ nt_ ->
     UndefChild nt_ con_ name_
   {-# INLINE rule41 #-}
   rule41 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingRule #-}
sem_Error_MissingRule :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_MissingRule arg_nt_ arg_con_ arg_field_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule42 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_field_ arg_nt_
         _me = rule43 arg_attr_ arg_con_ arg_field_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule44 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule42 #-}
   {-# LINE 286 "src-ag/PrintErrorMessages.ag" #-}
   rule42 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ field_ nt_ ->
                               {-# LINE 286 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Missing rule for", showAttrDef field_ attr_ , "in alternative"
                                                 , getName con_ , "of nonterminal",getName nt_ ,"."
                                                 ]
                                   pat   =   "SEM" >#< nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                   help  = wfill ["The", showAttrDef field_ attr_, "in alternative", getName con_
                                                 , "of nonterminal", getName nt_, "is missing and cannot be inferred"
                                                 ,"by a copy rule, so you should add an appropriate rule."
                                                 ]
                                   act  = wfill ["The value of the attribute has been set to undefined."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 810 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule43 #-}
   rule43 = \ attr_ con_ field_ nt_ ->
     MissingRule nt_ con_ field_ attr_
   {-# INLINE rule44 #-}
   rule44 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingNamedRule #-}
sem_Error_MissingNamedRule :: (NontermIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_MissingNamedRule arg_nt_ arg_con_ arg_name_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule45 _lhsIoptions _lhsIverbose _me arg_con_ arg_name_ arg_nt_
         _me = rule46 arg_con_ arg_name_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule47 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule45 #-}
   {-# LINE 299 "src-ag/PrintErrorMessages.ag" #-}
   rule45 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ name_ nt_ ->
                                  {-# LINE 299 "src-ag/PrintErrorMessages.ag" #-}
                                  let mesg  = wfill ["Missing rule name ", show name_ , "in alternative"
                                                    , getName con_ , "of nonterminal",getName nt_ ,"."
                                                    ]
                                      pat   =   "SEM" >#< nt_
                                            >-< indent 2 ("|" >#< getName con_ >#< show name_ >#< ": ... = ...")
                                      help  = wfill ["There is a dependency on a rule with name ", show name_ , "in alternative"
                                                    , getName con_ , "of nonterminal",getName nt_ ,", but no rule has been defined with this name. Maybe you misspelled it?"
                                                    ]
                                      act  = wfill ["Compilation cannot continue."]
                                  in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                                  {-# LINE 846 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule46 #-}
   rule46 = \ con_ name_ nt_ ->
     MissingNamedRule nt_ con_ name_
   {-# INLINE rule47 #-}
   rule47 = \ _me ->
     _me
{-# NOINLINE sem_Error_SuperfluousRule #-}
sem_Error_SuperfluousRule :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Identifier) -> T_Error 
sem_Error_SuperfluousRule arg_nt_ arg_con_ arg_field_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule48 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_field_ arg_nt_
         _me = rule49 arg_attr_ arg_con_ arg_field_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule50 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule48 #-}
   {-# LINE 311 "src-ag/PrintErrorMessages.ag" #-}
   rule48 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ field_ nt_ ->
                               {-# LINE 311 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Rule for non-existing", showAttrDef field_ attr_ , "at alternative"
                                                 , getName con_ , "of nonterminal",getName nt_, "."
                                                 ]
                                   pat   =   "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                   help =  wfill ["There is a rule for" , showAttrDef field_ attr_ , "in the definitions for alternative" , getName con_
                                                 ,"of nonterminal" , getName nt_,  ", but this attribute does not exist. Maybe you misspelled it?"
                                                 ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                                 ]
                                   act  = wfill ["The rule has been ignored."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 883 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule49 #-}
   rule49 = \ attr_ con_ field_ nt_ ->
     SuperfluousRule nt_ con_ field_ attr_
   {-# INLINE rule50 #-}
   rule50 = \ _me ->
     _me
{-# NOINLINE sem_Error_UndefLocal #-}
sem_Error_UndefLocal :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_UndefLocal arg_nt_ arg_con_ arg_var_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule51 _lhsIoptions _lhsIverbose _me arg_con_ arg_nt_ arg_var_
         _me = rule52 arg_con_ arg_nt_ arg_var_
         _lhsOme :: Error
         _lhsOme = rule53 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule51 #-}
   {-# LINE 325 "src-ag/PrintErrorMessages.ag" #-}
   rule51 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ nt_ var_ ->
                               {-# LINE 325 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Undefined local variable or field",getName var_, "at constructor"
                                                 , getName con_ , "of nonterminal",getName nt_, "."
                                                 ]
                                   pat   = "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                           >#< "..." >#< "@" >|< getName var_ >#< "..." )
                                   help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                                 , getName nt_ , "contains a local variable or field name that is not defined. "
                                                 ,"Maybe you misspelled it?"
                                                 ,"Otherwise either remove the rule or add an appropriate definition."
                                                 ]
                                   act  = wfill ["The generated program will not run."]
                               in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                               {-# LINE 922 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule52 #-}
   rule52 = \ con_ nt_ var_ ->
     UndefLocal nt_ con_ var_
   {-# INLINE rule53 #-}
   rule53 = \ _me ->
     _me
{-# NOINLINE sem_Error_ChildAsLocal #-}
sem_Error_ChildAsLocal :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_ChildAsLocal arg_nt_ arg_con_ arg_var_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule54 _lhsIoptions _lhsIverbose _me arg_con_ arg_nt_ arg_var_
         _me = rule55 arg_con_ arg_nt_ arg_var_
         _lhsOme :: Error
         _lhsOme = rule56 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule54 #-}
   {-# LINE 340 "src-ag/PrintErrorMessages.ag" #-}
   rule54 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me con_ nt_ var_ ->
                               {-# LINE 340 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Nontrivial field ",getName var_, "is used as local at constructor"
                                                 , getName con_ , "of nonterminal",getName nt_, "."
                                                 ]
                                   pat   = "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< "... = "
                                                           >#< "..." >#< "@" >|< getName var_ >#< "..." )
                                   help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                                 , getName nt_ , "contains a nontrivial field name", getName var_, "."
                                                 ,"You should use @", getName var_, ".self instead, where self is a SELF-attribute."
                                                 ]
                                   act  = wfill ["The generated program probably contains a type error or has undefined variables."]
                               in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                               {-# LINE 960 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule55 #-}
   rule55 = \ con_ nt_ var_ ->
     ChildAsLocal nt_ con_ var_
   {-# INLINE rule56 #-}
   rule56 = \ _me ->
     _me
{-# NOINLINE sem_Error_UndefAttr #-}
sem_Error_UndefAttr :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Identifier) -> (Bool) -> T_Error 
sem_Error_UndefAttr arg_nt_ arg_con_ arg_field_ arg_attr_ arg_isOut_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule57 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_field_ arg_isOut_ arg_nt_
         _me = rule58 arg_attr_ arg_con_ arg_field_ arg_isOut_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule59 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule57 #-}
   {-# LINE 354 "src-ag/PrintErrorMessages.ag" #-}
   rule57 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ field_ isOut_ nt_ ->
                               {-# LINE 354 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Undefined"
                                                 , if isOut_
                                                   then showAttrDef field_ attr_
                                                   else showAttrUse field_ attr_
                                                 , "at constructor"
                                                 , getName con_ , "of nonterminal",getName nt_, "."
                                                 ]
                                   pat   = "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                           >#< "..." >#< ppAttrUse field_ attr_ >#< "...")
                                   help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                                 ,getName  nt_ , "contains an attribute that is not defined"
                                                 ,"Maybe you misspelled it?"
                                                 ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                                 ]
                                   act  = wfill ["The generated program will not run."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 1003 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule58 #-}
   rule58 = \ attr_ con_ field_ isOut_ nt_ ->
     UndefAttr nt_ con_ field_ attr_ isOut_
   {-# INLINE rule59 #-}
   rule59 = \ _me ->
     _me
{-# NOINLINE sem_Error_Cyclic #-}
sem_Error_Cyclic :: (NontermIdent) -> (Maybe ConstructorIdent) -> ([String]) -> T_Error 
sem_Error_Cyclic arg_nt_ arg_mbCon_ arg_verts_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule60 _lhsIoptions _me arg_mbCon_ arg_nt_ arg_verts_
         _me = rule61 arg_mbCon_ arg_nt_ arg_verts_
         _lhsOme :: Error
         _lhsOme = rule62 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule60 #-}
   {-# LINE 382 "src-ag/PrintErrorMessages.ag" #-}
   rule60 = \ ((_lhsIoptions) :: Options) _me mbCon_ nt_ verts_ ->
                               {-# LINE 382 "src-ag/PrintErrorMessages.ag" #-}
                               let pos  = getPos nt_
                                   mesg = text "Circular dependency for nonterminal" >#< getName nt_
                                          >#< ( case mbCon_ of
                                                  Nothing  -> empty
                                                  Just con -> text "and constructor" >#< con
                                              )
                                          >#< ( case verts_ of
                                                    v : _ -> text "including vertex" >#< text v
                                                    _     -> empty
                                              )
                                   pat  = text "cyclic rule definition"
                                   help = hlist (text "The following attributes are all cyclic: " : map text verts_)
                                   act  = wfill ["code cannot be generated until the cycle is removed."]
                               in ppError (isError _lhsIoptions _me) pos mesg pat help act False
                               {-# LINE 1043 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule61 #-}
   rule61 = \ mbCon_ nt_ verts_ ->
     Cyclic nt_ mbCon_ verts_
   {-# INLINE rule62 #-}
   rule62 = \ _me ->
     _me
{-# NOINLINE sem_Error_CyclicSet #-}
sem_Error_CyclicSet :: (Identifier) -> T_Error 
sem_Error_CyclicSet arg_name_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule63 _lhsIoptions _lhsIverbose _me arg_name_
         _me = rule64 arg_name_
         _lhsOme :: Error
         _lhsOme = rule65 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule63 #-}
   {-# LINE 373 "src-ag/PrintErrorMessages.ag" #-}
   rule63 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me name_ ->
                               {-# LINE 373 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Cyclic definition for nonterminal set", getName name_]
                                   pat   = "SET" >#< getName name_ >#< "=" >#< "..." >#< getName name_ >#< "..."
                                   help =  wfill ["The defintion for a nonterminal set named" , getName name_
                                                 ,"directly or indirectly refers to itself."
                                                 ,"Adapt the definition of the nonterminal set, to remove the cyclic dependency."
                                                 ]
                                   act  = wfill ["The nonterminal set", getName name_, "is considered to be empty."]
                               in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                               {-# LINE 1077 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule64 #-}
   rule64 = \ name_ ->
     CyclicSet name_
   {-# INLINE rule65 #-}
   rule65 = \ _me ->
     _me
{-# NOINLINE sem_Error_CustomError #-}
sem_Error_CustomError :: (Bool) -> (Pos) -> (PP_Doc) -> T_Error 
sem_Error_CustomError arg_isWarning_ arg_pos_ arg_mesg_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule66 _lhsIoptions _me arg_mesg_ arg_pos_
         _me = rule67 arg_isWarning_ arg_mesg_ arg_pos_
         _lhsOme :: Error
         _lhsOme = rule68 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule66 #-}
   {-# LINE 397 "src-ag/PrintErrorMessages.ag" #-}
   rule66 = \ ((_lhsIoptions) :: Options) _me mesg_ pos_ ->
                               {-# LINE 397 "src-ag/PrintErrorMessages.ag" #-}
                               let pat   =  text "unknown"
                                   help = wfill ["not available."]
                                   act  = wfill ["unknown"]
                               in ppError (isError _lhsIoptions _me) pos_ mesg_ pat help act False
                               {-# LINE 1107 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule67 #-}
   rule67 = \ isWarning_ mesg_ pos_ ->
     CustomError isWarning_ pos_ mesg_
   {-# INLINE rule68 #-}
   rule68 = \ _me ->
     _me
{-# NOINLINE sem_Error_LocalCirc #-}
sem_Error_LocalCirc :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Bool) -> ([String]) -> T_Error 
sem_Error_LocalCirc arg_nt_ arg_con_ arg_attr_ arg_o_visit_ arg_path_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule69 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_ arg_o_visit_ arg_path_
         _me = rule70 arg_attr_ arg_con_ arg_nt_ arg_o_visit_ arg_path_
         _lhsOme :: Error
         _lhsOme = rule71 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule69 #-}
   {-# LINE 402 "src-ag/PrintErrorMessages.ag" #-}
   rule69 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ o_visit_ path_ ->
                               {-# LINE 402 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Circular dependency for local attribute", getName attr_
                                                 , "of alternative", getName con_, "of nonterminal", getName nt_]
                                   pat   = "SEM" >#< getName nt_
                                           >-< indent 2 ("|" >#< getName con_ >#< "loc." >|< getName attr_ >#< "="
                                                             >#< "..." >#< "@loc." >|< getName attr_ >#< "...")
                                   help  = if null path_
                                           then text "the definition is directly circular"
                                           else hlist ("The following attributes are involved in the cycle:": path_)
                                   act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                         | otherwise = text "The generated program might hang when run."
                               in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                               {-# LINE 1144 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule70 #-}
   rule70 = \ attr_ con_ nt_ o_visit_ path_ ->
     LocalCirc nt_ con_ attr_ o_visit_ path_
   {-# INLINE rule71 #-}
   rule71 = \ _me ->
     _me
{-# NOINLINE sem_Error_InstCirc #-}
sem_Error_InstCirc :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> (Bool) -> ([String]) -> T_Error 
sem_Error_InstCirc arg_nt_ arg_con_ arg_attr_ arg_o_visit_ arg_path_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule72 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_ arg_o_visit_ arg_path_
         _me = rule73 arg_attr_ arg_con_ arg_nt_ arg_o_visit_ arg_path_
         _lhsOme :: Error
         _lhsOme = rule74 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule72 #-}
   {-# LINE 414 "src-ag/PrintErrorMessages.ag" #-}
   rule72 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ o_visit_ path_ ->
                               {-# LINE 414 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Circular dependency for inst attribute", getName attr_
                                                 , "of alternative", getName con_, "of nonterminal", getName nt_]
                                   pat   = "SEM" >#< getName nt_
                                           >-< indent 2 ("|" >#< getName con_ >#< "inst." >|< getName attr_ >#< "="
                                                             >#< "..." >#< "@s.<some attribte>" >#< "...")
                                   help  = if null path_
                                           then text "the definition is directly circular"
                                           else hlist ("The following attributes are involved in the cycle:": path_)
                                   act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                         | otherwise = text "The generated program might hang when run."
                               in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                               {-# LINE 1181 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule73 #-}
   rule73 = \ attr_ con_ nt_ o_visit_ path_ ->
     InstCirc nt_ con_ attr_ o_visit_ path_
   {-# INLINE rule74 #-}
   rule74 = \ _me ->
     _me
{-# NOINLINE sem_Error_DirectCirc #-}
sem_Error_DirectCirc :: (NontermIdent) -> (Bool) -> ([((Identifier,Identifier),[String],[String])]) -> T_Error 
sem_Error_DirectCirc arg_nt_ arg_o_visit_ arg_cyclic_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule75 _lhsIoptions _lhsIverbose _me arg_cyclic_ arg_nt_ arg_o_visit_
         _me = rule76 arg_cyclic_ arg_nt_ arg_o_visit_
         _lhsOme :: Error
         _lhsOme = rule77 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule75 #-}
   {-# LINE 426 "src-ag/PrintErrorMessages.ag" #-}
   rule75 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me cyclic_ nt_ o_visit_ ->
                               {-# LINE 426 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["In nonterminal", getName nt_, "synthesized and inherited attributes are mutually dependent" ]
                                           >-< vlist (map showEdge cyclic_)
                                   pat   = text ""
                                   help  = vlist (map showEdgeLong cyclic_)
                                   act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                         | otherwise = text "The generated program might hang when run."
                               in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                               {-# LINE 1214 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule76 #-}
   rule76 = \ cyclic_ nt_ o_visit_ ->
     DirectCirc nt_ o_visit_ cyclic_
   {-# INLINE rule77 #-}
   rule77 = \ _me ->
     _me
{-# NOINLINE sem_Error_InducedCirc #-}
sem_Error_InducedCirc :: (NontermIdent) -> (CInterface) -> ([((Identifier,Identifier),[String],[String])]) -> T_Error 
sem_Error_InducedCirc arg_nt_ arg_cinter_ arg_cyclic_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule78 _lhsIoptions _lhsIverbose _me arg_cinter_ arg_cyclic_ arg_nt_
         _me = rule79 arg_cinter_ arg_cyclic_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule80 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule78 #-}
   {-# LINE 434 "src-ag/PrintErrorMessages.ag" #-}
   rule78 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me cinter_ cyclic_ nt_ ->
                               {-# LINE 434 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["After scheduling, in nonterminal", getName nt_, "synthesized and inherited attributes have an INDUCED mutual dependency" ]
                                           >-< vlist (map showEdge cyclic_)
                                   pat   = text ""
                                   showInter (CInterface segs) = concat (snd (mapAccumL (\i c -> (succ i :: Integer,("visit " ++ show i) : map ind (showsSegment c))) 0 segs))
                                   help  = vlist (("Interface for nonterminal " ++ getName nt_ ++ ":") : map ind (showInter cinter_))
                                           >-< vlist (map showEdgeLong cyclic_)
                                   act   = text "An unoptimized version was generated. It might hang when run."
                               in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                               {-# LINE 1248 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule79 #-}
   rule79 = \ cinter_ cyclic_ nt_ ->
     InducedCirc nt_ cinter_ cyclic_
   {-# INLINE rule80 #-}
   rule80 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingTypeSig #-}
sem_Error_MissingTypeSig :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_MissingTypeSig arg_nt_ arg_con_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule81 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_
         _me = rule82 arg_attr_ arg_con_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule83 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule81 #-}
   {-# LINE 443 "src-ag/PrintErrorMessages.ag" #-}
   rule81 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ ->
                               {-# LINE 443 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg = wfill ["Type signature needed, but not found for", showAttrDef _LOC attr_ , "in alternative"
                                                 , getName con_ , "of nonterminal",getName nt_ ,"."
                                                 ]>-<
                                           wfill ["Location:", (showPos attr_),"."]
                                   pat   =   "SEM" >#< nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< ": ...")
                                   help  = wfill ["The", showAttrDef _LOC attr_, "in alternative", getName con_
                                                 ,"of nonterminal", getName nt_, "is needed in two separate visits to", getName nt_
                                                 ,"so its type is needed to generate type signatures."
                                                 ,"Please supply its type."
                                                 ]
                                   act  = wfill ["The type signatures of semantic functions are not generated."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 1287 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule82 #-}
   rule82 = \ attr_ con_ nt_ ->
     MissingTypeSig nt_ con_ attr_
   {-# INLINE rule83 #-}
   rule83 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingInstSig #-}
sem_Error_MissingInstSig :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_MissingInstSig arg_nt_ arg_con_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule84 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_
         _me = rule85 arg_attr_ arg_con_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule86 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule84 #-}
   {-# LINE 457 "src-ag/PrintErrorMessages.ag" #-}
   rule84 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ ->
                               {-# LINE 457 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg = wfill ["Type signature needed, but not found for", showAttrDef _INST attr_ , "in alternative"
                                                 , getName con_ , "of nonterminal",getName nt_ ,"."
                                                 ]>-<
                                           wfill ["Location:", (showPos attr_),"."]
                                   pat   = "SEM" >#< nt_
                                             >-< indent 2 ("|" >#< getName con_ >#< ppAttr _INST attr_ >#< ": ...")
                                   help  = wfill ["The", showAttrDef _INST attr_, "in alternative", getName con_
                                                 ,"of nonterminal", getName nt_, "is a non-terminal attribute, so "
                                                 ,"its type is needed to attribute its value."
                                                 ,"Please supply its type."
                                                 ]
                                   act  = wfill ["It is not possible to proceed without this signature."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 1326 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule85 #-}
   rule85 = \ attr_ con_ nt_ ->
     MissingInstSig nt_ con_ attr_
   {-# INLINE rule86 #-}
   rule86 = \ _me ->
     _me
{-# NOINLINE sem_Error_DupUnique #-}
sem_Error_DupUnique :: (NontermIdent) -> (ConstructorIdent) -> (Identifier) -> T_Error 
sem_Error_DupUnique arg_nt_ arg_con_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule87 _lhsIoptions _lhsIverbose _me arg_attr_ arg_con_ arg_nt_
         _me = rule88 arg_attr_ arg_con_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule89 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule87 #-}
   {-# LINE 487 "src-ag/PrintErrorMessages.ag" #-}
   rule87 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ con_ nt_ ->
                               {-# LINE 487 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more unique-attribute signatures for"
                                                 ,showAttrDef _LOC attr_,"."
                                                 ]  >-<
                                           wfill ["First signature:", (showPos attr_),"."]
                                   pat   =   "SEM" >#< getName nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                                   help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                         ,", there is more than one unique-attribute signature for the" , showAttrDef _LOC attr_
                                                         ,". You should remove enough of them to make all unique-signatures for alternative"
                                                         ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                         ]
                                   act  = wfill ["Unpredicatable sharing of unique numbers may occur."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 1366 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule88 #-}
   rule88 = \ attr_ con_ nt_ ->
     DupUnique nt_ con_ attr_
   {-# INLINE rule89 #-}
   rule89 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingUnique #-}
sem_Error_MissingUnique :: (NontermIdent) -> (Identifier) -> T_Error 
sem_Error_MissingUnique arg_nt_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule90 _lhsIoptions _lhsIverbose _me arg_attr_ arg_nt_
         _me = rule91 arg_attr_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule92 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule90 #-}
   {-# LINE 471 "src-ag/PrintErrorMessages.ag" #-}
   rule90 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ nt_ ->
                               {-# LINE 471 "src-ag/PrintErrorMessages.ag" #-}
                               let mesg  = wfill ["Missing unique counter (chained attribute)"
                                                 , getName attr_
                                                 , "at nonterminal"
                                                 , getName nt_, "."
                                                 ]
                                   pat   = "ATTR" >#< getName nt_ >#< "[ |" >#< getName attr_ >#< " : ... | ]"
                                   help =  wfill ["A unique attribute signature in a constructor for nonterminal" , getName nt_
                                                 , "refers to an unique counter (chained attribute) named "
                                                 , getName attr_
                                                 ,"Maybe you misspelled it?"
                                                 ,"Otherwise either remove the signature or add an appropriate attribute definition."
                                                 ]
                                   act  = wfill ["It is not possible to proceed without this declaration."]
                               in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                               {-# LINE 1406 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule91 #-}
   rule91 = \ attr_ nt_ ->
     MissingUnique nt_ attr_
   {-# INLINE rule92 #-}
   rule92 = \ _me ->
     _me
{-# NOINLINE sem_Error_MissingSyn #-}
sem_Error_MissingSyn :: (NontermIdent) -> (Identifier) -> T_Error 
sem_Error_MissingSyn arg_nt_ arg_attr_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule93 _lhsIoptions _lhsIverbose _me arg_attr_ arg_nt_
         _me = rule94 arg_attr_ arg_nt_
         _lhsOme :: Error
         _lhsOme = rule95 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule93 #-}
   {-# LINE 504 "src-ag/PrintErrorMessages.ag" #-}
   rule93 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me attr_ nt_ ->
                              {-# LINE 504 "src-ag/PrintErrorMessages.ag" #-}
                              let mesg  = wfill ["Missing synthesized attribute"
                                                , getName attr_
                                                , "at nonterminal"
                                                , getName nt_, "."
                                                ]
                                  pat   = "ATTR" >#< getName nt_ >#< "[ | | " >#< getName attr_ >#< " : ... ]"
                                  help =  wfill ["An augment rule for a constructor for nonterminal" , getName nt_
                                                , "refers to a synthesized attribute named "
                                                , getName attr_
                                                ,"Maybe you misspelled it?"
                                                ,"Otherwise add an appropriate attribute definition."
                                                ]
                                  act  = wfill ["It is not possible to proceed without this declaration."]
                              in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                              {-# LINE 1446 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule94 #-}
   rule94 = \ attr_ nt_ ->
     MissingSyn nt_ attr_
   {-# INLINE rule95 #-}
   rule95 = \ _me ->
     _me
{-# NOINLINE sem_Error_IncompatibleVisitKind #-}
sem_Error_IncompatibleVisitKind :: (Identifier) -> (VisitIdentifier) -> (VisitKind) -> (VisitKind) -> T_Error 
sem_Error_IncompatibleVisitKind arg_child_ arg_vis_ arg_from_ arg_to_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule96 _lhsIoptions _lhsIverbose _me arg_child_ arg_from_ arg_to_ arg_vis_
         _me = rule97 arg_child_ arg_from_ arg_to_ arg_vis_
         _lhsOme :: Error
         _lhsOme = rule98 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule96 #-}
   {-# LINE 520 "src-ag/PrintErrorMessages.ag" #-}
   rule96 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me child_ from_ to_ vis_ ->
                              {-# LINE 520 "src-ag/PrintErrorMessages.ag" #-}
                              let mesg  = "visit" >#< vis_ >#< "of child" >#< child_ >#< " with kind" >#< show to_ >#< " cannot be called from a visit with kind " >#< show from_
                                  pat   = empty
                                  help  = empty
                                  act   = text "It is not possible to proceed without fixing this kind error."
                              in ppError (isError _lhsIoptions _me) (getPos child_) mesg pat help act _lhsIverbose
                              {-# LINE 1477 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule97 #-}
   rule97 = \ child_ from_ to_ vis_ ->
     IncompatibleVisitKind child_ vis_ from_ to_
   {-# INLINE rule98 #-}
   rule98 = \ _me ->
     _me
{-# NOINLINE sem_Error_IncompatibleRuleKind #-}
sem_Error_IncompatibleRuleKind :: (Identifier) -> (VisitKind) -> T_Error 
sem_Error_IncompatibleRuleKind arg_rule_ arg_kind_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule99 _lhsIoptions _lhsIverbose _me arg_kind_ arg_rule_
         _me = rule100 arg_kind_ arg_rule_
         _lhsOme :: Error
         _lhsOme = rule101 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule99 #-}
   {-# LINE 526 "src-ag/PrintErrorMessages.ag" #-}
   rule99 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me kind_ rule_ ->
                              {-# LINE 526 "src-ag/PrintErrorMessages.ag" #-}
                              let mesg  = "rule" >#< rule_ >#< "cannot be called from a visit with kind " >#< show kind_
                                  pat   = empty
                                  help  = empty
                                  act   = text "It is not possible to proceed without fixing this kind error."
                              in ppError (isError _lhsIoptions _me) (getPos rule_) mesg pat help act _lhsIverbose
                              {-# LINE 1508 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule100 #-}
   rule100 = \ kind_ rule_ ->
     IncompatibleRuleKind rule_ kind_
   {-# INLINE rule101 #-}
   rule101 = \ _me ->
     _me
{-# NOINLINE sem_Error_IncompatibleAttachKind #-}
sem_Error_IncompatibleAttachKind :: (Identifier) -> (VisitKind) -> T_Error 
sem_Error_IncompatibleAttachKind arg_child_ arg_kind_ = T_Error (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_Error_v1 
      v1 = \ (T_Error_vIn1 _lhsIoptions _lhsIverbose) -> ( let
         _lhsOpp :: PP_Doc
         _lhsOpp = rule102 _lhsIoptions _lhsIverbose _me arg_child_ arg_kind_
         _me = rule103 arg_child_ arg_kind_
         _lhsOme :: Error
         _lhsOme = rule104 _me
         __result_ = T_Error_vOut1 _lhsOme _lhsOpp
         in __result_ )
     in C_Error_s2 v1
   {-# INLINE rule102 #-}
   {-# LINE 533 "src-ag/PrintErrorMessages.ag" #-}
   rule102 = \ ((_lhsIoptions) :: Options) ((_lhsIverbose) :: Bool) _me child_ kind_ ->
                              {-# LINE 533 "src-ag/PrintErrorMessages.ag" #-}
                              let mesg  = "child" >#< child_ >#< "cannot be called from a visit with kind " >#< show kind_
                                  pat   = empty
                                  help  = empty
                                  act   = text "It is not possible to proceed without fixing this kind error."
                              in ppError (isError _lhsIoptions _me) (getPos child_) mesg pat help act _lhsIverbose
                              {-# LINE 1539 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule103 #-}
   rule103 = \ child_ kind_ ->
     IncompatibleAttachKind child_ kind_
   {-# INLINE rule104 #-}
   rule104 = \ _me ->
     _me

-- Errors ------------------------------------------------------
-- wrapper
data Inh_Errors  = Inh_Errors { dups_Inh_Errors :: ([String]), options_Inh_Errors :: (Options) }
data Syn_Errors  = Syn_Errors { pp_Syn_Errors :: (PP_Doc) }
{-# INLINABLE wrap_Errors #-}
wrap_Errors :: T_Errors  -> Inh_Errors  -> (Syn_Errors )
wrap_Errors (T_Errors act) (Inh_Errors _lhsIdups _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg4 = T_Errors_vIn4 _lhsIdups _lhsIoptions
        (T_Errors_vOut4 _lhsOpp) <- return (inv_Errors_s5 sem arg4)
        return (Syn_Errors _lhsOpp)
   )

-- cata
{-# NOINLINE sem_Errors #-}
sem_Errors :: Errors  -> T_Errors 
sem_Errors list = Prelude.foldr sem_Errors_Cons sem_Errors_Nil (Prelude.map sem_Error list)

-- semantic domain
newtype T_Errors  = T_Errors {
                             attach_T_Errors :: Identity (T_Errors_s5 )
                             }
newtype T_Errors_s5  = C_Errors_s5 {
                                   inv_Errors_s5 :: (T_Errors_v4 )
                                   }
data T_Errors_s6  = C_Errors_s6
type T_Errors_v4  = (T_Errors_vIn4 ) -> (T_Errors_vOut4 )
data T_Errors_vIn4  = T_Errors_vIn4 ([String]) (Options)
data T_Errors_vOut4  = T_Errors_vOut4 (PP_Doc)
{-# NOINLINE sem_Errors_Cons #-}
sem_Errors_Cons :: T_Error  -> T_Errors  -> T_Errors 
sem_Errors_Cons arg_hd_ arg_tl_ = T_Errors (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Errors_v4 
      v4 = \ (T_Errors_vIn4 _lhsIdups _lhsIoptions) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_Error (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_Errors (arg_tl_))
         (T_Error_vOut1 _hdIme _hdIpp) = inv_Error_s2 _hdX2 (T_Error_vIn1 _hdOoptions _hdOverbose)
         (T_Errors_vOut4 _tlIpp) = inv_Errors_s5 _tlX5 (T_Errors_vIn4 _tlOdups _tlOoptions)
         _verbose = rule105 _lhsIoptions
         _str = rule106 _hdIpp
         _lhsOpp :: PP_Doc
         _lhsOpp = rule107 _hdIpp _lhsIdups _str _tlIpp
         _tlOdups = rule108 _lhsIdups _str
         _hdOoptions = rule109 _lhsIoptions
         _hdOverbose = rule110 _verbose
         _tlOoptions = rule111 _lhsIoptions
         __result_ = T_Errors_vOut4 _lhsOpp
         in __result_ )
     in C_Errors_s5 v4
   {-# INLINE rule105 #-}
   {-# LINE 67 "src-ag/PrintErrorMessages.ag" #-}
   rule105 = \ ((_lhsIoptions) :: Options) ->
                       {-# LINE 67 "src-ag/PrintErrorMessages.ag" #-}
                       verbose _lhsIoptions
                       {-# LINE 1604 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule106 #-}
   {-# LINE 68 "src-ag/PrintErrorMessages.ag" #-}
   rule106 = \ ((_hdIpp) :: PP_Doc) ->
                      {-# LINE 68 "src-ag/PrintErrorMessages.ag" #-}
                      disp _hdIpp 5000 ""
                      {-# LINE 1610 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule107 #-}
   {-# LINE 70 "src-ag/PrintErrorMessages.ag" #-}
   rule107 = \ ((_hdIpp) :: PP_Doc) ((_lhsIdups) :: [String]) _str ((_tlIpp) :: PP_Doc) ->
                     {-# LINE 70 "src-ag/PrintErrorMessages.ag" #-}
                     if _str     `elem` _lhsIdups
                     then _tlIpp
                     else _hdIpp >-< _tlIpp
                     {-# LINE 1618 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule108 #-}
   {-# LINE 73 "src-ag/PrintErrorMessages.ag" #-}
   rule108 = \ ((_lhsIdups) :: [String]) _str ->
                      {-# LINE 73 "src-ag/PrintErrorMessages.ag" #-}
                      _str     : _lhsIdups
                      {-# LINE 1624 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule110 #-}
   rule110 = \ _verbose ->
     _verbose
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Errors_Nil #-}
sem_Errors_Nil ::  T_Errors 
sem_Errors_Nil  = T_Errors (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_Errors_v4 
      v4 = \ (T_Errors_vIn4 _lhsIdups _lhsIoptions) -> ( let
         _verbose = rule112 _lhsIoptions
         _lhsOpp :: PP_Doc
         _lhsOpp = rule113  ()
         __result_ = T_Errors_vOut4 _lhsOpp
         in __result_ )
     in C_Errors_s5 v4
   {-# INLINE rule112 #-}
   {-# LINE 67 "src-ag/PrintErrorMessages.ag" #-}
   rule112 = \ ((_lhsIoptions) :: Options) ->
                       {-# LINE 67 "src-ag/PrintErrorMessages.ag" #-}
                       verbose _lhsIoptions
                       {-# LINE 1652 "src-generated/PrintErrorMessages.hs"#-}
   {-# INLINE rule113 #-}
   {-# LINE 74 "src-ag/PrintErrorMessages.ag" #-}
   rule113 = \  (_ :: ()) ->
                     {-# LINE 74 "src-ag/PrintErrorMessages.ag" #-}
                     text ""
                     {-# LINE 1658 "src-generated/PrintErrorMessages.hs"#-}
