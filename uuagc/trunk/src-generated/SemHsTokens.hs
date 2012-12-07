{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemHsTokens where
{-# LINE 4 "./src-ag/SemHsTokens.ag" #-}

import qualified Data.Sequence as Seq
import Data.Sequence(Seq,empty,singleton,(><))
import Data.Foldable(toList)
import Pretty

import TokenDef
import HsToken
import ErrorMessages
{-# LINE 16 "dist/build/SemHsTokens.hs" #-}

{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 22 "dist/build/SemHsTokens.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 57 "./src-ag/SemHsTokens.ag" #-}

isNTname allnts (Just (NT nt _ _)) = nt `elem` allnts
isNTname allnts _                  = False
{-# LINE 29 "dist/build/SemHsTokens.hs" #-}
-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken { allfields_Inh_HsToken :: ([(Identifier,Type,ChildKind)]), allnts_Inh_HsToken :: ([Identifier]), attrs_Inh_HsToken :: ([(Identifier,Identifier)]), con_Inh_HsToken :: (Identifier), fieldnames_Inh_HsToken :: ([Identifier]), nt_Inh_HsToken :: (Identifier) }
data Syn_HsToken  = Syn_HsToken { errors_Syn_HsToken :: (Seq Error), output_Syn_HsToken :: (HsToken), tok_Syn_HsToken :: ((Pos,String)), usedAttrs_Syn_HsToken :: ([(Identifier,Identifier)]), usedFields_Syn_HsToken :: (Seq Identifier), usedLocals_Syn_HsToken :: ([Identifier]) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt
        (T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals) <- return (inv_HsToken_s2 sem arg)
        return (Syn_HsToken _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals)
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal var_ pos_ rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField field_ attr_ pos_ rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken value_ pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken value_ pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken value_ pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err mesg_ pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s2 )
                               }
newtype T_HsToken_s2  = C_HsToken_s2 {
                                     inv_HsToken_s2 :: (T_HsToken_v1 )
                                     }
data T_HsToken_s3  = C_HsToken_s3
type T_HsToken_v1  = (T_HsToken_vIn1 ) -> (T_HsToken_vOut1 )
data T_HsToken_vIn1  = T_HsToken_vIn1 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) ([Identifier]) (Identifier)
data T_HsToken_vOut1  = T_HsToken_vOut1 (Seq Error) (HsToken) ((Pos,String)) ([(Identifier,Identifier)]) (Seq Identifier) ([Identifier])
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ arg_pos_ arg_rdesc_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _tkAsLocal = rule0 arg_pos_ arg_rdesc_ arg_var_
         _tkAsField = rule1 arg_pos_ arg_rdesc_ arg_var_
         (_errors,_output,_tok,_usedLocals) = rule2 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt _tkAsField _tkAsLocal arg_pos_ arg_var_
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule3 _lhsIfieldnames arg_var_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule4 _errors
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule5  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule6 _usedLocals
         _lhsOoutput :: HsToken
         _lhsOoutput = rule7 _output
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule8 _tok
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 65 "./src-ag/SemHsTokens.ag" #-}
   rule0 = \ pos_ rdesc_ var_ ->
                                {-# LINE 65 "./src-ag/SemHsTokens.ag" #-}
                                AGLocal var_ pos_ rdesc_
                                {-# LINE 95 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule1 #-}
   {-# LINE 66 "./src-ag/SemHsTokens.ag" #-}
   rule1 = \ pos_ rdesc_ var_ ->
                                {-# LINE 66 "./src-ag/SemHsTokens.ag" #-}
                                AGField _LOC var_ pos_ rdesc_
                                {-# LINE 101 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule2 #-}
   {-# LINE 68 "./src-ag/SemHsTokens.ag" #-}
   rule2 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ((_lhsIallnts) :: [Identifier]) ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsIfieldnames) :: [Identifier]) ((_lhsInt) :: Identifier) _tkAsField _tkAsLocal pos_ var_ ->
                       {-# LINE 68 "./src-ag/SemHsTokens.ag" #-}
                       if var_ `elem` _lhsIfieldnames
                       then if  isNTname _lhsIallnts (lookup var_ (map (\(n,t,_) -> (n,t)) _lhsIallfields))
                            then (Seq.singleton(ChildAsLocal _lhsInt _lhsIcon var_), _tkAsLocal    ,(pos_,fieldname var_), []    )
                            else (Seq.empty, _tkAsLocal    , (pos_,fieldname var_), []    )
                       else if (_LOC,var_) `elem` _lhsIattrs
                            then (Seq.empty                                      , _tkAsField    , (pos_,locname   var_), [var_])
                            else (Seq.singleton(UndefLocal _lhsInt _lhsIcon var_), _tkAsField    , (pos_,locname   var_), []    )
                       {-# LINE 113 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule3 #-}
   {-# LINE 103 "./src-ag/SemHsTokens.ag" #-}
   rule3 = \ ((_lhsIfieldnames) :: [Identifier]) var_ ->
                               {-# LINE 103 "./src-ag/SemHsTokens.ag" #-}
                               if var_ `elem` _lhsIfieldnames
                                then Seq.singleton var_
                                else Seq.empty
                               {-# LINE 121 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule4 #-}
   rule4 = \ _errors ->
     _errors
   {-# INLINE rule5 #-}
   rule5 = \  (_ :: ()) ->
     []
   {-# INLINE rule6 #-}
   rule6 = \ _usedLocals ->
     _usedLocals
   {-# INLINE rule7 #-}
   rule7 = \ _output ->
     _output
   {-# INLINE rule8 #-}
   rule8 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule9 _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt arg_attr_ arg_field_
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedLocals :: [Identifier]
         (_lhsOusedAttrs,_lhsOusedLocals) = rule10 arg_attr_ arg_field_
         _addTrace = rule11 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule12 _addTrace arg_attr_ arg_field_ arg_pos_
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule13  ()
         _output = rule14 arg_attr_ arg_field_ arg_pos_ arg_rdesc_
         _lhsOoutput :: HsToken
         _lhsOoutput = rule15 _output
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule9 #-}
   {-# LINE 77 "./src-ag/SemHsTokens.ag" #-}
   rule9 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ((_lhsIcon) :: Identifier) ((_lhsIfieldnames) :: [Identifier]) ((_lhsInt) :: Identifier) attr_ field_ ->
                             {-# LINE 77 "./src-ag/SemHsTokens.ag" #-}
                             if (field_,attr_) `elem` _lhsIattrs
                             then Seq.empty
                             else if not(field_ `elem` (_LHS : _LOC: _lhsIfieldnames))
                                  then Seq.singleton (UndefChild _lhsInt _lhsIcon field_)
                                  else Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ attr_ False)
                             {-# LINE 169 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule10 #-}
   {-# LINE 92 "./src-ag/SemHsTokens.ag" #-}
   rule10 = \ attr_ field_ ->
                  {-# LINE 92 "./src-ag/SemHsTokens.ag" #-}
                  if field_ == _LOC
                  then ([], [attr_])
                  else ([(field_,attr_)], [])
                  {-# LINE 177 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule11 #-}
   {-# LINE 122 "./src-ag/SemHsTokens.ag" #-}
   rule11 = \ attr_ field_ rdesc_ ->
                        {-# LINE 122 "./src-ag/SemHsTokens.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                          Nothing -> id
                        {-# LINE 185 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule12 #-}
   {-# LINE 125 "./src-ag/SemHsTokens.ag" #-}
   rule12 = \ _addTrace attr_ field_ pos_ ->
                   {-# LINE 125 "./src-ag/SemHsTokens.ag" #-}
                   (pos_, _addTrace     $ attrname True field_ attr_)
                   {-# LINE 191 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule13 #-}
   rule13 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule14 #-}
   rule14 = \ attr_ field_ pos_ rdesc_ ->
     AGField field_ attr_ pos_ rdesc_
   {-# INLINE rule15 #-}
   rule15 = \ _output ->
     _output
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule16 arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule17  ()
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule18  ()
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule19  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule20  ()
         _output = rule21 arg_pos_ arg_value_
         _lhsOoutput :: HsToken
         _lhsOoutput = rule22 _output
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule16 #-}
   {-# LINE 127 "./src-ag/SemHsTokens.ag" #-}
   rule16 = \ pos_ value_ ->
                         {-# LINE 127 "./src-ag/SemHsTokens.ag" #-}
                         (pos_, value_)
                         {-# LINE 229 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule17 #-}
   rule17 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule18 #-}
   rule18 = \  (_ :: ()) ->
     []
   {-# INLINE rule19 #-}
   rule19 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule20 #-}
   rule20 = \  (_ :: ()) ->
     []
   {-# INLINE rule21 #-}
   rule21 = \ pos_ value_ ->
     HsToken value_ pos_
   {-# INLINE rule22 #-}
   rule22 = \ _output ->
     _output
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule23 arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule24  ()
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule25  ()
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule26  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule27  ()
         _output = rule28 arg_pos_ arg_value_
         _lhsOoutput :: HsToken
         _lhsOoutput = rule29 _output
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule23 #-}
   {-# LINE 129 "./src-ag/SemHsTokens.ag" #-}
   rule23 = \ pos_ value_ ->
                           {-# LINE 129 "./src-ag/SemHsTokens.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 279 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule24 #-}
   rule24 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule25 #-}
   rule25 = \  (_ :: ()) ->
     []
   {-# INLINE rule26 #-}
   rule26 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule27 #-}
   rule27 = \  (_ :: ()) ->
     []
   {-# INLINE rule28 #-}
   rule28 = \ pos_ value_ ->
     CharToken value_ pos_
   {-# INLINE rule29 #-}
   rule29 = \ _output ->
     _output
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule30 arg_pos_ arg_value_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule31  ()
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule32  ()
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule33  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule34  ()
         _output = rule35 arg_pos_ arg_value_
         _lhsOoutput :: HsToken
         _lhsOoutput = rule36 _output
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule30 #-}
   {-# LINE 134 "./src-ag/SemHsTokens.ag" #-}
   rule30 = \ pos_ value_ ->
                           {-# LINE 134 "./src-ag/SemHsTokens.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 326 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule31 #-}
   rule31 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule32 #-}
   rule32 = \  (_ :: ()) ->
     []
   {-# INLINE rule33 #-}
   rule33 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule34 #-}
   rule34 = \  (_ :: ()) ->
     []
   {-# INLINE rule35 #-}
   rule35 = \ pos_ value_ ->
     StrToken value_ pos_
   {-# INLINE rule36 #-}
   rule36 = \ _output ->
     _output
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err arg_mesg_ arg_pos_ = T_HsToken (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_HsToken_v1 
      v1 = \ (T_HsToken_vIn1 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule37 arg_mesg_ arg_pos_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule38 arg_pos_
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule39  ()
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule40  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule41  ()
         _output = rule42 arg_mesg_ arg_pos_
         _lhsOoutput :: HsToken
         _lhsOoutput = rule43 _output
         __result_ = T_HsToken_vOut1 _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsToken_s2 v1
   {-# INLINE rule37 #-}
   {-# LINE 50 "./src-ag/SemHsTokens.ag" #-}
   rule37 = \ mesg_ pos_ ->
                       {-# LINE 50 "./src-ag/SemHsTokens.ag" #-}
                       let m = text mesg_
                       in Seq.singleton (CustomError False pos_ m)
                       {-# LINE 374 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule38 #-}
   {-# LINE 135 "./src-ag/SemHsTokens.ag" #-}
   rule38 = \ pos_ ->
                           {-# LINE 135 "./src-ag/SemHsTokens.ag" #-}
                           (pos_, "")
                           {-# LINE 380 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule39 #-}
   rule39 = \  (_ :: ()) ->
     []
   {-# INLINE rule40 #-}
   rule40 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule41 #-}
   rule41 = \  (_ :: ()) ->
     []
   {-# INLINE rule42 #-}
   rule42 = \ mesg_ pos_ ->
     Err mesg_ pos_
   {-# INLINE rule43 #-}
   rule43 = \ _output ->
     _output

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens { allfields_Inh_HsTokens :: ([(Identifier,Type,ChildKind)]), allnts_Inh_HsTokens :: ([Identifier]), attrs_Inh_HsTokens :: ([(Identifier,Identifier)]), con_Inh_HsTokens :: (Identifier), fieldnames_Inh_HsTokens :: ([Identifier]), nt_Inh_HsTokens :: (Identifier) }
data Syn_HsTokens  = Syn_HsTokens { errors_Syn_HsTokens :: (Seq Error), output_Syn_HsTokens :: (HsTokens), tks_Syn_HsTokens :: ([(Pos,String)]), usedAttrs_Syn_HsTokens :: ([(Identifier,Identifier)]), usedFields_Syn_HsTokens :: (Seq Identifier), usedLocals_Syn_HsTokens :: ([Identifier]) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokens_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt
        (T_HsTokens_vOut4 _lhsOerrors _lhsOoutput _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals) <- return (inv_HsTokens_s5 sem arg)
        return (Syn_HsTokens _lhsOerrors _lhsOoutput _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals)
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s5 )
                                 }
newtype T_HsTokens_s5  = C_HsTokens_s5 {
                                       inv_HsTokens_s5 :: (T_HsTokens_v4 )
                                       }
data T_HsTokens_s6  = C_HsTokens_s6
type T_HsTokens_v4  = (T_HsTokens_vIn4 ) -> (T_HsTokens_vOut4 )
data T_HsTokens_vIn4  = T_HsTokens_vIn4 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) ([Identifier]) (Identifier)
data T_HsTokens_vOut4  = T_HsTokens_vOut4 (Seq Error) (HsTokens) ([(Pos,String)]) ([(Identifier,Identifier)]) (Seq Identifier) ([Identifier])
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_HsTokens_v4 
      v4 = \ (T_HsTokens_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut1 _hdIerrors _hdIoutput _hdItok _hdIusedAttrs _hdIusedFields _hdIusedLocals) = inv_HsToken_s2 _hdX2 (T_HsToken_vIn1 _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOfieldnames _hdOnt)
         (T_HsTokens_vOut4 _tlIerrors _tlIoutput _tlItks _tlIusedAttrs _tlIusedFields _tlIusedLocals) = inv_HsTokens_s5 _tlX5 (T_HsTokens_vIn4 _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOfieldnames _tlOnt)
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule44 _hdItok _tlItks
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule45 _hdIerrors _tlIerrors
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule46 _hdIusedAttrs _tlIusedAttrs
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule47 _hdIusedFields _tlIusedFields
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule48 _hdIusedLocals _tlIusedLocals
         _output = rule49 _hdIoutput _tlIoutput
         _lhsOoutput :: HsTokens
         _lhsOoutput = rule50 _output
         _hdOallfields = rule51 _lhsIallfields
         _hdOallnts = rule52 _lhsIallnts
         _hdOattrs = rule53 _lhsIattrs
         _hdOcon = rule54 _lhsIcon
         _hdOfieldnames = rule55 _lhsIfieldnames
         _hdOnt = rule56 _lhsInt
         _tlOallfields = rule57 _lhsIallfields
         _tlOallnts = rule58 _lhsIallnts
         _tlOattrs = rule59 _lhsIattrs
         _tlOcon = rule60 _lhsIcon
         _tlOfieldnames = rule61 _lhsIfieldnames
         _tlOnt = rule62 _lhsInt
         __result_ = T_HsTokens_vOut4 _lhsOerrors _lhsOoutput _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsTokens_s5 v4
   {-# INLINE rule44 #-}
   {-# LINE 117 "./src-ag/SemHsTokens.ag" #-}
   rule44 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 117 "./src-ag/SemHsTokens.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 471 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule45 #-}
   rule45 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule46 #-}
   rule46 = \ ((_hdIusedAttrs) :: [(Identifier,Identifier)]) ((_tlIusedAttrs) :: [(Identifier,Identifier)]) ->
     _hdIusedAttrs ++ _tlIusedAttrs
   {-# INLINE rule47 #-}
   rule47 = \ ((_hdIusedFields) :: Seq Identifier) ((_tlIusedFields) :: Seq Identifier) ->
     _hdIusedFields Seq.>< _tlIusedFields
   {-# INLINE rule48 #-}
   rule48 = \ ((_hdIusedLocals) :: [Identifier]) ((_tlIusedLocals) :: [Identifier]) ->
     _hdIusedLocals ++ _tlIusedLocals
   {-# INLINE rule49 #-}
   rule49 = \ ((_hdIoutput) :: HsToken) ((_tlIoutput) :: HsTokens) ->
     (:) _hdIoutput _tlIoutput
   {-# INLINE rule50 #-}
   rule50 = \ _output ->
     _output
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIfieldnames) :: [Identifier]) ->
     _lhsIfieldnames
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule60 #-}
   rule60 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule61 #-}
   rule61 = \ ((_lhsIfieldnames) :: [Identifier]) ->
     _lhsIfieldnames
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_HsTokens_v4 
      v4 = \ (T_HsTokens_vIn4 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule63  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule64  ()
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule65  ()
         _lhsOusedFields :: Seq Identifier
         _lhsOusedFields = rule66  ()
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule67  ()
         _output = rule68  ()
         _lhsOoutput :: HsTokens
         _lhsOoutput = rule69 _output
         __result_ = T_HsTokens_vOut4 _lhsOerrors _lhsOoutput _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsTokens_s5 v4
   {-# INLINE rule63 #-}
   {-# LINE 118 "./src-ag/SemHsTokens.ag" #-}
   rule63 = \  (_ :: ()) ->
                     {-# LINE 118 "./src-ag/SemHsTokens.ag" #-}
                     []
                     {-# LINE 554 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule64 #-}
   rule64 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule65 #-}
   rule65 = \  (_ :: ()) ->
     []
   {-# INLINE rule66 #-}
   rule66 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule67 #-}
   rule67 = \  (_ :: ()) ->
     []
   {-# INLINE rule68 #-}
   rule68 = \  (_ :: ()) ->
     []
   {-# INLINE rule69 #-}
   rule69 = \ _output ->
     _output

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot { allfields_Inh_HsTokensRoot :: ([(Identifier,Type,ChildKind)]), allnts_Inh_HsTokensRoot :: ([Identifier]), attrs_Inh_HsTokensRoot :: ([(Identifier,Identifier)]), con_Inh_HsTokensRoot :: (Identifier), nt_Inh_HsTokensRoot :: (Identifier) }
data Syn_HsTokensRoot  = Syn_HsTokensRoot { errors_Syn_HsTokensRoot :: (Seq Error), output_Syn_HsTokensRoot :: ([HsToken]), textLines_Syn_HsTokensRoot :: ([String]), usedAttrs_Syn_HsTokensRoot :: ([(Identifier,Identifier)]), usedFields_Syn_HsTokensRoot :: ([Identifier]), usedLocals_Syn_HsTokensRoot :: ([Identifier]) }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg = T_HsTokensRoot_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt
        (T_HsTokensRoot_vOut7 _lhsOerrors _lhsOoutput _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals) <- return (inv_HsTokensRoot_s8 sem arg)
        return (Syn_HsTokensRoot _lhsOerrors _lhsOoutput _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals)
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s8 )
                                         }
newtype T_HsTokensRoot_s8  = C_HsTokensRoot_s8 {
                                               inv_HsTokensRoot_s8 :: (T_HsTokensRoot_v7 )
                                               }
data T_HsTokensRoot_s9  = C_HsTokensRoot_s9
type T_HsTokensRoot_v7  = (T_HsTokensRoot_vIn7 ) -> (T_HsTokensRoot_vOut7 )
data T_HsTokensRoot_vIn7  = T_HsTokensRoot_vIn7 ([(Identifier,Type,ChildKind)]) ([Identifier]) ([(Identifier,Identifier)]) (Identifier) (Identifier)
data T_HsTokensRoot_vOut7  = T_HsTokensRoot_vOut7 (Seq Error) ([HsToken]) ([String]) ([(Identifier,Identifier)]) ([Identifier]) ([Identifier])
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_HsTokensRoot_v7 
      v7 = \ (T_HsTokensRoot_vIn7 _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt) -> ( let
         _tokensX5 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut4 _tokensIerrors _tokensIoutput _tokensItks _tokensIusedAttrs _tokensIusedFields _tokensIusedLocals) = inv_HsTokens_s5 _tokensX5 (T_HsTokens_vIn4 _tokensOallfields _tokensOallnts _tokensOattrs _tokensOcon _tokensOfieldnames _tokensOnt)
         _tokensOfieldnames = rule70 _lhsIallfields
         _lhsOusedFields :: [Identifier]
         _lhsOusedFields = rule71 _tokensIusedFields
         _lhsOtextLines :: [String]
         _lhsOtextLines = rule72 _tokensItks
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule73 _tokensIerrors
         _lhsOoutput :: [HsToken]
         _lhsOoutput = rule74 _tokensIoutput
         _lhsOusedAttrs :: [(Identifier,Identifier)]
         _lhsOusedAttrs = rule75 _tokensIusedAttrs
         _lhsOusedLocals :: [Identifier]
         _lhsOusedLocals = rule76 _tokensIusedLocals
         _tokensOallfields = rule77 _lhsIallfields
         _tokensOallnts = rule78 _lhsIallnts
         _tokensOattrs = rule79 _lhsIattrs
         _tokensOcon = rule80 _lhsIcon
         _tokensOnt = rule81 _lhsInt
         __result_ = T_HsTokensRoot_vOut7 _lhsOerrors _lhsOoutput _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals
         in __result_ )
     in C_HsTokensRoot_s8 v7
   {-# INLINE rule70 #-}
   {-# LINE 38 "./src-ag/SemHsTokens.ag" #-}
   rule70 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
                                       {-# LINE 38 "./src-ag/SemHsTokens.ag" #-}
                                       map (\(n,_,_) -> n) _lhsIallfields
                                       {-# LINE 639 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule71 #-}
   {-# LINE 107 "./src-ag/SemHsTokens.ag" #-}
   rule71 = \ ((_tokensIusedFields) :: Seq Identifier) ->
                                    {-# LINE 107 "./src-ag/SemHsTokens.ag" #-}
                                    toList _tokensIusedFields
                                    {-# LINE 645 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule72 #-}
   {-# LINE 114 "./src-ag/SemHsTokens.ag" #-}
   rule72 = \ ((_tokensItks) :: [(Pos,String)]) ->
                                   {-# LINE 114 "./src-ag/SemHsTokens.ag" #-}
                                   showTokens _tokensItks
                                   {-# LINE 651 "dist/build/SemHsTokens.hs"#-}
   {-# INLINE rule73 #-}
   rule73 = \ ((_tokensIerrors) :: Seq Error) ->
     _tokensIerrors
   {-# INLINE rule74 #-}
   rule74 = \ ((_tokensIoutput) :: HsTokens) ->
     _tokensIoutput
   {-# INLINE rule75 #-}
   rule75 = \ ((_tokensIusedAttrs) :: [(Identifier,Identifier)]) ->
     _tokensIusedAttrs
   {-# INLINE rule76 #-}
   rule76 = \ ((_tokensIusedLocals) :: [Identifier]) ->
     _tokensIusedLocals
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsIallfields) :: [(Identifier,Type,ChildKind)]) ->
     _lhsIallfields
   {-# INLINE rule78 #-}
   rule78 = \ ((_lhsIallnts) :: [Identifier]) ->
     _lhsIallnts
   {-# INLINE rule79 #-}
   rule79 = \ ((_lhsIattrs) :: [(Identifier,Identifier)]) ->
     _lhsIattrs
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsIcon) :: Identifier) ->
     _lhsIcon
   {-# INLINE rule81 #-}
   rule81 = \ ((_lhsInt) :: Identifier) ->
     _lhsInt
