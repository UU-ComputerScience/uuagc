

-- UUAGC 0.9.42.1 (src-ag/SemHsTokens.ag)
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
{-# LINE 57 "./src-ag/SemHsTokens.ag" #-}

isNTname allnts (Just (NT nt _ _)) = nt `elem` allnts
isNTname allnts _                  = False
{-# LINE 27 "dist/build/SemHsTokens.hs" #-}
-- HsToken -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         fieldnames           : [Identifier]
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         output               : HsToken 
         tok                  : (Pos,String)
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : Seq Identifier
         usedLocals           : [Identifier]
   alternatives:
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local tkAsLocal   : _
            local tkAsField   : _
            local errors      : _
            local output      : _
            local tok         : _
            local usedLocals  : _
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local addTrace    : _
            local output      : _
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local output      : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local output      : _
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
         visit 0:
            local output      : _
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
         visit 0:
            local output      : _
-}
-- cata
sem_HsToken :: HsToken ->
               T_HsToken
sem_HsToken (AGLocal _var _pos _rdesc) =
    (sem_HsToken_AGLocal _var _pos _rdesc)
sem_HsToken (AGField _field _attr _pos _rdesc) =
    (sem_HsToken_AGField _field _attr _pos _rdesc)
sem_HsToken (HsToken _value _pos) =
    (sem_HsToken_HsToken _value _pos)
sem_HsToken (CharToken _value _pos) =
    (sem_HsToken_CharToken _value _pos)
sem_HsToken (StrToken _value _pos) =
    (sem_HsToken_StrToken _value _pos)
sem_HsToken (Err _mesg _pos) =
    (sem_HsToken_Err _mesg _pos)
-- semantic domain
newtype T_HsToken = T_HsToken (([(Identifier,Type,ChildKind)]) ->
                               ([Identifier]) ->
                               ([(Identifier,Identifier)]) ->
                               Identifier ->
                               ([Identifier]) ->
                               Identifier ->
                               ( (Seq Error),HsToken,((Pos,String)),([(Identifier,Identifier)]),(Seq Identifier),([Identifier])))
data Inh_HsToken = Inh_HsToken {allfields_Inh_HsToken :: ([(Identifier,Type,ChildKind)]),allnts_Inh_HsToken :: ([Identifier]),attrs_Inh_HsToken :: ([(Identifier,Identifier)]),con_Inh_HsToken :: Identifier,fieldnames_Inh_HsToken :: ([Identifier]),nt_Inh_HsToken :: Identifier}
data Syn_HsToken = Syn_HsToken {errors_Syn_HsToken :: (Seq Error),output_Syn_HsToken :: HsToken,tok_Syn_HsToken :: ((Pos,String)),usedAttrs_Syn_HsToken :: ([(Identifier,Identifier)]),usedFields_Syn_HsToken :: (Seq Identifier),usedLocals_Syn_HsToken :: ([Identifier])}
wrap_HsToken :: T_HsToken ->
                Inh_HsToken ->
                Syn_HsToken
wrap_HsToken (T_HsToken sem) (Inh_HsToken _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) =
    (let ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt
     in  (Syn_HsToken _lhsOerrors _lhsOoutput _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals))
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGLocal var_ pos_ rdesc_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOusedFields :: (Seq Identifier)
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOoutput :: HsToken
                         _lhsOtok :: ((Pos,String))
                         -- "./src-ag/SemHsTokens.ag"(line 65, column 15)
                         _tkAsLocal =
                             ({-# LINE 65 "./src-ag/SemHsTokens.ag" #-}
                              AGLocal var_ pos_ rdesc_
                              {-# LINE 138 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 66, column 15)
                         _tkAsField =
                             ({-# LINE 66 "./src-ag/SemHsTokens.ag" #-}
                              AGField _LOC var_ pos_ rdesc_
                              {-# LINE 144 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 67, column 19)
                         (_errors,_output,_tok,_usedLocals) =
                             ({-# LINE 67 "./src-ag/SemHsTokens.ag" #-}
                              if var_ `elem` _lhsIfieldnames
                              then if  isNTname _lhsIallnts (lookup var_ (map (\(n,t,_) -> (n,t)) _lhsIallfields))
                                   then (Seq.singleton(ChildAsLocal _lhsInt _lhsIcon var_), _tkAsLocal    ,(pos_,fieldname var_), []    )
                                   else (Seq.empty, _tkAsLocal    , (pos_,fieldname var_), []    )
                              else if (_LOC,var_) `elem` _lhsIattrs
                                   then (Seq.empty                                      , _tkAsField    , (pos_,locname   var_), [var_])
                                   else (Seq.singleton(UndefLocal _lhsInt _lhsIcon var_), _tkAsField    , (pos_,locname   var_), []    )
                              {-# LINE 156 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 103, column 13)
                         _lhsOusedFields =
                             ({-# LINE 103 "./src-ag/SemHsTokens.ag" #-}
                              if var_ `elem` _lhsIfieldnames
                               then Seq.singleton var_
                               else Seq.empty
                              {-# LINE 164 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                              _errors
                              {-# LINE 170 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                         _lhsOusedAttrs =
                             ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 176 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                         _lhsOusedLocals =
                             ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                              _usedLocals
                              {-# LINE 182 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 188 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- copy rule (from local)
                         _lhsOtok =
                             ({-# LINE 120 "./src-ag/SemHsTokens.ag" #-}
                              _tok
                              {-# LINE 194 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken
sem_HsToken_AGField field_ attr_ pos_ rdesc_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOtok :: ((Pos,String))
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOoutput :: HsToken
                         -- "./src-ag/SemHsTokens.ag"(line 77, column 15)
                         _lhsOerrors =
                             ({-# LINE 77 "./src-ag/SemHsTokens.ag" #-}
                              if (field_,attr_) `elem` _lhsIattrs
                              then Seq.empty
                              else if not(field_ `elem` (_LHS : _LOC: _lhsIfieldnames))
                                   then Seq.singleton (UndefChild _lhsInt _lhsIcon field_)
                                   else Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ attr_ False)
                              {-# LINE 223 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 91, column 13)
                         (_lhsOusedAttrs,_lhsOusedLocals) =
                             ({-# LINE 91 "./src-ag/SemHsTokens.ag" #-}
                              if field_ == _LOC
                              then ([], [attr_])
                              else ([(field_,attr_)], [])
                              {-# LINE 231 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 122, column 8)
                         _addTrace =
                             ({-# LINE 122 "./src-ag/SemHsTokens.ag" #-}
                              case rdesc_ of
                                Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                                Nothing -> id
                              {-# LINE 239 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 125, column 8)
                         _lhsOtok =
                             ({-# LINE 125 "./src-ag/SemHsTokens.ag" #-}
                              (pos_, _addTrace     $ attrname True field_ attr_)
                              {-# LINE 245 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                         _lhsOusedFields =
                             ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 251 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              AGField field_ attr_ pos_ rdesc_
                              {-# LINE 257 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 263 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken
sem_HsToken_HsToken value_ pos_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOoutput :: HsToken
                         -- "./src-ag/SemHsTokens.ag"(line 127, column 14)
                         _lhsOtok =
                             ({-# LINE 127 "./src-ag/SemHsTokens.ag" #-}
                              (pos_, value_)
                              {-# LINE 286 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 292 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                         _lhsOusedAttrs =
                             ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 298 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                         _lhsOusedFields =
                             ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 304 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                         _lhsOusedLocals =
                             ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 310 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              HsToken value_ pos_
                              {-# LINE 316 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 322 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken
sem_HsToken_CharToken value_ pos_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOoutput :: HsToken
                         -- "./src-ag/SemHsTokens.ag"(line 129, column 16)
                         _lhsOtok =
                             ({-# LINE 129 "./src-ag/SemHsTokens.ag" #-}
                              (pos_, if null value_
                                        then ""
                                        else showCharShort (head value_)
                              )
                              {-# LINE 348 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 354 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                         _lhsOusedAttrs =
                             ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 360 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                         _lhsOusedFields =
                             ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 366 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                         _lhsOusedLocals =
                             ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 372 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              CharToken value_ pos_
                              {-# LINE 378 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 384 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken
sem_HsToken_StrToken value_ pos_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOoutput :: HsToken
                         -- "./src-ag/SemHsTokens.ag"(line 134, column 16)
                         _lhsOtok =
                             ({-# LINE 134 "./src-ag/SemHsTokens.ag" #-}
                              (pos_, showStrShort value_)
                              {-# LINE 407 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 413 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                         _lhsOusedAttrs =
                             ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 419 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                         _lhsOusedFields =
                             ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 425 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                         _lhsOusedLocals =
                             ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 431 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              StrToken value_ pos_
                              {-# LINE 437 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 443 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken
sem_HsToken_Err mesg_ pos_ =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOtok :: ((Pos,String))
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOoutput :: HsToken
                         -- "./src-ag/SemHsTokens.ag"(line 50, column 9)
                         _lhsOerrors =
                             ({-# LINE 50 "./src-ag/SemHsTokens.ag" #-}
                              let m = text mesg_
                              in Seq.singleton (CustomError False pos_ m)
                              {-# LINE 467 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- "./src-ag/SemHsTokens.ag"(line 135, column 16)
                         _lhsOtok =
                             ({-# LINE 135 "./src-ag/SemHsTokens.ag" #-}
                              (pos_, "")
                              {-# LINE 473 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                         _lhsOusedAttrs =
                             ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 479 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                         _lhsOusedFields =
                             ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                              Seq.empty
                              {-# LINE 485 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                         _lhsOusedLocals =
                             ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                              []
                              {-# LINE 491 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _output =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              Err mesg_ pos_
                              {-# LINE 497 "dist/build/SemHsTokens.hs" #-}
                              )
                         -- self rule
                         _lhsOoutput =
                             ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                              _output
                              {-# LINE 503 "dist/build/SemHsTokens.hs" #-}
                              )
                     in  ( _lhsOerrors,_lhsOoutput,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         fieldnames           : [Identifier]
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         output               : HsTokens 
         tks                  : [(Pos,String)]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : Seq Identifier
         usedLocals           : [Identifier]
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_HsTokens :: HsTokens ->
                T_HsTokens
sem_HsTokens list =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list))
-- semantic domain
newtype T_HsTokens = T_HsTokens (([(Identifier,Type,ChildKind)]) ->
                                 ([Identifier]) ->
                                 ([(Identifier,Identifier)]) ->
                                 Identifier ->
                                 ([Identifier]) ->
                                 Identifier ->
                                 ( (Seq Error),HsTokens,([(Pos,String)]),([(Identifier,Identifier)]),(Seq Identifier),([Identifier])))
data Inh_HsTokens = Inh_HsTokens {allfields_Inh_HsTokens :: ([(Identifier,Type,ChildKind)]),allnts_Inh_HsTokens :: ([Identifier]),attrs_Inh_HsTokens :: ([(Identifier,Identifier)]),con_Inh_HsTokens :: Identifier,fieldnames_Inh_HsTokens :: ([Identifier]),nt_Inh_HsTokens :: Identifier}
data Syn_HsTokens = Syn_HsTokens {errors_Syn_HsTokens :: (Seq Error),output_Syn_HsTokens :: HsTokens,tks_Syn_HsTokens :: ([(Pos,String)]),usedAttrs_Syn_HsTokens :: ([(Identifier,Identifier)]),usedFields_Syn_HsTokens :: (Seq Identifier),usedLocals_Syn_HsTokens :: ([Identifier])}
wrap_HsTokens :: T_HsTokens ->
                 Inh_HsTokens ->
                 Syn_HsTokens
wrap_HsTokens (T_HsTokens sem) (Inh_HsTokens _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt) =
    (let ( _lhsOerrors,_lhsOoutput,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt
     in  (Syn_HsTokens _lhsOerrors _lhsOoutput _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals))
sem_HsTokens_Cons :: T_HsToken ->
                     T_HsTokens ->
                     T_HsTokens
sem_HsTokens_Cons (T_HsToken hd_) (T_HsTokens tl_) =
    (T_HsTokens (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIfieldnames
                   _lhsInt ->
                     (let _lhsOtks :: ([(Pos,String)])
                          _lhsOerrors :: (Seq Error)
                          _lhsOusedAttrs :: ([(Identifier,Identifier)])
                          _lhsOusedFields :: (Seq Identifier)
                          _lhsOusedLocals :: ([Identifier])
                          _lhsOoutput :: HsTokens
                          _hdOallfields :: ([(Identifier,Type,ChildKind)])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOfieldnames :: ([Identifier])
                          _hdOnt :: Identifier
                          _tlOallfields :: ([(Identifier,Type,ChildKind)])
                          _tlOallnts :: ([Identifier])
                          _tlOattrs :: ([(Identifier,Identifier)])
                          _tlOcon :: Identifier
                          _tlOfieldnames :: ([Identifier])
                          _tlOnt :: Identifier
                          _hdIerrors :: (Seq Error)
                          _hdIoutput :: HsToken
                          _hdItok :: ((Pos,String))
                          _hdIusedAttrs :: ([(Identifier,Identifier)])
                          _hdIusedFields :: (Seq Identifier)
                          _hdIusedLocals :: ([Identifier])
                          _tlIerrors :: (Seq Error)
                          _tlIoutput :: HsTokens
                          _tlItks :: ([(Pos,String)])
                          _tlIusedAttrs :: ([(Identifier,Identifier)])
                          _tlIusedFields :: (Seq Identifier)
                          _tlIusedLocals :: ([Identifier])
                          -- "./src-ag/SemHsTokens.ag"(line 117, column 10)
                          _lhsOtks =
                              ({-# LINE 117 "./src-ag/SemHsTokens.ag" #-}
                               _hdItok : _tlItks
                               {-# LINE 598 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                          _lhsOerrors =
                              ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                               _hdIerrors Seq.>< _tlIerrors
                               {-# LINE 604 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                          _lhsOusedAttrs =
                              ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                               _hdIusedAttrs ++ _tlIusedAttrs
                               {-# LINE 610 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                          _lhsOusedFields =
                              ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                               _hdIusedFields Seq.>< _tlIusedFields
                               {-# LINE 616 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                          _lhsOusedLocals =
                              ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                               _hdIusedLocals ++ _tlIusedLocals
                               {-# LINE 622 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                               (:) _hdIoutput _tlIoutput
                               {-# LINE 628 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                               _output
                               {-# LINE 634 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallfields =
                              ({-# LINE 30 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIallfields
                               {-# LINE 640 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOallnts =
                              ({-# LINE 31 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIallnts
                               {-# LINE 646 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOattrs =
                              ({-# LINE 32 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIattrs
                               {-# LINE 652 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOcon =
                              ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIcon
                               {-# LINE 658 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOfieldnames =
                              ({-# LINE 36 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIfieldnames
                               {-# LINE 664 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _hdOnt =
                              ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                               _lhsInt
                               {-# LINE 670 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallfields =
                              ({-# LINE 30 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIallfields
                               {-# LINE 676 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOallnts =
                              ({-# LINE 31 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIallnts
                               {-# LINE 682 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOattrs =
                              ({-# LINE 32 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIattrs
                               {-# LINE 688 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOcon =
                              ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIcon
                               {-# LINE 694 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOfieldnames =
                              ({-# LINE 36 "./src-ag/SemHsTokens.ag" #-}
                               _lhsIfieldnames
                               {-# LINE 700 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- copy rule (down)
                          _tlOnt =
                              ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                               _lhsInt
                               {-# LINE 706 "dist/build/SemHsTokens.hs" #-}
                               )
                          ( _hdIerrors,_hdIoutput,_hdItok,_hdIusedAttrs,_hdIusedFields,_hdIusedLocals) =
                              hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOfieldnames _hdOnt
                          ( _tlIerrors,_tlIoutput,_tlItks,_tlIusedAttrs,_tlIusedFields,_tlIusedLocals) =
                              tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOfieldnames _tlOnt
                      in  ( _lhsOerrors,_lhsOoutput,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
sem_HsTokens_Nil :: T_HsTokens
sem_HsTokens_Nil =
    (T_HsTokens (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIfieldnames
                   _lhsInt ->
                     (let _lhsOtks :: ([(Pos,String)])
                          _lhsOerrors :: (Seq Error)
                          _lhsOusedAttrs :: ([(Identifier,Identifier)])
                          _lhsOusedFields :: (Seq Identifier)
                          _lhsOusedLocals :: ([Identifier])
                          _lhsOoutput :: HsTokens
                          -- "./src-ag/SemHsTokens.ag"(line 118, column 10)
                          _lhsOtks =
                              ({-# LINE 118 "./src-ag/SemHsTokens.ag" #-}
                               []
                               {-# LINE 731 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 43, column 37)
                          _lhsOerrors =
                              ({-# LINE 43 "./src-ag/SemHsTokens.ag" #-}
                               Seq.empty
                               {-# LINE 737 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 88, column 40)
                          _lhsOusedAttrs =
                              ({-# LINE 88 "./src-ag/SemHsTokens.ag" #-}
                               []
                               {-# LINE 743 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 100, column 40)
                          _lhsOusedFields =
                              ({-# LINE 100 "./src-ag/SemHsTokens.ag" #-}
                               Seq.empty
                               {-# LINE 749 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- use rule "./src-ag/SemHsTokens.ag"(line 87, column 40)
                          _lhsOusedLocals =
                              ({-# LINE 87 "./src-ag/SemHsTokens.ag" #-}
                               []
                               {-# LINE 755 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- self rule
                          _output =
                              ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                               []
                               {-# LINE 761 "dist/build/SemHsTokens.hs" #-}
                               )
                          -- self rule
                          _lhsOoutput =
                              ({-# LINE 142 "./src-ag/SemHsTokens.ag" #-}
                               _output
                               {-# LINE 767 "dist/build/SemHsTokens.hs" #-}
                               )
                      in  ( _lhsOerrors,_lhsOoutput,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))
-- HsTokensRoot ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,ChildKind)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         output               : [HsToken]
         textLines            : [String]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : [Identifier]
         usedLocals           : [Identifier]
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot ->
                    T_HsTokensRoot
sem_HsTokensRoot (HsTokensRoot _tokens) =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens))
-- semantic domain
newtype T_HsTokensRoot = T_HsTokensRoot (([(Identifier,Type,ChildKind)]) ->
                                         ([Identifier]) ->
                                         ([(Identifier,Identifier)]) ->
                                         Identifier ->
                                         Identifier ->
                                         ( (Seq Error),([HsToken]),([String]),([(Identifier,Identifier)]),([Identifier]),([Identifier])))
data Inh_HsTokensRoot = Inh_HsTokensRoot {allfields_Inh_HsTokensRoot :: ([(Identifier,Type,ChildKind)]),allnts_Inh_HsTokensRoot :: ([Identifier]),attrs_Inh_HsTokensRoot :: ([(Identifier,Identifier)]),con_Inh_HsTokensRoot :: Identifier,nt_Inh_HsTokensRoot :: Identifier}
data Syn_HsTokensRoot = Syn_HsTokensRoot {errors_Syn_HsTokensRoot :: (Seq Error),output_Syn_HsTokensRoot :: ([HsToken]),textLines_Syn_HsTokensRoot :: ([String]),usedAttrs_Syn_HsTokensRoot :: ([(Identifier,Identifier)]),usedFields_Syn_HsTokensRoot :: ([Identifier]),usedLocals_Syn_HsTokensRoot :: ([Identifier])}
wrap_HsTokensRoot :: T_HsTokensRoot ->
                     Inh_HsTokensRoot ->
                     Syn_HsTokensRoot
wrap_HsTokensRoot (T_HsTokensRoot sem) (Inh_HsTokensRoot _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt) =
    (let ( _lhsOerrors,_lhsOoutput,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) = sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt
     in  (Syn_HsTokensRoot _lhsOerrors _lhsOoutput _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens ->
                                 T_HsTokensRoot
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_) =
    (T_HsTokensRoot (\ _lhsIallfields
                       _lhsIallnts
                       _lhsIattrs
                       _lhsIcon
                       _lhsInt ->
                         (let _tokensOfieldnames :: ([Identifier])
                              _lhsOusedFields :: ([Identifier])
                              _lhsOtextLines :: ([String])
                              _lhsOerrors :: (Seq Error)
                              _lhsOoutput :: ([HsToken])
                              _lhsOusedAttrs :: ([(Identifier,Identifier)])
                              _lhsOusedLocals :: ([Identifier])
                              _tokensOallfields :: ([(Identifier,Type,ChildKind)])
                              _tokensOallnts :: ([Identifier])
                              _tokensOattrs :: ([(Identifier,Identifier)])
                              _tokensOcon :: Identifier
                              _tokensOnt :: Identifier
                              _tokensIerrors :: (Seq Error)
                              _tokensIoutput :: HsTokens
                              _tokensItks :: ([(Pos,String)])
                              _tokensIusedAttrs :: ([(Identifier,Identifier)])
                              _tokensIusedFields :: (Seq Identifier)
                              _tokensIusedLocals :: ([Identifier])
                              -- "./src-ag/SemHsTokens.ag"(line 38, column 18)
                              _tokensOfieldnames =
                                  ({-# LINE 38 "./src-ag/SemHsTokens.ag" #-}
                                   map (\(n,_,_) -> n) _lhsIallfields
                                   {-# LINE 840 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- "./src-ag/SemHsTokens.ag"(line 107, column 18)
                              _lhsOusedFields =
                                  ({-# LINE 107 "./src-ag/SemHsTokens.ag" #-}
                                   toList _tokensIusedFields
                                   {-# LINE 846 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- "./src-ag/SemHsTokens.ag"(line 114, column 18)
                              _lhsOtextLines =
                                  ({-# LINE 114 "./src-ag/SemHsTokens.ag" #-}
                                   showTokens _tokensItks
                                   {-# LINE 852 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- use rule "./src-ag/SemHsTokens.ag"(line 18, column 18)
                              _lhsOerrors =
                                  ({-# LINE 18 "./src-ag/SemHsTokens.ag" #-}
                                   _tokensIerrors
                                   {-# LINE 858 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOoutput =
                                  ({-# LINE 141 "./src-ag/SemHsTokens.ag" #-}
                                   _tokensIoutput
                                   {-# LINE 864 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOusedAttrs =
                                  ({-# LINE 20 "./src-ag/SemHsTokens.ag" #-}
                                   _tokensIusedAttrs
                                   {-# LINE 870 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (up)
                              _lhsOusedLocals =
                                  ({-# LINE 19 "./src-ag/SemHsTokens.ag" #-}
                                   _tokensIusedLocals
                                   {-# LINE 876 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (down)
                              _tokensOallfields =
                                  ({-# LINE 30 "./src-ag/SemHsTokens.ag" #-}
                                   _lhsIallfields
                                   {-# LINE 882 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (down)
                              _tokensOallnts =
                                  ({-# LINE 31 "./src-ag/SemHsTokens.ag" #-}
                                   _lhsIallnts
                                   {-# LINE 888 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (down)
                              _tokensOattrs =
                                  ({-# LINE 32 "./src-ag/SemHsTokens.ag" #-}
                                   _lhsIattrs
                                   {-# LINE 894 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (down)
                              _tokensOcon =
                                  ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                                   _lhsIcon
                                   {-# LINE 900 "dist/build/SemHsTokens.hs" #-}
                                   )
                              -- copy rule (down)
                              _tokensOnt =
                                  ({-# LINE 29 "./src-ag/SemHsTokens.ag" #-}
                                   _lhsInt
                                   {-# LINE 906 "dist/build/SemHsTokens.hs" #-}
                                   )
                              ( _tokensIerrors,_tokensIoutput,_tokensItks,_tokensIusedAttrs,_tokensIusedFields,_tokensIusedLocals) =
                                  tokens_ _tokensOallfields _tokensOallnts _tokensOattrs _tokensOcon _tokensOfieldnames _tokensOnt
                          in  ( _lhsOerrors,_lhsOoutput,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))))