

-- UUAGC 0.9.42.1 (src-ag/PrintVisitCode.ag)
module PrintVisitCode where
{-# LINE 10 "./src-ag/PrintVisitCode.ag" #-}

import CommonTypes
import SequentialTypes
import Options
import CodeSyntax
import ErrorMessages
import GrammarInfo
import DeclBlocks
import Pretty

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import UU.Scanner.Position

import Data.List(partition,intersperse,intersect,(\\))
import Data.Maybe(fromJust,isJust)
{-# LINE 27 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 2 "./src-ag/CodeSyntax.ag" #-}

import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)
{-# LINE 35 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 2 "./src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 42 "dist/build/PrintVisitCode.hs" #-}

{-# LINE 2 "./src-ag/DeclBlocks.ag" #-}

import Code (Decl,Expr)
{-# LINE 47 "dist/build/PrintVisitCode.hs" #-}
{-# LINE 32 "./src-ag/PrintVisitCode.ag" #-}

type PP_Docs = [PP_Doc]

ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs
{-# LINE 61 "dist/build/PrintVisitCode.hs" #-}
-- CGrammar ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         output               : PP_Docs
   alternatives:
      alternative CGrammar:
         child typeSyns       : {TypeSyns}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : CNonterminals 
         child pragmas        : {PragmaMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Set Identifier))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))}
         child multivisit     : {Bool}
-}
-- cata
sem_CGrammar :: CGrammar ->
                T_CGrammar
sem_CGrammar (CGrammar _typeSyns _derivings _wrappers _nonts _pragmas _paramMap _contextMap _quantMap _aroundsMap _mergeMap _multivisit) =
    (sem_CGrammar_CGrammar _typeSyns _derivings _wrappers (sem_CNonterminals _nonts) _pragmas _paramMap _contextMap _quantMap _aroundsMap _mergeMap _multivisit)
-- semantic domain
newtype T_CGrammar = T_CGrammar (Options ->
                                 ( PP_Docs))
data Inh_CGrammar = Inh_CGrammar {options_Inh_CGrammar :: !(Options)}
data Syn_CGrammar = Syn_CGrammar {output_Syn_CGrammar :: !(PP_Docs)}
wrap_CGrammar :: T_CGrammar ->
                 Inh_CGrammar ->
                 Syn_CGrammar
wrap_CGrammar (T_CGrammar sem) (Inh_CGrammar _lhsIoptions) =
    (let ( _lhsOoutput) = sem _lhsIoptions
     in  (Syn_CGrammar _lhsOoutput))
sem_CGrammar_CGrammar :: TypeSyns ->
                         Derivings ->
                         (Set NontermIdent) ->
                         T_CNonterminals ->
                         PragmaMap ->
                         ParamMap ->
                         ContextMap ->
                         QuantMap ->
                         (Map NontermIdent (Map ConstructorIdent (Set Identifier))) ->
                         (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier])))) ->
                         Bool ->
                         T_CGrammar
sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ (T_CNonterminals nonts_) pragmas_ paramMap_ contextMap_ quantMap_ aroundsMap_ mergeMap_ multivisit_ =
    (T_CGrammar (\ _lhsIoptions ->
                     (case (({-# LINE 53 "./src-ag/PrintVisitCode.ag" #-}
                             []
                             {-# LINE 115 "dist/build/PrintVisitCode.hs" #-}
                             )) of
                      { _lhsOoutput ->
                      ( _lhsOoutput) })))
-- CInterface --------------------------------------------------
{-
   alternatives:
      alternative CInterface:
         child seg            : CSegments 
-}
-- cata
sem_CInterface :: CInterface ->
                  T_CInterface
sem_CInterface (CInterface _seg) =
    (sem_CInterface_CInterface (sem_CSegments _seg))
-- semantic domain
newtype T_CInterface = T_CInterface (( ))
data Inh_CInterface = Inh_CInterface {}
data Syn_CInterface = Syn_CInterface {}
wrap_CInterface :: T_CInterface ->
                   Inh_CInterface ->
                   Syn_CInterface
wrap_CInterface (T_CInterface sem) (Inh_CInterface) =
    (let ( ) = sem
     in  (Syn_CInterface))
sem_CInterface_CInterface :: T_CSegments ->
                             T_CInterface
sem_CInterface_CInterface (T_CSegments seg_) =
    (T_CInterface ( ))
-- CNonterminal ------------------------------------------------
{-
   alternatives:
      alternative CNonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : CProductions 
         child inter          : CInterface 
-}
-- cata
sem_CNonterminal :: CNonterminal ->
                    T_CNonterminal
sem_CNonterminal (CNonterminal _nt _params _inh _syn _prods _inter) =
    (sem_CNonterminal_CNonterminal _nt _params _inh _syn (sem_CProductions _prods) (sem_CInterface _inter))
-- semantic domain
newtype T_CNonterminal = T_CNonterminal (( ))
data Inh_CNonterminal = Inh_CNonterminal {}
data Syn_CNonterminal = Syn_CNonterminal {}
wrap_CNonterminal :: T_CNonterminal ->
                     Inh_CNonterminal ->
                     Syn_CNonterminal
wrap_CNonterminal (T_CNonterminal sem) (Inh_CNonterminal) =
    (let ( ) = sem
     in  (Syn_CNonterminal))
sem_CNonterminal_CNonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 Attributes ->
                                 Attributes ->
                                 T_CProductions ->
                                 T_CInterface ->
                                 T_CNonterminal
sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ (T_CProductions prods_) (T_CInterface inter_) =
    (T_CNonterminal ( ))
-- CNonterminals -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CNonterminal 
         child tl             : CNonterminals 
      alternative Nil:
-}
-- cata
sem_CNonterminals :: CNonterminals ->
                     T_CNonterminals
sem_CNonterminals list =
    (Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list))
-- semantic domain
newtype T_CNonterminals = T_CNonterminals (( ))
data Inh_CNonterminals = Inh_CNonterminals {}
data Syn_CNonterminals = Syn_CNonterminals {}
wrap_CNonterminals :: T_CNonterminals ->
                      Inh_CNonterminals ->
                      Syn_CNonterminals
wrap_CNonterminals (T_CNonterminals sem) (Inh_CNonterminals) =
    (let ( ) = sem
     in  (Syn_CNonterminals))
sem_CNonterminals_Cons :: T_CNonterminal ->
                          T_CNonterminals ->
                          T_CNonterminals
sem_CNonterminals_Cons (T_CNonterminal hd_) (T_CNonterminals tl_) =
    (T_CNonterminals ( ))
sem_CNonterminals_Nil :: T_CNonterminals
sem_CNonterminals_Nil =
    (T_CNonterminals ( ))
-- CProduction -------------------------------------------------
{-
   alternatives:
      alternative CProduction:
         child con            : {ConstructorIdent}
         child visits         : CVisits 
         child children       : {[(Identifier,Type,ChildKind)]}
         child terminals      : {[Identifier]}
-}
-- cata
sem_CProduction :: CProduction ->
                   T_CProduction
sem_CProduction (CProduction _con _visits _children _terminals) =
    (sem_CProduction_CProduction _con (sem_CVisits _visits) _children _terminals)
-- semantic domain
newtype T_CProduction = T_CProduction (( ))
data Inh_CProduction = Inh_CProduction {}
data Syn_CProduction = Syn_CProduction {}
wrap_CProduction :: T_CProduction ->
                    Inh_CProduction ->
                    Syn_CProduction
wrap_CProduction (T_CProduction sem) (Inh_CProduction) =
    (let ( ) = sem
     in  (Syn_CProduction))
sem_CProduction_CProduction :: ConstructorIdent ->
                               T_CVisits ->
                               ([(Identifier,Type,ChildKind)]) ->
                               ([Identifier]) ->
                               T_CProduction
sem_CProduction_CProduction con_ (T_CVisits visits_) children_ terminals_ =
    (T_CProduction ( ))
-- CProductions ------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CProduction 
         child tl             : CProductions 
      alternative Nil:
-}
-- cata
sem_CProductions :: CProductions ->
                    T_CProductions
sem_CProductions list =
    (Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list))
-- semantic domain
newtype T_CProductions = T_CProductions (( ))
data Inh_CProductions = Inh_CProductions {}
data Syn_CProductions = Syn_CProductions {}
wrap_CProductions :: T_CProductions ->
                     Inh_CProductions ->
                     Syn_CProductions
wrap_CProductions (T_CProductions sem) (Inh_CProductions) =
    (let ( ) = sem
     in  (Syn_CProductions))
sem_CProductions_Cons :: T_CProduction ->
                         T_CProductions ->
                         T_CProductions
sem_CProductions_Cons (T_CProduction hd_) (T_CProductions tl_) =
    (T_CProductions ( ))
sem_CProductions_Nil :: T_CProductions
sem_CProductions_Nil =
    (T_CProductions ( ))
-- CRule -------------------------------------------------------
{-
   alternatives:
      alternative CRule:
         child name           : {Identifier}
         child isIn           : {Bool}
         child hasCode        : {Bool}
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child childnt        : {Maybe NontermIdent}
         child tp             : {Maybe Type}
         child pattern        : Pattern 
         child rhs            : {[String]}
         child defines        : {Map Int (Identifier,Identifier,Maybe Type)}
         child owrt           : {Bool}
         child origin         : {String}
         child uses           : {Set (Identifier, Identifier)}
         child explicit       : {Bool}
         child mbNamed        : {Maybe Identifier}
      alternative CChildVisit:
         child name           : {Identifier}
         child nt             : {NontermIdent}
         child nr             : {Int}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child isLast         : {Bool}
-}
-- cata
sem_CRule :: CRule ->
             T_CRule
sem_CRule (CRule _name _isIn _hasCode _nt _con _field _childnt _tp _pattern _rhs _defines _owrt _origin _uses _explicit _mbNamed) =
    (sem_CRule_CRule _name _isIn _hasCode _nt _con _field _childnt _tp (sem_Pattern _pattern) _rhs _defines _owrt _origin _uses _explicit _mbNamed)
sem_CRule (CChildVisit _name _nt _nr _inh _syn _isLast) =
    (sem_CRule_CChildVisit _name _nt _nr _inh _syn _isLast)
-- semantic domain
newtype T_CRule = T_CRule (( ))
data Inh_CRule = Inh_CRule {}
data Syn_CRule = Syn_CRule {}
wrap_CRule :: T_CRule ->
              Inh_CRule ->
              Syn_CRule
wrap_CRule (T_CRule sem) (Inh_CRule) =
    (let ( ) = sem
     in  (Syn_CRule))
sem_CRule_CRule :: Identifier ->
                   Bool ->
                   Bool ->
                   NontermIdent ->
                   ConstructorIdent ->
                   Identifier ->
                   (Maybe NontermIdent) ->
                   (Maybe Type) ->
                   T_Pattern ->
                   ([String]) ->
                   (Map Int (Identifier,Identifier,Maybe Type)) ->
                   Bool ->
                   String ->
                   (Set (Identifier, Identifier)) ->
                   Bool ->
                   (Maybe Identifier) ->
                   T_CRule
sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ (T_Pattern pattern_) rhs_ defines_ owrt_ origin_ uses_ explicit_ mbNamed_ =
    (T_CRule ( ))
sem_CRule_CChildVisit :: Identifier ->
                         NontermIdent ->
                         Int ->
                         Attributes ->
                         Attributes ->
                         Bool ->
                         T_CRule
sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_ =
    (T_CRule ( ))
-- CSegment ----------------------------------------------------
{-
   alternatives:
      alternative CSegment:
         child inh            : {Attributes}
         child syn            : {Attributes}
-}
-- cata
sem_CSegment :: CSegment ->
                T_CSegment
sem_CSegment (CSegment _inh _syn) =
    (sem_CSegment_CSegment _inh _syn)
-- semantic domain
newtype T_CSegment = T_CSegment (( ))
data Inh_CSegment = Inh_CSegment {}
data Syn_CSegment = Syn_CSegment {}
wrap_CSegment :: T_CSegment ->
                 Inh_CSegment ->
                 Syn_CSegment
wrap_CSegment (T_CSegment sem) (Inh_CSegment) =
    (let ( ) = sem
     in  (Syn_CSegment))
sem_CSegment_CSegment :: Attributes ->
                         Attributes ->
                         T_CSegment
sem_CSegment_CSegment inh_ syn_ =
    (T_CSegment ( ))
-- CSegments ---------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CSegment 
         child tl             : CSegments 
      alternative Nil:
-}
-- cata
sem_CSegments :: CSegments ->
                 T_CSegments
sem_CSegments list =
    (Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list))
-- semantic domain
newtype T_CSegments = T_CSegments (( ))
data Inh_CSegments = Inh_CSegments {}
data Syn_CSegments = Syn_CSegments {}
wrap_CSegments :: T_CSegments ->
                  Inh_CSegments ->
                  Syn_CSegments
wrap_CSegments (T_CSegments sem) (Inh_CSegments) =
    (let ( ) = sem
     in  (Syn_CSegments))
sem_CSegments_Cons :: T_CSegment ->
                      T_CSegments ->
                      T_CSegments
sem_CSegments_Cons (T_CSegment hd_) (T_CSegments tl_) =
    (T_CSegments ( ))
sem_CSegments_Nil :: T_CSegments
sem_CSegments_Nil =
    (T_CSegments ( ))
-- CVisit ------------------------------------------------------
{-
   alternatives:
      alternative CVisit:
         child inh            : {Attributes}
         child syn            : {Attributes}
         child vss            : Sequence 
         child intra          : Sequence 
         child ordered        : {Bool}
-}
-- cata
sem_CVisit :: CVisit ->
              T_CVisit
sem_CVisit (CVisit _inh _syn _vss _intra _ordered) =
    (sem_CVisit_CVisit _inh _syn (sem_Sequence _vss) (sem_Sequence _intra) _ordered)
-- semantic domain
newtype T_CVisit = T_CVisit (( ))
data Inh_CVisit = Inh_CVisit {}
data Syn_CVisit = Syn_CVisit {}
wrap_CVisit :: T_CVisit ->
               Inh_CVisit ->
               Syn_CVisit
wrap_CVisit (T_CVisit sem) (Inh_CVisit) =
    (let ( ) = sem
     in  (Syn_CVisit))
sem_CVisit_CVisit :: Attributes ->
                     Attributes ->
                     T_Sequence ->
                     T_Sequence ->
                     Bool ->
                     T_CVisit
sem_CVisit_CVisit inh_ syn_ (T_Sequence vss_) (T_Sequence intra_) ordered_ =
    (T_CVisit ( ))
-- CVisits -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CVisit 
         child tl             : CVisits 
      alternative Nil:
-}
-- cata
sem_CVisits :: CVisits ->
               T_CVisits
sem_CVisits list =
    (Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list))
-- semantic domain
newtype T_CVisits = T_CVisits (( ))
data Inh_CVisits = Inh_CVisits {}
data Syn_CVisits = Syn_CVisits {}
wrap_CVisits :: T_CVisits ->
                Inh_CVisits ->
                Syn_CVisits
wrap_CVisits (T_CVisits sem) (Inh_CVisits) =
    (let ( ) = sem
     in  (Syn_CVisits))
sem_CVisits_Cons :: T_CVisit ->
                    T_CVisits ->
                    T_CVisits
sem_CVisits_Cons (T_CVisit hd_) (T_CVisits tl_) =
    (T_CVisits ( ))
sem_CVisits_Nil :: T_CVisits
sem_CVisits_Nil =
    (T_CVisits ( ))
-- DeclBlocks --------------------------------------------------
{-
   alternatives:
      alternative DeclBlock:
         child defs           : {[Decl]}
         child visit          : {Decl}
         child next           : DeclBlocks 
      alternative DeclTerminator:
         child defs           : {[Decl]}
         child result         : {Expr}
-}
-- cata
sem_DeclBlocks :: DeclBlocks ->
                  T_DeclBlocks
sem_DeclBlocks (DeclBlock _defs _visit _next) =
    (sem_DeclBlocks_DeclBlock _defs _visit (sem_DeclBlocks _next))
sem_DeclBlocks (DeclTerminator _defs _result) =
    (sem_DeclBlocks_DeclTerminator _defs _result)
-- semantic domain
newtype T_DeclBlocks = T_DeclBlocks (( ))
data Inh_DeclBlocks = Inh_DeclBlocks {}
data Syn_DeclBlocks = Syn_DeclBlocks {}
wrap_DeclBlocks :: T_DeclBlocks ->
                   Inh_DeclBlocks ->
                   Syn_DeclBlocks
wrap_DeclBlocks (T_DeclBlocks sem) (Inh_DeclBlocks) =
    (let ( ) = sem
     in  (Syn_DeclBlocks))
sem_DeclBlocks_DeclBlock :: ([Decl]) ->
                            Decl ->
                            T_DeclBlocks ->
                            T_DeclBlocks
sem_DeclBlocks_DeclBlock defs_ visit_ (T_DeclBlocks next_) =
    (T_DeclBlocks ( ))
sem_DeclBlocks_DeclTerminator :: ([Decl]) ->
                                 Expr ->
                                 T_DeclBlocks
sem_DeclBlocks_DeclTerminator defs_ result_ =
    (T_DeclBlocks ( ))
-- DeclBlocksRoot ----------------------------------------------
{-
   alternatives:
      alternative DeclBlocksRoot:
         child blocks         : DeclBlocks 
-}
-- cata
sem_DeclBlocksRoot :: DeclBlocksRoot ->
                      T_DeclBlocksRoot
sem_DeclBlocksRoot (DeclBlocksRoot _blocks) =
    (sem_DeclBlocksRoot_DeclBlocksRoot (sem_DeclBlocks _blocks))
-- semantic domain
newtype T_DeclBlocksRoot = T_DeclBlocksRoot (( ))
data Inh_DeclBlocksRoot = Inh_DeclBlocksRoot {}
data Syn_DeclBlocksRoot = Syn_DeclBlocksRoot {}
wrap_DeclBlocksRoot :: T_DeclBlocksRoot ->
                       Inh_DeclBlocksRoot ->
                       Syn_DeclBlocksRoot
wrap_DeclBlocksRoot (T_DeclBlocksRoot sem) (Inh_DeclBlocksRoot) =
    (let ( ) = sem
     in  (Syn_DeclBlocksRoot))
sem_DeclBlocksRoot_DeclBlocksRoot :: T_DeclBlocks ->
                                     T_DeclBlocksRoot
sem_DeclBlocksRoot_DeclBlocksRoot (T_DeclBlocks blocks_) =
    (T_DeclBlocksRoot ( ))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : Pattern 
   alternatives:
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (( Pattern))
data Inh_Pattern = Inh_Pattern {}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: !(Pattern)}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern) =
    (let ( _lhsOcopy) = sem
     in  (Syn_Pattern _lhsOcopy))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (case (pats_) of
                { ( _patsIcopy) ->
                    (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                            Constr name_ _patsIcopy
                            {-# LINE 593 "dist/build/PrintVisitCode.hs" #-}
                            )) of
                     { _copy ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 598 "dist/build/PrintVisitCode.hs" #-}
                             )) of
                      { _lhsOcopy ->
                      ( _lhsOcopy) }) }) }))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (case (pats_) of
                { ( _patsIcopy) ->
                    (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                            Product pos_ _patsIcopy
                            {-# LINE 610 "dist/build/PrintVisitCode.hs" #-}
                            )) of
                     { _copy ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 615 "dist/build/PrintVisitCode.hs" #-}
                             )) of
                      { _lhsOcopy ->
                      ( _lhsOcopy) }) }) }))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (case (pat_) of
                { ( _patIcopy) ->
                    (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                            Alias field_ attr_ _patIcopy
                            {-# LINE 628 "dist/build/PrintVisitCode.hs" #-}
                            )) of
                     { _copy ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 633 "dist/build/PrintVisitCode.hs" #-}
                             )) of
                      { _lhsOcopy ->
                      ( _lhsOcopy) }) }) }))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (case (pat_) of
                { ( _patIcopy) ->
                    (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                            Irrefutable _patIcopy
                            {-# LINE 644 "dist/build/PrintVisitCode.hs" #-}
                            )) of
                     { _copy ->
                     (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                             _copy
                             {-# LINE 649 "dist/build/PrintVisitCode.hs" #-}
                             )) of
                      { _lhsOcopy ->
                      ( _lhsOcopy) }) }) }))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                       Underscore pos_
                       {-# LINE 658 "dist/build/PrintVisitCode.hs" #-}
                       )) of
                { _copy ->
                (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                        _copy
                        {-# LINE 663 "dist/build/PrintVisitCode.hs" #-}
                        )) of
                 { _lhsOcopy ->
                 ( _lhsOcopy) }) }))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : Patterns 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (( Patterns))
data Inh_Patterns = Inh_Patterns {}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: !(Patterns)}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns) =
    (let ( _lhsOcopy) = sem
     in  (Syn_Patterns _lhsOcopy))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (case (tl_) of
                 { ( _tlIcopy) ->
                     (case (hd_) of
                      { ( _hdIcopy) ->
                          (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                  (:) _hdIcopy _tlIcopy
                                  {-# LINE 707 "dist/build/PrintVisitCode.hs" #-}
                                  )) of
                           { _copy ->
                           (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                                   _copy
                                   {-# LINE 712 "dist/build/PrintVisitCode.hs" #-}
                                   )) of
                            { _lhsOcopy ->
                            ( _lhsOcopy) }) }) }) }))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                        []
                        {-# LINE 720 "dist/build/PrintVisitCode.hs" #-}
                        )) of
                 { _copy ->
                 (case (({-# LINE 22 "./src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 725 "dist/build/PrintVisitCode.hs" #-}
                         )) of
                  { _lhsOcopy ->
                  ( _lhsOcopy) }) }))
-- Sequence ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : CRule 
         child tl             : Sequence 
      alternative Nil:
-}
-- cata
sem_Sequence :: Sequence ->
                T_Sequence
sem_Sequence list =
    (Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list))
-- semantic domain
newtype T_Sequence = T_Sequence (( ))
data Inh_Sequence = Inh_Sequence {}
data Syn_Sequence = Syn_Sequence {}
wrap_Sequence :: T_Sequence ->
                 Inh_Sequence ->
                 Syn_Sequence
wrap_Sequence (T_Sequence sem) (Inh_Sequence) =
    (let ( ) = sem
     in  (Syn_Sequence))
sem_Sequence_Cons :: T_CRule ->
                     T_Sequence ->
                     T_Sequence
sem_Sequence_Cons (T_CRule hd_) (T_Sequence tl_) =
    (T_Sequence ( ))
sem_Sequence_Nil :: T_Sequence
sem_Sequence_Nil =
    (T_Sequence ( ))