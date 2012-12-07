

-- UUAGC 0.9.42.2 (src-ag/ErrorMessages.ag)
module ErrorMessages where
{-# LINE 2 "./src-ag/ErrorMessages.ag" #-}

import UU.Scanner.Position(Pos)
import Pretty
import CodeSyntax
import CommonTypes
{-# LINE 12 "dist/build/ErrorMessages.hs" #-}
-- Error -------------------------------------------------------
{-
   alternatives:
      alternative ParserError:
         child pos            : {Pos}
         child problem        : {String}
         child action         : {String}
      alternative HsParseError:
         child pos            : {Pos}
         child msg            : {String}
      alternative DupAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child occ1           : {ConstructorIdent}
      alternative DupSynonym:
         child nt             : {NontermIdent}
         child occ1           : {NontermIdent}
      alternative DupSet:
         child name           : {NontermIdent}
         child occ1           : {NontermIdent}
      alternative DupInhAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupSynAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         child occ1           : {Identifier}
      alternative DupRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child occ1           : {Identifier}
      alternative DupRuleName:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child nm             : {Identifier}
      alternative DupSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative UndefNont:
         child nt             : {NontermIdent}
      alternative UndefAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
      alternative UndefChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
      alternative MissingRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative MissingNamedRule:
         child nt             : {NontermIdent}
         child con            : {Identifier}
         child name           : {Identifier}
      alternative SuperfluousRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative UndefLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
      alternative ChildAsLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
      alternative UndefAttr:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child isOut          : {Bool}
      alternative Cyclic:
         child nt             : {NontermIdent}
         child mbCon          : {Maybe ConstructorIdent}
         child verts          : {[String]}
      alternative CyclicSet:
         child name           : {Identifier}
      alternative CustomError:
         child isWarning      : {Bool}
         child pos            : {Pos}
         child mesg           : {PP_Doc}
      alternative LocalCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
      alternative InstCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
      alternative DirectCirc:
         child nt             : {NontermIdent}
         child o_visit        : {Bool}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
      alternative InducedCirc:
         child nt             : {NontermIdent}
         child cinter         : {CInterface}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
      alternative MissingTypeSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative MissingInstSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative DupUnique:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
      alternative MissingUnique:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
      alternative MissingSyn:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
      alternative IncompatibleVisitKind:
         child child          : {Identifier}
         child vis            : {VisitIdentifier}
         child from           : {VisitKind}
         child to             : {VisitKind}
      alternative IncompatibleRuleKind:
         child rule           : {Identifier}
         child kind           : {VisitKind}
      alternative IncompatibleAttachKind:
         child child          : {Identifier}
         child kind           : {VisitKind}
-}
data Error = ParserError (Pos) (String) (String)
           | HsParseError (Pos) (String)
           | DupAlt (NontermIdent) (ConstructorIdent) (ConstructorIdent)
           | DupSynonym (NontermIdent) (NontermIdent)
           | DupSet (NontermIdent) (NontermIdent)
           | DupInhAttr (NontermIdent) (Identifier) (Identifier)
           | DupSynAttr (NontermIdent) (Identifier) (Identifier)
           | DupChild (NontermIdent) (ConstructorIdent) (Identifier) (Identifier)
           | DupRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) (Identifier)
           | DupRuleName (NontermIdent) (ConstructorIdent) (Identifier)
           | DupSig (NontermIdent) (ConstructorIdent) (Identifier)
           | UndefNont (NontermIdent)
           | UndefAlt (NontermIdent) (ConstructorIdent)
           | UndefChild (NontermIdent) (ConstructorIdent) (Identifier)
           | MissingRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier)
           | MissingNamedRule (NontermIdent) (Identifier) (Identifier)
           | SuperfluousRule (NontermIdent) (ConstructorIdent) (Identifier) (Identifier)
           | UndefLocal (NontermIdent) (ConstructorIdent) (Identifier)
           | ChildAsLocal (NontermIdent) (ConstructorIdent) (Identifier)
           | UndefAttr (NontermIdent) (ConstructorIdent) (Identifier) (Identifier) (Bool)
           | Cyclic (NontermIdent) ((Maybe ConstructorIdent)) (([String]))
           | CyclicSet (Identifier)
           | CustomError (Bool) (Pos) (PP_Doc)
           | LocalCirc (NontermIdent) (ConstructorIdent) (Identifier) (Bool) (([String]))
           | InstCirc (NontermIdent) (ConstructorIdent) (Identifier) (Bool) (([String]))
           | DirectCirc (NontermIdent) (Bool) (([((Identifier,Identifier),[String],[String])]))
           | InducedCirc (NontermIdent) (CInterface) (([((Identifier,Identifier),[String],[String])]))
           | MissingTypeSig (NontermIdent) (ConstructorIdent) (Identifier)
           | MissingInstSig (NontermIdent) (ConstructorIdent) (Identifier)
           | DupUnique (NontermIdent) (ConstructorIdent) (Identifier)
           | MissingUnique (NontermIdent) (Identifier)
           | MissingSyn (NontermIdent) (Identifier)
           | IncompatibleVisitKind (Identifier) (VisitIdentifier) (VisitKind) (VisitKind)
           | IncompatibleRuleKind (Identifier) (VisitKind)
           | IncompatibleAttachKind (Identifier) (VisitKind)
-- Errors ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Error 
         child tl             : Errors 
      alternative Nil:
-}
type Errors = [Error]