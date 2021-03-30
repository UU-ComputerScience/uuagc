{-# LANGUAGE DeriveGeneric #-}

module MirageSyntax where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Grammar = Grammar [Nonterminal] deriving Generic

instance ToJSON Grammar

data Nonterminal = Nonterminal
  String       -- ^ The name of this nonterminal.
  [String]     -- ^ The names of the parameters.
  [Attribute]  -- ^ The inherited attributes.
  [Attribute]  -- ^ The synthesized attributes.
  [Production] -- ^ The production rules with this nonterminal on the left hand side.
  deriving Generic

instance ToJSON Nonterminal

data Attribute = Attribute
  String -- ^ The name of this attribute.
  Type   -- ^ The type of this attribute.
  deriving Generic

instance ToJSON Attribute

data Production = Production
  String  -- ^ The name of this production rule.
  [Child] -- ^ The children of this rule; the right hand side.
  [Rule]  -- ^ The attribute rules of this production.
  deriving Generic

instance ToJSON Production

data Child = Child
  String -- ^ The name of this child.
  Type   -- ^ The type of this child.
  deriving Generic

instance ToJSON Child

data Rule = Rule
  [Address] -- ^ Targets of this rule; the left hand side.
  [Address] -- ^ Sources of this rule; the right hand side.
  Bool      -- ^ Whether this rule is defined explicitly by the user.
  String    -- ^ The origin of this rule; which source file contains this rule?
  String    -- ^ A pretty printed representation of this rule.
  deriving Generic

instance ToJSON Rule

data Type
  = Haskell String -- ^ A plain Haskell type.
  | NT             -- ^ An attribute grammar nonterminal
      String       -- ^ The name of the nonterminal
      [String]     -- ^ The arguments of this nonterminal; must always be plain Haskell values.
  | Self           -- ^ Refers to the nonterminal in which this type is used.
  deriving Generic

instance ToJSON Type

data Address = Address
  String -- ^ The part before the dot: 'lhs', 'loc', or the name of a child.
  String -- ^ The attribute name
  deriving Generic

instance ToJSON Address

