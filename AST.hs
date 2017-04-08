-- Copyright 2013 - 2017 John F. Miller
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RankNTypes,  NamedFieldPuns #-}
-- | Data structures for the Abstract Syntax Tree
module AST where

-- import Scope (Value, Scope)
import Var
import Name
import Parameters
import LineParser

-- | Primary Abstract Syntax Tree
data Exp = Exp {node::Node, position::Position}  deriving Show
data Node =
    EVar Var -- ^ a possibly scoped variable
  | EInt Integer -- ^ Integer literal 
  | EFloat Double  -- ^ real number literal
  | EString String -- ^ string literal
  | ExString [Exp] -- ^ concat all elements together as a string
  | EAtom Name     -- ^ atom literal (:foo)
  | EArray [Exp]   -- ^ an Array Literal
  | EHash  [(Exp,Exp)] -- ^ a Hash Literal
  | EIVar Name   -- ^ Named istance variable (\@foo)
  | ECVar Name   -- ^ Named class variable (\@@foo)
  | ENil | EFalse | ETrue
    -- | Operator embeded Equation
    --
    --   Operator Strings of the form `a * b + c - d`. The first expression
    --   (`a`) is the first argument.  The list are pairs of operators and
    --   expressions ( [(*,b),(+,c),(-,d)] in the example).  This is turned
    --   into a tree structure using Dykstra's Yard Shuninig Algorithm found
    --   in Eval.hs.
  | OpStr Exp [(Op,Exp)]
  | Index Exp [Exp] -- ^ an expression followed by an index (foo[3])
  | Lambda Parameter [Exp] -- ^ an anonymous function declairation
  | Def Visibility Order Name Parameter [Exp] -- ^ a method declairartion
    -- | a method declairation of the form `def self.xxx`
  | DefSelf Visibility Order Name Parameter [Exp]
    -- | application of the actual params [Exp] to the function found at var
  | Apply Var [Exp] Visibility
    -- ^ application of the actual params [Exp] to the function derived 
    --   from Exp
  | ApplyFn Exp [Exp]
  | Call Exp Name [Exp] -- ^ method invocation (foo.bar(x))
  | Send Exp Name [Exp] -- ^ concurrent method invocation (foo->bar(x))
  | ESuper (Maybe [Exp]) -- ^ a call to `super` with or without args
  | Assign LHS Exp -- ^ assignment of a var (see LHS)
  | OpAssign LHS Op Exp -- ^ assign new value based on the old ( a += 12)
    -- | conditional expression if the predicate is not false or nil evaluate
    --   the consequent otherwise evaluate the alternate.
  | If {predicate :: Exp,consequent :: Exp ,alternate :: Maybe Exp}
  | While Exp Exp -- ^ evaluate the second Exp while the first is true
  | Until Exp Exp -- ^ evaluate the second Exp until the first becomes true
    -- | Build a new class or reopen and existing one at Var with (Maybe Var)
    --   as super class.  the Exp will be run in the in the context of the
    --   class.
  | EClass Var (Maybe Var) CodeBlock -- ^ Create or reopen a class
  | Module Var CodeBlock -- ^ Create or reopen a module
  | Block [Exp]-- ^ A block of sequential expressions.
--  | EValue (forall m. Scope m => Value m) -- ^ allows values to be "shoved" back into expressions
     deriving Show

-- | Left Hand Side data structure
--
--   This represent anything that can be assigned to -- that is could go on the
--   left hand side of an `=`.  These include local and scoped vars, IVars,
--   CVars, indexed vars, and method calls (via the foo= convention).
data LHS =
    LVar  Var
  | LIVar Name
  | LCVar Name
  | LIndex Exp [Exp]
  | LCall Exp Name [Exp]
  | LSend Exp Name [Exp]  deriving Show

