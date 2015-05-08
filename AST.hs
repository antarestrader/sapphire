-- Copyright 2013, 2014 John F. Miller

-- | Data structures for the Abstract Syntax Tree
module AST where

import Object(Value)
import Var

-- | Primary Abstract Syntax Tree
data Exp =
    EVar Var -- ^ a possibly scoped variable
  | EInt Integer -- ^ Integer literal
  | EValue Value -- ^ allows values to be "shoved" values back into expressions
  | EFloat Double -- ^ real number literal
  | EString String -- ^ string literal
  | ExString [Exp] -- ^ concat all elements together as a string
  | EAtom String -- ^ atom literal (:foo)
  | EArray [Exp] -- ^ an Array Literal
  | EHash  [(Exp,Exp)] -- ^ a Hash Literal
  | EIVar String -- ^ Named istance variable (\@foo)
  | ENil | EFalse | ETrue
    -- | Operator embeded Equation
    --
    --   Operator Strings of the form `a * b + c - d`. The first expression
    --   (`a`) is the first argument.  The list are pairs of operators and
    --   expressions ( [(*,b),(+,c),(-,d)] in the example).  This is turned
    --   into a tree structure using Dykstra's Yard Shuninig Algorithm found
    --   in Eval.hs.
  | OpStr Exp [(Op,Exp)]
  | Index Exp Exp -- ^ an expression followed by an index (foo[3])
  | Lambda [Parameter] Exp -- ^ an anonymous function declairation
  | Def String [Parameter] Exp -- ^ a method declairartion
  | Apply Var [Exp] -- ^ application of the actual params [Exp] to the function found at var
  | ApplyFn Exp [Exp] -- ^ application of the actual params [Exp] to the function derived from Exp
  | Call Exp String [Exp] -- ^ method invocation (foo.bar(x))
  | Send Exp String [Exp] -- ^ concurrent method invocation (foo->bar(x))
  | Assign LHS Exp -- ^ assignment of a var (see LHS)
  | OpAssign LHS Op Exp -- ^ assign new value based on the old ( a += 12)
  | If {predicate :: Exp,consequent :: Exp ,alternate :: Maybe Exp}
  | While Exp Exp
  | Until Exp Exp
    -- | Build a new class or reopen and existing one at Var with (Maybe Var)
    --   as super class.  the Exp will be run in the in the context of the
    --   class.
  | EClass Var (Maybe Var) Exp
  | Module Var Exp -- ^ Create or reopen a module
  | Block [Exp] FilePath -- ^ A block of sequential expressions.
    deriving Show

-- | Left Hand Side data structure
--
--   This represent anything that can be assigned to -- that is could go on the
--   left hand side of an `=`.  These include local and scoped vars, IVars,
--   CVars, indexed vars, and method calls (via the foo= convention).
data LHS =
    LVar  Var
  | LIVar String
  | LCVar String
  | LIndex Exp [Exp]
  | LCall Exp String
  | LSend Exp String deriving Show

data Parameter =
    Param String
  | Default String Exp
  | VarArg String
    deriving (Show)

getParamName (Param n) = n
getParamName (VarArg n) = n
getParamName (Default n _) = n
