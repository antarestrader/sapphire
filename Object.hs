-- Object.hs Copyright 2013-2017 John F. Miller

{-# LANGUAGE OverloadedStrings,  NamedFieldPuns,
    MultiParamTypeClasses, RankNTypes, LiberalTypeSynonyms  #-}

{-|
  Module      : Object
  Description : Datatypes for Sapphire Values
  Copyright   : Copyright 2013-2017 John F. Miller
  Maintainer  : jfmiller@alumni.calpoly.edu
  Stability   : Active Development

  Sapphire is an untyped language.  That means that all values are of a
  single sum type.  This file contains that datatype and some accompanying
  structures used to run the system which rely on the actual type of 'Object'.
-}
module Object (
    -- * Types from 'R.Runtime' refined for Objects
    Response, Runtime, PID, Fn(..)

    -- * Primary Datatypes
  , Object(..), Primitive(..), State(..), SystemState(..)

    -- * Function to build Objects
    -- | These functions help build objects. They provide a fast way to get
    --   and object. Some of these are in the .boot-hs file which does not
    --   export the constructors of Object so this is also a useful way for
    --   modules with circular dependencies on Object can still make a few
    --   useful values like 'Nil'.
  , vnil, vint, vfloat, verror, vbool

    -- * Unique Identifier
  , getUID
  )

where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Foldable (toList)

import String
import Name
import {-# SOURCE #-} Scope
import Object.Array
import Object.Hash (Hash(..))
import Object.UID
import Boot.Options
import AST (Exp)
import Parameters


import qualified Runtime as R
import qualified Runtime.Runtime as RR
import Err

-- | A value of this type represents the action of making a response to
--   a method call.  Various actions in the Scope class produce Responses,
--   and one of them must be called. Having this value is proof that it has
--   been done.  Beyond this function, the actual datatype carries no
--   information.
type Response = RR.Response

-- | A version of the more primitive Runtime monad with state and object
--   types filled in to make it a simple kind.
type Runtime = R.Runtime SystemState Object

-- | Likewise, a version of PID from Runtime with the object value filled in
--   to make it a simple kind.
type PID = R.PID Object

-- | A sum type of various forms fo functions. It must be a datatype to avoid
--   impredictive types. So it cannot be collapsed into a type even if the
--   sum type is for some reason refactored away.
data Fn = Fn  {fn :: forall m . Scope m =>[Object] -> m Response}
        | AST {params :: Parameter, asts :: [Exp]}

-- | In "untyped" languages all data is actually a member of a single Sum type.
--   This is that type.  All data in Saphire is a value of type Object.
data Object = Prim !Primitive
              -- ^ A built in types with a express Haskell representation.
            | Process !PID
              -- ^ A remote process accessable via its PID
            | TrueClass  -- ^ The Boolean value "true"
            | FalseClass -- ^ The Boolean value "false"
            | Nil -- ^ A value of Nil
            | VFunction {function:: Fn, cacheable :: Bool, fUID::UID}
              -- ^ Functions are first class values and live here
            | Object State -- ^ Normal Sapphire Object
            | VError !(Err Object) -- ^ An error value

-- | the value containing primitives of Sapphire
data Primitive = VInt    !Integer
               | VString !SapString
               | VFloat  !Double
               | VArray !Array
               | VHash !Hash
               | VAtom !Name -- ^ The name of something in value form
                 deriving Eq

instance Eq Object where
  (Prim a)     == (Prim b)     = a == b
  (Process a)  == (Process b)  = a == b
  (VError a)   == (VError b)   = a == b
  (TrueClass)  == (TrueClass)  = True
  (FalseClass) == (FalseClass) = True
  (Nil)        == (Nil)        = True
  (Object a)   == (Object b)   = uid a == uid b
  (VFunction {fUID = a}) == (VFunction {fUID = b}) = a == b
  _ == _ = False

instance IsString Primitive where
  fromString str = VString $ fromString str

instance IsString Object where
  fromString str = Prim $ fromString str

instance R.Obj Object where
  toObj pid = Process pid

-- | For typical (non-primitive) objects this is all the data they need to
--   function correctly.  It is held in the Object constructor.  This is the
--   value that Runtime will return in its 'MonadState' instance.
data State =
    ROOT -- ^ Used as a bottom value to signal the end of the inheritance
         --   chain.
  | Instance {
        ivars :: Namespace Object -- ^ Instance variables
      , instanceOfClass :: PID -- ^ the Class of this instance
      , globalNamespace :: PID
        -- ^ where methods on this object look for ::Foo constanct
      , localNamespace  :: PID -- ^ the nearest module or class
      , localCache :: Namespace Fn
        -- ^ Methods which can be cached to prevent needing to repeatedly
        --   look them up in the Class structure.
      , primitive  :: Maybe Primitive
        -- ^ If the reciever of this instance is a primitive value the
        --   actual value lives here. For all other this is Nothing.
      , uid :: UID -- ^ A unique identifier for this object. It provides
                   --   the same kind of function as memory based equality.
      }  -- ^ State for a normal instance
  | Class {
        ivars :: Namespace Object -- ^ Instance variables
      , instanceOfClass :: PID -- ^ The class for this /Instance/ (likely Class)
      , globalNamespace :: PID -- ^ Look for global constants here
      , localNamespace  :: PID -- ^ Look for local constants here
      , localCache  :: Namespace Fn -- ^ a cache of function for this /instance/
      , superClass  :: PID
        -- ^ The parent class of this object. If we do not define a method look
        --   here next.
      , methods     :: Namespace Fn -- ^ The methods defined in this class
      , methodCache :: Namespace Fn -- ^ Up stream methods cached here
      , modules     :: [PID] -- ^ Modules included in this class
      , uid :: UID -- ^ our unique identifier (for equality)
      } -- ^ State for a Class
  | Module {
        ivars :: Namespace Object
      , instanceOfClass :: PID
      , globalNamespace :: PID
      , localNamespace  :: PID
      , localCache   :: Namespace Fn -- ^ A cache of function for this /instance/
      , methods      :: Namespace Fn -- ^ The methods defined in this module
      , childModules :: Namespace PID -- ^ Constants defined in this namespace
      , uid :: UID
      } -- ^ State for a Module

instance R.StateClass State Object where
  -- markState :: State -> IO [PID]
  markState
    Instance {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      } =
        return
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : (mark ivars)
          )
  markState
    Class {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      , superClass
      , modules
      } =
        return $
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : superClass
            : (mark ivars)
          ) ++ modules
  markState
    Module {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      , childModules
      } =
        return $
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : (mark ivars)
          ) ++ M.elems childModules

mark map = foldMap f map
  where
    f :: Object -> [PID]
    f (Process pid) = [pid]
    f _ = []

-- | This is the datatype the is actually stored in the 'R.Runtime' monad.
--   In addation to the state of the object it contains some fields for
--   holding system constants and the varialbles in the local scope.
data SystemState = SystemState
  { objectState :: State -- ^ The actual state of the object
  , uidSource   :: UIDSource -- ^ look here for new unique identifiers
  , localScope  :: [Namespace Object] --Note: Process is responsible for tracking this
    -- ^ A stack of nested local scopes.  Each contains the local variables
    --   for its scope.  When a new scope is created a new map is pushed onto
    --   the stack, once that scope exits the map is popped off the stack.
    --   When looking for a local variable, start at the head and work back
    --   up the stack.  When writing to a local variable, first see if it
    --   exists anywhere in the stack. If it does, write it there, otherwise
    --   write it to the head of the stack.
  , cmdLineOptions :: Options -- ^ Options parsed from the command line.
  , reciever    :: Maybe Object 
    -- ^ If set the reciever (a.k.a self) for the currently exicuting 
    --   function is a value contained within the process. When not set, the
    --   value of the process itself is the reciever.
  }

instance R.StateClass SystemState Object where
  markState ss = do
    xs <- R.markState (objectState ss)
    return $ xs ++ concatMap mark (localScope ss)

vnil :: Object
vnil = Nil

vint :: Integer -> Object
vint = Prim . VInt

vfloat :: Double -> Object
vfloat = Prim . VFloat

verror :: String -> Object
verror = VError . strMsg

vbool :: Bool -> Object
vbool True  = TrueClass
vbool False = FalseClass

-- | Many object types have unique identifiers.  If there is one, it is
--   return, otherwise Nothing.
getUID :: Object -> Maybe UID
getUID (Object obj) = Just $ uid obj
getUID (Prim (VHash (Hash{hUID})))  = Just hUID
getUID (VFunction{fUID}) = Just fUID
getUID _ = Nothing

instance Show Object where
  show (Prim p) = show p
  show TrueClass = "true"
  show FalseClass = "false"
  show Nil = "nil"
  show (VFunction{fUID}  ) = "<function: ("++show fUID++")>"
  show (Process p) = "<PID "++ show p ++">"
  show (VError e) = show e
  show (Object ROOT) = "ROOT"
  show (Object Instance{}) = "<Instance>"
  show (Object Class{}) = "<Class>"
  show (Object Module{}) = "<Module>"


instance Show Primitive where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VString st) = show st
  show (VAtom a) = ':':a
  show (VArray a) = show $ toList a
  show (VHash h) = show $ h



