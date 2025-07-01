module Web.Minion.Introspect.Internal where

import Data.Kind
import Data.Typeable (Typeable)
import Data.Void (Void)
import Unsafe.Coerce (unsafeCoerce)

data Introspected
  = QueryParam
  | Capture
  | Captures
  | Header
  | Request
  | Response
  | Description

class AbsolutelyNothing a
instance AbsolutelyNothing a

data IntrospectionDictionary c where
  IntrospectionDictionary :: (c) => IntrospectionDictionary c

data ErasedIntrospectionDictionary where
  ErasedIntrospectionDictionary :: IntrospectionDictionary c -> ErasedIntrospectionDictionary

type family PackOfIntrospections ii t x where
  PackOfIntrospections (i ': ii) t x = IntrospectionDictionary (IntrospectionFor i t x) ': PackOfIntrospections ii t x
  PackOfIntrospections '[] t x = '[]

withIntrospection :: forall i ii t x a. (Elem i ii, Introspection ii t x) => ((IntrospectionFor i t x) => a) -> a
withIntrospection a = case introspections @ii @t @x !! indexOf @i @ii of
  ErasedIntrospectionDictionary dict -> case unsafeCoerce @_ @(IntrospectionDictionary (IntrospectionFor i t x)) dict of
    IntrospectionDictionary -> a

class Introspection ii t x where
  introspections :: [ErasedIntrospectionDictionary]

instance (IntrospectionFor i t x, Introspection ii t x) => Introspection (i ': ii) t x where
  introspections = ErasedIntrospectionDictionary (IntrospectionDictionary @(IntrospectionFor i t x)) : introspections @ii @t @x

instance (Typeable x) => Introspection '[] t x where
  introspections = []

class Elem i ii where
  indexOf :: Int

instance Elem i (i ': ii) where
  indexOf = 0

instance {-# OVERLAPPABLE #-} (Elem i ii) => Elem i (x ': ii) where
  indexOf = 1 + indexOf @i @ii

class HasIntrospection i where
  type IntrospectionFor i (v :: Introspected) :: Type -> Constraint

instance HasIntrospection Void where
  type IntrospectionFor Void v = AbsolutelyNothing
