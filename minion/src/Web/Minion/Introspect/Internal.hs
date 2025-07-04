module Web.Minion.Introspect.Internal where

import Data.Data (Proxy (Proxy))
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, eqT)
import Data.Void (Void)
import GHC.Base (WithDict (withDict))
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

withIntrospection :: forall i ii t x a. (Elem i ii, Introspection ii t x) => ((IntrospectionFor i t x) => a) -> a
withIntrospection a = case introspections @ii @t @x !! indexOf @i @ii of
  ErasedIntrospectionDictionary dict -> case unsafeCoerce @_ @(IntrospectionDictionary (IntrospectionFor i t x)) dict of
    IntrospectionDictionary -> a

withElem :: forall (i :: Type) ii a. (MaybeElem ii, Typeable i) => a -> ((Elem i ii) => a) -> a
withElem fallback action = fromMaybe fallback $ castElem @i @ii action

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

class MaybeElem (ii :: [Type]) where
  maybeIndexOf :: forall (i :: Type). (Typeable i) => Proxy i -> Maybe Int

instance MaybeElem '[] where
  maybeIndexOf _ = Nothing

instance (Typeable x, MaybeElem ii) => MaybeElem ((x :: Type) ': ii) where
  maybeIndexOf (Proxy @i) = case eqT @i @x of
    Just _ -> Just 0
    Nothing -> (1 +) <$> maybeIndexOf @ii (Proxy @i)

castElem :: forall (i :: Type) ii a. (MaybeElem ii, Typeable i) => ((Elem i ii) => a) -> Maybe a
castElem action = case maybeIndexOf @ii (Proxy @i) of
  Just index -> Just $ withDict @(Elem i ii) index action
  Nothing -> Nothing

class HasIntrospection i where
  type IntrospectionFor i (v :: Introspected) :: Type -> Constraint

instance HasIntrospection Void where
  type IntrospectionFor Void v = AbsolutelyNothing
