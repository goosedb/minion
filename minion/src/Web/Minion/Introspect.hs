module Web.Minion.Introspect where

import Data.Kind
import Data.Void (Void)

data Introspected
  = QueryParam
  | Capture
  | Captures
  | Header
  | Request
  | Response
  | Description

type family Introspection i (t :: Introspected) :: Type -> Constraint

type instance Introspection Void QueryParam = AbsolutelyNothing
type instance Introspection Void Capture = AbsolutelyNothing
type instance Introspection Void Captures = AbsolutelyNothing
type instance Introspection Void Header = AbsolutelyNothing
type instance Introspection Void Request = AbsolutelyNothing
type instance Introspection Void Response = AbsolutelyNothing

class AbsolutelyNothing a
instance AbsolutelyNothing a
