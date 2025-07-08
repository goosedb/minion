module Web.Minion.Introspect (
  -- $comment
  Introspected (..),
  AbsolutelyNothing,
  withIntrospection,
  withElem,
  Elem (),
  MaybeElem (),
  HasIntrospection (..),
  Introspection,
) where

import Web.Minion.Introspect.Internal

{- $comment
Tools for router introspection. See README and example "Web.Minion.Examples.Introspection"
-}
