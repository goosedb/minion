module Web.Minion.Response.Union where

import Data.Kind
import Web.Minion

data Union (as :: [Type]) where
  This :: !a -> Union (a ': as)
  That :: !(Union as) -> Union (a ': as)

class Inject a as where
  inject :: a -> Union as

instance Inject a (a ': as) where
  inject = This

instance {-# OVERLAPPABLE #-} (Inject a as) => Inject a (x ': as) where
  inject = That . inject

instance (CanRespond a, CanRespond (Union as)) => CanRespond (Union (a ': as)) where
  canRespond h = canRespond @a h && canRespond @(Union as) h

instance CanRespond (Union '[]) where
  canRespond _ = True

instance (ToResponse m a, Monad m, ToResponse m (Union as)) => ToResponse m (Union (a ': as)) where
  toResponse accept = \case
    This a -> toResponse accept a
    That as -> toResponse accept as

instance (Monad m) => ToResponse m (Union '[]) where
  toResponse _ = error "impossible"
