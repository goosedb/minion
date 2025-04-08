{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Web.Minion.Args.Internal where

import Data.Functor (($>))
import Data.Kind (Type)
import Data.Void (Void)
import Web.Minion.Request (IsRequest (..))

data (a :: Type) :+ (b :: Type)

infixl 9 :+
data HList ts where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

-- | Reversed HList
data RHList ts where
  RHNil :: RHList Void
  (:#!) :: t -> RHList ts -> RHList (ts :+ t)

type family MapElem ts t t' where
  MapElem (ts :+ t) t t' = ts :+ t'
  MapElem (ts :+ x) t t' = MapElem ts t t' :+ x
  MapElem Void t t' = Void

infixr 1 :#
infixr 1 :#!

deriving instance Show (RHList Void)
deriving instance (Show (RHList as), Show a) => Show (RHList (as :+ a))

deriving instance Show (HList '[])
deriving instance (Show (HList as), Show a) => Show (HList (a ': as))

type family RevToList ts where
  RevToList Void = '[]
  RevToList (as :+ a) = a ': RevToList as

class RHListToHList (ts :: Type) where
  type HListTypes ts :: [Type]
  revHListToList :: RHList ts -> HList (HListTypes ts)

instance RHListToHList Void where
  type HListTypes Void = '[]
  revHListToList _ = HNil

instance (RHListToHList as) => RHListToHList (as :+ a) where
  type HListTypes (as :+ a) = a ': HListTypes as
  revHListToList (a :#! as) = a :# revHListToList as

class GetByType t ts where
  getByType :: HList ts -> t

instance (GetByType t ts) => GetByType t (x ': ts) where
  getByType (_ :# as) = getByType @t @ts as

instance {-# OVERLAPPING #-} GetByType t (t ': ts) where
  getByType (a :# _) = a

class Reverse' (l1 :: [Type]) (l2 :: [Type]) (l3 :: [Type]) | l1 l2 -> l3 where
  reverse' :: HList l1 -> HList l2 -> HList l3

instance Reverse' '[] l2 l2 where
  reverse' _ l = l

instance (Reverse' l (x ': l') z) => Reverse' (x ': l) l' z where
  reverse' (x :# l) l' = reverse' l (x :# l')

class Reverse xs sx | xs -> sx, sx -> xs where
  reverseHList :: HList xs -> HList sx

instance
  ( Reverse' xs '[] sx
  , Reverse' sx '[] xs
  ) =>
  Reverse xs sx
  where
  reverseHList l = reverse' l HNil

data Lenient e
data Strict

data Required
data Optional

class IsRequired a where
  isRequired :: Bool

instance IsRequired Required where
  isRequired = True

instance IsRequired Optional where
  isRequired = False

class IsLenient a where
  isLenient :: Bool

instance IsLenient (Lenient a) where
  isLenient = True

instance IsLenient Strict where
  isLenient = False

type family Arg presence parsing a where
  Arg Required (Lenient e) a = (Either e a)
  Arg Required Strict a = a
  Arg Optional (Lenient e) a = (Maybe (Either e a))
  Arg Optional Strict a = (Maybe a)

newtype WithHeader presence parsing m a = WithHeader (m (Arg presence parsing a))
newtype WithQueryParam presence parsing m a = WithQueryParam (m (Arg presence parsing a))
newtype WithPiece a = WithPiece a
newtype WithPieces a = WithPieces [a]
newtype WithReq m r = WithReq (m r)
newtype Hide a = Hide a
newtype Computed m a = Computed (m a)

class Hidden m a where
  runHidden :: Hide a -> m ()

instance (Monad m) => Hidden m (WithHeader a b m v) where
  runHidden (Hide (WithHeader a)) = a $> ()

instance (Monad m) => Hidden m (WithQueryParam a b m v) where
  runHidden (Hide (WithQueryParam a)) = a $> ()

instance (Monad m) => Hidden m (WithPiece a) where
  runHidden (Hide (WithPiece _)) = pure ()

instance (Monad m) => Hidden m (WithPieces a) where
  runHidden (Hide (WithPieces _)) = pure ()

instance (Monad m) => Hidden m (WithReq m a) where
  runHidden (Hide (WithReq a)) = a $> ()

instance (Hidden m a) => Hidden m (Hide a) where
  runHidden (Hide a) = runHidden a

class FunArgs (ts :: [Type]) where
  type ts ~> r :: Type

  apply :: (ts ~> r) -> HList ts -> r

type HandleArgs ts st m =
  ( FunArgs (DelayedArgs st)
  , RHListToHList ts
  , Reverse (HListTypes ts) st
  , RunDelayed st m
  , Monad m
  )

instance FunArgs '[] where
  type '[] ~> r = r
  {-# INLINE apply #-}
  apply a _ = a

instance (RunDelayed as m) => RunDelayed (Computed m a ': as) m where
  type DelayedArgs (Computed m a ': as) = a ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (Computed hIO :# as) = do
    h <- hIO
    rest <- runDelayed as
    pure $ h :# rest

instance (FunArgs as) => FunArgs (a ': as) where
  type (a ': as) ~> r = a -> as ~> r
  {-# INLINE apply #-}
  apply a (x :# xs) = apply (a x) xs

class (Monad m) => RunDelayed ts m where
  type DelayedArgs ts :: [Type]
  runDelayed :: HList ts -> m (HList (DelayedArgs ts))

instance (Monad m) => RunDelayed '[] m where
  type DelayedArgs '[] = '[]
  {-# INLINE runDelayed #-}
  runDelayed :: (Monad m) => HList '[] -> m (HList (DelayedArgs '[]))
  runDelayed HNil = pure HNil

instance (RunDelayed as m) => RunDelayed (WithHeader required lenient m a ': as) m where
  type DelayedArgs (WithHeader required lenient m a ': as) = Arg required lenient a ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (WithHeader hIO :# as) = do
    h <- hIO
    rest <- runDelayed as
    pure $ h :# rest

instance (RunDelayed as m, IsRequest r) => RunDelayed (WithReq m r ': as) m where
  type DelayedArgs (WithReq m r ': as) = RequestValue r ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (WithReq hIO :# as) = do
    h <- hIO
    rest <- runDelayed as
    pure $ getRequestValue h :# rest

instance (RunDelayed as m) => RunDelayed (WithQueryParam required lenient m a ': as) m where
  type DelayedArgs (WithQueryParam required lenient m a ': as) = Arg required lenient a ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (WithQueryParam a :# as) = do
    a' <- a
    rest <- runDelayed as
    pure $ a' :# rest

instance (RunDelayed as m) => RunDelayed (WithPiece a ': as) m where
  type DelayedArgs (WithPiece a ': as) = a ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (WithPiece a :# as) = do
    rest <- runDelayed as
    pure $ a :# rest

instance (RunDelayed as m) => RunDelayed (WithPieces a ': as) m where
  type DelayedArgs (WithPieces a ': as) = [a] ': DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (WithPieces a :# as) = do
    rest <- runDelayed as
    pure $ a :# rest

instance (RunDelayed as m, Hidden m a) => RunDelayed (Hide a ': as) m where
  type DelayedArgs (Hide a ': as) = DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (a :# as) = runHidden a >> runDelayed as
