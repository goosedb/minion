{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Web.Minion.Args.Internal where

import Data.Functor (($>))
import Data.Kind (Type)
import Data.Void (Void)
import GHC.TypeError qualified as TE
import Web.Minion.Request (IsRequest (..))

data (a :: Type) :+ (b :: Type)

infixl 9 :+
data HList ts where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)


class Bite piece args where
  bite :: Args args -> (Args piece, Args (Rest piece args))

type family Rest piece args :: Type where
  Rest ts ts = Void
  Rest ts (ts' :+ t) = Rest ts ts' :+ t

instance {-# OVERLAPPING #-} Bite (ts :+ t) (ts :+ t) where
  bite a = (a, ANil)

instance (Bite (ts :+ x) ts', Rest (ts :+ x) (ts' :+ t) ~ Rest (ts :+ x) ts' :+ t) => Bite (ts :+ x) (ts' :+ t) where

  bite (a :#! as) = 
    let (x, y) = bite @(ts :+ x) @ts' as
    in (x, a :#! y)

-- | R(eversed) HList
data Args ts where
  ANil :: Args Void
  (:#!) :: t -> Args ts -> Args (ts :+ t)

type family MapElem ts t t' where
  MapElem (ts :+ t) t t' = ts :+ t'
  MapElem (ts :+ x) t t' = MapElem ts t t' :+ x
  MapElem Void t t' = Void

infixr 1 :#
infixr 1 :#!

deriving instance Show (Args Void)
deriving instance (Show (Args as), Show a) => Show (Args (as :+ a))

-- deriving instance Show (HList '[])
-- deriving instance (Show (HList as), Show a) => Show (HList (a ': as))

type family RevToList ts where
  RevToList Void = '[]
  RevToList (as :+ a) = a ': RevToList as

-- class ArgsToHList (ts :: Type) where
--   type HListTypes ts :: [Type]
--   revHListToList :: Args ts -> HList (HListTypes ts)

-- instance ArgsToHList Void where
--   type HListTypes Void = '[]
--   revHListToList _ = HNil

-- instance (ArgsToHList as) => ArgsToHList (as :+ a) where
--   type HListTypes (as :+ a) = a ': HListTypes as
--   revHListToList (a :#! as) = a :# revHListToList as

class GetByType t ts where
  getByType :: Args ts -> t

instance (GetByType t ts) => GetByType t (ts :+ x) where
  getByType (_ :#! as) = getByType @t @ts as

instance {-# OVERLAPPING #-} GetByType t (ts :+ t) where
  getByType (a :#! _) = a

instance (TE.TypeError (TE.Text "Can't find " TE.:<>: TE.ShowType t TE.:<>: TE.Text " in context")) => GetByType t Void where
  getByType _ = undefined

-- class Reverse' (l1 :: [Type]) (l2 :: [Type]) (l3 :: [Type]) | l1 l2 -> l3 where
--   reverse' :: HList l1 -> HList l2 -> HList l3

-- instance Reverse' '[] l2 l2 where
--   reverse' _ l = l

-- instance (Reverse' l (x ': l') z) => Reverse' (x ': l) l' z where
--   reverse' (x :# l) l' = reverse' l (x :# l')

-- class Reverse xs sx | xs -> sx, sx -> xs where
--   reverseHList :: HList xs -> HList sx

-- instance
--   ( Reverse' xs '[] sx
--   , Reverse' sx '[] xs
--   ) =>
--   Reverse xs sx
--   where
--   reverseHList l = reverse' l HNil

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

class FunArgs (ts :: Type) where
  type ts ~> r :: Type
  apply :: (ts ~> r) -> Args ts -> r

type HandleArgs ts m =
  ( FunArgs (DelayedArgs ts)
  , RunDelayed ts m
  , Monad m
  )

instance FunArgs Void where
  type Void ~> r = r
  {-# INLINE apply #-}
  apply a _ = a


instance (FunArgs as) => FunArgs (as :+ a) where
  type (as :+ a) ~> r = as ~> (a -> r)
  {-# INLINE apply #-}
  apply a (x :#! xs) = apply a xs x

class (Monad m) => RunDelayed ts m where
  type DelayedArgs ts :: Type
  runDelayed :: Args ts -> m (Args (DelayedArgs ts))

instance (Monad m) => RunDelayed Void m where
  type DelayedArgs Void = Void
  {-# INLINE runDelayed #-}
  runDelayed :: (Monad m) => Args Void -> m (Args (DelayedArgs Void))
  runDelayed ANil = pure ANil

instance (RunDelayed as m) => RunDelayed (as :+ WithHeader required lenient m a ) m where
  type DelayedArgs (as :+ WithHeader required lenient m a) = DelayedArgs as :+ Arg required lenient a
  {-# INLINE runDelayed #-}
  runDelayed (WithHeader hIO :#! as) = do
    rest <- runDelayed as
    h <- hIO
    pure $ h :#! rest

instance (RunDelayed as m, IsRequest r) => RunDelayed (as :+ WithReq m r ) m where
  type DelayedArgs (as :+ WithReq m r) =DelayedArgs as :+ RequestValue r 
  {-# INLINE runDelayed #-}
  runDelayed (WithReq hIO :#! as) = do
    rest <- runDelayed as
    h <- hIO
    pure $ getRequestValue h :#! rest

instance (RunDelayed as m) => RunDelayed (as :+ WithQueryParam required lenient m a) m where
  type DelayedArgs (as :+ WithQueryParam required lenient m a) = DelayedArgs as :+  Arg required lenient a
  {-# INLINE runDelayed #-}
  runDelayed (WithQueryParam a :#! as) = do
    rest <- runDelayed as
    a' <- a
    pure $ a' :#! rest

instance (RunDelayed as m) => RunDelayed (as :+ WithPiece a) m where
  type DelayedArgs ( as :+ WithPiece a) = DelayedArgs as :+ a 
  {-# INLINE runDelayed #-}
  runDelayed (WithPiece a :#! as) = do
    rest <- runDelayed as
    pure $ a :#! rest

instance (RunDelayed as m) => RunDelayed (as :+ WithPieces a) m where
  type DelayedArgs (as :+ WithPieces a) = DelayedArgs as :+ [a]
  {-# INLINE runDelayed #-}
  runDelayed (WithPieces a :#! as) = do
    rest <- runDelayed as
    pure $ a :#! rest

instance (RunDelayed as m, Hidden m a) => RunDelayed (as :+ Hide a) m where
  type DelayedArgs (as :+ Hide a) = DelayedArgs as
  {-# INLINE runDelayed #-}
  runDelayed (a :#! as) = runDelayed as <* runHidden a
