{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Web.Minion.Response.Header where

import Data.ByteString qualified as Bytes
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.HttpApiData (ToHttpApiData (..))
import Web.Minion.Args.Internal
import Web.Minion.Response (CanRespond (..), ToResponse (..))

newtype AddHeader name a = AddHeader a
  deriving (Functor)

newtype RawHeaderValue = RawHeaderValue Bytes.ByteString

instance ToHttpApiData RawHeaderValue where
  {-# INLINE toUrlPiece #-}
  toUrlPiece = coerce Text.Encode.decodeUtf8
  {-# INLINE toHeader #-}
  toHeader = coerce

data AddHeaders hs a = AddHeaders
  { headers :: HList hs
  , body :: a
  }
  deriving (Functor)

instance (CanRespond a) => CanRespond (AddHeaders hs a) where
  {-# INLINE canRespond #-}
  canRespond = canRespond @a

instance (ToResponse m a, UnwindHeaders hs, Monad m) => ToResponse m (AddHeaders hs a) where
  {-# INLINE toResponse #-}
  toResponse accept AddHeaders{..} =
    Wai.mapResponseHeaders (unwindHeaders @hs headers <>) <$> toResponse accept body

class UnwindHeaders hs where
  unwindHeaders :: HList hs -> [Http.Header]

instance UnwindHeaders '[] where
  {-# INLINE unwindHeaders #-}
  unwindHeaders :: HList '[] -> [Http.Header]
  unwindHeaders _ = []

instance (UnwindHeaders hs, KnownSymbol name, ToHttpApiData typ) => UnwindHeaders (AddHeader name typ ': hs) where
  {-# INLINE unwindHeaders #-}
  unwindHeaders (AddHeader val :# hs) =
    (CI.mk $ Text.Encode.encodeUtf8 $ Text.pack $ symbolVal (Proxy @name), toHeader @typ val)
      : unwindHeaders @hs hs
