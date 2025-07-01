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

data AddHeader name a = AddHeader a | OverwriteHeader a
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
    Wai.mapResponseHeaders (\headers' -> unwindHeaders @hs headers' headers) <$> toResponse accept body

class UnwindHeaders hs where
  unwindHeaders :: [Http.Header] -> HList hs -> [Http.Header]

instance UnwindHeaders '[] where
  {-# INLINE unwindHeaders #-}
  unwindHeaders :: [Http.Header] -> HList '[] -> [Http.Header]
  unwindHeaders = const

instance (UnwindHeaders hs, KnownSymbol name, ToHttpApiData typ) => UnwindHeaders (AddHeader name typ ': hs) where
  {-# INLINE unwindHeaders #-}
  unwindHeaders headers = \case
    AddHeader val :# hs -> unwindHeaders ((name, toHeader @typ val) : headers) hs
    OverwriteHeader val :# hs -> unwindHeaders ((name, toHeader @typ val) : filter ((/= name) . fst) headers) hs
   where
    name = CI.mk $ Text.Encode.encodeUtf8 $ Text.pack $ symbolVal (Proxy @name)
