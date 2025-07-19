module Web.Minion.Auth.Jwt where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.JOSE qualified as Jose
import Crypto.JWT (JWTError)
import Crypto.JWT qualified as Jose
import Data.Aeson (FromJSON (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Header qualified as Http
import Network.Wai qualified as Wai
import Web.Minion
import Web.Minion.Auth.Cookie
import Web.Minion.Client.Types (
  ApplyAuth (..),
  AuthParam,
 )

defaultJwtAuthSettings ::
  (MonadIO m) =>
  m Jose.JWK ->
  -- | Audience predicate
  (Jose.StringOrURI -> Bool) ->
  (MakeError -> Either JWTError (JwtPayload payload) -> m (AuthResult a)) ->
  JwtAuthSettings m payload a
defaultJwtAuthSettings jwk audCheck check =
  JwtAuthSettings
    { getNow = liftIO Time.getCurrentTime
    , jwk = jwk
    , validationSettings = pure (Jose.defaultJWTValidationSettings audCheck)
    , check = check
    }

cookieJwtAuthSettings :: forall name payload m a. (Monad m, FromJSON payload) => JwtAuthSettings m payload a -> CookieAuthSettings m (JWTCookie name) a
cookieJwtAuthSettings settings =
  CookieAuthSettings
    { check = \makeError (JWTCookie token) -> checkJwt makeError settings (Text.encodeUtf8 token)
    }

data Bearer payload

data JwtPayload a = JwtPayload
  { claims :: Jose.ClaimsSet
  , payload :: a
  }

data JwtAuthSettings m payload a = JwtAuthSettings
  { getNow :: m Time.UTCTime
  , jwk :: m Jose.JWK
  , validationSettings :: m Jose.JWTValidationSettings
  , check :: MakeError -> Either JWTError (JwtPayload payload) -> m (AuthResult a)
  }

newtype JWTCookie name = JWTCookie Text

instance (KnownSymbol name) => IsCookie (JWTCookie name) where
  cookieName = Text.pack $ symbolVal (Proxy @name)

instance (KnownSymbol name) => FromCookie (JWTCookie name) where
  decodeCookie = Right . JWTCookie

instance Jose.HasClaimsSet (JwtPayload a) where
  claimsSet f JwtPayload{..} = f claims <&> \c -> JwtPayload{claims = c, ..}

instance (FromJSON a) => FromJSON (JwtPayload a) where
  parseJSON v =
    JwtPayload
      <$> parseJSON v
      <*> parseJSON v

instance (MonadIO m, FromJSON payload) => IsAuth (Bearer payload) m a where
  type Settings (Bearer payload) m a = JwtAuthSettings m payload a
  toAuth settings buildError req = do
    let mbToken = do
          authHeader <- Wai.requestHeaders req & lookup Http.hAuthorization
          Bytes.stripPrefix prefix authHeader
    case mbToken of
      Nothing -> pure Indefinite
      Just token -> checkJwt (buildError req) settings token
   where
    prefix = "Bearer "

newtype JwtToken = JwtToken Bytes.ByteString

instance ApplyAuth (Bearer a) where
  type AuthParam (Bearer a) = JwtToken
  applyAuth (JwtToken token) req = pure do
    req{Http.requestHeaders = (Http.hAuthorization, "Bearer " <> token) : Http.requestHeaders req}

checkJwt :: forall payload m a. (Monad m, FromJSON payload) => MakeError -> JwtAuthSettings m payload a -> Bytes.ByteString -> m (AuthResult a)
checkJwt makeError JwtAuthSettings{..} token = do
  jwk_ <- jwk
  now <- getNow
  settings <- validationSettings
  payload <- Jose.runJOSE do
    jwt <- Jose.decodeCompact $ Bytes.Lazy.fromStrict token
    Jose.verifyJWTAt settings jwk_ now jwt
  check makeError payload
