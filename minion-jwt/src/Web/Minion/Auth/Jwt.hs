module Web.Minion.Auth.Jwt where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.JOSE qualified as Jose
import Crypto.JWT (JWTError)
import Crypto.JWT qualified as Jose
import Data.Aeson (FromJSON (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Time qualified as Time
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Header qualified as Http
import Network.Wai qualified as Wai
import Web.Minion
import Web.Minion.Client.Types (
  ApplyAuth (..),
  AuthParam,
 )

data JwtAuthSettings m payload a = JwtAuthSettings
  { getNow :: m Time.UTCTime
  , jwk :: m Jose.JWK
  , validationSettings :: m Jose.JWTValidationSettings
  , check :: MakeError -> Either JWTError (JwtPayload payload) -> m (AuthResult a)
  }

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

data JwtPayload a = JwtPayload
  { claims :: Jose.ClaimsSet
  , payload :: a
  }

instance Jose.HasClaimsSet (JwtPayload a) where
  claimsSet f JwtPayload{..} = f claims <&> \c -> JwtPayload{claims = c, ..}

instance (FromJSON a) => FromJSON (JwtPayload a) where
  parseJSON v =
    JwtPayload
      <$> parseJSON v
      <*> parseJSON v

data Bearer payload

instance (MonadIO m, FromJSON payload) => IsAuth (Bearer payload) m a where
  type Settings (Bearer payload) m a = JwtAuthSettings m payload a
  toAuth JwtAuthSettings{..} buildError req = do
    jwk_ <- jwk
    now <- getNow
    settings <- validationSettings
    payload <- Jose.runJOSE $ runMaybeT do
      authHeader <- Wai.requestHeaders req & lookup Http.hAuthorization & hoistMaybe
      compact <- hoistMaybe $ Bytes.stripPrefix prefix authHeader
      jwt <- Jose.decodeCompact $ Bytes.Lazy.fromStrict compact
      Jose.verifyJWTAt settings jwk_ now jwt
    case payload of
      Left e -> check (buildError req) (Left e)
      Right Nothing -> pure Indefinite
      Right (Just (v :: JwtPayload payload)) -> check (buildError req) (Right v)
   where
    prefix = "Bearer "

    hoistMaybe = MaybeT . pure

newtype JwtToken = JwtToken Bytes.ByteString

instance ApplyAuth (Bearer a) where
  type AuthParam (Bearer a) = JwtToken
  applyAuth (JwtToken token) req = pure do
    req{Http.requestHeaders = (Http.hAuthorization, "Bearer " <> token) : Http.requestHeaders req}
