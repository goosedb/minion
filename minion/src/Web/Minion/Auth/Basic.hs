module Web.Minion.Auth.Basic where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Base64 qualified as Bytes.Base64
import Data.Function ((&))
import Data.String (IsString (..))
import Data.String.Conversions (ConvertibleStrings (convertString))
import Network.HTTP.Types.Header qualified as Http
import Network.Wai qualified as Wai
import Web.Minion

newtype BasicAuthSettings m a = BasicAuthSettings
  { check :: MakeError -> BasicAuth -> m (AuthResult a)
  }

data BasicAuth = BasicAuth
  { username :: Username
  , password :: Password
  }
  deriving (Eq, Ord)

newtype Username = Username {rawUsername :: Bytes.ByteString}
  deriving (Eq, Ord)

instance IsString Username where
  fromString = Username . convertString

instance IsString Password where
  fromString = Password . convertString

newtype Password = Password {rawPassword :: Bytes.ByteString}
  deriving (Eq, Ord)

data Basic

instance (Monad m) => IsAuth Basic m a where
  type Settings Basic m a = BasicAuthSettings m a
  toAuth BasicAuthSettings{..} buildError req = do
    mbBasicAuth <- runMaybeT do
      authHeader <- Wai.requestHeaders req & lookup Http.hAuthorization & hoistMaybe
      base64 <- Bytes.stripPrefix "Basic " authHeader & hoistMaybe
      let decoded = base64 & Bytes.Base64.decodeLenient
      -- 58 is ':'
      [Username -> username, Password -> password] <- Bytes.split 58 decoded & pure & hoistMaybe
      pure BasicAuth{..}
    maybe
      do pure Indefinite
      do check (buildError req)
      do mbBasicAuth
   where
    hoistMaybe = MaybeT . pure
