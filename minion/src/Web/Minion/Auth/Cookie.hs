module Web.Minion.Auth.Cookie where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Cookie qualified as Cookie
import Web.Minion.Auth
import Web.Minion.Router (MakeError)

class IsCookie a where
  cookieName :: Text

class (IsCookie a) => FromCookie a where
  decodeCookie :: Text -> Either Text a

newtype CookieAuthSettings m cookie a = CookieAuthSettings
  { check :: MakeError -> cookie -> m (AuthResult a)
  }

data Cookie (a :: Type) = Cookie

instance (Monad m, FromCookie cookie) => IsAuth (Cookie cookie) m a where
  type Settings (Cookie cookie) m a = CookieAuthSettings m cookie a
  toAuth CookieAuthSettings{..} errorBuilder req = do
    let mbRawCookie =
          Wai.requestHeaders req
            & lookup Http.hCookie
            >>= lookup (cookieName @cookie) . Cookie.parseCookiesText
    case mbRawCookie of
      Nothing -> pure Indefinite
      Just rawCookie -> case decodeCookie @cookie rawCookie of
        Left e -> pure $ BadAuth e
        Right v -> check (errorBuilder req) v
