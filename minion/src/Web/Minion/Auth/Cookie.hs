module Web.Minion.Auth.Cookie where
import Data.Text (Text)
import Web.Minion.Auth
import Web.Minion.Router (MakeError)
import Data.Kind (Type)
import qualified Network.Wai as Wai
import Data.Function ((&))
import qualified Network.HTTP.Types as Http
import qualified Web.Cookie as Cookie

class IsCookie a where
  parseCookie :: Text -> Either Text a
  cookieName :: Text

newtype CookieAuthSettings m cookie a = CookieAuthSettings
  { check :: MakeError -> cookie -> m (AuthResult a)
  }

data Cookie (a :: Type) 

instance (Monad m, IsCookie cookie) => IsAuth (Cookie cookie) m a where
  type Settings (Cookie cookie) m a = CookieAuthSettings m cookie a
  toAuth CookieAuthSettings{..} errorBuilder req = do 
    let mbRawCookie = Wai.requestHeaders req & lookup Http.hCookie >>= lookup (cookieName @cookie) . Cookie.parseCookiesText 
    case mbRawCookie of
      Nothing -> pure Indefinite
      Just rawCookie -> case parseCookie @cookie rawCookie of
        Left e -> pure $ BadAuth e
        Right v -> check (errorBuilder req) v