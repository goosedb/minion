module Web.Minion.Request.Header.Cookie where

import Control.Monad.Catch (MonadThrow)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Web.Cookie qualified as Cookie
import Web.Minion.Args (Strict, WithHeader)
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header (HeaderStrict (..))
import Web.Minion.Router (ValueCombinator)

newtype Cookies = Cookies {cookiesMap :: Map.Map Text Text}

cookies ::
  forall presence m ts i.
  (HeaderStrict presence, MonadThrow m, I.Introspection i I.Header Cookies) =>
  ValueCombinator i (WithHeader presence Strict m Cookies) ts m
cookies = header "Cookie" \_ ->
  pure . Cookies . Map.fromList . concat . F.toList . fmap Cookie.parseCookiesText
