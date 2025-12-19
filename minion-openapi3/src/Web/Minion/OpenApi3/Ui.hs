module Web.Minion.OpenApi3.Ui (
  openapi3,
) where

import Web.Minion hiding (description)

import Control.Lens hiding (index, (.>))
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Network.Wai qualified as Wai
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Minion.Files (indexTemplate, ui)
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.Html (Html)
import Web.Minion.OpenApi3
import Web.Minion.Request.Body (IsRequest (..))
import Web.Minion.Response (Redirect (Redirect))
import Web.Minion.Static

newtype CurrentPath = CurrentPath Text.Text

instance IsRequest CurrentPath where
  type RequestValue CurrentPath = CurrentPath
  getRequestValue = id

currentPath :: (Monad m) => ValueCombinator '[] (WithReq m CurrentPath) ts m
currentPath = Request \_ req -> pure $ CurrentPath $ Text.intercalate "/" $ Wai.pathInfo req

openapi3 ::
  forall m ts i.
  ( HandleArgs ts m
  , MonadIO m
  , I.Elem OpenApi3 i
  ) =>
  Router' i ts m ->
  Router Void m
openapi3 r =
  [ currentPath .> handle @Redirect GET (pure . Redirect . indexHtmlPath)
  , "openapi.json" /> handleBody @Ok @'[Json] GET (pure $ generateOpenApi3 r)
  , "static" /> [staticFiles defaultExtsMap ui', index_html /> getIndex]
  ]
 where
  indexHtmlPath (CurrentPath route) = Text.pack $ Text.unpack route <> "/static/" <> index_html

  index_html :: (IsString s) => s
  index_html = "index.html"
  ui' = map (first (dropWhile (== '/'))) ui
  getIndex = handle GET do
    pure $ RespBody @Ok @'[Html] index

  index =
    indexTemplate
      & Text.replace "SWAGGER_UI_SCHEMA" "openapi.json"
      & Text.replace "SWAGGER_UI_DIR" "static"
      & preEscapedToMarkup
      & renderHtml
