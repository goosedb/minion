module Web.Minion.OpenApi3.Ui (
  openapi3,
  OpenApi3Config (..),
) where

import Web.Minion hiding (description)

import Control.Lens hiding (index)
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.HttpApiData (ToHttpApiData (..))
import Web.Minion.Files (indexTemplate, ui)
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.Html (Html)
import Web.Minion.OpenApi3
import Web.Minion.Response (Redirect (Redirect))
import Web.Minion.Response.Status (OK)
import Web.Minion.Static

data OpenApi3Config = OpenApi3Config
  { routePrefix :: String
  , openapi3File :: FilePath
  , staticDir :: FilePath
  }

openapi3 ::
  forall m ts st i.
  (HandleArgs ts st m, MonadIO m, I.Elem OpenApi3 i) =>
  OpenApi3Config ->
  Router' i ts m ->
  Router' '[] Void m
openapi3 OpenApi3Config{..} r =
  fromString routePrefix
    /> [ handle @Redirect GET (pure $ Redirect indexHtmlPath)
       , fromString openapi3File /> handleJson GET (pure $ generateOpenApi3 r)
       , fromString staticDir /> [staticFiles defaultExtsMap ui', index_html /> getIndex]
       ]
 where
  indexHtmlPath = Text.pack $ routePrefix <> "/" <> staticDir <> "/" <> index_html

  index_html :: (IsString s) => s
  index_html = "index.html"
  ui' = map (first (dropWhile (== '/'))) ui
  getIndex = handle GET do
    pure $ RespBody @OK @'[Html] index

  index =
    indexTemplate
      & Text.replace "SWAGGER_UI_SCHEMA" (toUrlPiece openapi3File)
      & Text.replace "SWAGGER_UI_DIR" (toUrlPiece staticDir)
      & preEscapedToMarkup
      & renderHtml
