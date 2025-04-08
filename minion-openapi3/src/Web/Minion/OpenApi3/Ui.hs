module Web.Minion.OpenApi3.Ui (
  openapi3,
  OpenApi3Config (..),
) where

import Web.Minion hiding (description)

import Control.Lens hiding (index)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.HttpApiData (ToHttpApiData (..))
import Web.Minion.Error (redirect)
import Web.Minion.Files (indexTemplate, ui)
import Web.Minion.OpenApi3
import Web.Minion.Response.Header qualified as Header
import Web.Minion.Static

data OpenApi3Config = OpenApi3Config
  { openapi3File :: FilePath
  , staticDir :: FilePath
  }

openapi3 ::
  forall m ts st.
  (HandleArgs ts st m, MonadIO m) =>
  OpenApi3Config ->
  Router' OpenApi3 ts m ->
  Router' Void Void m
openapi3 OpenApi3Config{..} r =
  [ handle @NoBody GET (liftIO $ redirect $ Text.Encoding.encodeUtf8 $ Text.pack $ staticDir <> "/" <> index_html)
  , fromString openapi3File /> handleJson GET (pure $ generateOpenApi3 r)
  , fromString staticDir /> [staticFiles defaultExtsMap ui', index_html /> getIndex]
  ]
 where
  index_html :: (IsString s) => s
  index_html = "index.html"
  ui' = map (first (dropWhile (== '/'))) ui
  getIndex = handle GET do
    pure $
      Header.AddHeaders
        { headers = Header.AddHeader @"Content-Type" (Header.RawHeaderValue "text/html") :# HNil
        , body = LazyBytes index
        }

  index =
    indexTemplate
      & Text.replace "SWAGGER_UI_SCHEMA" (toUrlPiece openapi3File)
      & Text.replace "SWAGGER_UI_DIR" (toUrlPiece staticDir)
      & preEscapedToMarkup
      & renderHtml
