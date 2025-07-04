module Web.Minion.Static (StaticFileResponse, staticFiles, defaultExtsMap) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Map.Strict qualified as Map
import Data.Void
import Network.HTTP.Media
import System.FilePath (takeExtension)
import Web.Minion
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.OctetStream (Bytes, OctetStream)
import Web.Minion.Response.Header qualified as Header

type StaticFileResponse = Header.AddHeaders '[Header.AddHeader "Content-Type" Header.RawHeaderValue] (RespBody Ok '[OctetStream Bytes] Bytes.Lazy.ByteString)

{-# INLINE staticFiles #-}
staticFiles ::
  (Monad m, I.Introspection i I.Response StaticFileResponse, MonadIO m) =>
  -- | see 'defaultExtsMap'
  Map.Map String MediaType ->
  [(FilePath, Bytes.ByteString)] ->
  Router' i Void m
staticFiles extsMap = foldMap \(path, content) ->
  let contentType = getContentType extsMap (takeExtension path)
   in piece path /> handle @StaticFileResponse GET do
        pure
          Header.AddHeaders
            { headers = Header.OverwriteHeader @"Content-Type" (Header.RawHeaderValue contentType) :# HNil
            , body = RespBody $ Bytes.Lazy.fromStrict content
            }

getContentType :: (RenderHeader a) => Map.Map String a -> FilePath -> Bytes.ByteString
getContentType extsMap path = maybe "application/octet-stream" renderHeader $ Map.lookup (takeExtension path) extsMap

defaultExtsMap :: Map.Map String MediaType
defaultExtsMap =
  Map.fromList
    [ ".aac" ~> "audio/aac"
    , ".abw" ~> "application/x-abiword"
    , ".arc" ~> "application/x-freearc"
    , ".avif" ~> "image/avif"
    , ".avi" ~> "video/x-msvideo"
    , ".azw" ~> "application/vnd.amazon.ebook"
    , ".bin" ~> "application/octet-stream"
    , ".bmp" ~> "image/bmp"
    , ".bz" ~> "application/x-bzip"
    , ".bz2" ~> "application/x-bzip2"
    , ".cda" ~> "application/x-cdf"
    , ".csh" ~> "application/x-csh"
    , ".css" ~> "text/css"
    , ".csv" ~> "text/csv"
    , ".doc" ~> "application/msword"
    , ".docx" ~> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    , ".eot" ~> "application/vnd.ms-fontobject"
    , ".epub" ~> "application/epub+zip"
    , ".gz" ~> "application/gzip"
    , ".gif" ~> "image/gif"
    , ".htm" ~> "text/html"
    , ".ico" ~> "image/vnd.microsoft.icon"
    , ".ics" ~> "text/calendar"
    , ".jar" ~> "application/java-archive"
    , ".jpeg" ~> "image/jpeg"
    , ".jpg" ~> "image/jpeg"
    , ".js" ~> "text/javascript"
    , ".json" ~> "application/json"
    , ".jsonld" ~> "application/ld+json"
    , ".mid" ~> "audio/midi"
    , ".midi" ~> "audio/midi"
    , ".mjs" ~> "text/javascript"
    , ".mp3" ~> "audio/mpeg"
    , ".mp4" ~> "video/mp4"
    , ".mpeg" ~> "video/mpeg"
    , ".mpkg" ~> "application/vnd.apple.installer+xml"
    , ".odp" ~> "application/vnd.oasis.opendocument.presentation"
    , ".ods" ~> "application/vnd.oasis.opendocument.spreadsheet"
    , ".odt" ~> "application/vnd.oasis.opendocument.text"
    , ".oga" ~> "audio/ogg"
    , ".ogv" ~> "video/ogg"
    , ".ogx" ~> "application/ogg"
    , ".opus" ~> "audio/opus"
    , ".otf" ~> "font/otf"
    , ".png" ~> "image/png"
    , ".pdf" ~> "application/pdf"
    , ".php" ~> "application/x-httpd-php"
    , ".ppt" ~> "application/vnd.ms-powerpoint"
    , ".pptx" ~> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    , ".rar" ~> "application/vnd.rar"
    , ".rtf" ~> "application/rtf"
    , ".sh" ~> "application/x-sh"
    , ".svg" ~> "image/svg+xml"
    , ".tar" ~> "application/x-tar"
    , ".tif" ~> "image/tiff"
    , ".tiff" ~> "image/tiff"
    , ".ts" ~> "video/mp2t"
    , ".ttf" ~> "font/ttf"
    , ".txt" ~> "text/plain"
    , ".vsd" ~> "application/vnd.visio"
    , ".wav" ~> "audio/wav"
    , ".weba" ~> "audio/webm"
    , ".webm" ~> "video/webm"
    , ".webp" ~> "image/webp"
    , ".woff" ~> "font/woff"
    , ".woff2" ~> "font/woff2"
    , ".xhtml" ~> "application/xhtml+xml"
    , ".xls" ~> "application/vnd.ms-excel"
    , ".xlsx" ~> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    , ".xml" ~> "application/xml"
    , ".xul" ~> "application/vnd.mozilla.xul+xml"
    , ".zip" ~> "application/zip"
    , ".3gp" ~> "video/3gpp"
    , ".3g2" ~> "video/3gpp2"
    , ".7z" ~> "application/x-7z-compressed"
    ]
 where
  (~>) :: String -> MediaType -> (String, MediaType)
  (~>) = (,)
