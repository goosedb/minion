module Web.Minion.Embed where

import Control.Monad.IO.Class (MonadIO (..))
import FileEmbedLzma.Untyped
import Language.Haskell.TH
import System.Directory

indexName :: FilePath
indexName = "index.html.tmpl"

fixPath :: (MonadIO m) => FilePath -> m FilePath
fixPath path = liftIO do
  currDirectory <- getCurrentDirectory
  curDirectoryContents <- getDirectoryContents currDirectory
  pure
    if indexName `elem` curDirectoryContents
      then currDirectory <> "/" <> path
      else currDirectory <> "/minion-openapi3/" <> path

embedIndex :: Q Exp
embedIndex = fixPath indexName >>= embedText

embedUi :: Q Exp
embedUi = fixPath "ui" >>= embedRecursiveDir
