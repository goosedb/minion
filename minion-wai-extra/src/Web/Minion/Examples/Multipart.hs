module Web.Minion.Examples.Multipart (app) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Void
import Network.Wai.Parse (FileInfo (..))
import System.Environment (getArgs)
import Web.Minion
import Web.Minion.Request.Multipart

app :: IO (ApplicationM IO)
app =
  getArgs >>= \case
    ["tmp"] -> do
      putStrLn "Tmp mode running"
      pure do
        serve apiMem
    ["mem"] -> do
      putStrLn "Mem mode running"
      pure do
        \req resp -> runResourceT (serve apiTmp req resp)
    _ -> error "Mode required"

data ReportMem = ReportMem
  { reporter :: Text
  , report :: Bytes.Lazy.ByteString
  }

data ReportTmp = ReportTmp
  { reporter :: Text
  , report :: FilePath
  }

instance FromMultipart Mem ReportMem where
  fromMultipart =
    ReportMem
      <$> (Text.Encode.decodeUtf8 <$> getParam "reporter")
      <*> (fileContent <$> getFile "report")

instance FromMultipart Tmp ReportTmp where
  fromMultipart =
    ReportTmp
      <$> (Text.Encode.decodeUtf8 <$> getParam "reporter")
      <*> (fileContent <$> getFile "report")

apiMem :: Router Void IO
apiMem =
  "api"
    /> "multipart"
    /> multipartBody @Mem @ReportMem
    .> handle @NoBody POST endpoint
 where
  endpoint ReportMem{..} = do
    putStrLn $ "Reporter: " <> Text.unpack reporter
    putStrLn $ "Report size: " <> show (Bytes.Lazy.length report)
    pure NoBody

-- multipartBody @Tmp requires MonadResource
apiTmp :: Router Void (ResourceT IO)
apiTmp =
  "api"
    /> "multipart"
    /> multipartBody @Tmp @ReportTmp
    .> handle @NoBody POST endpoint
 where
  endpoint ReportTmp{..} = liftIO do
    putStrLn $ "Reporter: " <> Text.unpack reporter
    reportSize <- Bytes.Lazy.length <$> Bytes.Lazy.readFile report
    putStrLn $ "Report size: " <> show reportSize
    pure NoBody
