module Web.Minion.Examples.Websocket where

import Network.WebSockets qualified as Websocket
import Web.Minion
import Web.Minion.Websocket

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api =
  "api" /> "websocket" /> websocket (withConn wsApp)
 where
  withConn action req = withConnection Websocket.defaultConnectionOptions req action
  wsApp conn = do
    Websocket.sendDataMessage conn (Websocket.Binary "hello!")
    Websocket.receive conn >>= print
