{-# LANGUAGE PatternSynonyms #-}

module Web.Minion.Request.Method where

import Network.HTTP.Types qualified as Http

pattern GET :: Http.Method
pattern GET = "GET"

pattern POST :: Http.Method
pattern POST = "POST"

pattern PUT :: Http.Method
pattern PUT = "PUT"

pattern PATCH :: Http.Method
pattern PATCH = "PATCH"

pattern HEAD :: Http.Method
pattern HEAD = "HEAD"

pattern DELETE :: Http.Method
pattern DELETE = "DELETE"

pattern OPTIONS :: Http.Method
pattern OPTIONS = "OPTIONS"

pattern TRACE :: Http.Method
pattern TRACE = "TRACE"

pattern CONNECT :: Http.Method
pattern CONNECT = "CONNECT"
