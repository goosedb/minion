module Web.Minion.Request where

class IsRequest r where
  type RequestValue r
  getRequestValue :: r -> RequestValue r
