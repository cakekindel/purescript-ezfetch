module HTTP (fetch, module X) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTP.Header (headers) as X
import HTTP.Request (class Request, Method(..)) as X
import HTTP.Request as Req
import HTTP.Response (Response)

foreign import fetchImpl :: String -> String -> Object String -> Nullable Req.RawRequestBody -> Effect (Promise Response)

fetch :: forall m a. MonadAff m => Req.Request a => a -> m Response
fetch req = do
  url <- Req.requestUrl req
  method <- Req.requestMethod req
  body <- Req.requestBody req
  headers <- Req.requestHeaders req

  let
    methodStr = case method of
      Req.GET -> "GET"
      Req.PUT -> "PUT"
      Req.POST -> "POST"
      Req.PATCH -> "PATCH"
      Req.DELETE -> "DELETE"
    headers' = Object.fromFoldableWithIndex headers

  liftAff $ Promise.toAffE $ fetchImpl (unwrap url) methodStr headers' $ Nullable.toNullable body