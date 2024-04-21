module HTTP.Node (fetchProxy, module X) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.URL (URL)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTP.Header (Headers(..))
import HTTP.Header (headers) as X
import HTTP.Request (bodyToRaw)
import HTTP.Request (class Request, Method(..)) as X
import HTTP.Request as Req
import HTTP.Response (Response)

foreign import fetchImpl :: URL -> URL -> String -> Object String -> Nullable Req.RawRequestBody -> Effect (Promise Response)

fetchProxy :: forall m a. MonadAff m => Req.Request a => URL -> a -> m Response
fetchProxy pxy req = do
  url <- Req.requestUrl req
  method <- Req.requestMethod req
  body <- Req.requestBody req
  bodyRaw <- bodyToRaw body
  Headers headers <- Req.requestHeaders req

  let
    methodStr = case method of
      Req.GET -> "GET"
      Req.PUT -> "PUT"
      Req.POST -> "POST"
      Req.PATCH -> "PATCH"
      Req.DELETE -> "DELETE"
      Req.HEAD -> "HEAD"
    headers' = Object.fromFoldableWithIndex headers

  liftAff $ Promise.toAffE $ fetchImpl pxy url methodStr headers' $ Nullable.toNullable bodyRaw
