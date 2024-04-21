module HTTP.Request
  ( class Request
  , Body(..)
  , RawRequestBody
  , Method(..)
  , bodyToRaw
  , json
  , form
  , blob
  , buffer
  , arrayBuffer
  , requestBody
  , requestHeaders
  , requestUrl
  , requestMethod
  , rawRequestBodySize
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Tuple.Containing (extract)
import Data.Tuple.Nested (type (/\))
import Data.URL (URL)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import HTTP.Form (Form, RawFormData)
import HTTP.Form as Form
import HTTP.Header (ContentType(..), Headers(..))
import HTTP.Header as Header
import HTTP.MIME (MIME)
import HTTP.MIME as MIME
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Simple.JSON (class WriteForeign, writeJSON)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob)
import Web.File.Blob as Blob

foreign import data RawRequestBody :: Type

foreign import blobArrayBufferImpl :: Blob -> Effect (Promise ArrayBuffer)
foreign import rawRequestBodySize :: RawRequestBody -> Effect Int

unsafeEmptyRawRequestBody :: RawRequestBody
unsafeEmptyRawRequestBody = unsafeCoerce Nullable.null

unsafeFormDataToRawRequestBody :: RawFormData -> RawRequestBody
unsafeFormDataToRawRequestBody = unsafeCoerce

unsafeArrayBufferToRawRequestBody :: ArrayBuffer -> RawRequestBody
unsafeArrayBufferToRawRequestBody = unsafeCoerce

unsafeBlobToRawRequestBody :: forall m. MonadAff m => Blob -> m RawRequestBody
unsafeBlobToRawRequestBody = map unsafeArrayBufferToRawRequestBody <<< liftAff <<< Promise.toAffE <<< blobArrayBufferImpl

data Body
  = BodyString String (Maybe ContentType)
  | BodyArrayBuffer ArrayBuffer (Maybe ContentType)
  | BodyBuffer Buffer (Maybe ContentType)
  | BodyBlob Blob
  | BodyForm Form
  | BodyEmpty

json :: forall a. WriteForeign a => a -> Body
json = flip BodyString (Just $ ContentType MIME.Json) <<< writeJSON

form :: Form -> Body
form = BodyForm

blob :: Blob -> Body
blob = BodyBlob

buffer :: MIME -> Buffer -> Body
buffer mime buf = BodyBuffer buf $ Just $ ContentType mime

arrayBuffer :: MIME -> ArrayBuffer -> Body
arrayBuffer mime buf = BodyArrayBuffer buf $ Just $ ContentType mime

bodyHeaders :: forall m. MonadEffect m => Body -> m Headers
bodyHeaders (BodyForm _) = pure mempty
bodyHeaders (BodyEmpty) = pure mempty
bodyHeaders (BodyString _ ct) = liftEffect $ Header.headers ct
bodyHeaders (BodyBuffer _ ct) = liftEffect $ Header.headers ct
bodyHeaders (BodyArrayBuffer _ ct) = liftEffect $ Header.headers ct
bodyHeaders (BodyBlob b) = liftEffect $ Header.headers <<< map (ContentType <<< MIME.fromString <<< unwrap) $ Blob.type_ b

bodyToRaw :: forall m. MonadAff m => Body -> m (Maybe RawRequestBody)
bodyToRaw (BodyString body ct) = flip bind bodyToRaw $ liftEffect $ map (flip BodyBuffer ct) $ Buffer.fromString body UTF8
bodyToRaw (BodyBuffer body ct) = flip bind bodyToRaw $ liftEffect $ map (flip BodyArrayBuffer ct) $ Buffer.toArrayBuffer body
bodyToRaw (BodyArrayBuffer body _) = pure $ Just $ unsafeArrayBufferToRawRequestBody body
bodyToRaw (BodyForm form') = map Just $ map unsafeFormDataToRawRequestBody $ Form.toRawFormData form'
bodyToRaw (BodyBlob body) = map Just $ unsafeBlobToRawRequestBody body
bodyToRaw BodyEmpty = pure Nothing

data Method
  = GET
  | PUT
  | POST
  | DELETE
  | PATCH
  | HEAD

derive instance Generic Method _
instance Eq Method where
  eq = genericEq

instance Show Method where
  show = genericShow

class Request :: Type -> Constraint
class Request a where
  requestUrl :: forall m. MonadAff m => a -> m URL
  requestMethod :: forall m. MonadAff m => a -> m Method
  requestBody :: forall m. MonadAff m => a -> m Body
  requestHeaders :: forall m. MonadAff m => a -> m Headers

instance Request (Method /\ URL /\ Body /\ Effect Headers) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody = pure <<< extract
  requestHeaders req = do
    hs <- liftEffect $ extract req
    bodyHs <- bodyHeaders $ extract req
    pure $ hs <> bodyHs

instance Request (Method /\ URL /\ Body /\ Headers) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody = pure <<< extract
  requestHeaders req = do
    let hs = extract req
    bodyHs <- bodyHeaders $ extract req
    pure $ hs <> bodyHs

instance Request (Method /\ URL /\ Body) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody = pure <<< extract
  requestHeaders _ = pure mempty

instance Request (Method /\ URL /\ Headers) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody _ = pure BodyEmpty
  requestHeaders = pure <<< extract

instance Request (Method /\ URL /\ Effect Headers) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody _ = pure BodyEmpty
  requestHeaders = liftEffect <<< extract @(Effect Headers)

instance Request (Method /\ URL) where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody _ = pure BodyEmpty
  requestHeaders _ = pure mempty

instance Request URL where
  requestUrl = pure
  requestMethod _ = pure GET
  requestBody _ = pure BodyEmpty
  requestHeaders _ = pure mempty
