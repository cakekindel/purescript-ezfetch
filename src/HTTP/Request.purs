module HTTP.Request
  ( class Request
  , Body
  , RawRequestBody
  , Method(..)
  , json
  , form
  , blob
  , buffer
  , arrayBuffer
  , requestBody
  , requestHeaders
  , requestUrl
  , requestMethod
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import HTTP.Form (Form, RawFormData)
import HTTP.Form as Form
import HTTP.Header (ContentType(..), Headers(..))
import HTTP.Header as Header
import HTTP.MIME (MIME)
import HTTP.MIME as MIME
import HTTP.URL (URL)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Simple.JSON (class WriteForeign, writeJSON)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob)
import Web.File.Blob as Blob

foreign import blobArrayBufferImpl :: Blob -> Effect (Promise ArrayBuffer)
foreign import data RawRequestBody :: Type

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
bodyHeaders (BodyBlob b) = Header.headers <<< map (ContentType <<< MIME.fromString <<< unwrap) $ Blob.type_ b

bodyToRaw :: forall m. MonadAff m => Body -> m RawRequestBody
bodyToRaw (BodyString body ct) = flip bind bodyToRaw $ liftEffect $ map (flip BodyBuffer ct) $ Buffer.fromString body UTF8
bodyToRaw (BodyBuffer body ct) = flip bind bodyToRaw $ liftEffect $ map (flip BodyArrayBuffer ct) $ Buffer.toArrayBuffer body
bodyToRaw (BodyArrayBuffer body _) = pure $ unsafeArrayBufferToRawRequestBody body
bodyToRaw (BodyForm form') = map unsafeFormDataToRawRequestBody $ Form.toRawFormData form'
bodyToRaw (BodyBlob body) = unsafeBlobToRawRequestBody body
bodyToRaw BodyEmpty = liftEffect $ map unsafeArrayBufferToRawRequestBody $ ArrayBuffer.empty 0

data Method
  = GET
  | PUT
  | POST
  | DELETE
  | PATCH

-- | given a tuple of any size with at least 1 value
-- | of type `a`, `extract` the first occurence of `a`
-- | from the tuple
class TupleContaining a tup where
  extract :: tup -> a

instance TupleContaining a a where
  extract = identity
else instance TupleContaining a (Tuple a b) where
  extract = fst
else instance TupleContaining b (Tuple a b) where
  extract = snd
else instance TupleContaining a tail => TupleContaining a (Tuple head tail) where
  extract (_ /\ tail) = extract tail

class Request :: Type -> Constraint
class Request a where
  requestUrl :: forall m. MonadAff m => a -> m URL
  requestMethod :: forall m. MonadAff m => a -> m Method
  requestBody :: forall m. MonadAff m => a -> m (Maybe RawRequestBody)
  requestHeaders :: forall m. MonadAff m => a -> m (Map String String)

instance
  ( TupleContaining Body a
  , TupleContaining URL a
  , TupleContaining Method a
  , TupleContaining Headers a
  ) =>
  Request a where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody = map Just <<< bodyToRaw <<< extract
  requestHeaders req = do
    let (Headers hs) = extract req
    (Headers bodyHs) <- bodyHeaders $ extract req
    pure $ Map.union hs bodyHs
else instance
  ( TupleContaining Body a
  , TupleContaining URL a
  , TupleContaining Method a
  ) =>
  Request a where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody = map Just <<< bodyToRaw <<< extract
  requestHeaders _ = pure Map.empty
else instance
  ( TupleContaining Headers a
  , TupleContaining URL a
  , TupleContaining Method a
  ) =>
  Request a where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody _ = Just <$> bodyToRaw BodyEmpty
  requestHeaders = (\(Headers h) -> pure h) <<< extract
else instance
  ( TupleContaining URL a
  , TupleContaining Method a
  ) =>
  Request a where
  requestUrl = pure <<< extract
  requestMethod = pure <<< extract
  requestBody _ = Just <$> bodyToRaw BodyEmpty
  requestHeaders _ = pure Map.empty
else instance
  ( TupleContaining URL a
  ) =>
  Request a where
  requestUrl = pure <<< extract
  requestMethod _ = pure GET
  requestBody _ = Just <$> bodyToRaw BodyEmpty
  requestHeaders _ = pure Map.empty