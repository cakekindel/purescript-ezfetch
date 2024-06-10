module Effect.Aff.HTTP.Form where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (hush)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (Foreign, unsafeReadTagged, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.MIME (MIME)
import Data.MIME as MIME
import Simple.JSON (readImpl, unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob)

type FileRecord = { filename :: Nullable String, mime :: String, buf :: ArrayBuffer }

foreign import data RawFormData :: Type
foreign import unsafeMakeBlob :: FileRecord -> Effect Blob
foreign import unsafeMakeFormData :: Object (Array Foreign) -> Effect RawFormData
foreign import unsafeUnmakeFormData :: RawFormData -> Effect (Promise (Object (Array Foreign)))

newtype Filename = Filename String

derive instance Generic Filename _
derive instance Newtype Filename _
instance Show Filename where
  show = genericShow

derive newtype instance Eq Filename
derive newtype instance Ord Filename

data Value
  = ValueString String
  | ValueFile (Maybe Filename) ArrayBuffer MIME

valueForeign :: Value -> Effect Foreign
valueForeign (ValueString s) = pure $ unsafeToForeign s
valueForeign (ValueFile filename buf mime) = unsafeToForeign <$> unsafeMakeBlob { filename: Nullable.toNullable $ unwrap <$> filename, buf, mime: MIME.toString mime }

valueFromForeign :: Foreign -> Effect Value
valueFromForeign f = do
  let
    file :: Maybe { filename :: Nullable String, buf :: Foreign, mime :: String }
    file = hush $ runExcept $ readImpl f
    string = hush $ runExcept $ unsafeReadTagged "String" f

  case file of
    Just { filename, buf, mime } ->
      let
        buf' :: ArrayBuffer
        buf' = unsafeCoerce buf
      in
        pure $ ValueFile (wrap <$> Nullable.toMaybe filename) buf' (MIME.fromString mime)
    Nothing -> do
      s <- liftMaybe (error $ "invalid form value " <> unsafeStringify f) string
      pure $ ValueString s

derive instance Generic Value _
instance Show Value where
  show (ValueString s) = "(ValueString " <> show s <> ")"
  show (ValueFile filename _ mime) = "(ValueFile (" <> show filename <> ") <ArrayBuffer> (" <> show mime <> "))"

newtype Form = Form (Map String (Array Value))

derive instance Newtype Form _
derive newtype instance Show Form

fromRaw :: forall m. MonadAff m => RawFormData -> m Form
fromRaw f = do
  obj <- liftAff $ Promise.toAffE $ unsafeUnmakeFormData f
  let
    formMap = Map.fromFoldableWithIndex obj
  map' <- liftEffect $ for formMap (\vs -> for vs valueFromForeign)
  pure $ Form map'

toRawFormData :: forall m. MonadEffect m => Form -> m RawFormData
toRawFormData =
  let
    collect k o vs = do
      o' <- o
      vs' <- for vs valueForeign
      pure $ Object.insert k vs' o'
  in
    liftEffect <<< flip bind unsafeMakeFormData <<< foldlWithIndex collect (pure Object.empty) <<< unwrap
