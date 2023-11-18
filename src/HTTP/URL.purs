module HTTP.URL where

import Prelude

import Data.Newtype (class Newtype)

newtype URL = URL String

derive instance Newtype URL _
derive newtype instance Show URL
derive newtype instance Eq URL
derive newtype instance Ord URL
