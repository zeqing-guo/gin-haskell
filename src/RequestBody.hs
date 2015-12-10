{-# LANGUAGE DeriveGeneric #-}

module RequestBody (encodeJson) where

import           Data.Aeson
import           Data.Text    as T
import Data.ByteString.Lazy.Char8

import           GHC.Generics

data RequestBody = RequestBody {
  title  :: T.Text,
  body   :: T.Text,
  labels :: [T.Text]
} deriving Generic

instance ToJSON RequestBody

encodeJson :: Text -> Text -> [Text] -> ByteString
encodeJson t c tags = encode $ RequestBody t c tags
