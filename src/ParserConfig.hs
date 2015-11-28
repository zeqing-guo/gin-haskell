{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import           Data.Yaml
import           GHC.Generics

data Config = Config {github_token :: T.Text, copyright :: T.Text}
            deriving (Show, Generic)

instance FromJSON Config

parseConfig :: FilePath -> IO (Either ParseException Config)
parseConfig = decodeFileEither
