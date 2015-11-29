{- |
Module      : ParseConfig
Copyright   : Zeqing Guo
Licence     : MIT (see LICENSE in the distribution)
Maintainer  : github.com/zeqing-guo
Stability   : experimental
Portability : portable
This file parse _config.yml file and get the configuration of blog.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseConfig ( Config(..)
                   , parseConfig) where 

import qualified Data.Text    as T
import           Data.Yaml
import           GHC.Generics

data Config = Config {github_token :: T.Text, copyright :: T.Text}
            deriving (Show, Generic)

instance FromJSON Config

parseConfig :: FilePath -> IO (Either ParseException Config)
parseConfig = decodeFileEither
