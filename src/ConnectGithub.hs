{-# LANGUAGE OverloadedStrings #-}

module ConnectGithub where

import qualified Data.ByteString.Lazy   as L
import           Data.Monoid
import           Network.HTTP.Conduit

import           Control.Monad.IO.Class (liftIO)

sendRequest token body url = do
  initReq <- parseUrl url
  let request = initReq { method = "methodPost"
                        , requestHeaders = [("Content-Type", "application/json")
                                           , ("Authorization", "token " <> token)]
                        , requestBody = RequestBodyLBS body
                    }
  liftIO $ withManager $ httpLbs request

