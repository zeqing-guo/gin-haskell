{-# LANGUAGE OverloadedStrings #-}

module ConnectGithub where

import qualified Control.Exception         as E
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)



sendRequest :: T.Text -> T.Text -> T.Text -> T.Text -> IO (Maybe BC.ByteString)
sendRequest token body url method_ = do
  initReq <- parseUrl $ T.unpack url
  let request = initReq { method = encodeUtf8 method_
                        , requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:42.0) Gecko/20100101 Firefox/42.0")
                                           , ("Content-Type", "application/json")
                                           , ("Authorization", "token " <> encodeUtf8 token)]
                        , requestBody = RequestBodyBS $ encodeUtf8 body
                    }
  liftIO $ putStrLn $ BC.unpack $ encodeUtf8 body
  liftIO $ print request
  manager <- newManager tlsManagerSettings
  (httpLbs request manager >>= \res -> getId res) `E.catch`
        (\e@(StatusCodeException s _ _) -> processException s e)
  where
    processException s e = case statusCode s of
                             400 -> putStrLn "problems parsing JSON request" >> return Nothing
                             401 -> putStrLn "token error" >> return Nothing
                             _ -> print e >> return Nothing
    getId res =
      do let b = responseBody res
             (_, numWithTail) = BC.breakSubstring "\"number\": " $ BL.toStrict b
             (issueId, _) = BC.breakSubstring "," numWithTail
         return $ Just issueId


