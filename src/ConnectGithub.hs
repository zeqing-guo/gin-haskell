{-# LANGUAGE OverloadedStrings #-}

module ConnectGithub where

import qualified Control.Exception         as E
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding
import qualified Data.Text.IO as TIO
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import System.Exit



sendRequest :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO (Maybe BC.ByteString)
sendRequest token body url method_ title = do
  TIO.putStrLn $ "Upload " `T.append` title `T.append` "..."
  initReq <- parseUrl $ T.unpack url
  let request = initReq { method = encodeUtf8 method_
                        , requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:42.0) Gecko/20100101 Firefox/42.0")
                                           , ("Content-Type", "application/json")
                                           , ("Authorization", "token " <> encodeUtf8 token)]
                        , requestBody = RequestBodyBS $ encodeUtf8 body
                    }
  manager <- newManager tlsManagerSettings
  (httpLbs request manager >>= \res -> getId res) `E.catch`
        (\e@(StatusCodeException s _ _) -> processException s e)
  where
    processException s e = case statusCode s of
                             400 -> putStrLn "problems parsing JSON request" >> exitFailure
                             401 -> putStrLn "token error" >> exitFailure
                             404 -> putStrLn "please invoke token with *public_repo* scopes." >> exitFailure
                             _ -> print e >> exitFailure
    getId res =
      do let b = responseBody res
             (_, numWithTail) = BC.breakSubstring "\"number\": " $ BL.toStrict b
             issueId = last $ BC.split ' ' (fst $ BC.breakSubstring "," numWithTail)
         return $ Just issueId


