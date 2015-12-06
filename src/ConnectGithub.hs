{-# LANGUAGE OverloadedStrings #-}

module ConnectGithub where

import qualified Control.Exception         as E
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)


sendRequest token body url = do
  initReq <- parseUrl url
  let request = initReq { method = "methodPost"
                        , requestHeaders = [("Content-Type", "application/json")
                                           , ("Authorization", "token " <> token)]
                        , requestBody = RequestBodyLBS body
                    }
  manager <- newManager tlsManagerSettings
  (httpLbs request manager >>= \res -> getId res) `E.catch`
        (\(StatusCodeException s _ _) -> processException s)
  where
    processException s = case statusCode s of
                           400 -> putStrLn "problems parsing JSON request" >> return Nothing
                           401 -> putStrLn "token error" >> return Nothing
                           _ -> print s >> return Nothing
    getId res =
      do let b = responseBody res
             (_, numWithTail) = BC.breakSubstring "\"number\": " (BL.toStrict b)
             (issueId, _) = BC.breakSubstring "," numWithTail
         return $ Just issueId


