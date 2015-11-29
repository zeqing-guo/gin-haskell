{- |
Module      : Exception
Copyright   : Zeqing Guo
Licence     : MIT (see LICENSE in the distribution)
Maintainer  : github.com/zeqing-guo
Stability   : experimental
Portability : portable
This file contains all things about exception.
-}
module Exception
       ( GinError(..)
       , ThrowsError
       ) where

import Control.Monad.Error

data GinError = Parser String
              | Default String

showError :: GinError -> String
showError (Parser message) = "Parse error at" ++ message
showError (Default message) = message

instance Show GinError where 
  show = showError

instance Error GinError where 
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either GinError
