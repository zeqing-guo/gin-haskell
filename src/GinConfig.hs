module GinConfig (configFile
                 , postDirectory
                 , postExample
                 , ginConfig
                 , ginRecordFile
                 , mediaDirectory) where

import System.FilePath.Posix

configFile :: String
configFile = "_config.yml"

postDirectory :: String
postDirectory = "post"

postExample :: String
postExample = "post" </> "hello-world.md"

ginConfig :: String
ginConfig = ".gin"

ginRecordFile :: String
ginRecordFile = ".gin" </> "lastModificationTime"

mediaDirectory :: String
mediaDirectory = "media"
