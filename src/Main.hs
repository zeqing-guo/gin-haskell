{- |
Module      : Main
Copyright   : Zeqing Guo
Licence     : MIT (see LICENSE in the distribution)
Maintainer  : github.com/zeqing-guo
Stability   : experimental
Portability : portable
This file providing command-line argument parsing
-}

{-# LANGUAGE OverloadedStrings #-}


module Main where
import           Control.Exception.Base

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO

import qualified GinConfig              as GC
import           Controller

data Flag = InitiateBlog String
          | CreatePost String
          | PublishPost
          | Help
          | Version
          deriving Show

flags :: [OptDescr Flag]
flags =
  [
    Option ['i'] ["init"] (ReqArg InitiateBlog "DIR")
      "Create a blog including a config file and an example post",
    Option ['n'] ["new"] (ReqArg CreatePost "FILE")
      "Create a post",
    Option ['p'] ["publish"] (NoArg PublishPost)
      "Publish new and modified posts on Github",
    Option ['h'] ["help"] (NoArg Help)
      "Print this help message",
    Option ['v'] ["version"] (NoArg Help)
      "Show gin's version"
  ]

parse :: [String] -> IO Flag
parse argv = case getOpt Permute flags argv of
               ([Help], [], []) -> do putStrLn $ usageInfo header flags
                                      exitSuccess
               ([Version], [], []) -> do putStrLn "gin 0.1.0.0"
                                         exitSuccess
               ([arg], [], []) -> return arg
               (_, _, errs) -> exitF $ concat errs ++ usageInfo header flags
  where
    header = "Usage: gin [-inphv] [file]"

exitF :: String -> IO a
exitF errmsg = do hPutStrLn stderr errmsg
                  exitFailure

exitFBS :: T.Text -> IO a
exitFBS errmsg = do TIO.hPutStrLn stderr errmsg
                    exitFailure

-- |Create some files and dirctories for new blog
gin :: Flag -> IO ()
gin (InitiateBlog blog) =
  do let config = T.pack $ blog </> GC.configFile
         post = T.pack $ blog </> GC.postDirectory
         example = T.pack $ blog </> GC.postExample
         gin = T.pack $ blog </> GC.ginConfig
         ginRecordFile = T.pack $ blog </> GC.ginRecordFile
         packedBlog = T.pack blog
     isConfigExist <- doesDirectoryExist blog
     if isConfigExist
        then exitFBS $ "Error: " `T.append` packedBlog `T.append` " already exists\n"
        else do TIO.putStrLn $ "Copy data to " `T.append` packedBlog `T.append` "\n"
                createDirectory blog `catch` handlerIO
                createDirectory (T.unpack post) `catch` handlerIO
                createDirectory (T.unpack gin) `catch` handlerIO
                TIO.writeFile (T.unpack ginRecordFile) "DO NOT MODIFY IT!" `catch` handlerIO
                TIO.writeFile (T.unpack config) configTemplate `catch` handlerIO
                ct <- now "%c"
                TIO.writeFile (T.unpack example) (exampleTemplate ct) `catch` handlerIO
     where
       handlerIO :: IOException -> IO ()
       handlerIO = print
gin (CreatePost post) = do ct <- now "%c"
                           ct' <- now "%F"
                           let postName = GC.postDirectory </> (ct' ++ " " ++ post <.> "md")
                           isExist <- doesFileExist postName
                           if isExist
                              then exitF $ "Error: " ++ postName ++ " already exists\n"
                              else do TIO.writeFile postName $ newPostTemplate post ct
                                      TIO.putStrLn $ "Create " `T.append` T.pack postName `T.append` "\n"
gin PublishPost = commitPosts
gin Help = putStrLn $ usageInfo header flags
  where
    header = "Usage: gin [-inphv] [file]"
gin Version = putStrLn "gin: 0.1.0.0\n"

now :: String -> IO String
now f = do czt <- getZonedTime
           return $ formatTime defaultTimeLocale f czt

main :: IO ()
main = do arg <- getArgs >>= parse
          gin arg


-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             ["init", blogName] -> initBlog $ blogName ++ "/"
--             ["commit"] -> commitPosts
--             ["help"] -> putStrLn "help"
--             ["new", postName] -> initPost postName
--             xs -> putStrLn $ "gin: unrecognised command: " ++ concatMap (++ " ") xs ++ "(try help)"


-- initPost :: String -> IO ()
-- initPost postName = do now <- getZonedTime
--                        currentDirectory <- liftM (++ "/") getCurrentDirectory
--                        let thisPostName = GC.postDirectory ++ show (utctDay $ zonedTimeToUTC now) ++ " " ++ postName ++ ".md"
--                        isPostExist <- doesFileExist thisPostName
--                        if isPostExist
--                           then putStrLn $ "Error: " ++ currentDirectory ++ thisPostName ++ " already exists!"
--                           else do writeFile thisPostName $ newPostTemplate postName $ show now
--                                   putStrLn $ "Created: " ++ currentDirectory ++ thisPostName

configTemplate :: T.Text
configTemplate = "github_token: your token\n\
        \github_repo: your blog repo\n\
        \copyright: your copyright statement"

exampleTemplate :: String -> T.Text
exampleTemplate now = "---\n\
        \title: Hello World!\n\
        \date: "
        `T.append` T.pack now
        `T.append` "\ntags:\n\
        \- Blog\n\
        \- Example\n\
        \---\n\
        \Gin is a static blog generator helping you publish your blog to your repository's issues on Github.\n\n\
        \Now, let's have a glance at the basic styles: [link](https://github.com/zeqing-guo/gin-haskell),\n\n\
        \**strong**, *italic*, <del>deletion</del>, <ins>insertion</ins>.\n\n\
        \<hr>\n\
        \# Header 1\n\
        \## Header 2\n\
        \### Header 3\n\
        \#### Header 4\n\
        \##### Header 5\n\
        \###### Header 6\n\n\
        \- list item 1\n\
        \- list item 2\n\
        \- list item 3\n\n\
        \1. list item 1\n\
        \2. list item 2\n\
        \3. list item 3\n\n\
        \> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n\n\
        \![](https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png)\n\n\
        \| Tables        | Are           | Cool  |\n\
        \| ------------- |:-------------:| -----:|\n\
        \| col 3 is      | right-aligned | $1600 |\n\
        \| col 2 is      | centered      |   $12 |\n\
        \| zebra stripes | are neat      |    $1 |\n\n\
        \This is an inline equation: $E=mc^2$\n\n\
        \A big one: $$\\oint_{(x,y)\\in C} x^3\\, dx + 4y^2\\, dy$$\n\n\
        \```haskell\n\
        \-- Hello.hs\n\
        \main :: IO ()\n\
        \main = putStrLn \"Hello World!\"\n```\n\n\
        \{{ copyright }}"

newPostTemplate :: String -> String -> T.Text
newPostTemplate filename now = "---\ntitle: "
                               `T.append` T.pack filename
                               `T.append` "\ndate: "
                               `T.append` T.pack now
                               `T.append` "\ntags:\n- Blog\n---"
