{- |
Module      : Main
Copyright   : Zeqing Guo
Licence     : MIT (see LICENSE in the distribution)
Maintainer  : github.com/zeqing-guo
Stability   : experimental
Portability : portable
This file providing command-line argument parsing
-}

module Main where
import System.Environment
import System.IO
import System.Directory

main :: IO ()
main = do args <- getArgs
          case args of
            ["init"] -> initBlog
            ["commit"] -> putStrLn "commit"
            ["help"] -> putStrLn "help"
            xs -> putStrLn $ "gin: unrecognised command: " ++ concatMap (++ " ") xs ++ "(try help)"

-- |Create some files and dirctories for new blog
initBlog :: IO ()
initBlog = do let config = "_config.yml"
                  post = "post/"
                  example = "post/hello-world.md"
                  gin = ".gin/"
              isConfigExist <- doesFileExist config
              if isConfigExist
                 then putStrLn "Error: _config.yml already exists"
                 else do createDirectoryIfMissing True post
                         createDirectoryIfMissing True gin
                         writeFile config configTemplate
                         writeFile example exampleTemplate

configTemplate :: String
configTemplate = "# A token get from github to allow gin create issues, push to repository\ngithub_token: your token"

exampleTemplate :: String
exampleTemplate = "---\ntitle: \"Hello World!\"\ndate: {{ data }}\ncategories:\n- Blog\n- Example\n---\nGin is a static blog generator helping you publish your blog to your repository's issues on Github.\nNow, let's have a glance at the basic styles: [link](https://github.com/zeqing-guo/gin-haskell),\n**strong**, *italic*, <del>deletion</del>, <ins>insertion</ins>.\n<hr>\n# Header 1\n## Header 2\n### Header 3\n#### Header 4\n##### Header 5\n###### Header 6\n- list item 1\n- list item 2\n- list item 3\n1. list item 1\n2. list item 2\n3. list item 3\n> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n![]({{site.baseurl}}/images/image.png)\n<table>\n    <thead>\n        <tr>\n            <th>Name</th>\n            <th>Age</th>\n            <th>Fruit</th>\n        </tr>\n    </thead>\n    <tbody>\n        <tr>\n            <td>Alex</td>\n            <td>22</td>\n            <td>Apple</td>\n        </tr>\n        <tr>\n            <td>Bran</td>\n            <td>20</td>\n            <td>Orange</td>\n        </tr>\n        <tr>\n            <td>Mike</td>\n            <td>21</td>\n            <td>Waltermelon</td>\n        </tr>\n    </tbody>\n</table>\n```haskell\n— Hello.hs\nmain :: IO ()\nmain = putStrLn \"Hello World!\"\n```"
