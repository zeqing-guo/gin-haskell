{- |
Module      : ParsePost
Copyright   : Zeqing Guo
Licence     : MIT (see LICENSE in the distribution)
Maintainer  : github.com/zeqing-guo
Stability   : experimental
Portability : portable
This file parse posts.
-}

{-# LANGUAGE OverloadedStrings #-}

module ParsePost (Post(..)
                 , FrontMatter(..)
                 , MarkdownPlus(..)
                 , parsePost) where

import           Control.Applicative
import           Control.Monad.Error

import Data.Maybe (maybe)
import qualified Data.Text            as T
import Data.List (elem)
import Data.Char (isDigit, isSpace)
import           Data.Attoparsec.Text

import           Prelude              hiding (takeWhile)

import ParseConfig
import GinConfig

data FrontMatter = FrontMatter {title :: T.Text
                               , date :: T.Text
                               , issueId :: T.Text
                               , tag  :: [T.Text]}
                 deriving (Show)

data MarkdownPlus = Content T.Text
                  | Picture {picAlt :: T.Text, picPath :: T.Text, picTitle :: T.Text}
                  | InlineEquation T.Text
                  | OutlineEquation T.Text
                  | Liquid T.Text
                  deriving (Show)

data Post = Post {frontMatter  :: FrontMatter,
                  markdownPlus :: [MarkdownPlus]}
          deriving (Show)

isControl :: Char -> Bool
isControl = (`elem` ['\n', '\r', '\t', '\f', '\v'])

parsePost :: Parser Post
parsePost = Post
            <$ string "---\n"
            <*> parseFrontMatter <* string "---\n"
            <*> parseMarkdownPlus


parseTitle :: Parser T.Text
parseTitle = T.stripEnd <$ string "title:" <* takeWhile (== ' ') <*> takeWhile1 (not . isControl)

parseTime :: Parser T.Text
parseTime = T.stripEnd <$ string "date:" <* takeWhile (== ' ') <*> takeWhile1 (not . isControl)

parseIssueId :: Parser T.Text
parseIssueId = T.stripEnd <$ string "issue id:" <* takeWhile (== ' ') <*> takeWhile1 isDigit

parseTags :: Parser [T.Text]
parseTags = map T.strip <$ string "tags:" <* takeWhile (== ' ') <*> (parseInline <|> parseBlock)
            where
              parseInline = char '['
                            *> sepBy (takeWhile1 (\c -> c /= ',' && c /= ']')) (char ',')
                            <* char ']'
                            <* takeWhile isSpace
              parseBlock = takeWhile isSpace
                           *> char '-'
                           *> takeWhile isSpace
                           *> sepBy (takeWhile1 (not . isControl)
                                     <* satisfy isControl)
                                    (T.append
                                     <$> takeWhile isSpace
                                     <*> (T.cons
                                     <$> char '-'
                                     <*> takeWhile1 isSpace))

-- parseNone1 :: Parser T.Text
-- parseNone1 = (try (string "issue id:" *> takeWhile isSpace)
--               <|> takeWhile isSpace) *> return T.empty

-- parseNone2 :: Parser [T.Text]
-- parseNone2 = (try (string "tags:" <* takeWhile isSpace <* satisfy isSpace)
--                <|> takeWhile isSpace) *> return []

parseFrontMatter :: Parser FrontMatter
parseFrontMatter = FrontMatter <$> parseTitle <* takeWhile1 isSpace
                               <*> parseTime <* takeWhile1 isSpace
                               <*> (try parseIssueId <|> return T.empty) <* takeWhile isSpace
                               <*> (try parseTags <|> return [])


parsePicture :: Parser MarkdownPlus
parsePicture = Picture <$ string "!["
                       <*> takeWhile (/= ']')
                       <* string "]("
                       <*> (T.stripEnd
                            <$> takeWhile1 (\c -> c /= '"' && c /= ')'))
                       <*> (try (char ')' *> return "")
                            <|> char '"'
                                *> takeWhile (/= '"')
                                <* takeWhile (/= ' ')
                                <* char ')')

plusGrammar :: (T.Text -> MarkdownPlus)
            -> Parser a
            -> Parser a
            -> Parser MarkdownPlus
plusGrammar f l r = f . T.pack <$ l <*> manyTill anyChar r

parseInlineEquation :: Parser MarkdownPlus
parseInlineEquation = plusGrammar InlineEquation (char '$') (char '$')

parseOutlineEquation :: Parser MarkdownPlus
parseOutlineEquation = plusGrammar OutlineEquation (string "$$") (string "$$")

parseLiquid :: Parser MarkdownPlus
parseLiquid = plusGrammar Liquid (string "{{") (string "}}")

parseContent :: Parser MarkdownPlus
parseContent = Content <$> (T.cons
                            <$> anyChar
                            <*> takeWhile (\c -> c /= '!'
                                                 && c /= '$'
                                                 && c /= '{'))

parseMarkdownPlus :: Parser [MarkdownPlus]
parseMarkdownPlus = many $ try parsePicture
                           <|> try parseInlineEquation
                           <|> try parseOutlineEquation
                           <|> try parseLiquid
                           <|> parseContent

-- notSpace :: Parser Char
-- notSpace = satisfy $ not . isSpace

-- notChar :: Char -> Parser Char
-- notChar c = satisfy (/= c)

-- parseContent :: Parser MarkdownPlus
-- parseContent = do
--   x <- anyChar
--   xs <- many (noneOf "!${")
--   return $ Content $ T.pack (x : xs)


-- readFrontMatter :: String -> Either ParseError [FrontMatter]
-- readFrontMatter = parse parseFrontMatter "gin"

-- readMarkdownPlus :: String -> Either ParseError [MarkdownPlus]
-- readMarkdownPlus = parse parseMarkdownPlus "gin"

-- readPost :: String -> ThrowsError Post
-- readPost post = case B.breakSubstring (B.pack "---\n") (B.pack post) of
--   ("", rest) -> case B.breakSubstring (B.pack "---\n") (B.drop 4 rest) of
--     ("", _) -> throwError $ Parser "nothing find in front matter"
--     (fm, mp) -> case readFrontMatter $ B.unpack fm of
--       Left err -> throwError $ Parser $ show err
--       Right fms -> case readMarkdownPlus $ B.unpack mp of
--         Left err -> throwError $ Parser $ show err
--         Right mps -> return $ Post fms mps
--   _ -> throwError $ Parser "\"gin\" (line 1):\nexpecting \"---\""

-- --------------------- Test -------------------------------------

exampleTemplate :: String
exampleTemplate = "---\ntitle: Hello World!\ndate: 2015.11.29\ntags:\n- Blog\n- Example\n---\nGin is a static blog generator helping you publish your blog to your repository's issues on Github.\nNow, let's have a glance at the basic styles: [link](https://github.com/zeqing-guo/gin-haskell),\n**strong**, *italic*, <del>deletion</del>, <ins>insertion</ins>.\n<hr>\n# Header 1\n## Header 2\n### Header 3\n#### Header 4\n##### Header 5\n###### Header 6\n- list item 1\n- list item 2\n- list item 3\n1. list item 1\n2. list item 2\n3. list item 3\n> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n![](/images/image.png)\n<table>\n    <thead>\n        <tr>\n            <th>Name</th>\n            <th>Age</th>\n            <th>Fruit</th>\n        </tr>\n    </thead>\n    <tbody>\n        <tr>\n            <td>Alex</td>\n            <td>22</td>\n            <td>Apple</td>\n        </tr>\n        <tr>\n            <td>Bran</td>\n            <td>20</td>\n            <td>Orange</td>\n        </tr>\n        <tr>\n            <td>Mike</td>\n            <td>21</td>\n            <td>Waltermelon</td>\n        </tr>\n    </tbody>\n</table>\n```haskell\n-- Hello.hs\nmain :: IO ()\nmain = putStrLn \"Hello World!\"\n```\n{{copyright}}!"

exampleFM :: String
exampleFM = "title: Hello World!\ndate: 2015.11.29\ntags:\n- Blog\n- Example\n"

exampleBody :: String
exampleBody = "Gin is a static blog generator helping you publish your blog to your repository's issues on Github.\nNow, let's have a glance at the basic styles: [link](https://github.com/zeqing-guo/gin-haskell),\n**strong**, *italic*, <del>deletion</del>, <ins>insertion</ins>.\n<hr>\n# Header 1\n## Header 2\n### Header 3\n#### Header 4\n##### Header 5\n###### Header 6\n- list item 1\n- list item 2\n- list item 3\n1. list item 1\n2. list item 2\n3. list item 3\n> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n![](/images/image.png)\n<table>\n    <thead>\n        <tr>\n            <th>Name</th>\n            <th>Age</th>\n            <th>Fruit</th>\n        </tr>\n    </thead>\n    <tbody>\n        <tr>\n            <td>Alex</td>\n            <td>22</td>\n            <td>Apple</td>\n        </tr>\n        <tr>\n            <td>Bran</td>\n            <td>20</td>\n            <td>Orange</td>\n        </tr>\n        <tr>\n            <td>Mike</td>\n            <td>21</td>\n            <td>Waltermelon</td>\n        </tr>\n    </tbody>\n</table>\n```haskell\n-- Hello.hs\nmain :: IO ()\nmain = putStrLn \"Hello World!\"\n```\n{{copyright}}!"
-- test = case B.breakSubstring (B.pack "---\n") (B.pack exampleTemplate) of
--   ("", rest) -> case B.breakSubstring (B.pack "---") (B.drop 4 rest) of
--     -- (fm, mp) -> readFrontMatter $ B.unpack fm
--     (fm, mp) -> readMarkdownPlus (B.unpack $ B.drop 4 mp)
