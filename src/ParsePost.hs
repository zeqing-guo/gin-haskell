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

module ParsePost where
import qualified Data.ByteString.Char8         as B
import           Data.Char
import qualified Data.Text                     as T
import           Text.ParserCombinators.Parsec

data FrontMatter = Title B.ByteString
                 | Time B.ByteString
                 | Tag [B.ByteString]
                 deriving (Show)

data MarkdownPlus = Content T.Text
                  | Picture {alt :: T.Text, path :: T.Text, title :: T.Text}
                  | InlineEquation T.Text
                  | OutlineEquation T.Text
                  | Liquid T.Text
                  deriving (Show)

data Post = Post {
  frontMatter :: [FrontMatter],
  markdownPlus :: [MarkdownPlus]
  } deriving (Show)

notSpace :: Parser Char
notSpace = satisfy $ not . isSpace

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

parseTitle :: Parser FrontMatter
parseTitle = do string "title:" *> spaces
                str <- many (noneOf "\n\r")
                spaces
                return $ Title $ B.pack str

parseTime :: Parser FrontMatter
parseTime = do string "date:"
               spaces
               str <- many (noneOf "\n\r")
               spaces
               return $ Title $ B.pack str

parseTag :: Parser FrontMatter
parseTag = do string "tag:"
              spaces
              xs <- commaList <|> hyphenList
              return $ Tag xs
                where
                  hyphenList :: Parser [B.ByteString]
                  hyphenList = do
                    xs <- many1 (char '-' >> many1 space >> many notSpace <* spaces)
                    return $ Prelude.map B.pack xs
                  commaList :: Parser [B.ByteString]
                  commaList = do
                    char '[' >> spaces
                    xs <- sepBy (many $ noneOf ", \t\n\r\f\v") spaces
                    return $ Prelude.map B.pack xs

parseFrontMatter :: Parser [FrontMatter]
parseFrontMatter = many1 (try parseTitle <|> parseTime <|> parseTag) 

parsePicture :: Parser MarkdownPlus
parsePicture = do
  string "!["
  alt <- many (notChar ']')
  string "]("
  path <- many (notChar ' ') <|> string "\\ "
  many (char ' ')
  title <- many (notChar ')')
  char ')'
  return $ Picture (T.pack alt) (T.pack path) (T.pack title)

parseInlineEquation :: Parser MarkdownPlus
parseInlineEquation = do
  char '$'
  equation <- many (notChar '$') <|> string "\\$"
  char '$'
  return $ InlineEquation $ T.pack equation

parseOutlineEquation :: Parser MarkdownPlus
parseOutlineEquation = do
  string "$$"
  equation <- many (notChar '$') <|> string "\\$"
  string "$$"
  return $ OutlineEquation $ T.pack equation

parseLiquid :: Parser MarkdownPlus
parseLiquid = do
  string "{{"
  xs <- many (noneOf "{}")
  string "}}"
  return $ Liquid $ T.pack xs

parseContent :: Parser MarkdownPlus
parseContent = (Content . T.pack) <$>
               manyTill anyChar (try parsePicture
                                 <|> try parseInlineEquation
                                 <|> try parseOutlineEquation
                                 <|> try parseLiquid)
parseContent' :: Parser MarkdownPlus
parseContent' = Content . T.pack <$> many anyChar

parseMarkdownPlus :: Parser [MarkdownPlus]
parseMarkdownPlus = many $ try parsePicture
                           <|> try parseInlineEquation
                           <|> try parseOutlineEquation
                           <|> try parseLiquid
                           <|> try parseContent
                           <|> parseContent'

-- readOrThrow :: Parser a -> String -> ThrowsError a
-- readOrThrow parser input = case parse parser "gin" input of
--   Left err -> throwError $ Parse err
--   Right val -> return val

readFrontMatter :: String -> Either ParseError [FrontMatter]
readFrontMatter = parse parseFrontMatter "gin"

readMarkdownPlus :: String -> Either ParseError [MarkdownPlus]
readMarkdownPlus = parse parseMarkdownPlus "gin"

exampleTemplate :: String
exampleTemplate = "---\ntitle: Hello World!\ndate: 2015.11.29\ntag:\n- Blog\n- Example\n---\nGin is a static blog generator helping you publish your blog to your repository's issues on Github.\nNow, let's have a glance at the basic styles: [link](https://github.com/zeqing-guo/gin-haskell),\n**strong**, *italic*, <del>deletion</del>, <ins>insertion</ins>.\n<hr>\n# Header 1\n## Header 2\n### Header 3\n#### Header 4\n##### Header 5\n###### Header 6\n- list item 1\n- list item 2\n- list item 3\n1. list item 1\n2. list item 2\n3. list item 3\n> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n![]({{site.baseurl}}/images/image.png)\n<table>\n    <thead>\n        <tr>\n            <th>Name</th>\n            <th>Age</th>\n            <th>Fruit</th>\n        </tr>\n    </thead>\n    <tbody>\n        <tr>\n            <td>Alex</td>\n            <td>22</td>\n            <td>Apple</td>\n        </tr>\n        <tr>\n            <td>Bran</td>\n            <td>20</td>\n            <td>Orange</td>\n        </tr>\n        <tr>\n            <td>Mike</td>\n            <td>21</td>\n            <td>Waltermelon</td>\n        </tr>\n    </tbody>\n</table>\n```haskell\n-- Hello.hs\nmain :: IO ()\nmain = putStrLn \"Hello World!\"\n```"

test = case B.breakSubstring (B.pack "---\n") (B.pack exampleTemplate) of
  ("", rest) -> case B.breakSubstring (B.pack "---") (B.drop 4 rest) of
    -- (fm, mp) -> readFrontMatter $ B.unpack fm 
    (fm, mp) -> readMarkdownPlus (B.unpack $ B.drop 4 mp)
