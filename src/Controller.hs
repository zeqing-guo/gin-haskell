{-# LANGUAGE OverloadedStrings #-}

module Controller (commitPosts) where

import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import           Data.Conduit
import Data.ByteString.Lazy (toStrict)
import qualified Data.Conduit.Attoparsec      as CA
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Text            as CT
import qualified Data.List                    as DL
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding as TE

import qualified System.Directory             as SD
import qualified System.FilePath              as SF
import           System.FilePath.Find
import           System.Posix.Types           (EpochTime)
import qualified System.PosixCompat.Files     as PF (getFileStatus,
                                                     modificationTime)

import           ConnectGithub
import qualified GinConfig                    as GC
import           ParseConfig
import           ParsePost
import qualified RequestBody as RB

-- | Get the existing blogs mate data from blogs.json
-- data Blog = Blog {
--   blogName  :: T.Text
--   , md5     :: T.Text
--   , issueId :: Integer
--   } deriving (Show, Generic)
--
-- instance FromJSON Blog
--
-- getBlogsJson :: (Monad m, MonadIO m) => Conduit BS.ByteString m [Blog]
-- getBlogsJson = do rawBlog <- await
--                      case rawBlog of
--                        Nothing -> return ()
--                        Just blog -> case decodeStrict blog of
--                                       Nothing -> return ()
--                                       Just b -> do yield b
--                                                    getBlogsJson
--

-- | Get the time of last modification of this blog from .gin/lastModificationTime
getRecModTime :: IO EpochTime
getRecModTime = PF.modificationTime 
                <$> PF.getFileStatus GC.ginRecordFile

-- | Get the post list who were created or modified after the time store
-- in .gin/lastModificationTime
getChangedPost :: EpochTime -> IO [FilePath]
getChangedPost recordTime = findPosts recordTime GC.postDirectory
  where
    findPosts :: EpochTime -> String -> IO [FilePath]
    findPosts t = find always 
                  (extension ==? ".md"
                   &&? modificationTime >? t)

readAndParse :: FilePath -> IO ()
readAndParse path = runResourceT 
                    $ CB.sourceFile path
                    =$= CT.detectUtf
                    =$= CA.conduitParserEither parsePost
                    =$= awaitForever getPost
                    =$= processPost path
                    $$ commitIssues
  where
    getPost (Left s) = error $ show s
    getPost (Right (_, post)) = yield post

processPost :: (Monad m, MonadResource m) => FilePath -> Conduit Post m Post
processPost fp =
  do let mediaDir = getDir fp
     rawPost <- await
     case rawPost of
       Nothing -> return ()
       Just rp -> do post <- liftIO $ replacePost mediaDir rp
                     yield post
                     processPost fp
       where
         -- a post path is like that: post/postName.md,
         -- we need to get postName of this post
         getDir mediaPath = GC.mediaDirectory ++ SF.dropExtension (SF.takeFileName mediaPath)
         replacePost mediaDir post =
           do mp <- replaceMarkdown mediaDir (markdownPlus post) []
              return $ Post (frontMatter post) mp

replaceMarkdown :: FilePath -> [MarkdownPlus] -> [MarkdownPlus] -> IO [MarkdownPlus]
replaceMarkdown _ [] ys = return $ reverse ys
replaceMarkdown mediaPath (p@(Picture alt path t) : xs) ys =
  let unpackPicPath = T.unpack path
      newPath = mediaPath SF.</> SF.takeFileName unpackPicPath
  in if T.isPrefixOf "http" path || SF.equalFilePath (SF.takeDirectory unpackPicPath) mediaPath
     then replaceMarkdown mediaPath xs (p : ys)
     else do SD.copyFile unpackPicPath (mediaPath SF.</> SF.takeFileName unpackPicPath)
             replaceMarkdown mediaPath xs (Picture alt (T.pack newPath) t : ys)
replaceMarkdown mediaPath (x : xs) ys = replaceMarkdown mediaPath xs (x : ys) -- add liquid

commitIssues :: (Monad m, MonadResource m) => Consumer Post m ()
commitIssues =
  do maybePost <- await
     case maybePost of
       Nothing ->
         do liftIO $ putStrLn "All posts have been updated."
            return ()
       Just post ->
         do c <- liftIO $ parseConfig GC.configFile
            case c of
              Left e -> do liftIO $ print e
                           return ()
              Right config ->
                do let j = generateJson config post
                   r <- liftIO $ sendRequest (github_token config) j (url post config) (method post)
                   -- liftIO $ print (url post config) pass test
                   case r of
                      Nothing -> return ()
                      Just issueId -> commitIssues
       where
         url post config = let xs = T.split (== '/') (github_repo config)
                               iId = issueId (frontMatter post)
                            in "https://api.github.com/repos/"
                               `T.append` DL.last (init xs)
                               `T.append` "/"
                               `T.append` DL.last xs
                               `T.append` "/issues"
                               `T.append` if T.empty == iId
                                             then ""
                                             else "/" `T.append` iId
         method post = if issueId (frontMatter post) == T.empty
                          then "POST"
                          else "PATCH"

-- | refer to https://developer.github.com/v3/issues/
generateJson :: Config -> Post -> T.Text
generateJson config post = TE.decodeUtf8 $ toStrict $ RB.encodeJson t c tags
  where
    t = title $ frontMatter post
    c = T.intercalate "" (map (showVal config) $ markdownPlus post)
    tags = tag $ frontMatter post

showVal :: Config -> MarkdownPlus -> T.Text
showVal _ (Content t) = t
showVal _ (InlineEquation i) = T.cons '$' $ T.snoc i '$'
showVal _ (OutlineEquation o) = T.center 2 '$' o
showVal config (Liquid "Copyright") =
  fromMaybe "" $ copyright config
showVal _ (Liquid _) = ""
showVal config (Picture pa pp pt) =
  "!["
  `T.append` pa
  `T.append` "]("
  `T.append` (if T.isPrefixOf "http" pp
              then pp
              else T.dropWhileEnd (== '/') (github_repo config) `T.snoc` '/' `T.append` pp)
  `T.append` if pt == T.empty
             then ")"
             else " \"" `T.append` pt `T.append` "\")"

commitPosts :: IO ()
commitPosts = 
  do recordTime <- getRecModTime
     posts <- getChangedPost recordTime
     mapM_ readAndParse posts
