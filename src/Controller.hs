{-# LANGUAGE OverloadedStrings #-}

module Controller (commitPosts) where

import qualified Conduit                      as C

import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit
import qualified Data.Conduit.Attoparsec      as CA
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Foldable                (forM_)
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import qualified System.Directory             as SD
import qualified System.FilePath              as SF
import           System.FilePath.Find
import           System.Posix.Types           (EpochTime)
import qualified System.PosixCompat.Files     as PF (FileStatus, getFileStatus,
                                                     modificationTime)

import           Exception
import qualified GinConfig                    as GC
import           ParsePost

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
getRecModTime :: (Monad m, MonadIO m) => Producer m EpochTime
getRecModTime = do t <- liftIO (PF.modificationTime
                                <$> PF.getFileStatus GC.ginRecordFile)
                   yield t

-- | Get the post list who were created or modified after the time store
-- in .gin/lastModificationTime
getChangedPost :: (Monad m, MonadIO m) => Conduit EpochTime m FilePath
getChangedPost = do t <- await
                    case t of
                      Nothing -> return ()
                      Just recordTime -> do xs <- findPosts recordTime GC.postDirectory
                                            mapM_ yield xs
                                            return ()
                where
                  findPosts :: (Monad m, MonadIO m) => EpochTime -> String -> m [FilePath]
                  findPosts t postsPath = liftIO $ find always --(const False <$> always)
                                                        (extension ==? ".md"
                                                         &&? modificationTime >? t)
                                                        postsPath

readAndParse :: (Monad m, MonadResource m) => Conduit FilePath m Post
readAndParse =
  do maybePath <- await
     case maybePath of
       Nothing -> return ()
       Just path -> CB.sourceFile path
                    =$= CT.detectUtf
                    =$= CA.conduitParserEither parsePost
                    =$= awaitForever getPost
                    =$= processPost path
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
  in if SF.equalFilePath unpackPicPath mediaPath
       then replaceMarkdown mediaPath xs (p : ys)
       else do SD.copyFile unpackPicPath (mediaPath SF.</> SF.takeFileName unpackPicPath)
               replaceMarkdown mediaPath xs (Picture alt (T.pack newPath) t : ys)
replaceMarkdown mediaPath (x : xs) ys = replaceMarkdown mediaPath xs (x : ys)


commitPosts :: IO ()
commitPosts = runResourceT $ getRecModTime
                             $= getChangedPost
                             =$= readAndParse
                             $$ C.printC

