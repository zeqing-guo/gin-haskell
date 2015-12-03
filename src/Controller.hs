{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller (commitPosts) where

import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8    as BS
import           Data.Conduit
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Foldable            (forM_)
import           Data.Monoid
import qualified Data.Text                as T
import           GHC.Generics
import           System.FilePath.Find
import           System.Posix.Types       (EpochTime)
import qualified System.PosixCompat.Files as PF (FileStatus, getFileStatus,
                                                 modificationTime)

import           Exception
import qualified GinConfig                as GC

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
getRecModTime = do t <- liftIO (PF.modificationTime <$> PF.getFileStatus GC.ginRecordFile)
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

commitPosts :: IO ()
commitPosts = do xs <- getRecModTime $= getChangedPost $$ CL.consume
                 mapM_ putStrLn xs

