{-# LANGUAGE OverloadedStrings #-}

module Controller (commitPosts) where

import           Control.Monad                (when)
import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8        as BC
import           Data.ByteString.Lazy         (toStrict)
import           Data.Conduit
import qualified Data.Conduit.Attoparsec      as CA
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Text            as CT
import qualified Data.List                    as DL
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time.Clock.POSIX        (getPOSIXTime)

import qualified System.Directory             as SD
import qualified System.FilePath              as SF
import           System.FilePath.Find
import           System.Posix.Types           (EpochTime)
import qualified System.PosixCompat.Files     as PF (getFileStatus,
                                                     modificationTime,
                                                     setFileTimes)

import           ConnectGithub
import qualified GinConfig                    as GC
import           ParseConfig
import           ParsePost
import qualified RequestBody                  as RB

-- | Get the time of last modification of this blog from .gin/lastModificationTime
getRecModTime :: IO EpochTime
getRecModTime = PF.modificationTime
                <$> PF.getFileStatus GC.ginRecordFile

setRecModTime :: IO ()
setRecModTime = do t <- getPOSIXTime
                   let newT = fromInteger $ round t
                   PF.setFileTimes GC.ginRecordFile newT newT

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
                    =$= commitIssues
                    $$ CB.sinkFile $ GC.ginConfig SF.</> SF.takeFileName path
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

commitIssues :: (Monad m, MonadResource m) => Conduit Post m BC.ByteString
commitIssues =
  do maybePost <- await
     case maybePost of
       Nothing -> return ()
       Just post ->
         do c <- liftIO $ parseConfig GC.configFile
            case c of
              Left e -> do liftIO $ print e
                           return ()
              Right config ->
                do let j = generateJson config post
                   r <- liftIO $ sendRequest (github_token config) j (url post config) (method post) (title $ frontMatter post)
                   case r of
                      Nothing -> return ()
                      Just iId -> if issueId (frontMatter post) == T.empty
                                  then do yield $ replaceIssueId post iId
                                          commitIssues
                                  else do yield $ showValForFile post
                                          commitIssues
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

replaceIssueId :: Post -> BC.ByteString -> BC.ByteString
replaceIssueId p iId = let fm = frontMatter p
                           newFm = FrontMatter (title fm) (date fm) (TE.decodeUtf8 iId) (tag fm)
                           newPost = Post newFm (markdownPlus p)
                       in showValForFile newPost

showValForFile :: Post -> BC.ByteString
showValForFile p = "---\n"
                   `BC.append` showValFM (frontMatter p)
                   `BC.append` "---\n"
                   `BC.append` showValMP (markdownPlus p) ""

showValFM :: FrontMatter -> BC.ByteString
showValFM fm = "title: "
               `BC.append` TE.encodeUtf8 (title fm)
               `BC.append` "\ndate: "
               `BC.append` TE.encodeUtf8 (date fm)
               `BC.append` "\nissue id: "
               `BC.append` TE.encodeUtf8 (issueId fm)
               `BC.append` "\ntags: \n"
               `BC.append` DL.foldl' (\acc t -> acc
                                                `BC.append` "- "
                                                `BC.append` TE.encodeUtf8 t
                                                `BC.append` "\n")
                                     ""
                                     (tag fm)

showValMP :: [MarkdownPlus] -> BC.ByteString -> BC.ByteString
showValMP [] p = p
showValMP (Content t : xs) p = showValMP xs (p `BC.append` TE.encodeUtf8 t)
showValMP (InlineEquation i : xs) p = let encodeI = TE.encodeUtf8 i
                                          newI = BC.cons '$' $ BC.snoc encodeI '$'
                                      in showValMP xs (p `BC.append` newI)
showValMP (OutlineEquation o : xs) p = let newO = T.center 2 '$' o
                                           encodeO = TE.encodeUtf8 newO
                                       in showValMP xs (p `BC.append` encodeO)
showValMP (Liquid l : xs) p = let newL = "{{ " `T.append` l `T.append` " }}"
                                  encodeL = TE.encodeUtf8 newL
                              in showValMP xs (p `BC.append` encodeL)
showValMP (Picture pa pp pt : xs) p =
  let newP = "!["
             `BC.append` TE.encodeUtf8 pa
             `BC.append` "]("
             `BC.append` TE.encodeUtf8 pp
             `BC.append` TE.encodeUtf8 (if pt == T.empty
                                        then ")"
                                        else " \"" `T.append` pt `T.append` "\")")
  in showValMP xs (p `BC.append` newP)

-- | refer to https://developer.github.com/v3/issues/
generateJson :: Config -> Post -> T.Text
generateJson config post = TE.decodeUtf8 $ toStrict $ RB.encodeJson t c tags
  where
    t = title $ frontMatter post
    c = T.intercalate "" (map (showValForJson config) $ markdownPlus post)
    tags = tag $ frontMatter post

showValForJson :: Config -> MarkdownPlus -> T.Text
showValForJson _ (Content t) = t
showValForJson _ (InlineEquation i) = T.cons '$' $ T.snoc i '$'
showValForJson _ (OutlineEquation o) = T.center 2 '$' o
showValForJson config (Liquid "Copyright") =
  fromMaybe "" $ copyright config
showValForJson _ (Liquid _) = ""
showValForJson config (Picture pa pp pt) =
  "!["
  `T.append` pa
  `T.append` "]("
  `T.append` (if T.isPrefixOf "http" pp
              then pp
              else T.dropWhileEnd (== '/') (github_repo config) `T.snoc` '/' `T.append` pp)
  `T.append` if pt == T.empty
             then ")"
             else " \"" `T.append` pt `T.append` "\")"

replaceFiles :: FilePath -> IO ()
replaceFiles post =
  do let postName = SF.takeFileName post
         ginPostName = GC.ginConfig SF.</> postName
     exist <- SD.doesFileExist ginPostName
     when exist $ SD.renameFile ginPostName post >> setRecModTime

commitPosts :: IO ()
commitPosts =
  do recordTime <- getRecModTime
     posts <- getChangedPost recordTime
     mapM_ readAndParse posts
     mapM_ replaceFiles posts
     putStrLn "All posts have been updated."
