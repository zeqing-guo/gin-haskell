import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Binary as CB 
import           Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import System.IO

fileInputSource :: (Monad m, MonadResource m) => IO Handle -> Source m [String]
fileInputSource file = 
  CB.sourceIOHandle file -- Reads input from the file
    $= loop                            -- ...and feeds it to our loop
  where
    loop = do
      mbs <- await                   -- Await the next chunk of file input
      case mbs of
        Nothing -> return ()         -- On no input, terminate
        Just bs -> do
          yield $ Prelude.words $ BC.unpack bs  -- Otherwise, split the input into words and push it downstream
          loop                       -- ...and continue looping

conduitToByteString :: (Monad m, MonadIO m) => Conduit [String] m BC.ByteString
conduitToByteString = awaitForever (\xs -> 
                                       do liftIO $ mapM_ putStrLn xs
                                          yield . BC.pack . unlines $ xs) 

main = do
  let hInput = openFile "words.txt" ReadMode
      hOutput = openFile "words.txt" WriteMode
  _ <- runResourceT
    $  fileInputSource hInput
    $= conduitToByteString
    $$ CB.sinkIOHandle hOutput -- Writes the input stream to a file
  return ()
