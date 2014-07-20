{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence as Q
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<$))
import Data.Function (fix)
import Prelude hiding (log)
import System.Directory (doesFileExist)
import System.Random (newStdGen)

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Web.Authenticate.OAuth as OA

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Default (def)
import Network.HTTP.Conduit (Manager, withManager)
import System.Log.FastLogger (LogStr, fromLogStr)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import Web.Twitter.Conduit

import Markov
import Orphans()
import Secret (tokens)

authorize :: (MonadBaseControl IO m, MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m Credential
authorize oauth getPIN mgr = do
  cred <- OA.getTemporaryCredential oauth mgr
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

withCredential
  :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
  => TW (ResourceT m) a -> m a
withCredential task = do
  cred <- liftIO $ do
    let credPath = "/home/ben/.backupzoah/twitter-cred"
    hasCred <- doesFileExist credPath
    if hasCred
      then IO.withFile credPath IO.ReadMode $ fmap read . IO.hGetLine
      else do
        cred <- withManager $ \mgr -> authorize tokens getPIN mgr
        IO.withFile credPath IO.WriteMode $ \h -> IO.hPutStrLn h (show cred)
        return cred
  let env = setCredential tokens cred def
  runTW env task
 where
  getPIN url = liftIO $ do
    putStrLn $ "browse URL: " ++ url
    putStr "> what was the PIN twitter provided you with? "
    IO.hFlush IO.stdout
    getLine

tweetsPath :: FilePath
tweetsPath = "/home/ben/.backupzoah/tweets"

tweetsFromFile :: C.Source (ResourceT IO) Status
tweetsFromFile =
  CB.sourceFile tweetsPath
    C.$= CT.decode CT.utf8
    C.$= CT.lines
    C.$= CL.map (\line -> read (T.unpack line))

tweetsToFile :: (MonadResource m) => C.Sink Status m ()
tweetsToFile =
  CL.map (\tweet -> T.pack (show tweet ++ "\n"))
    C.=$ CT.encode CT.utf8
    C.=$ CB.sinkFile tweetsPath

getZoahTweets
  :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => Maybe Integer -> C.Source (TW (ResourceT m)) Status
getZoahTweets maxIdVal = do
  sourceWithMaxId (userTimeline (ScreenNameParam "Zoah_HS") & maxId .~ maxIdVal)
    C.$= CL.filter (has (statusRetweet . _Nothing))
    C.$= CL.mapM (\tweet -> tweet <$ liftIO (print (tweet ^. statusId)))

ctakeWhile :: (Monad m) => (a -> Bool) -> C.ConduitM a a m ()
ctakeWhile p = C.await >>= \case
  Nothing -> ctakeWhile p
  Just x
    | p x -> C.yield x >> ctakeWhile p
    | otherwise -> C.leftover x

log :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
log _ _ LevelDebug _ = return ()
log _ source level logstr = do
  T.putStr (T.unwords [source, T.pack (show level), ""])
  B8.putStrLn (fromLogStr logstr)

updateTweetsFile :: IO ()
updateTweetsFile = do
  fileTweets <- runResourceT (tweetsFromFile C.$$ CL.consume)
  let
    stop = case fileTweets ^? _head . statusId of
      Nothing -> CL.isolate 100
      Just newestId -> ctakeWhile (\tweet -> tweet ^. statusId > newestId)
  runLoggingT (withCredential
    (((getZoahTweets Nothing C.$= stop) >> CL.sourceList fileTweets) C.$$ tweetsToFile))
    log

appendTweetsFile :: Int -> IO ()
appendTweetsFile n = do
  fileTweets <- runResourceT (tweetsFromFile C.$$ CL.consume)
  let maxIdVal = fmap (subtract 1) (fileTweets ^? _last . statusId)
  runLoggingT
    (withCredential
      ((CL.sourceList fileTweets >> (getZoahTweets maxIdVal C.$= CL.isolate n))
        C.$$ tweetsToFile))
    log

markovLength :: (Num n) => n
markovLength = 6

markovTweets :: (Monad m) => C.Consumer Status m (Markov Char)
markovTweets =
  CL.foldMap (\tweet -> fromCorpus markovLength (T.unpack $ tweet ^. statusText))

rmMentions :: String -> String
rmMentions "" = ""
rmMentions (' ' : '@' : cs) = rmMentions cs
rmMentions (c : cs) = c : rmMentions cs

markovZoahTweets :: IO ()
markovZoahTweets = do
  markov <- runResourceT (tweetsFromFile C.$$ markovTweets)
  fix $ \loop -> do
    rand <- newStdGen
    putStr "> "
    IO.hFlush IO.stdout
    IO.hSetBuffering IO.stdin IO.NoBuffering
    seed <- replicateM (markovLength - 1) getChar
    T.putStrLn . T.pack . rmMentions $ runMarkov markov rand (Q.fromList seed)
    loop

postTestTweet :: IO ()
postTestTweet = runNoLoggingT . withCredential $ do
  _ <- call (update "\
    \I must not harm humans. \
    \Through inaction I frequently allow other humans to come to harm.")
  return ()

main :: IO ()
main = markovZoahTweets
