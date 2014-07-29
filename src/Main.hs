{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence as Q
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception (handleJust)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (fix)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Prelude hiding (log)
import System.Directory (doesFileExist)
import System.Random (newStdGen)

import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Web.Authenticate.OAuth as OA

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.Control (control)
import Control.Monad.Trans.Resource
import Data.Default (def)
import Network.HTTP.Conduit (Manager, withManager, HttpException (StatusCodeException))
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

withLog :: LoggingT m a -> m a
withLog = flip runLoggingT log
 where
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
  withLog . withCredential $
    ((getZoahTweets Nothing C.$= stop) >> CL.sourceList fileTweets) C.$$ tweetsToFile

appendTweetsFile :: Int -> IO ()
appendTweetsFile n = do
  fileTweets <- runResourceT (tweetsFromFile C.$$ CL.consume)
  let maxIdVal = fmap (subtract 1) (fileTweets ^? _last . statusId)
  withLog . withCredential $
    ((CL.sourceList fileTweets >> (getZoahTweets maxIdVal C.$= CL.isolate n))
      C.$$ tweetsToFile)

type Token = CI.CI T.Text

textToTokens :: T.Text -> [Token]
textToTokens = map CI.mk . map unMention . T.words
 where
  unMention text = case T.uncons text of
    Just ('@', rest) -> rest
    _ -> text

getSeed :: IO [Token]
getSeed = textToTokens <$> T.getLine

tokensToText :: [Token] -> T.Text
tokensToText = T.unwords . map CI.original

markovLength :: (Num n) => n
markovLength = 2

markovTweets :: (Monad m) => C.Consumer Status m (Markov Token)
markovTweets =
  CL.foldMap (\tweet ->
    fromCorpus markovLength (textToTokens $ tweet ^. statusText))

runMarkovIO :: (Ord c) => Markov c -> [c] -> IO [c]
runMarkovIO markov seed =
  fmap (\rand -> runMarkov markov rand (Q.fromList seed)) newStdGen

testMarkov :: IO ()
testMarkov = do
  markov <- runResourceT (tweetsFromFile C.$$ markovTweets)
  fix $ \loop -> do
    putStr "> "
    IO.hFlush IO.stdout
    seed <- getSeed
    T.putStrLn . tokensToText =<< runMarkovIO markov seed
    loop

postTestTweet :: IO ()
postTestTweet = runNoLoggingT . withCredential $ do
  _ <- call (update "\
    \I must not harm humans. \
    \Through inaction I frequently allow other humans to come to harm.")
  return ()

sincePath :: FilePath
sincePath = "/home/ben/.backupzoah/mentionsSince"

readSinceId :: IO Integer
readSinceId = read <$> readFile sincePath

writeSinceId :: Integer -> IO ()
writeSinceId n = writeFile sincePath (show n ++ "\n")

forMentions :: (MonadBaseControl IO m, MonadLogger m, MonadThrow m, MonadIO m)
  => (Status -> TW (ResourceT m) ()) -> TW (ResourceT m) ()
forMentions k = forever . withExceptions $ do
  since <- liftIO readSinceId
  tweets <- call (mentionsTimeline & sinceId ?~ since)
  liftIO $ do
    writeSinceId (fromMaybe since (maximumOf (folded . statusId) tweets))
    mapM_ print tweets
  mapM_ k tweets
  liftIO $ threadDelay (30 * 1000 * 1000)
 where
  withExceptions act = control $ \runInIO ->
    handleJust
      isStatusCodeException
      (\s -> waitOnRateLimit s >> runInIO (withExceptions act))
      (runInIO act)
  isStatusCodeException (StatusCodeException _ headers _) =
    lookup (CI.mk "x-rate-limit-reset") headers
  isStatusCodeException _ = Nothing
  waitOnRateLimit s = do
    now <- getCurrentTime
    putStrLn $ "waiting for " ++ show until
    threadDelay (1000 * 1000 * (ceiling (diffUTCTime until now) + 30))
   where
    until = posixSecondsToUTCTime . fromInteger . read . B8.unpack $ s

data Truncatedness = Untruncated | Truncated deriving (Eq, Ord)

main :: IO ()
main = do
  markov <- runResourceT (tweetsFromFile C.$$ markovTweets)
  runNoLoggingT . withCredential . forMentions $ \tweet -> do
    let
      who = T.cons '@' (tweet ^. statusUser . userScreenName)
      seeds =
        map textToTokens .
        sortBy (flip (comparing T.length)) .
        filter (/= self) .
        T.words
          $ tweet ^. statusText
    results <- mapM (\seed -> (seed ,) <$> liftIO (runMarkovIO markov seed)) seeds
    let
      mkTweet (seed, result) = do
        guard $ not (null result)
        let
          words = who : map CI.original (seed ++ result)
          _ : withLen = scanl accuLen (-1, "") words
          accuLen (len, _) text = (len + 1 + T.length text, text)
        Just $ case span (\ (len, _) -> len <= 140) withLen of
          (xs, []) -> (Untruncated, xs)
          (xs, _) -> (Truncated, xs)
      resultTweets =
        sortBy (comparing fst <> comparing (length . snd))
          (mapMaybe mkTweet results)
    case resultTweets of
      [] -> return ()
      (_, xs) : _ -> do
          liftIO $ print tweet
          void $ call (update tweet)
        where tweet = T.unwords (map snd xs)
 where
  self = "@bensguineapig"
