{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Queries
  ( delNP
  , getNowPlaying
  , insertRandomSong
  )
    where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Maybe (listToMaybe)
import qualified Data.Time.Clock.System as T
import qualified Data.Time as T
import           Database.Esqueleto
import           System.Random (randomRIO)

import           DB
import           Types


getRandomSong :: MonadIO m => SqlPersistT m (Entity Song)
getRandomSong = do
  s <- select $
    from $ \song -> return song
  ix <- liftIO $ randomRIO (0, length s - 1)
  return $ s !! ix

insertRandomSong :: T.SystemTime -> App Song
insertRandomSong t = doDb $ do
  s <- getRandomSong
  let np = NowPlaying (entityKey s) (T.systemToUTCTime t)
  insert np
  return (entityVal s)

getNowPlaying :: App (Maybe (Entity NowPlaying, Entity Song))
getNowPlaying = doDb $ do
  nps <- select $
    from $ \(np, s) -> do
      where_ (np ^. NowPlayingSongId ==. s ^. SongId)
      limit 1
      return (np, s)
  return $ listToMaybe nps

delNP :: Key NowPlaying -> App ()
delNP k = doDb $ do
  deleteKey k

page :: Int -> App [(String, String)]
page n = doDb $ do
  ss <- select $
    from $ \song -> do
      orderBy [desc (song ^. SongCode)]
      limit 20
      offset $ fromIntegral (20 * (n - 1))
      return song
  return $ map (f . entityVal) ss
    where f s = (songName s, songCode s)

libraryCodes :: App [String]
libraryCodes = doDb $ do
  ss <- select $
    from $ \song -> return song
  return $ map (songCode . entityVal) ss

