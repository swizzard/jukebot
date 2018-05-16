module Queries
  ( delNP
  , getNowPlaying
  , insertRandomSong
  )
    where

import           Control.Monad.IO.Class
import           Data.Maybe (listToMaybe)
import qualified Data.Time.Clock.System as T
import qualified Data.Time as T
import           Database.Esqueleto
import           System.Random (randomRIO)

import           DB
import           Types


getRandomSong = do
  s <- select $
    from $ \song -> do
      return song
  let sIds = map entityKey s
  ix <- liftIO $ randomRIO (0, length sIds - 1)
  return $ sIds !! ix

insertRandomSong :: T.SystemTime -> App ()
insertRandomSong t = doDb $ do
  sId <- getRandomSong
  let np = NowPlaying sId (T.systemToUTCTime t)
  insert np
  return ()

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
