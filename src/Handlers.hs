{-# LANGUAGE MultiWayIf #-}
module Handlers
  (
  handler
  ) where


import           Conduit
import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe (fromJust, maybe)
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.System as T
import qualified Data.Time.LocalTime as T
import qualified Database.Persist as P
import           Web.Hastodon

import           DB
import           Queries
import           Types


handler :: StreamingPayload -> App ()
handler p = do
  case p of
    Thump -> handleThump
    (SNotification n) -> handleNotification n
    _ -> App $ return ()

handleThump :: App ()
handleThump = App $ do
  t' <- liftIO T.getSystemTime
  unApp $ handleNowPlaying t'

handleNowPlaying :: T.SystemTime -> App ()
handleNowPlaying t = do
  nps <- getNowPlaying
  case nps of
    Nothing -> insertRandomSong t
    Just (np, s) -> do
      let dur = (songDuration . P.entityVal) s
          started = (nowPlayingStartedAt . P.entityVal) np
      if tDiff t started >= dur
         then delNP (P.entityKey np)
         else return ()

tDiff :: T.SystemTime -> T.UTCTime -> Int
tDiff sysT songT = fromIntegral $ (T.systemSeconds sysT) - (T.systemSeconds $
                                                            T.utcToSystemTime songT)


-- newSong :: App Song
-- newSong = do
--   cs <- use dbUrl
--   songIds <- getSongIds cs :: App [Key Song]
--   undefined

  -- nowPlaying <- V.h <$> asks songs
  -- if songOver nowPlaying t' then handleSongOver

-- handleNotification :: Notification -> App ()
handleNotification n = do
  case (notificationType n) of
    "mention" -> let na = notificationAccount n
                     ns = notificationStatus n
                  in maybe (App $ return ()) (handleMention na) ns
    _ -> App $ return ()

-- handleMention :: Account -> Status -> App ()
handleMention n s = App $ do
  undefined

