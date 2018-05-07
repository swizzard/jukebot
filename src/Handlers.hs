module Handlers
  (
  handler
  ) where


import           Conduit
import           Control.Lens
import           Control.Monad.State
import           Data.Maybe (maybe)
import qualified Data.Time.Clock.System as T
import qualified Data.Time.LocalTime as T
import qualified Data.Vector as V
import           Web.Hastodon

import           DB
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
  thumpCount += 1
  timeStamp .= t'

handleNowPlaying :: T.SystemTime -> App ()
handleNowPlaying t = App $ do
  ss <- use songs
  undefined

  -- nowPlaying <- V.h <$> asks songs
  -- if songOver nowPlaying t' then handleSongOver

handleNotification :: Notification -> App ()
handleNotification n = do
  case (notificationType n) of
    "mention" -> let na = notificationAccount n
                     ns = notificationStatus n
                  in maybe (App $ return ()) (handleMention na) ns
    _ -> App $ return ()

handleMention :: Account -> Status -> App ()
handleMention n s = App $ do
  undefined

