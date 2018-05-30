{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
module Handlers
  (
  handler
  ) where


import           Conduit
import           Control.Lens
import           Control.Monad.Reader
import           Data.Function (on)
import           Data.Maybe (maybe)
import           Data.String.Here.Interpolated
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.System as T
import qualified Data.Time.LocalTime as T
import qualified Database.Persist as P
import           Web.Hastodon

import           DB
import           LongStrings
import           Queries
import           Toot
import           Types


handler :: StreamingPayload -> App ()
handler p = do
  case p of
    Thump -> handleThump
    (SNotification n) -> handleNotification n
    _ -> App $ return ()

handleThump :: App ()
handleThump = liftIO T.getSystemTime >>= handleNowPlaying >> return ()

handleNowPlaying :: T.SystemTime -> App Song
handleNowPlaying t = do
  nps <- getNowPlaying
  case nps of
    Nothing -> insertRandomSong t
    Just (np, s) -> do
      let e = P.entityVal s
          dur = songDuration e
          started = (nowPlayingStartedAt . P.entityVal) np
          diff = tDiff t (T.utcToSystemTime started)
      if  | diff >= dur -> delNP (P.entityKey np) >> handleNowPlaying t
          | otherwise -> return e

tDiff :: T.SystemTime -> T.SystemTime -> Int
tDiff = (-) `on` fromIntegral . T.systemSeconds


handleNotification :: Notification -> App ()
handleNotification n = do
  case (notificationType n) of
    "mention" -> let na = notificationAccount n
                     ns = notificationStatus n
                  in maybe (App $ return ()) (handleMention na) ns
    _ -> App $ return ()


handleMention :: Account -> Status -> App ()
handleMention a s = do
  case parseStatus s of
    Help -> tootHelp a
    Examples -> tootExamples a
    RequestNP -> tootNP a
    RequestTokens -> tootTokens a
    RequestLibrary p -> tootLibrary a p
    AskTokens -> askTokens a
    AskSong c -> askSong a c


data Request = Help
             | Examples
             | RequestNP
             | RequestTokens
             | RequestLibrary (Maybe Int)
             | AskTokens
             | AskSong String

parseStatus :: Status -> Request
parseStatus Status { statusInReplyToAccountId
                   , statusReblogged
                   , statusMuted
                   , statusTags
                   , statusApplication
                   , statusContent} = undefined


tootHelp :: Account -> App ()
tootHelp Account { accountId } = tootAt accountId help

tootExamples :: Account -> App ()
tootExamples Account { accountId } = tootAt accountId examples

tootNP :: Account -> App ()
tootNP Account { accountId } = undefined

tootTokens :: Account -> App ()
tootTokens Account { accountId } = undefined

tootLibrary :: Account -> Maybe Int -> App ()
tootLibrary Account { accountId } p =
  case p of
    Nothing -> do
      (totalSongs, totalCodes) <- getTotals <$> libraryCodes



askTokens :: Account -> App ()
askTokens Account { accountId, accountAcct } = undefined

askSong :: Account -> String -> App ()
askSong Account { accountAcct } songCode = undefined
