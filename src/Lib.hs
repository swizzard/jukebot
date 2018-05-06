{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import DB

import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Async
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.State.Strict as ST
import Conduit
import Data.Conduit.Lift (evalStateC)
import Data.Maybe (maybe)
import Data.Time.Clock.System (getSystemTime, SystemTime)
import System.Environment (getEnv)
import Web.Hastodon

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data LoopState = LoopState { _dbUrl :: BS.ByteString
                           , _thumpCount :: Int
                           , _timeStamp :: SystemTime
                           }
makeLenses ''LoopState


loop :: IO ()
loop = do
  dbUrl <- BS.pack <$> getEnv "DATABASE_URL"
  mastoClient <- HastodonClient <$> getEnv "INSTANCE" <*> getEnv "TOKEN"
  t <- getSystemTime
  let s = LoopState dbUrl 0 t
  runConduitRes $ evalStateC s $ streamUser mastoClient .| mapM_C handler

handler :: StreamingPayload -> StateT LoopState (ResourceT IO) ()
handler Thump = do
  thumpCount += 1
  t' <- liftIO getSystemTime
  timeStamp .= t'
handler (SNotification n) = do
  case (notificationType n) of
    "mention" -> let na = notificationAccount n
                     ns = notificationStatus n
                  in maybe (return ()) (handleMention na) ns
    _ -> return ()
handler _ = return ()

handleMention :: Account -> Status -> StateT LoopState (ResourceT IO) ()
handleMention account status = undefined
