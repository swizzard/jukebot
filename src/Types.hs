{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( LoopState(..)
  , dbUrl
  , thumpCount
  , timeStamp
  , songs
  , initialState
  , App(..)
  , getClient
  ) where

import           Control.Lens.TH
import           Control.Monad.Trans.Resource
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock.System
import qualified Data.Vector as V
import           System.Environment (getEnv)
import           System.Random
import           Web.Hastodon

import           DB (Song(..))


data LoopState = LoopState { _dbUrl :: BS.ByteString
                           , _thumpCount :: Int
                           , _timeStamp :: SystemTime
                           , _songs :: V.Vector Song
                           , _gen :: StdGen
                           }
makeLenses ''LoopState

initialState :: IO LoopState
initialState = do
  dbu <- BS.pack <$> getEnv "DATABASE_URL"
  ts <- getSystemTime
  g <- getStdGen
  return $ LoopState dbu 0 ts V.empty g

getClient :: IO HastodonClient
getClient = HastodonClient <$> getEnv "INSTANCE" <*> getEnv "TOKEN"

newtype App a = App {
  unApp :: StateT LoopState (ResourceT IO) a
                    } deriving (Functor,
                                Applicative,
                                Monad,
                                MonadIO,
                                MonadResource,
                                MonadState LoopState
                               )
