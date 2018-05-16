{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Config(..)
  , App(..)
  , initialConfig
  ) where

import           Control.Lens.TH
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock.System
import qualified Data.Vector as V
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           System.Environment (getEnv)
import           System.Random
import           Web.Hastodon


data Config = Config { pool :: ConnectionPool
                     , startedTime :: SystemTime
                     , client :: HastodonClient
                     }

getClient :: IO HastodonClient
getClient = HastodonClient <$> getEnv "INSTANCE" <*> getEnv "TOKEN"

initialConfig :: IO Config
initialConfig = do
  dbu <- BS.pack <$> getEnv "DATABASE_URL"
  poolSize <- read <$> getEnv "POOL_SIZE"
  pool <- runStderrLoggingT $ createPostgresqlPool dbu poolSize
  ts <- getSystemTime
  cl <- getClient
  return $ Config pool ts cl

newtype App a = App {
  unApp :: ReaderT Config IO a
                    } deriving (Functor,
                                Applicative,
                                Monad,
                                MonadIO,
                                MonadReader Config,
                                MonadUnliftIO
                               )
