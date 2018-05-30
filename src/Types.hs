{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Config(..)
  , App(..)
  , initialConfig
  , logErr
  ) where

import           Control.Lens.TH
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (maybe)
import           Data.Time.Clock.System
import qualified Data.Vector as V
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           System.Environment (getEnv, lookupEnv)
import           System.Random
import           Text.Read (readMaybe)
import           Web.Hastodon


newtype LoggerFunc = LoggerFunc { runLoggerFunc :: String -> IO () }

logErr :: Show a => LoggerFunc -> a -> IO ()
logErr f = runLoggerFunc f . show

debugLogFunc :: LoggerFunc
debugLogFunc = LoggerFunc putStrLn

prodLogFunc :: LoggerFunc
prodLogFunc = LoggerFunc undefined


data Config = Config { pool :: ConnectionPool
                     , startedTime :: SystemTime
                     , client :: HastodonClient
                     , logger :: LoggerFunc
                     , songsPerPage :: Int
                     }

getClient :: IO HastodonClient
getClient = HastodonClient <$> getEnv "INSTANCE" <*> getEnv "TOKEN"

getLogger :: Maybe String -> LoggerFunc
getLogger deb = case deb of
             Nothing -> debugLogFunc
             (Just v) -> if v == "false" then prodLogFunc else debugLogFunc

getPerPage :: Maybe String -> Int
getPerPage p = case (p >>= readMaybe) of
                 Nothing -> 20
                 (Just v) -> v

initialConfig :: IO Config
initialConfig = do
  dbu <- BS.pack <$> getEnv "DATABASE_URL"
  poolSize <- read <$> getEnv "POOL_SIZE"
  pool <- runStderrLoggingT $ createPostgresqlPool dbu poolSize
  ts <- getSystemTime
  cl <- getClient
  l <- getLogger <$> lookupEnv "DEBUG"
  pp <- getPerPage <$> lookupEnv "PER_PAGE"
  return $ Config pool ts cl l pp

newtype App a = App {
  unApp :: ReaderT Config IO a
                    } deriving (Functor,
                                Applicative,
                                Monad,
                                MonadIO,
                                MonadReader Config,
                                MonadUnliftIO
                               )
