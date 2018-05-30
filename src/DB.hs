{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module DB where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Pool
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.System as T
import qualified Data.Time.LocalTime as T
import qualified Database.Esqueleto as E
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import Types


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Song json
  name String
  artist String
  code String
  url String
  duration Int
  deriving Show
User json
  mastodonId String
  tokens Int
  lastRequest T.UTCTime default=now()
  deriving Show
NowPlaying json
  songId SongId
  startedAt T.UTCTime
|]


doDb f = do
  p <- asks pool
  liftIO $ flip runSqlPersistMPool p $ f

migrate :: App ()
migrate = doDb $ runMigration migrateAll >> return ()
