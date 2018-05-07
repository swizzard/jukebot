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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Int (Int64)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Album json
  name String
  code String
  deriving Show
Song json
  name String
  artist String
  album AlbumId
  url String
  duration Int
  deriving Show
User json
  mastodonId String
  tokens Int
  lastRequest UTCTime default=now()
  deriving Show
|]

doDb :: (MonadLogger m, MonadUnliftIO m) =>
  ConnectionString ->
  ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a ->
  m a
doDb cs f = do
  withPostgresqlPool cs 10 $ \pool -> liftIO $ do
    runSqlPersistMPool f pool


-- runStdoutLoggingT $ doMigrate
doMigrate :: (MonadLogger m, MonadUnliftIO m) => ConnectionString -> m ()
doMigrate cs = doDb cs $ runMigration migrateAll >> return ()

getSongIds :: (MonadLogger m, MonadUnliftIO m) => ConnectionString -> m [Key Song]
getSongIds cs = doDb cs $ f <$> selectList [] []
  where f = map entityKey
