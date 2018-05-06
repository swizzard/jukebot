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
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time.Clock
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql (rawExecute)
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Album json
  name String
  code String
  deriving Show
Song json
  name String
  artist String
  album AlbumId
  deriving Show
User json
  mastodonId String
  tokens Int
  lastRequest UTCTime default=now()
  deriving Show
|]


doMigrate :: ConnectionString -> IO ()
doMigrate cs = do
  runStdoutLoggingT $ withPostgresqlPool cs 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      return ()
