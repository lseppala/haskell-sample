{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Meatbar
    ( runWithInMemorySqlite
    , runMigrations
    , loadCsvData
    , startServer
    ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy    as BL
import           Database.Persist.Sqlite

import           Meatbar.Env
import           Meatbar.Load
import           Meatbar.Model
import           Meatbar.Web
import           Meatbar.Database.Consumption


-- | Run actions in the in-memory SQLite database, without debug logging
runWithInMemorySqlite :: ReaderT Env (NoLoggingT IO) () -> IO ()
runWithInMemorySqlite actions =
    runNoLoggingT $ withSqliteConn "meatbar.db" $
        \conn -> actions `runReaderT` Env conn


-- | Run all model migrations
runMigrations :: MonadIO m
              => ReaderT Env m ()
runMigrations =
    withBackend $ do
        runMigration migrateModel
        rollupConsumption


-- | Given a FilePath to CSV file, load of all the consumption records
-- 'fail' ('IOException', likely) if there's any error parsing the file
loadCsvData :: MonadIO m
            => FilePath
            -> ReaderT Env m ()
loadCsvData fp = do
    csv <- liftIO (BL.readFile fp)
    runExceptT (decodeAndLoadRecords csv)
      >>= either fail return
