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


runWithInMemorySqlite :: ReaderT Env (NoLoggingT IO) () -> IO ()
runWithInMemorySqlite actions = do
    runNoLoggingT $ withSqliteConn ":memory:" $
        \conn -> actions `runReaderT` Env conn


runMigrations :: MonadIO m
              => ReaderT Env m ()
runMigrations =
    withBackend (runMigration migrateModel)


loadCsvData :: MonadIO m
            => FilePath
            -> ReaderT Env m ()
loadCsvData fp = do
    csv <- liftIO (BL.readFile fp)
    runExceptT (decodeAndLoadRecords csv)
      >>= either fail return
