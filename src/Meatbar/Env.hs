{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Meatbar.Env
    ( Env(..)
    , transact
    )where


import           Control.Monad.Reader      (MonadIO, MonadReader, asks, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Pool                 (Pool)
import           Database.Persist.Sql      (SqlBackend, SqlPersistT, runSqlPool)

-- | The environment for use in the application. Currently only contains the
-- database pool, but can be extended as required
data Env = Env
    { _dbPool :: Pool SqlBackend }


-- | Run a database action and lifts the result into the right transformer.
transact :: (MonadReader Env m, MonadTrans t, MonadIO (t m))
         => SqlPersistT IO a
         -> t m a
transact query = lift (asks _dbPool) >>= liftIO . runSqlPool query

