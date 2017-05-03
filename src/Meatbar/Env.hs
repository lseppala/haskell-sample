{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Env
    ( Env(..)
    , transact
    , withBackend
    )where


import           Control.Monad.Reader
import           Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlConn)


-- | Environmental dependencies for use in the application. Currently only
-- contains the database connection, but could be extended as requirements
-- change
data Env = Env
    { _dbConn :: SqlBackend }


-- | Run a database action and lifts the result into the right transformer.
transact :: (MonadReader Env m, MonadTrans t, MonadIO (t m))
         => SqlPersistT IO a
         -> t m a
transact query = lift (asks _dbConn) >>= liftIO . runSqlConn query


withBackend :: MonadIO m
            => ReaderT SqlBackend m ()
            -> ReaderT Env m ()
withBackend = withReaderT _dbConn
