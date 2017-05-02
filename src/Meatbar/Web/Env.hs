{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Meatbar.Web.Env
    ( Meatbar
    , MeatbarAction
    , MeatbarScotty
    , Env(..)
    , transact
    )where


import           Control.Monad.Reader      (MonadIO, MonadReader, asks, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Pool                 (Pool)
import           Database.Persist.Sql      (SqlBackend, SqlPersistT, runSqlPool)
import           Web.Scotty.Trans          (ActionT, ScottyError, ScottyT)


-- | Type alias for convenience and to provide access to 'Env' in route
-- handlers. This allows you to type
-- @
--   routeAction :: MeatbarAction ()
-- @
-- instead of
-- @
--   routeAction :: (MonadReader Env m, MonadIO m, ScottyError e) => ActionT e m a
-- @
type Meatbar t a =
    forall m e.
        ( MonadReader Env m
        , MonadIO m
        , ScottyError e
        )
    => t e m a

type MeatbarAction a = Meatbar ActionT a

type MeatbarScotty a = Meatbar ScottyT a


-- | The environment for use in route handlers. Currently only
-- contains the database pool, but could be extended as required
data Env = Env
    { _dbPool :: Pool SqlBackend }


-- | Run a database action and lifts the result into the right transformer.
-- This will be 'MeatbarAction a' in all cases.
transact :: (MonadReader Env m, MonadTrans t, MonadIO (t m))
         => (SqlPersistT IO a)
         -> t m a
transact query = lift (asks _dbPool) >>= liftIO . runSqlPool query
