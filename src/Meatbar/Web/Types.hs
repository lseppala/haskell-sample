{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Meatbar.Web.Types
    ( MeatbarAction
    , MeatbarScotty
    , Env(..)
    , transact
    )where


import           Control.Monad.Reader      (MonadIO, MonadReader)
import           Web.Scotty.Trans          (ActionT, ScottyError, ScottyT)

import           Meatbar.Env


-- | Type alias for convenience and to provide access to 'Env' in route
-- handlers. This allows you to type
-- @
--   routeAction :: MeatbarAction ()
-- @
-- instead of
-- @
--   routeAction :: (MonadReader Env m, MonadIO m, ScottyError e) => ActionT e m a
-- @
type MeatbarWebCtx t a =
    forall m e.
        ( MonadReader Env m
        , MonadIO m
        , ScottyError e
        )
    => t e m a

type MeatbarAction a = MeatbarWebCtx ActionT a

type MeatbarScotty a = MeatbarWebCtx ScottyT a
