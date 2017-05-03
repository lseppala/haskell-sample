{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Meatbar.Web.Types
    ( MeatbarAction
    , MeatbarScotty
    , Env(..)
    , transact
    )where


import           Control.Monad.Reader (MonadIO, MonadReader)
import           Data.Text.Lazy            (Text)
import           Web.Scotty.Trans     (ActionT, ScottyError, ScottyT)

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
type MeatbarAction a =
    forall m e.
        ( MonadReader Env m
        , MonadIO m
        , ScottyError e
        )
    => ActionT e m a

-- | Like 'MeatbarAction', but for 'ScottyT'. Specifies 'Text' as the
-- 'ScottyError' type
type MeatbarScotty a =
    forall m.
        ( MonadReader Env m
        , MonadIO m
        )
    => ScottyT Text m a
