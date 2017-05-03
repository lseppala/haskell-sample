{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Web where

import Web.Scotty.Trans
import Control.Monad.Reader

import Meatbar.Env
import Meatbar.Web.Route


startServer :: (MonadReader Env m, MonadIO m)
            => Int
            -> m ()
startServer port = do
    env <- ask
    scottyT port (`runReaderT` env) routes
