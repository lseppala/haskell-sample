{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Database.Consumption where

import           Control.Monad.IO.Class
import           Control.Monad.Reader   (runReaderT)
import           Database.Persist

import           Meatbar.Model


type ConsumptionBackend backend =
    ( PersistQueryRead backend
    , PersistRecordBackend Consumption backend
    , PersistUniqueWrite backend
    )

-- | list all meatbar consumptions, in ascending order (oldest first)
listConsumptions :: (ConsumptionBackend backend, MonadIO m)
           => backend
           -> m [Entity Consumption]
listConsumptions db = selectList [] [Asc ConsumptionConsumedAt] `runReaderT` db



-- | Idempotently add a Consumption to the database.
putConsumption :: (ConsumptionBackend backend, MonadIO m)
               => Consumption
               -> backend
               -> m (Either (Entity Consumption) (Key Consumption))
putConsumption consumption db = insertBy consumption `runReaderT` db
