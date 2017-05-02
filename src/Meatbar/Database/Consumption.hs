{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Database.Consumption where

import           Control.Monad.IO.Class
import           Database.Persist.Sql

import           Meatbar.Model


-- | list all meatbar consumptions, in ascending order (oldest first)
listConsumptions :: MonadIO m
                 => SqlPersistT m [Entity Consumption]
listConsumptions = selectList [] [Asc ConsumptionConsumedAt]



-- | Idempotently add a Consumption to the database.
putConsumption  :: MonadIO m
                => Consumption
                -> SqlPersistT m (Either (Entity Consumption) (Key Consumption))
putConsumption consumption = insertBy consumption
