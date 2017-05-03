{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Database.Consumption where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function          (on)
import           Data.List              (sortBy)
import           Database.Persist.Sql

import           Meatbar.Data.Analysis
import           Meatbar.Model


-- | list all meatbar consumptions, in ascending order (oldest first)
listConsumptions :: MonadIO m
                 => SqlPersistT m [Entity Consumption]
listConsumptions = selectList [] [Asc ConsumptionConsumedAt]


-- | Idempotently add a Consumption to the database.
putConsumption  :: MonadIO m
                => Consumption
                -> SqlPersistT m (Either (Entity Consumption) (Key Consumption))
putConsumption = insertBy

rollupConsumption :: MonadIO m
                  => SqlPersistT m ()
rollupConsumption = do
    cs <- listConsumptions
    let groups = groupedBy byPersonAndYearMonth cs
    forM_ groups $ \g -> do
        let pId = fst $ groupKey g
            yM = snd $ groupKey g
            numBars = groupCount g
            es = groupElems g
            meatTypeGroups =
                groupedBy byMeatType $ sortBy (compare `on` byMeatType) es
            maxMeatType = groupKey $ largestGroup meatTypeGroups
            consumptionRollup =
                ConsumptionRollup pId yM numBars maxMeatType

        upsert consumptionRollup
            [ ConsumptionRollupNumBarsConsumed =. numBars
            , ConsumptionRollupMostConsumedBarType =. maxMeatType
            ]

    where
        byPersonAndYearMonth c =
            ( consumptionPersonId $ entityVal c
            , timeToYearMonth . consumptionConsumedAt $ entityVal c)
        byMeatType = consumptionMeatType . entityVal

