{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Meatbar.Model where

import           Data.Text
import           Data.Time.Clock
import           Database.Persist.TH

import           Meatbar.Data.Analysis


-- Entities with foreign-key relationships must be defined in the same QQ
-- block, thus 'Person' and 'Consumption must be defined at the same time
--
-- 'UniqueName': In our domain, people have universally unique names
-- 'UniqueConsume': a person can only eat one bar at a given time
share [mkPersist sqlSettings, mkMigrate "migrateModel"] [persistLowerCase|
    Person json
      name Text
      UniqueName name
    Consumption json
      personId PersonId
      meatType Text
      consumedAt UTCTime
      UniqueConsume personId consumedAt
    ConsumptionRollup
      personId PersonId
      yearMonth YearMonth
      numBarsConsumed Int
      mostConsumedBarType Text
      UniqueNumBarsPersonYearMonth personId yearMonth
|]
