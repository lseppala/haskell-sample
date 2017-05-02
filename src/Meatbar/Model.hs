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

-- Entities with foreign-key relationships must be defined in the same QQ
-- block, thus 'Person' and 'Consumption must be defined at the same time
share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
    Person json
      name Text
      UniqueName name
    Consumption json
      personId PersonId
      meatType Text
      consumed_at UTCTime
|]
