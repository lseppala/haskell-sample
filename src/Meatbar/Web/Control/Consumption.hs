{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Meatbar.Web.Control.Consumption
    ( getConsumptions
    , createConsumption
    , getConsumptionStreak
    , getMonthlyStats
    ) where

import           Control.Monad                (void)
import           Data.Aeson                   (Value, object, (.=))
import           Data.Time.Calendar           (toGregorian)
import           Database.Persist.Types       (entityVal)
import qualified Network.HTTP.Types.Status    as HTTP
import           Web.Scotty.Trans

import           Meatbar.Data.Analysis
import qualified Meatbar.Database.Consumption as Query
import qualified Meatbar.Database.Person      as Query
import           Meatbar.Model
import           Meatbar.Web.Types


getConsumptions :: MeatbarAction ()
getConsumptions =
    transact Query.listConsumptions >>= json

-- | Create a new 'Consumption' in the database from a POST request.
-- A 'Consumption' JSON request has the form:
-- @
--    { "meatType": <string>,
--      "consumedAt": <ISO 8601 date formated string>,
--      "personId": <integer>
--    }
-- @
-- Possible HTTP responses:
--  * 201: 'Consumption' successfully created in the database
--  * 400: The JSON request was unparsable or the specified person does not exist
--  * 409: A 'Consumption' with the same Person and time already exists
createConsumption :: MeatbarAction ()
createConsumption = do
    newConsume <- rescue jsonData (const haltBadParse)
    void $ transact (Query.lookupPerson (consumptionPersonId newConsume))
        >>= maybe haltNoPerson return
    newConsumeResult <- transact $ Query.putConsumption newConsume
    either
        ({-exists -} const haltConflict)
        ({-created-} const $ status HTTP.created201 >> finish)
        newConsumeResult

   where
       errorObject :: String -> Value
       errorObject msg = object [ "error" .= msg ]

       haltBadParse = do
            status HTTP.badRequest400
            json $ errorObject
                "Unparsable JSON: the JSON request was badly formed."
            finish
       haltNoPerson = do
           status HTTP.badRequest400
           json $ errorObject
               "No such person with the given personId exists"
           finish
       haltConflict = do
           status HTTP.conflict409
           json $ errorObject
               "Conflict: A meatbar was already consumed\
               \ by that person at the given time."
           finish


getConsumptionStreak :: MeatbarAction ()
getConsumptionStreak = do
    cs <- transact Query.listConsumptions
    json $ fmap streakObject <$> allHigherCountDailyStreaks byConsumedAt cs
    where
        byConsumedAt = consumptionConsumedAt . entityVal
        streakObject (date, count) =
            object [ "date" .= date, "count" .= count ]


getMonthlyStats :: MeatbarAction ()
getMonthlyStats = do
    cs <- transact Query.listConsumptions
    json $ monthObject <$> largestDayEachMonth byConsumedAt cs
    where
        byConsumedAt = consumptionConsumedAt . entityVal
        monthObject (date, count) =
            let (year, month, day) = toGregorian date
            in object
                [ "month" .= (show year ++ "-" ++ show month)
                , "dayOfMonth" .= show day
                , "count" .= count ]
