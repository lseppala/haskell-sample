{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Meatbar.Web.Control.Consumption
    ( getConsumptions
    , createConsumption
    , getConsumptionStreak
    , getMonthlyStats
    ) where

import           Data.Aeson                   (Value, object, (.=))
import           Network.HTTP.Types.Status    (created201, badRequest400, conflict409)
import           Web.Scotty.Trans

import qualified Meatbar.Database.Consumption as Query
import           Meatbar.Web.Env


getConsumptions :: MeatbarAction ()
getConsumptions =
    transact (Query.listConsumptions) >>= json

-- | Create a new 'Consumption' in the database from a POST request.
-- Possible HTTP responses:
--  * 201: 'Consumption' successfully created in the database
--  * 400: The JSON request was unparsable
--  * 409: A 'Consumption' the same Person name and time already exists
createConsumption :: MeatbarAction ()
createConsumption = do
    newConsume <- rescue jsonData (const haltBadParse)
    newConsumeResult <- transact $ Query.putConsumption newConsume
    either
        (\_exists -> haltConflict)
        (\_new    -> status created201 >> finish)
        newConsumeResult

   where
       errorObject :: String -> Value
       errorObject msg = object [ "error" .= msg ]
       haltBadParse = do
            status badRequest400
            json $ errorObject
                "Unparsable JSON: the JSON request was badly formed."
            finish
       haltConflict = do
           status conflict409
           json $ errorObject
               "Conflict: A meatbar was already consumed \
               \ by that person at that time."
           finish


getConsumptionStreak :: MeatbarAction ()
getConsumptionStreak = return ()


getMonthlyStats :: MeatbarAction ()
getMonthlyStats = return ()
