{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Meatbar.Web.Route (routes) where

import           Web.Scotty.Trans

import           Meatbar.Web.Control.Consumption
import           Meatbar.Web.Control.Person
import           Meatbar.Web.Types


routes :: MeatbarScotty ()
routes = do
    -- List all persons
    get  "/person" getPersons

    -- List all meatbar consumptions
    get  "/consumption" getConsumptions

    -- Create a new meatbar consumption
    post "/consumption" createConsumption

    -- Provide the longest consecutive meatbar consumption streak
    get  "/consumption/streak" getConsumptionStreak

    -- Provide statistics of day with highest consumption for each month
    get  "/consumption/month_stats" getMonthlyStats

