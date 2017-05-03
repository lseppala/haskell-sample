{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Meatbar.Load where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Lazy
import           Data.Csv
import           Data.Foldable
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format
import           Database.Persist.Types       (entityKey)

import           Meatbar.Database.Consumption
import           Meatbar.Database.Person
import           Meatbar.Env
import           Meatbar.Model


data ConsumptionRecord = ConsumptionRecord
    { _personName        :: Text
    , _meatbarType       :: Text
    , _meatbarConsumedAt :: UTCTime
    }


instance FromNamedRecord ConsumptionRecord where
    parseNamedRecord r =
        ConsumptionRecord
        <$> r .: "person"
        <*> r .: "meat-bar-type"
        <*> (r .: "date" >>= parseIso8601Time)


parseIso8601Time :: Monad m => String -> m UTCTime
parseIso8601Time = parseTimeM True defaultTimeLocale "%FT%T%QZ"


-- | Decode records from CSV and load them into the database.
-- Database operations are idempotent, so no issue with loading the same record
-- multiple times.
decodeAndLoadRecords :: ( MonadReader Env m
                        , MonadIO (t m)
                        , MonadTrans t
                        , MonadError String (t m)
                        )
                     => ByteString
                     -> t m ()
decodeAndLoadRecords bs =
    case decodeByName bs of
      Left err ->
          throwError err
      Right (_, records) ->
          traverse_ loadConsumptionRecord records


loadConsumptionRecord :: ( MonadReader Env m
                         , MonadIO (t m)
                         , MonadTrans t )
                      => ConsumptionRecord
                      -> t m ()
loadConsumptionRecord ConsumptionRecord{..} =
    void . transact $ do
        personId <- readPersonId <$> putPerson (Person _personName)
        putConsumption $ Consumption personId _meatbarType _meatbarConsumedAt
    where
        readPersonId =
            either entityKey id
