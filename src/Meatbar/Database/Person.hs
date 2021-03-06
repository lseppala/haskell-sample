{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Database.Person where

import           Control.Monad.IO.Class
import           Database.Persist.Sql

import           Meatbar.Model


-- | List all persons
listPersons :: MonadIO m
            => SqlPersistT m [Entity Person]
listPersons = selectList [] []


-- | lookup a person by ID
lookupPerson :: MonadIO m
             => Key Person
             -> SqlPersistT m (Maybe Person)
lookupPerson = get


-- | Idempotently add a person to the database.
putPerson :: MonadIO m
          => Person
          -> SqlPersistT m (Either (Entity Person) (Key Person))
putPerson = insertBy

