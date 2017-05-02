{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Database.Person where

import           Control.Monad.IO.Class
import           Control.Monad.Reader   (runReaderT)
import           Database.Persist

import           Meatbar.Model


type PersonBackend backend =
    ( PersistQueryRead backend
    , PersistRecordBackend Person backend
    , PersistUniqueWrite backend
    )

-- | List all persons
listPersons :: (PersonBackend backend, MonadIO m)
           => backend
           -> m [Entity Person]
listPersons db = selectList [] [] `runReaderT` db

-- | Idempotently add a person to the database.
putPerson :: (PersonBackend backend, MonadIO m)
          => Person
          -> backend
          -> m (Either (Entity Person) (Key Person))
putPerson person db = insertBy person `runReaderT` db
