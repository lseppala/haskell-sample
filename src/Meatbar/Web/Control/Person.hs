{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Meatbar.Web.Control.Person
    ( getPersons
    ) where


import           Web.Scotty.Trans        (json)

import qualified Meatbar.Database.Person as Query
import           Meatbar.Web.Env


getPersons :: MeatbarAction ()
getPersons =
    transact Query.listPersons >>= json
