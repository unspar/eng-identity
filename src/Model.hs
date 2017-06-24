{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Instances
import Data.UUID (UUID)
--import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  uuid UUID sqltype=uuid default=uuid_generate_v4()
  Primary uuid
  ident Text --email?
  password Text Maybe
  UniqueUser ident
  deriving Typeable
Email
  email Text
  userId UserId Maybe
  verkey Text Maybe
  UniqueEmail email
Comment json -- Adding "json" causes ToJSON and FromJSON instances to be derived.
  message Text
  userId UserId Maybe
  deriving Eq
  deriving Show
|]
