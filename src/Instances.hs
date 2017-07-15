
{-# LANGUAGE OverloadedStrings          #-}

module Instances where 
import qualified Data.ByteString.Char8 as B8
import Database.Persist
import Database.Persist.Sql
import Data.Maybe
import Data.Either
import Data.Aeson
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import ClassyPrelude.Yesod

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific ( B8.pack (UUID.toString u))
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString ( B8.unpack t) of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"


instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

{-
instance ToJSON UUID where
  toJSON u =  String $ UUID.toText u



instance FromJSON UUID where
  parseJSON = withText "string" parse_uuid
    where
      parse_uuid x = case (UUID.fromText x) of
        Just u -> return u
        Nothing -> return UUID.nil --default is empty, NOT error
-}
instance PathPiece UUID where
  toPathPiece u = UUID.toText u
  fromPathPiece mbu = UUID.fromText mbu
-- Note we're taking advantage of
-- PostgreSQL understanding UUID values,
-- thus "PersistDbSpecific"
