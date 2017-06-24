{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Create where


import Import
--import Data.UUID as UUID
import Data.UUID.V4 as UUID4
import Crypto.BCrypt as BC


data RegistrationForm = RegistrationForm {
  fname :: Text,
  lname :: Text,
  company:: Text,
  email_address :: Text,
  password :: Text
} deriving (Show, Generic)

instance ToJSON RegistrationForm
instance FromJSON RegistrationForm

postCreateR :: Handler Value
postCreateR = do
  -- let user = User email password
  --newID <- runDB $ insert $ User "Test" Nothing
  --check if new_user uuid == nil  if so throw error
  reg <- (requireJsonBody :: Handler RegistrationForm) 
  let pw = encodeUtf8 (password reg)
  hashed_pw <-liftIO( BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy pw)
  let pass = case hashed_pw of Just b_pass ->Just ( decodeUtf8  b_pass)
                               Nothing -> Nothing
  new_user_id <- liftIO UUID4.nextRandom
  let new_user =  User new_user_id  (email_address reg) pass
  inserted_user <- runDB $ insertEntity new_user
  --generate new uuid and assign
  returnJson ("Success!" ::Text)


