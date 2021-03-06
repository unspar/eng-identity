{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.User where


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

postUserR :: Handler Value
postUserR = do
  -- let user = User email password
  --newID <- runDB $ insert $ User "Test" Nothing
  --check if new_user uuid == nil  if so throw error
  reg <- (requireJsonBody :: Handler RegistrationForm) 
  let pw = encodeUtf8 (password reg)
  hashed_pw <-liftIO( BC.hashPasswordUsingPolicy BC.fastBcryptHashingPolicy pw)
  let pass = case hashed_pw of Just b_pass ->( decodeUtf8  b_pass)
                               Nothing -> ""
  new_user_id <- liftIO UUID4.nextRandom
  let new_user =  User new_user_id  (email_address reg) pass
  inserted_user <- runDB $ insertEntity new_user
  --generate new uuid and assign
  returnJson ("Success!" ::Text)

deleteUserR :: Handler Value
deleteUserR = do
  -- let user = User email password
  --newID <- runDB $ insert $ User "Test" Nothing
  --check if new_user uuid == nil  if so throw error
  reg <- (requireJsonBody :: Handler RegistrationForm) 
  let pw = encodeUtf8 (password reg)
  hashed_pw <-liftIO( BC.hashPasswordUsingPolicy BC.fastBcryptHashingPolicy pw)
  let pass = case hashed_pw of Just b_pass ->( decodeUtf8  b_pass)
                               Nothing -> ""
  new_user_id <- liftIO UUID4.nextRandom
  let new_user =  User new_user_id  (email_address reg) pass
  inserted_user <- runDB $ insertEntity new_user
  --generate new uuid and assign
  returnJson ("Success!" ::Text)


getUserR :: Handler Value
getUserR = do
  -- let user = User email password
  --newID <- runDB $ insert $ User "Test" Nothing
  --check if new_user uuid == nil  if so throw error
  reg <- (requireJsonBody :: Handler RegistrationForm) 
  let pw = encodeUtf8 (password reg)
  hashed_pw <-liftIO( BC.hashPasswordUsingPolicy BC.fastBcryptHashingPolicy pw)
  let pass = case hashed_pw of Just b_pass ->( decodeUtf8  b_pass)
                               Nothing -> ""
  new_user_id <- liftIO UUID4.nextRandom
  let new_user =  User new_user_id  (email_address reg) pass
  inserted_user <- runDB $ insertEntity new_user
  --generate new uuid and assign
  returnJson ("Success!" ::Text)
