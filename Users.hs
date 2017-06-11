module Engineooity.Users where

import Import
import qualified Crypto.BCrypt as BC

{-
A module to handle commen user based actions
-}


hashFunction:: ByteString
hashFunction = encodeUtf8 "2y"
hashStrength :: Int
hashStrength = 14

checkPassword :: User -> Text -> Bool
checkPassword (User id pass salt _) user_attempt = case pass of 
  Nothing -> False
  Just hashed_pw -> BC.validatePassword (encodeUtf8 hashed_pw) saltedattempt
  where
    saltedattempt :: ByteString 
    saltedattempt = case salt of
      Just some_salt -> encodeUtf8 (user_attempt ++ some_salt)
      Nothing -> encodeUtf8 user_attempt
 

--takes a user-provided password and returns a hased version
hashRawPassword :: Text -> Text -> Maybe Text
hashRawPassword pass salt = case (BC.hashPassword b_pass salt_settings) of
  Just hash -> Just (decodeUtf8 hash)
  Nothing -> Nothing
  where
    b_pass = encodeUtf8 pass
    b_salt = encodeUtf8 salt
    salt_settings = case BC.genSalt hashFunction hashStrength b_salt of
      Just settings -> settings
      Nothing -> encodeUtf8 ""





