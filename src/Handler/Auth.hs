{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where
import  Import 
import qualified Data.Time as DT
import qualified Crypto.JWT as JWT
import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.BCrypt as BC
import qualified Data.Aeson as DA
import qualified System.Environment as SE
import qualified Data.ByteString.Lazy as DBL

data AuthRequest = AuthRequest{
  ident::T.Text, 
  password:: T.Text
} deriving (Show,Generic)

instance DA.ToJSON AuthRequest
instance DA.FromJSON AuthRequest

data AuthResponse = AuthResponse {
  success :: Bool,
  response :: T.Text
} deriving (Show, Generic)
instance DA.ToJSON AuthResponse
instance DA.FromJSON AuthResponse

doJwtSign :: JWK.JWK -> JWT.ClaimsSet -> IO (Either JWT.JWTError JWT.SignedJWT)
doJwtSign key_obj claims = runExceptT $ do
  signing_algo <- JWK.bestJWSAlg key_obj
  JWT.signClaims key_obj (JWT.newJWSHeader ((), signing_algo)) claims



postAuthR :: Handler Value
postAuthR = do
  auth_request <- (requireJsonBody :: Handler AuthRequest)
  key_loc <- liftIO (SE.getEnv "JWK_PATH") 
  Just jwk <- liftIO $ DA.decode <$> DBL.readFile key_loc
  let claims = JWT.emptyClaimsSet
  --new_key <- JWK.genJWK (JWK.RSAGenParam (4096 `div` 8))
  user <- runDB $ getBy404 $ (UniqueUser  (ident auth_request))
  let atpw = encodeUtf8 $ password auth_request
      actpw = encodeUtf8 $userPassword $entityVal user
      success = BC.validatePassword actpw atpw 
  --time <- liftIO DT.getCurrentTime
  signed_key <- liftIO $ doJwtSign jwk claims
  let jwk_key = case signed_key of Left e -> pack (show e)
                                   Right s_token-> TL.toStrict (decodeUtf8 $ JWT.encodeCompact s_token)
  case success of True -> returnJson (AuthResponse True jwk_key)
                  False -> returnJson (AuthResponse False "Password failed")


