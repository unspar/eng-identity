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
import qualified Data.Vector as DV 
import qualified System.Environment as SE
import qualified Data.ByteString.Lazy as DBL
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))



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


jwk_fold :: (T.Text, DA.Value) -> JWT.ClaimsSet -> JWT.ClaimsSet
jwk_fold (claim, value) claim_set = JWT.addClaim claim value claim_set

postAuthR :: Handler Value
postAuthR = do
  auth_request <- (requireJsonBody :: Handler AuthRequest)
  key_loc <- liftIO (SE.getEnv "JWK_PATH") 
  Just jwk <- liftIO $ DA.decode <$> DBL.readFile key_loc
  --new_key <- JWK.genJWK (JWK.RSAGenParam (4096 `div` 8))
  user <- runDB $ getBy404 $ (UniqueUser  (ident auth_request))
  
  roles <- runDB
    $ E.select
    $ E.from $ \(userrole `E.InnerJoin` role) -> do
      E.on $ userrole ^. UserroleRole E.==. role ^. RoleId
      E.where_ $ userrole ^. UserroleUser  E.>. E.val (entityKey user)
      return
        ( role  ^. RoleName
        --, role  ^. RoleName
        --, user ^.  UserIdent
        )
  
  time <- liftIO DT.getCurrentTime
  
  --roles <- runDB $( selectList [UserroleUser ==. (entityKey user)] [])
  -- user_roles = DA.Array (DV.fromList ["testing"])
  let user_roles = DA.Array (DV.fromList (Import.map (\ (E.Value x) -> DA.String x) roles))
      claims_list = ([("iss", "http://identity.engineooity.com"),
                    ("scopes", user_roles), 
                    ("sub", DA.String (ident auth_request)), 
                    ("exp",DA.String $T.pack (DT.formatTime defaultTimeLocale "%s%Qs" time)) ])
      claims = Prelude.foldr jwk_fold JWT.emptyClaimsSet claims_list
      atpw = encodeUtf8 $ password auth_request
      actpw = encodeUtf8 $userPassword $entityVal user
      success = BC.validatePassword actpw atpw 
  signed_key <- liftIO $ doJwtSign jwk claims
  let jwk_key = case signed_key of Left e -> pack (show e)
                                   Right s_token-> TL.toStrict (decodeUtf8 $ JWT.encodeCompact s_token)
  case success of True -> returnJson (AuthResponse True jwk_key)
                  False -> returnJson (AuthResponse False "Password failed")


