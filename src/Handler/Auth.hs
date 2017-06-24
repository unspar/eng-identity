{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where
import Import 
import Data.Time as DT
import Crypto.JWT
import Data.Aeson as DA
import Control.Monad.Except (runExceptT)

data AuthRequest = AuthRequest{
  email:: Text, 
  password:: Text
} deriving (Show,Generic)


instance ToJSON AuthRequest
instance FromJSON AuthRequest


--sample_claim :: ByteString
sample_claim = "{\"sub\": \"1234567890\",\"name\": \"John Doe\",\"admin\": true}"
sample_keyfile = "{\"keys\":[{\"kty\":\"RSA\",\"d\":\"l3iu9_ezwrz78AgU4RHgf6o3Zbn3SirXXprhQuCZKFjlBjhE52IMDny3GRm5a9qPJ-aK2G3Nvweg7OJaLXPFhiGY9ZxFFCFpINjTIXClKOqFO4P_pnN4fmNJUC6_ViAn__Ur18zgpI-O9MBZEbxDMSdk_NcgQmzGYmIIXxaVAXLzzhVruFQoqEZPxMCyc-eRHS9enLaktxAHUZZ41F0ep-UnS9iBOe_InwjC414fLBBnmFyKcc_dAcbojyyqC_94E3Q9SXFaY7oWRlEBhV28rKSh681TKHTmpWH2umXUNZ2S3TUTzkzRT5_58Xwox5rhs44Vy0h8uOUvzaTVGFWN4Q\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"test 1\",\"alg\":\"RS256\",\"n\":\"puVpd5wyzB1UhF93RslLclM3rQZ-qGFhLC-D5fpEa4E2BQPwTDFj0eYzQmwgY8zhvr3v80wnuHlWVv0ai8i3Jo9NDglGEgBVH_7YsE6IZA6o8rbzEG_3rO62x1Nbb-929lAAVM74mnYvU-g7k2zcJTtaMejH0GcOxID1byzzd42aP3Ei8gdVqa736ch_wHiVKSzyzWOVGQoY2ZFwkqn4j2a1Fx4jNkPU6q2fICRVXqQK502Rh7yqhwb2YqhivdlsJFdtsAx1LbFGAr--btkZKfw-Uvqi3d__97esmQTIWPZhvJL5jXS_tBtd53u4SdJxjLjnhndn6c0MzWwNv9QOkQ\"}]}"


{-
doJwtSign :: IO ()
doJwtSign = do
  Just jwk <- decode <$> L.readFile jwkFilename
  Just claims <- decode <$> sample_claim
  result <- runExceptT $ do
    alg <- bestJWSAlg jwk
    signClaims jwk (newJWSHeader ((), alg)) claims
  case result of
    Left e -> print (e :: Error) >> exitFailure
    Right jwt -> L.putStr (encodeCompact jwt)
-}
postAuthR :: Handler Value
postAuthR = do
  auth_request <- (requireJsonBody :: Handler AuthRequest)

  let claims = (DA.decode sample_claim :: Maybe Value)
  let keyfile = (DA.decode sample_keyfile ::Maybe Value)
  let jwk = case keyfile of Just k -> k

  time <- liftIO DT.getCurrentTime
  result <- runExceptT $ do
    alg <- bestJWSAlg jwk
    signClaims jwk (newJWSHeader ((), alg)) claims
  case result of
    Left e -> returnJson auth_request
    Right jwt -> returnJson (encodeCompact jwt)

  --query database for user

  --if user doesn't exist, yield jwt with status message
  --if user exists, yeidl jwt with 

