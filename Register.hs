module Handler.Register where


import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Engineooity.Layout
import Data.Aeson
import qualified Engineooity.Users as EU

data RegistrationForm = RegistrationForm {
  fname :: Text, 
  lname :: Text,
  company:: Text,
  email_address :: Text,
  raw_password :: Text
} deriving (Show)


registrationForm :: Form RegistrationForm
registrationForm = renderBootstrap3 BootstrapBasicForm $ RegistrationForm
    <$> areq textField "First Name: " Nothing 
    <*> areq textField "Last Name: " Nothing
    <*> areq textField "Company: " Nothing
    <*> areq emailField "Email address: " Nothing
    <*> areq passwordField "Password: " Nothing
    -- Add attributes like the placeholder and CSS classes.
    {-
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }
    -}


getRegisterR :: Handler Html
getRegisterR = do
    (formWidget, formEnctype) <- generateFormPost registrationForm
    simpleLayout $ do
        setTitle "Engineooity"
        $(widgetFile "register")



postRegisterR :: Handler Html
postRegisterR = do
  
  -- let user = User email password
  --newID <- runDB $ insert $ User "Test" Nothing

  ((result, formWidget), formEnctype) <- runFormPost registrationForm
  case result of
    FormSuccess reg -> do
      new_id <- runDB$ insert $ User (fname reg) hashed_pw (Just "salty") Nothing
      defaultLayout [whamlet|<p>#{show( new_id)}|]
      where
          hashed_pw = EU.hashRawPassword (raw_password reg) ("salty" :: Text)
    _ -> simpleLayout  [whamlet|<p>nothing to see|]




