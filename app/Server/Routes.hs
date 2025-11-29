{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Routes where 

import Api.Model
import Server.SSE
import Data.Proxy
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Servant.API
import Servant.Server
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

type API = 
         "health" :> Get '[PlainText] String
    :<|> "checkboxes" :> Get '[JSON] CheckboxResponse
    :<|> "checkboxes" :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "checkboxes" :> Capture "id" Int :> ReqBody '[JSON] CheckboxUpdate :> Patch '[JSON] Checkbox
    :<|> "checkboxes" :> Capture "id" Int :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "events" :> Raw
    :<|> Raw

handlerCheckboxesTodos :: Connection -> Handler CheckboxResponse
handlerCheckboxesTodos conn = do 
    res <- liftIO $ query_ conn "SELECT id, checked FROM Checkbox ORDER BY id" 
    let result = map (\(id', checked') -> Checkbox id' checked') res
    pure (CheckboxResponse result)

handlerCheckboxUpdate :: Connection -> BroadcastChannel -> Int -> CheckboxUpdate -> Handler Checkbox
handlerCheckboxUpdate conn broadcast idParam checkboxUpdate = do 
    res <- liftIO $ execute conn "UPDATE Checkbox SET checked = ? WHERE id = ?" (checkedValue checkboxUpdate, idParam)
    if res > 0
        then do
            updated <- liftIO $ query conn "SELECT id, checked FROM Checkbox WHERE id = ?" (Only idParam)
            case updated of
                [(id', checked')] -> do
                    let checkbox = Checkbox id' checked'
                    let jsonData = encode checkbox
                    liftIO $ broadcastEvent broadcast jsonData
                    pure checkbox
                _ -> throwError err500
        else throwError err404

handlerHealth :: Handler String
handlerHealth = pure "OK"

options :: Handler ()
options = pure ()

optionsWithId :: Int -> Handler ()
optionsWithId _ = pure ()

serveSSE :: BroadcastChannel -> Server Raw
serveSSE broadcast = Tagged $ sseApplication broadcast

serveStatic :: Server Raw
serveStatic = Tagged $ staticApp $ defaultFileServerSettings "frontend"

server :: Connection -> BroadcastChannel -> Server API 
server conn broadcast = handlerHealth
            :<|> handlerCheckboxesTodos conn 
            :<|> options 
            :<|> handlerCheckboxUpdate conn broadcast
            :<|> optionsWithId
            :<|> serveSSE broadcast
            :<|> serveStatic

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "PATCH", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

addCorsHeader :: Middleware
addCorsHeader = cors (const $ Just corsPolicy)

app :: Connection -> BroadcastChannel -> Application 
app conn broadcast = addCorsHeader (serve (Proxy @API) (server conn broadcast))
