-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Data.Aeson
import Miso
import Miso.Html.Element as H
import Miso.Html.Event as H
-----------------------------------------------------------------------------
import Data.Proxy
import Servant.Miso.Client
import Servant.API
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent myComponent
  { initialAction = Just Start
  }
-----------------------------------------------------------------------------
type MyComponent = App () Action
-----------------------------------------------------------------------------
myComponent :: MyComponent
myComponent = component () update_ $ \() ->
  H.div_ []
  [ button_ [ onClick Download ] [ "download" ]
  ] where
      update_ = \case
        Download -> do
          io_ (consoleLog "clicked")
          downloadGithub Downloaded DownloadError
        DownloadError err -> io_ $ do
          consoleError err
        Downloaded value -> io_ $ do
          consoleLog $ ms $ show value
        Start -> io_ $ do
          consoleLog "starting..."
-----------------------------------------------------------------------------
data Action
  = Downloaded Value
  | DownloadError MisoString
  | Download
  | Start
-----------------------------------------------------------------------------
type API = UploadFile :<|> DownloadFile
-----------------------------------------------------------------------------
type UploadFile
  = "api" :> "upload" :> "file1" :> ReqBody '[OctetStream] File :> PostNoContent
-----------------------------------------------------------------------------
type DownloadFile
  = "api" :> "download" :> "file1" :> QueryParam "foo" MisoString :> Get '[OctetStream] File
-----------------------------------------------------------------------------
uploadFile
  :: File
  -- ^ File to upload
  -> Action
  -- ^ Successful callback (expecting no response)
  -> (MisoString -> Action)
  -- ^ Errorful callback, with error message as param
  -> Transition () Action
-----------------------------------------------------------------------------
downloadFile
  :: Maybe MisoString
  -> (File -> Action)
  -- ^ Received file
  -> (MisoString -> Action)
  -- ^ Error message
  -> Transition () Action
-----------------------------------------------------------------------------
uploadFile :<|> downloadFile = toClient mempty (Proxy @MyComponent) (Proxy @API)
-----------------------------------------------------------------------------
type GitHubAPI = Get '[JSON] Value
-----------------------------------------------------------------------------
downloadGithub :: (Value -> Action) -> (MisoString -> Action) -> Effect ROOT () Action
downloadGithub = toClient "https://api.github.com" (Proxy @MyComponent) (Proxy @GitHubAPI)
-----------------------------------------------------------------------------
