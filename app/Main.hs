-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.JSON
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
        DownloadError Response {..} -> io_ $ do
          consoleError $ ms (show errorMessage)
        Downloaded Response {..} -> io_ $ do
          consoleLog $ ms $ show body
        Start -> io_ $ do
          consoleLog "starting..."
-----------------------------------------------------------------------------
data Action
  = Downloaded (Response Value)
  | DownloadError (Response MisoString)
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
  -> (Response () -> IO ())
  -- ^ Successful callback (expecting no response)
  -> (Response MisoString -> IO ())
  -- ^ Errorful callback, with error message as param
  -> IO ()
-----------------------------------------------------------------------------
downloadFile
  :: Maybe MisoString
  -> (Response File -> IO ())
  -- ^ Received file
  -> (Response MisoString -> IO ())
  -- ^ Error message
  -> IO ()
-----------------------------------------------------------------------------
uploadFile :<|> downloadFile = toClient mempty (Proxy @API)
-----------------------------------------------------------------------------
type GitHubAPI = Get '[JSON] Value
-----------------------------------------------------------------------------
downloadGithub :: (Response Value -> Action) -> (Response MisoString -> Action) -> Effect ROOT () Action
downloadGithub successsful errorful = withSink $ \sink ->
  toClient "https://api.github.com" (Proxy @GitHubAPI) (sink . successsful) (sink . errorful)
-----------------------------------------------------------------------------
