-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
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
main = startApp defaultEvents myComponent
  { mount = Just Start
  }
-----------------------------------------------------------------------------
type MyComponent = App () Action
-----------------------------------------------------------------------------
myComponent :: MyComponent
myComponent = component () update_ view_
  where
#if MIN_VERSION_miso(1,13,0)
      view_ :: () -> () -> () -> View () () Action
      view_ _ _ _ =
#elif MIN_VERSION_miso(1,11,0)
      view_ :: () -> () -> View () Action
      view_ _ _ =
#else
      view_  :: () -> View () Action
      view_ _ =
#endif
        H.div_ []
        [ button_ [ onClick Download ] [ "download" ]
        ]
      
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
#if MIN_VERSION_miso(1,13,0)
downloadGithub :: (Response Value -> Action) -> (Response MisoString -> Action) -> Effect () props () Action
#elif MIN_VERSION_miso(1,11,0)
downloadGithub :: (Response Value -> Action) -> (Response MisoString -> Action) -> Effect ROOT () () Action
#else
downloadGithub :: (Response Value -> Action) -> (Response MisoString -> Action) -> Effect ROOT () Action
#endif
downloadGithub successsful errorful = withSink $ \sink ->
  toClient "https://api.github.com" (Proxy @GitHubAPI) (sink . successsful) (sink . errorful)
-----------------------------------------------------------------------------
