-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
type MyComponent = App () Action
-----------------------------------------------------------------------------
myComponent :: MyComponent
myComponent = component () update_ $ \() ->
  H.div_ []
  [ button_ [ onClick Download ] [ "download" ]
  ] where
      update_ = \case
        Download ->
          downloadFile Downloaded DownloadError
        DownloadError err -> io_ $ do
          consoleError err
        Downloaded (File file) -> io_ $ do
          consoleLog "got file"
          consoleLog' file
-----------------------------------------------------------------------------
data Action
  = Downloaded File
  | DownloadError MisoString
  | Download
-----------------------------------------------------------------------------
type API = UploadFile :<|> DownloadFile
-----------------------------------------------------------------------------
type UploadFile
  = "api" :> "upload" :> "file1" :> ReqBody '[OctetStream] File :> PostNoContent
-----------------------------------------------------------------------------
type DownloadFile
  = "api" :> "download" :> "file1" :> Get '[OctetStream] File
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
  :: (File -> Action)
  -- ^ Received file
  -> (MisoString -> Action)
  -- ^ Error message
  -> Transition () Action
-----------------------------------------------------------------------------
uploadFile :<|> downloadFile = toClient (Proxy @MyComponent) (Proxy @API)
-----------------------------------------------------------------------------
