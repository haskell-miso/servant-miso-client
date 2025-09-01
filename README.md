🍜 servant-client-miso
===================================

This is a [servant-client](https://github.com/haskell-servant/servant) binding to [miso](https://github.com/dmjio/miso).

> [!WARNING]
> This is still a work-in-progress. This requires `miso` > `1.9` and `servant` with a custom patch for `MimeUnrender` and `MimeRender`. As seen [here](https://github.com/haskell-servant/servant/pull/1840)



```haskell
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.String
-----------------------------------------------------------------------------
import Servant.Client.Miso
import Servant.API
-----------------------------------------------------------------------------
type API = UploadFile :<|> DownloadFile
type UploadFile = "api" :> "upload" :> ReqBody '[OctetStream] File :> PostNoContent
type DownloadFile = "api" :> "download" :> Post '[OctetStream] File
-----------------------------------------------------------------------------
uploadFile
  :: File
  -- ^ File to upload
  -> Effect parent model action
-----------------------------------------------------------------------------
downloadFile
  :: (File -> action)
  -- ^ Received file
  -> (MisoString -> action)
  -- ^ Error message
  -> Effect parent model action
uploadFile :<|> downloadFile = toClient (Proxy @API)
-----------------------------------------------------------------------------
```

### Build

```bash
cabal build
```

### Dev

```bash
cabal build
```
