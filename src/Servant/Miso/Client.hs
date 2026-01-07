-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
-----------------------------------------------------------------------------
module Servant.Miso.Client
  ( HasClient (..)
  , MimeRender (..)
  , MimeUnrender (..)
  , toClient
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON
import qualified Data.Map.Strict as M
import           GHC.TypeLits
import           Data.Proxy
import           Servant.API hiding (MimeRender(..), MimeUnrender(..))
import           Data.Kind
import           Data.Map (Map)
-----------------------------------------------------------------------------
import           Miso (JSVal, ToJSVal(..), fromJSValUnchecked, CONTENT_TYPE(..))
import           Miso.FFI (fetch, Blob, ArrayBuffer, File, URLSearchParams, FormData, Response(..))
import           Miso.String
import qualified Miso.String as MS
-----------------------------------------------------------------------------
class HasClient (api :: k) where
  type ClientType api :: Type
  toClientInternal
    :: Proxy api
    -> Request
    -> ClientType api
-----------------------------------------------------------------------------
toClient
  :: HasClient api
  => MisoString
  -> Proxy api
  -> ClientType api
toClient baseUrl api =
  toClientInternal api emptyRequestState { _baseUrl = baseUrl }
-----------------------------------------------------------------------------
emptyRequestState :: Request
emptyRequestState = Request mempty mempty Nothing mempty mempty mempty (ms "")
-----------------------------------------------------------------------------
data Request
  = Request
  { _headers :: Map MisoString MisoString
  , _queryParams :: Map MisoString MisoString
  , _reqBody :: Maybe (IO JSVal)
  , _flags :: [MisoString]
  , _paths :: [MisoString]
  , _frags :: [MisoString]
  , _baseUrl :: MisoString
  }
-----------------------------------------------------------------------------
class Accept ctyp => MimeRender ctyp a where
  type MimeRenderType a :: Type
  mimeRender :: Proxy ctyp -> a -> MimeRenderType a
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSON a) => MimeRender JSON a where
  type MimeRenderType a = IO JSVal
  mimeRender Proxy x = toJSVal (encode x)
-----------------------------------------------------------------------------
instance MimeRender OctetStream Blob where
  type MimeRenderType Blob = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender OctetStream ArrayBuffer where
  type MimeRenderType ArrayBuffer = IO JSVal
  mimeRender Proxy = toJSVal
----------------------------------------------------------------------------
instance MimeRender OctetStream File where
  type MimeRenderType File = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded URLSearchParams where
  type MimeRenderType URLSearchParams = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded FormData where
  type MimeRenderType FormData = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender PlainText MisoString where
  type MimeRenderType MisoString = IO JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
class Accept ctyp => MimeUnrender ctyp a where
  type MimeUnrenderType a :: Type
  mimeUnrenderType :: Proxy ctyp -> Proxy a -> CONTENT_TYPE
  mimeUnrender :: Proxy ctyp -> MimeUnrenderType a -> IO (Either MisoString a)
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream File where
  type MimeUnrenderType File = JSVal
  mimeUnrenderType Proxy Proxy = BLOB
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream Blob where
  type MimeUnrenderType Blob = JSVal
  mimeUnrenderType Proxy Proxy = BLOB
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream ArrayBuffer where
  type MimeUnrenderType ArrayBuffer = JSVal
  mimeUnrenderType Proxy Proxy = ARRAY_BUFFER
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender PlainText MisoString where
  type MimeUnrenderType MisoString = JSVal
  mimeUnrenderType Proxy Proxy = TEXT
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance FromJSON json => MimeUnrender JSON json where
  type MimeUnrenderType json = JSVal
  mimeUnrenderType Proxy Proxy = JSON
  mimeUnrender Proxy jval = do
    value :: Value <- fromJSValUnchecked jval
    pure $ case fromJSON @json value of
      Success result -> Right result
      Error message -> Left (ms message)
-----------------------------------------------------------------------------
instance (KnownSymbol path, HasClient api) => HasClient (path :> api) where
  type ClientType (path :> api) = ClientType api
  toClientInternal Proxy req@Request{..} =
    toClientInternal (Proxy @api) req { _paths = _paths ++ [path] }
      where
        path = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (HasClient left, HasClient right) => HasClient (left :<|> right) where
  type ClientType (left :<|> right) = ClientType left :<|> ClientType right
  toClientInternal Proxy req =
    toClientInternal (Proxy @left) req :<|>
      toClientInternal (Proxy @right) req
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api) => HasClient (Capture name a :> api) where
  type ClientType (Capture name a :> api) = a -> ClientType api
  toClientInternal Proxy req@Request{..} x =
    toClientInternal (Proxy @api) req { _paths = _paths ++ [toMisoString x] }
-----------------------------------------------------------------------------
instance (MimeRender t a, HasClient api) => HasClient (ReqBody (t ': ts) a :> api) where
  type ClientType (ReqBody (t ': ts) a :> api) = a -> ClientType api
  toClientInternal Proxy req body = toClientInternal (Proxy @api) req {
    _reqBody = Just (mimeRender (Proxy @t) body)
  , _headers = _headers req <> M.singleton (ms "Content-Type") (ms (show (contentType (Proxy @t))))
  }
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToMisoString a, HasClient api) => HasClient (Header name a :> api) where
  type ClientType (Header name a :> api) = a -> ClientType api
  toClientInternal Proxy request@Request{..} x =
    toClientInternal (Proxy @api) request
      { _headers = M.insert name (toMisoString x) _headers
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (KnownSymbol name, HasClient api) => HasClient (QueryFlag name :> api) where
  type ClientType (QueryFlag name :> api) = Bool -> ClientType api
  toClientInternal _ req@Request{..} hasFlag =
    toClientInternal (Proxy @api) req {
      _flags = _flags ++ [ name | hasFlag ]
    } where
        name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance HasClient EmptyAPI where
  type ClientType EmptyAPI = IO ()
  toClientInternal _ _ = pure ()
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api, KnownSymbol name) => HasClient (QueryParam name a :> api) where
  type ClientType (QueryParam name a :> api) = Maybe a -> ClientType api
  toClientInternal Proxy request = \case
    Nothing ->
      toClientInternal (Proxy @api) request
    Just param ->
      toClientInternal (Proxy @api) request {
        _queryParams = M.insert name (ms param) (_queryParams request)
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient api) => HasClient (Fragment a :> api) where
  type ClientType (Fragment a :> api) = a -> ClientType api
  toClientInternal Proxy req@Request {..} frag =
    toClientInternal (Proxy @api) req {
      _frags = _frags ++ [ms frag]
    }
-----------------------------------------------------------------------------
-- | This response can be 'json', 'text', 'arrayBuffer', 'blob', or 'none'
instance (MimeUnrender t response, ReflectMethod method) => HasClient (Verb method code (t ': ts) response) where
  type ClientType (Verb method code (t ': ts) response)
     = (Response response -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetch (makeFullPath req) method body_ (M.toList (_headers <> acceptHeader))
      successed errorful (mimeUnrenderType (Proxy @t) (Proxy @response))
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept") (ms (show (contentType (Proxy @t))))
          successed resp@Response {..} = do
            mimeUnrender (Proxy @t) body >>= \case
              Left errorMessage_ -> do
                body_ <- fromJSValUnchecked body
                errorful resp { errorMessage = Just errorMessage_, body = body_ }
              Right result ->
                successful $ resp { body = result }
-----------------------------------------------------------------------------
instance ReflectMethod method => HasClient (NoContentVerb method) where
  type ClientType (NoContentVerb method)
     = (Response () -> IO ())
    -> (Response MisoString -> IO ())
    -> IO ()
  toClientInternal Proxy req@Request {..} successful errorful = do
    body_ <- sequenceA _reqBody
    fetch (makeFullPath req) method body_ (M.toList (_headers <> acceptHeader))
      successful errorful NONE
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept") (ms "*/*")
-----------------------------------------------------------------------------
makeFullPath :: Request -> MisoString
makeFullPath Request {..} = path <> queryParams <> queryFlags <> fragments
  where
    basedUrl
      | _baseUrl == ms "/" = _baseUrl
      | MS.null _baseUrl = ms "/" -- dmj: relative url
      | otherwise = _baseUrl <> ms "/"
    path = basedUrl <> MS.intercalate (ms "/") _paths
    queryParams = MS.concat
      [ ms "?" <>
        MS.intercalate (ms "&")
        [ k <> ms "=" <> v
        | (k,v) <- M.toList _queryParams
        ]
      | M.size _queryParams > 0
      ]
    queryFlags = MS.concat [ ms "?" <> x | x <- _flags ]
    fragments = MS.concat [ ms "#" <> x | x <- _frags ]
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (Host sym :> api) where
  type ClientType (Host sym :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (HttpVersion :> api) where
  type ClientType (HttpVersion :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (DeepQuery sym a :> api) where
  type ClientType (DeepQuery sym a :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (IsSecure :> api) where
  type ClientType (IsSecure :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient api => HasClient (RemoteHost :> api) where
  type ClientType (RemoteHost :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported yet
instance HasClient api => HasClient (CaptureAll sym a :> api) where
  type ClientType (CaptureAll sym a :> api) = ClientType api
  toClientInternal Proxy req = toClientInternal (Proxy @api) req
-----------------------------------------------------------------------------
