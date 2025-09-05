-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PolyKinds             #-}
-----------------------------------------------------------------------------
module Servant.Miso.Client
  ( HasClient (..)
  , MimeRender (..)
  , MimeUnrender (..)
  , toClient
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           Language.Javascript.JSaddle hiding (Success)
import           GHC.TypeLits
import           Data.Proxy
import           Servant.API hiding (MimeRender(..), MimeUnrender(..))
import           Data.Kind
import           Data.Map (Map)
-----------------------------------------------------------------------------
import           Miso (Component)
import           Miso.FFI (fetch, Blob, ArrayBuffer, File, URLSearchParams, FormData)
import qualified Miso.FFI as FFI
import           Miso.String
import           Miso.Effect
import qualified Miso.String as MS
-----------------------------------------------------------------------------
class HasClient (parent :: Type) (model :: Type) (action :: Type) (api :: k) where
  type ClientType parent model action api :: Type
  toClientInternal
    :: Proxy (Component parent model action)
    -> Proxy api
    -> Request
    -> ClientType parent model action api
-----------------------------------------------------------------------------
toClient
  :: forall parent model action api
   . HasClient parent model action api
  => MisoString
  -> Proxy (Component parent model action)
  -> Proxy api
  -> ClientType parent model action api
toClient baseUrl effect api = toClientInternal effect api emptyRequestState { _baseUrl = baseUrl }
-----------------------------------------------------------------------------
emptyRequestState :: Request
emptyRequestState = Request mempty mempty Nothing mempty mempty mempty (ms "")
-----------------------------------------------------------------------------
data Request
  = Request
  { _headers :: Map MisoString MisoString
  , _queryParams :: Map MisoString MisoString
  , _reqBody :: Maybe (JSM JSVal)
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
  type MimeRenderType a = JSM JSVal
  mimeRender Proxy = FFI.jsonStringify
-----------------------------------------------------------------------------
instance MimeRender OctetStream Blob where
  type MimeRenderType Blob = JSM JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender OctetStream ArrayBuffer where
  type MimeRenderType ArrayBuffer = JSM JSVal
  mimeRender Proxy = toJSVal
----------------------------------------------------------------------------
instance MimeRender OctetStream File where
  type MimeRenderType File = JSM JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded URLSearchParams where
  type MimeRenderType URLSearchParams = JSM JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender FormUrlEncoded FormData where
  type MimeRenderType FormData = JSM JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
instance MimeRender PlainText MisoString where
  type MimeRenderType MisoString = JSM JSVal
  mimeRender Proxy = toJSVal
-----------------------------------------------------------------------------
class Accept ctyp => MimeUnrender ctyp a where
  type MimeUnrenderType a :: Type
  mimeUnrenderType :: Proxy ctyp -> Proxy a -> MisoString
  mimeUnrender :: Proxy ctyp -> MimeUnrenderType a -> JSM (Either MisoString a)
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream File where
  type MimeUnrenderType File = JSVal
  mimeUnrenderType Proxy Proxy = ms "file"
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream Blob where
  type MimeUnrenderType Blob = JSVal
  mimeUnrenderType Proxy Proxy = ms "blob"
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender OctetStream ArrayBuffer where
  type MimeUnrenderType ArrayBuffer = JSVal
  mimeUnrenderType Proxy Proxy = ms "arrayBuffer"
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance MimeUnrender PlainText MisoString where
  type MimeUnrenderType MisoString = JSVal
  mimeUnrenderType Proxy Proxy = ms "text"
  mimeUnrender Proxy = fmap pure . fromJSValUnchecked
-----------------------------------------------------------------------------
instance FromJSON json => MimeUnrender JSON json where
  type MimeUnrenderType json = JSVal
  mimeUnrenderType Proxy Proxy = ms "json"
  mimeUnrender Proxy jval = do
    value :: Value <- fromJSValUnchecked jval
    pure $ case fromJSON @json value of
      Success result -> Right result
      Error message -> Left (ms message)
-----------------------------------------------------------------------------
instance (KnownSymbol path, HasClient p m action api) => HasClient p m action (path :> api) where
  type ClientType p m action (path :> api) = ClientType p m action api
  toClientInternal p Proxy req@Request{..} =
    toClientInternal p (Proxy @api) req { _paths = _paths ++ [path] }
      where
        path = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (HasClient p m action left, HasClient p m action right) => HasClient p m action (left :<|> right) where
  type ClientType p m action (left :<|> right) = ClientType p m action left :<|> ClientType p m action right
  toClientInternal p Proxy req =
    toClientInternal p (Proxy @left) req :<|>
      toClientInternal p (Proxy @right) req
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient p m action api) => HasClient p m action (Capture name a :> api) where
  type ClientType p m action (Capture name a :> api) = a -> ClientType p m action api
  toClientInternal p Proxy req@Request{..} x =
    toClientInternal p (Proxy @api) req { _paths = _paths ++ [toMisoString x] }
-----------------------------------------------------------------------------
instance (MimeRender t a, HasClient p m action api) => HasClient p m action (ReqBody (t ': ts) a :> api) where
  type ClientType p m action (ReqBody (t ': ts) a :> api) = a -> ClientType p m action api
  toClientInternal p Proxy req body = toClientInternal p (Proxy @api) req {
    _reqBody = Just (mimeRender (Proxy @t) body)
  , _headers = _headers req <> M.singleton (ms "Content-Type") (ms $ show (contentType (Proxy @t)))
  }
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToMisoString a, HasClient p m action api) => HasClient p m action (Header name a :> api) where
  type ClientType p m action (Header name a :> api) = a -> ClientType p m action api
  toClientInternal p Proxy request@Request{..} x =
    toClientInternal p (Proxy @api) request
      { _headers = M.insert name (toMisoString x) _headers
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (KnownSymbol name, HasClient p m action api) => HasClient p m action (QueryFlag name :> api) where
  type ClientType p m action (QueryFlag name :> api) = Bool -> ClientType p m action api
  toClientInternal p _ req@Request{..} hasFlag =
    toClientInternal p (Proxy @api) req {
      _flags = _flags ++ [ name | hasFlag ]
    } where
        name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance HasClient p m a EmptyAPI where
  type ClientType p m a EmptyAPI = Effect p m a
  toClientInternal _ Proxy _ = pure ()
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient p m action api, KnownSymbol name) => HasClient p m action (QueryParam name a :> api) where
  type ClientType p m action (QueryParam name a :> api) = Maybe a -> ClientType p m action api
  toClientInternal p Proxy request = \case
    Nothing ->
      toClientInternal p (Proxy @api) request
    Just param ->
      toClientInternal p (Proxy @api) request {
        _queryParams = M.insert name (ms param) (_queryParams request)
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient p m action api) => HasClient p m action (Fragment a :> api) where
  type ClientType p m action (Fragment a :> api) = a -> ClientType p m action api
  toClientInternal p Proxy req@Request {..} frag =
    toClientInternal p (Proxy @api) req {
      _frags = _frags ++ [ms frag]
    }
-----------------------------------------------------------------------------
-- | This response can be 'json', 'text', 'arrayBuffer', 'blob', or 'none'
instance (MimeUnrender t response, ReflectMethod method) => HasClient p m action (Verb method code (t ': ts) response) where
  type ClientType p m action (Verb method code (t ': ts) response)
     = (response -> action)
    -> (MisoString -> action)
    -> Effect p m action
  toClientInternal _ Proxy req@Request {..} successful errorful = withSink $ \sink -> do
    body_ <- sequenceA _reqBody
    fetch (makeFullPath req) method body_ (M.toList (_headers <> acceptHeader))
      (successed sink) (errored sink) (mimeUnrenderType (Proxy @t) (Proxy @response))
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept") (ms $ show (contentType (Proxy @t)))
          errored sink jval = sink (errorful jval)
          successed sink jval = do
            mimeUnrender (Proxy @t) jval >>= \case
              Left errorMessage ->
                sink (errorful errorMessage)
              Right result ->
                sink (successful result)
-----------------------------------------------------------------------------
instance ReflectMethod method => HasClient p m action (NoContentVerb method) where
  type ClientType p m action (NoContentVerb method)
     = action
    -> (MisoString -> action)
    -> Effect p m action
  toClientInternal _ Proxy req@Request {..} successful errorful = withSink $ \sink -> do
    body_ <- sequenceA _reqBody
    fetch (makeFullPath req) method body_ (M.toList (_headers <> acceptHeader))
      (sink . const successful) (errored sink) (ms "none")
        where
          method = ms $ reflectMethod (Proxy @method)
          acceptHeader = M.singleton (ms "Accept") (ms "*/*")
          errored sink jval = sink (errorful jval)
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
instance HasClient p m a api => HasClient p m a (Host sym :> api) where
  type ClientType p m a (Host sym :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient p m a api => HasClient p m a (HttpVersion :> api) where
  type ClientType p m a (HttpVersion :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient p m a api => HasClient p m a (DeepQuery sym a :> api) where
  type ClientType p m a (DeepQuery sym a :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient p m a api => HasClient p m a (IsSecure :> api) where
  type ClientType p m a (IsSecure :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported
instance HasClient p m a api => HasClient p m a (RemoteHost :> api) where
  type ClientType p m a (RemoteHost :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
-- | Not supported yet
instance HasClient p m action api => HasClient p m action (CaptureAll sym a :> api) where
  type ClientType p m action (CaptureAll sym a :> api) = ClientType p m action api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
