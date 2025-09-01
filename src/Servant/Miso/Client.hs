-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances  #-}
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
  , toClient
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
import           Language.Javascript.JSaddle
import           GHC.TypeLits
import           Data.Proxy
import           Servant.API
import           Data.Kind
import           Data.Map (Map)
-----------------------------------------------------------------------------
import           Miso.FFI (fetch)
import           Miso.String
import qualified Miso.String as MS
-- import           Miso.Effect
-----------------------------------------------------------------------------
class HasClient api where
  type ClientType api :: Type
  toClientInternal :: Proxy api -> Request -> ClientType api
-----------------------------------------------------------------------------
toClient
  :: HasClient api
  => Proxy api
  -> ClientType api
toClient proxy = toClientInternal proxy emptyRequestState
-----------------------------------------------------------------------------
emptyRequestState :: Request
emptyRequestState = Request mempty mempty Nothing mempty mempty mempty
-----------------------------------------------------------------------------
data Request
  = Request
  { _headers :: Map MisoString MisoString
  , _queryParams :: Map MisoString MisoString
  , _reqBody :: Maybe (JSM JSVal)
  , _flags   :: [MisoString]
  , _paths   :: [MisoString]
  , _frags   :: [MisoString]
  }
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
instance (ToJSVal a, HasClient api) => HasClient (ReqBody types a :> api) where
  type ClientType (ReqBody types a :> api) = a -> ClientType api
  toClientInternal Proxy req body = toClientInternal (Proxy @api) req {
    _reqBody = Just (toJSVal body)
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
  toClientInternal Proxy req@Request{..} hasFlag =
    toClientInternal (Proxy @api) req {
      _flags = _flags ++ [ name | hasFlag ]
    } where
        name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance HasClient EmptyAPI where
  type ClientType EmptyAPI = JSM ()
  toClientInternal Proxy _ = pure ()
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
instance (ReflectMethod method) => HasClient (Verb method code types a) where
  type ClientType (Verb method code types a) = JSM ()
  toClientInternal Proxy Request {..} = do
    body_ <-
      case _reqBody of
        Nothing -> pure Nothing
        Just action -> Just <$> action
    fetch method paths body_ (M.toList _queryParams)
      undefined undefined (ms "")
        where
          paths = ms "/" <> MS.intercalate (ms "/") _paths
          method = ms $ show $ reflectMethod (Proxy @method)
-----------------------------------------------------------------------------
-- Request
-- { _headers :: Map MisoString MisoString
-- , _queryParams :: Map MisoString MisoString
-- , _reqBody :: Maybe (JSM JSVal)
-- , _flags :: [MisoString]
-- , _paths :: [MisoString]
-- , _frags :: [MisoString]
-- }
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
