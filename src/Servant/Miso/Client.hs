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
import           Miso.Effect
import qualified Miso.String as MS
-----------------------------------------------------------------------------
class HasClient (parent :: Type) (model :: Type) (action :: Type) (api :: k) where
  type ClientType parent model action api :: Type
  toClientInternal
    :: Proxy (Effect parent model action)
    -> Proxy api
    -> Request
    -> ClientType parent model action api
-----------------------------------------------------------------------------
toClient
  :: forall parent model action api
   . HasClient parent model action api
  => Proxy (Effect parent model action)
  -> Proxy api
  -> ClientType parent model action api
toClient effect api = toClientInternal effect api emptyRequestState
-----------------------------------------------------------------------------
emptyRequestState :: Request
emptyRequestState = Request mempty mempty Nothing mempty mempty mempty
-----------------------------------------------------------------------------
data Request
  = Request
  { _headers :: Map MisoString MisoString
  , _queryParams :: Map MisoString MisoString
  , _reqBody :: Maybe (JSM JSVal)
  , _flags :: [MisoString]
  , _paths :: [MisoString]
  , _frags :: [MisoString]
  }
-----------------------------------------------------------------------------
instance (KnownSymbol path, HasClient p m a api) => HasClient p m a (path :> api) where
  type ClientType p m a (path :> api) = ClientType p m a api
  toClientInternal p Proxy req@Request{..} =
    toClientInternal p (Proxy @api) req { _paths = _paths ++ [path] }
      where
        path = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (HasClient p m a left, HasClient p m a right) => HasClient p m a (left :<|> right) where
  type ClientType p m a (left :<|> right) = ClientType p m a left :<|> ClientType p m a right
  toClientInternal p Proxy req =
    toClientInternal p (Proxy @left) req :<|>
      toClientInternal p (Proxy @right) req
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient p m a api) => HasClient p m a (Capture name a :> api) where
  type ClientType p m a (Capture name a :> api) = a -> ClientType p m a api
  toClientInternal p Proxy req@Request{..} x =
    toClientInternal p (Proxy @api) req { _paths = _paths ++ [toMisoString x] }
-----------------------------------------------------------------------------
instance (ToJSVal a, HasClient p m a api) => HasClient p m a (ReqBody types a :> api) where
  type ClientType p m a (ReqBody types a :> api) = a -> ClientType p m a api
  toClientInternal p Proxy req body = toClientInternal p (Proxy @api) req {
    _reqBody = Just (toJSVal body)
  }
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToMisoString a, HasClient p m a api) => HasClient p m a (Header name a :> api) where
  type ClientType p m a (Header name a :> api) = a -> ClientType p m a api
  toClientInternal p Proxy request@Request{..} x =
    toClientInternal p (Proxy @api) request
      { _headers = M.insert name (toMisoString x) _headers
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (KnownSymbol name, HasClient p m a api) => HasClient p m a (QueryFlag name :> api) where
  type ClientType p m a (QueryFlag name :> api) = Bool -> ClientType p m a api
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
instance (ToMisoString a, HasClient p m a api, KnownSymbol name) => HasClient p m a (QueryParam name a :> api) where
  type ClientType p m a (QueryParam name a :> api) = Maybe a -> ClientType p m a api
  toClientInternal p Proxy request = \case
    Nothing ->
      toClientInternal p (Proxy @api) request
    Just param ->
      toClientInternal p (Proxy @api) request {
        _queryParams = M.insert name (ms param) (_queryParams request)
      } where
          name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance (ToMisoString a, HasClient p m a api) => HasClient p m a (Fragment a :> api) where
  type ClientType p m a (Fragment a :> api) = a -> ClientType p m a api
  toClientInternal p Proxy req@Request {..} frag =
    toClientInternal p (Proxy @api) req {
      _frags = _frags ++ [ms frag]
    }
-----------------------------------------------------------------------------
instance (FromJSVal a, Accept types, ReflectMethod method) => HasClient p m action (Verb method code types a) where
  type ClientType p m action (Verb method code types a)
     = (a -> action)
    -> (MisoString -> action)
    -> Effect p m action
  toClientInternal _ Proxy Request {..} successful errorful = withSink $ \sink -> do
    body_ <- sequenceA _reqBody
    fetch method paths body_ (M.toList _queryParams)
      (successed sink) (errored sink) (ms "")
        where
          successed sink jval =
            sink =<< successful <$>
              fromJSValUnchecked jval

          errored sink jval =
            sink (errorful jval)

          paths = ms "/" <> MS.intercalate (ms "/") _paths
          method = ms $ show $ reflectMethod (Proxy @method)
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
instance HasClient p m a api => HasClient p m a (CaptureAll sym a :> api) where
  type ClientType p m a (CaptureAll sym a :> api) = ClientType p m a api
  toClientInternal p Proxy req = toClientInternal p (Proxy @api) req
-----------------------------------------------------------------------------
