module Network.HTTP.Req.URI where

import Data.Proxy (Proxy)
import Network.HTTP.Req
import Prelude

reqURI :: (MonadHttp m, HttpMethod method, HttpBody body, HttpResponse response, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
  => method
  -> Url scheme
  -> body
  -> Proxy response
  -> Option scheme
  -> m response
reqURI = undefined
