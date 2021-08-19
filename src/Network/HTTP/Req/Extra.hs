module Network.HTTP.Req.Extra (
  httpConfigRetryTimeouts,
) where

import Control.Exception (SomeException, fromException)
import Control.Retry (RetryPolicy, constantDelay, limitRetriesByCumulativeDelay)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ResponseTimeout))
import Network.HTTP.Req (HttpConfig, HttpException (VanillaHttpException), defaultHttpConfig, httpConfigRetryJudgeException, httpConfigRetryPolicy)

-- | An HttpConfig that adds a retry handler for ResponseTimeout
httpConfigRetryTimeouts :: HttpConfig
httpConfigRetryTimeouts =
  defaultHttpConfig
    { httpConfigRetryJudgeException = const isResponseTimeout
    , httpConfigRetryPolicy = retryPolicy
    }

-- | Retry every 5 seconds for up to 5 minutes
retryPolicy :: RetryPolicy
retryPolicy = limitRetriesByCumulativeDelay fiveMinutes (constantDelay fiveSeconds)
  where
    -- five minutes
    fiveMinutes :: Int
    fiveMinutes = 5 * 60 * 1_000_000

    -- five seconds
    fiveSeconds :: Int
    fiveSeconds = 5 * 1_000_000

-- | Is the Exception a ResponseTimeout?
isResponseTimeout :: SomeException -> Bool
isResponseTimeout exc =
  case fromException exc of
    Just (VanillaHttpException (HttpExceptionRequest _ ResponseTimeout)) -> True
    _ -> False
