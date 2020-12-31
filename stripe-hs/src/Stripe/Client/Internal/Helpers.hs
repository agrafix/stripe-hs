-- | Private helper functions. Note that all contents of this module are excluded from the versioning scheme.
{-# LANGUAGE BangPatterns #-}
module Stripe.Client.Internal.Helpers where

import Servant.Client
import Network.HTTP.Types.Status

runRequest :: Int -> Int -> IO (Either ClientError a) -> IO (Either ClientError a)
runRequest maxRetries !retryCount makeRequest =
  do res <- makeRequest
     case res of
       Right ok -> pure (Right ok)
       Left err@(ConnectionError _) -> maybeRetry err
       Left err@(FailureResponse _ resp)
         | ("stripe-should-retry", "true") `elem` responseHeaders resp -> maybeRetry err
         | ("stripe-should-retry", "false") `elem` responseHeaders resp -> pure (Left err)
         | responseStatusCode resp == conflict409 -> maybeRetry err
         | statusCode (responseStatusCode resp) >= 500 -> maybeRetry err
         | otherwise -> pure (Left err)
       Left err -> pure (Left err)
  where
    maybeRetry err =
      if retryCount + 1 >= maxRetries
      then pure (Left err)
      else runRequest maxRetries (retryCount + 1) makeRequest
