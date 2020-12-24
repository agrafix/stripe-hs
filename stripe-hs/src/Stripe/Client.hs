{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}
module Stripe.Client
  ( ApiKey
  , createCustomer, retrieveCustomer, updateCustomer
  , Customer(..), CustomerCreate(..), CustomerUpdate(..)
  )
where

import Stripe.Api
import Stripe.Resources

import Data.Proxy
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type ApiKey = T.Text

api :: Proxy StripeApi
api = Proxy

stripeBaseUrl :: BaseUrl
stripeBaseUrl = BaseUrl Https "api.stripe.com" 443 ""

--mergeError :: Either ServantErroar (ApiResponse a) -> Either (Either ServantError ApiError) a
--mergeError e =
--    case e of
--      Left x -> Left (Left x)
--      Right (ARError x) -> Left (Right x)
--      Right (AROkay x) -> Right x

#define EP(N, ARG, R) \
    N##' :: BasicAuthData -> ARG -> ClientM R;\
    N :: ApiKey -> ARG -> Manager -> IO (Either ClientError R);\
    N k a m = runClientM (N##' (BasicAuthData (T.encodeUtf8 k) "") a) (mkClientEnv m stripeBaseUrl)

#define EP2(N, ARG, ARG2, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ClientM R;\
    N :: ApiKey -> ARG -> ARG2 -> Manager -> IO (Either ClientError R);\
    N k a b m = runClientM (N##' (BasicAuthData (T.encodeUtf8 k) "") a b) (mkClientEnv m stripeBaseUrl)

#define EP3(N, ARG, ARG2, ARG3, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ARG3 -> ClientM R;\
    N :: ApiKey -> ARG -> ARG2 -> ARG3 -> Manager -> IO (Either ClientError R);\
    N k a b c m = runClientM (N##' (BasicAuthData (T.encodeUtf8 k) "") a b c) (mkClientEnv m stripeBaseUrl)

EP(createCustomer, CustomerCreate, Customer)
EP(retrieveCustomer, CustomerId, Customer)
EP2(updateCustomer, CustomerId, CustomerUpdate, Customer)

createCustomer' :<|> retrieveCustomer' :<|> updateCustomer'
  = client api
