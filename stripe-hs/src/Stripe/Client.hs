{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}
module Stripe.Client
  ( -- * Basics
    ApiKey, StripeClient, makeStripeClient
    -- * Helper types
  , TimeStamp(..), StripeList(..)
    -- * Customers
  , createCustomer, retrieveCustomer, updateCustomer, listCustomers
  , Customer(..), CustomerCreate(..), CustomerUpdate(..)
    -- * Product catalog
  , ProductId(..), PriceId(..), Product(..), Price(..), PriceRecurring(..)
  , ProductCreate(..), PriceCreate(..), PriceCreateRecurring(..)
  , createProduct, retrieveProduct
  , createPrice, retrievePrice, listPrices
    -- * Subscriptions
  , SubscriptionId(..), SubscriptionItemId(..), Subscription(..), SubscriptionItem(..), SubscriptionCreate(..), SubscriptionCreateItem(..)
  , createSubscription, retrieveSubscription, listSubscriptions
    -- * Customer Portal
  , CustomerPortalId(..), CustomerPortal(..), CustomerPortalCreate(..)
  , createCustomerPortal
    -- * Checkout
  , CheckoutSessionId(..), CheckoutSession(..), CheckoutSessionCreate(..), CheckoutSessionCreateLineItem(..)
  , createCheckoutSession, retrieveCheckoutSession
    -- * Events
  , retrieveEvent, listEvents
  , Event(..), EventData(..)
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

-- | Your Stripe API key. Can be obtained from the Stripe dashboard. Format: @sk_<mode>_<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data StripeClient
  = StripeClient
  { scBasicAuthData :: BasicAuthData
  , scManager :: Manager
  }

-- | Construct a 'StripeClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeStripeClient :: ApiKey -> Manager -> StripeClient
makeStripeClient k = StripeClient (BasicAuthData (T.encodeUtf8 k) "")

api :: Proxy StripeApi
api = Proxy

stripeBaseUrl :: BaseUrl
stripeBaseUrl = BaseUrl Https "api.stripe.com" 443 ""

#define EP(N, ARG, R) \
    N##' :: BasicAuthData -> ARG -> ClientM R;\
    N :: StripeClient -> ARG -> IO (Either ClientError R);\
    N sc a = runClientM (N##' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) stripeBaseUrl)

#define EP2(N, ARG, ARG2, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ClientM R;\
    N :: StripeClient -> ARG -> ARG2 -> IO (Either ClientError R);\
    N sc a b = runClientM (N##' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) stripeBaseUrl)

#define EP3(N, ARG, ARG2, ARG3, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ARG3 -> ClientM R;\
    N :: StripeClient -> ARG -> ARG2 -> ARG3 -> IO (Either ClientError R);\
    N sc a b c = runClientM (N##' (scBasicAuthData sc) a b c) (mkClientEnv (scManager sc) stripeBaseUrl)

EP(createCustomer, CustomerCreate, Customer)
EP(retrieveCustomer, CustomerId, Customer)
EP2(updateCustomer, CustomerId, CustomerUpdate, Customer)
EP(listCustomers, Maybe CustomerId, (StripeList Customer))

EP(createProduct, ProductCreate, Product)
EP(retrieveProduct, ProductId, Product)

EP(createPrice, PriceCreate, Price)
EP(retrievePrice, PriceId, Price)
EP(listPrices, Maybe T.Text, (StripeList Price))

EP(createSubscription, SubscriptionCreate, Subscription)
EP(retrieveSubscription, SubscriptionId, Subscription)
EP(listSubscriptions, Maybe CustomerId, (StripeList Subscription))

EP(createCheckoutSession, CheckoutSessionCreate, CheckoutSession)
EP(retrieveCheckoutSession, CheckoutSessionId, CheckoutSession)

EP(createCustomerPortal, CustomerPortalCreate, CustomerPortal)

EP(retrieveEvent, EventId, Event)
EP(listEvents, Maybe EventId, (StripeList Event))

(createCustomer' :<|> retrieveCustomer' :<|> updateCustomer' :<|> listCustomers')
  :<|> (createProduct' :<|> retrieveProduct')
  :<|> (createPrice' :<|> retrievePrice' :<|> listPrices')
  :<|> (createSubscription' :<|> retrieveSubscription' :<|> listSubscriptions')
  :<|> (createCheckoutSession' :<|> retrieveCheckoutSession')
  :<|> (createCustomerPortal')
  :<|> (retrieveEvent' :<|> listEvents')
  = client api
