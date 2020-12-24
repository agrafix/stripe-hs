-- | The API

module Stripe.Api where

import Stripe.Resources

import Servant.API
import qualified Data.Text as T

type StripeAuth = BasicAuth "Stripe API" ()

type StripeApi
  = "v1" :> StripeApiInternal

type StripeApiInternal
  = "customers" :> CustomerApi
  :<|> "products" :> ProductApi
  :<|> "prices" :> PriceApi
  :<|> "checkout" :> "sessions" :> CheckoutApi
  :<|> "events" :> EventApi

type CustomerApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] CustomerCreate :> Post '[JSON] Customer
  :<|> StripeAuth :> Capture ":customer_id" CustomerId :> Get '[JSON] Customer
  :<|> StripeAuth :> Capture ":customer_id" CustomerId :> ReqBody '[FormUrlEncoded] CustomerUpdate :> Post '[JSON] Customer
  :<|> StripeAuth :> QueryParam "starting_after" CustomerId :> Get '[JSON] (StripeList Customer)

type EventApi
  = StripeAuth :> Capture ":event_id" EventId :> Get '[JSON] Event
  :<|> StripeAuth :> QueryParam "starting_after" EventId :> Get '[JSON] (StripeList Event)

type ProductApi
  = StripeAuth :> Capture ":product_id" ProductId :> Get '[JSON] Product

type PriceApi
  = StripeAuth :> Capture ":product_id" PriceId :> Get '[JSON] Price
  :<|> StripeAuth :> QueryParam "lookup_keys" T.Text :> Get '[JSON] (StripeList Price)

type CheckoutApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] CheckoutSessionCreate :> Post '[JSON] CheckoutSession
  :<|> StripeAuth :> Capture ":session_id" CheckoutSessionId :> Get '[JSON] CheckoutSession
