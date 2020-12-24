-- | The API

module Stripe.Api where

import Stripe.Resources

import Servant.API

type StripeAuth = BasicAuth "Stripe API" ()

type StripeApi
  = "v1" :> StripeApiInternal

type StripeApiInternal
  = "customers" :> CustomerApi
  :<|> "events" :> EventApi

type CustomerApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] CustomerCreate :> Post '[JSON] Customer
  :<|> StripeAuth :> Capture ":customer_id" CustomerId :> Get '[JSON] Customer
  :<|> StripeAuth :> Capture ":customer_id" CustomerId :> ReqBody '[FormUrlEncoded] CustomerUpdate :> Post '[JSON] Customer
  :<|> StripeAuth :> QueryParam "starting_after" CustomerId :> Get '[JSON] (StripeList Customer)

type EventApi
  = StripeAuth :> Capture ":event_id" EventId :> Get '[JSON] Event
  :<|> StripeAuth :> QueryParam "starting_after" EventId :> Get '[JSON] (StripeList Event)
