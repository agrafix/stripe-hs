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
  :<|> "subscriptions" :> SubscriptionApi
  :<|> "invoices" :> InvoiceApi
  :<|> "payment_methods" :> PaymentMethodApi
  :<|> "checkout" :> "sessions" :> CheckoutApi
  :<|> "billing_portal" :> "sessions" :> CustomerPortalApi
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
  = StripeAuth :> ReqBody '[FormUrlEncoded] ProductCreate :> Post '[JSON] Product
  :<|> StripeAuth :> Capture ":product_id" ProductId :> Get '[JSON] Product

type PriceApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] PriceCreate :> Post '[JSON] Price
  :<|> StripeAuth :> Capture ":product_id" PriceId :> Get '[JSON] Price
  :<|> StripeAuth :> QueryParam "lookup_keys[]" T.Text :> Get '[JSON] (StripeList Price)

type InvoiceApi =
  StripeAuth :> Capture ":invoice_id" InvoiceId :> Get '[JSON] Invoice
    :<|> StripeAuth :> QueryParam "customer" CustomerId :> QueryParam "expand[]" T.Text :> Get '[JSON] (StripeList Invoice)

type PaymentMethodApi =
  StripeAuth :> QueryParam "customer" CustomerId :> QueryParam "type" T.Text :> Get '[JSON] (StripeList PaymentMethod)

type SubscriptionApi =
  StripeAuth :> ReqBody '[FormUrlEncoded] SubscriptionCreate :> Post '[JSON] Subscription
    :<|> StripeAuth :> Capture ":subscription_id" SubscriptionId :> Get '[JSON] Subscription
    :<|> StripeAuth :> QueryParam "customer" CustomerId :> Get '[JSON] (StripeList Subscription)
    :<|> StripeAuth :> Capture ":subscription_id" SubscriptionId :> ReqBody '[FormUrlEncoded] SubscriptionUpdate :> Post '[JSON] Subscription

type CheckoutApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] CheckoutSessionCreate :> Post '[JSON] CheckoutSession
  :<|> StripeAuth :> Capture ":session_id" CheckoutSessionId :> Get '[JSON] CheckoutSession

type CustomerPortalApi
  = StripeAuth :> ReqBody '[FormUrlEncoded] CustomerPortalCreate :> Post '[JSON] CustomerPortal
