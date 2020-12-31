# stripe-hs

Unofficial and incomplete Stripe SDK/client for Haskell. It's generated via `servant-client` from `stripe-servant` with a small amount of hand-written code. Contributions are welcome!

## Install

``` sh
# stack
stack install stripe-hs

# cabal
cabal install stripe-hs
```

## Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Stripe.Client

import System.Environment (getEnv)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "STRIPE_KEY"
     -- create a stripe client that automatically retries up to 4 times on network
     -- errors
     let client = makeStripeClient apiKey manager 4
     result <-
         createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
     print result
```

## Features

The package provides a module for webhook signature verification (see `Stripe.Webhook.Verify`).

The implementation retries automatically according to [Stripe's error handling documentation](https://stripe.com/docs/error-handling#safely-retrying-requests-with-idempotency).

### Supported APIs/Resources:

* Customers
* Products
* Prices
* CustomerPortal
* CheckoutSession
* Events

*Note that all resources are likely missing fields. The library is currently focused on to be used in combination with Stripe's hosted surfaces (Customer Portal and Checkout).*

## Running the tests

You can run all tests with `stack test`. You'll need a Stripe testmode API Key assigned to the `STRIPE_KEY` environment variable and you'll need to setup a Customer Portal configuration in the Stripe dashboard before running them.
