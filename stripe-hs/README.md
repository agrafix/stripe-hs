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
     let client = makeStripeClient apiKey manager
     result <-
         createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
     print result
```

## Features

The package provides a module for webhook signature verification (see `Stripe.Webhook.Verify`).

Supported APIs/Resources:
* Customers *(missing fields)*
* Events *(missing fields)*
