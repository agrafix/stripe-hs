module Stripe.Webhook.Verify
  ( verifyStripeSignature
  , WebhookSecret, VerificationResult(..)
  )
where

import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Bifunctor
import Data.ByteArray.Encoding
import Data.Time
import Data.Time.Clock.POSIX
import Safe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- | Your webhook secret, can be obtained from the Stripe dashboard. Format: @whsec_<redacted>@
type WebhookSecret = BS.ByteString

-- | Output of the webhook signature verification
data VerificationResult
  = VOk UTCTime
  -- ^ Signature verification successful, check the time against the current time and reject /too old/ requests.
  | VFailed
  -- ^ Signature verification failed. Check that your 'WebhookSecret' is correct.
  | VInvalidSignature
  -- ^ Invalid signature. Verify that you are passing the raw contents of the @stripe-signature@ header.
  deriving (Show, Eq)

-- | Verify the @stripe-signature@ header
verifyStripeSignature ::
  WebhookSecret
  -- ^ Your webhook secret
  -> BS.ByteString
  -- ^ Value of the @stripe-signature@ header
  -> BS.ByteString
  -- ^ Raw request body received from Stripe
  -> VerificationResult
verifyStripeSignature secret sig rawBody =
  let sigMap = map (second (BS.drop 1) . BSC.break (\c -> c == '=')) . BSC.split ',' $ sig
      needed =
        do t <- lookup "t" sigMap
           (parsedTime :: Int) <- readMay (BSC.unpack t)
           v1 <- lookup "v1" sigMap
           pure (t, posixSecondsToUTCTime $ fromIntegral parsedTime, v1)
  in case needed of
       Nothing -> VInvalidSignature
       Just (rawTime, time, v1) ->
         let payload = rawTime <> BSC.singleton '.' <> rawBody
             computedSig :: HMAC SHA256
             computedSig = hmac secret payload
             hexSig = convertToBase Base16 computedSig
         in if hexSig == v1
               then VOk time
               else VFailed
