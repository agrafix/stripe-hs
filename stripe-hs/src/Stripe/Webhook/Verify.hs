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

type WebhookSecret = BS.ByteString

data VerificationResult
  = VOk UTCTime
  | VFailed
  | VInvalidSignature
  deriving (Show, Eq)

verifyStripeSignature :: WebhookSecret -> BS.ByteString -> BS.ByteString -> VerificationResult
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
