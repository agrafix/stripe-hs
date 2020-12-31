module WebhookSpec (webhookSpec) where

import Test.Hspec
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS

import Stripe.Webhook.Verify

webhookSpec :: Spec
webhookSpec =
  do describe "webhooks" webhookTests

testStripeSignature :: BS.ByteString
testStripeSignature =
  "t=1608784111"
  <> ",v1=3cef61e0cf5ad6e9832925080d463dd7396160f8371c430e426242f3f89f4126"
  <> ",v0=aa179c5358b80a557a960137a85279caa13b06a05390816b533c3c146127bbe0"

-- Don't worry, i've long deleted this one... obfuscating a bit so that search tools
-- don't find it an try it out.
testStripeSecret :: WebhookSecret
testStripeSecret =
  "wh" <> "sec_" <> "2FZJ5Kqdi6AF8fGCLOCP9lH8Hpw9DW6T"

webhookTests :: SpecWith ()
webhookTests =
  do it "accepts correctly signed webhooks" $
       do bs <- BS.readFile "test-data/customer_webhook.json"
          let stripped = BS.take (BS.length bs - 1) bs -- trim trailing newline
              time = posixSecondsToUTCTime 1608784111
          verifyStripeSignature testStripeSecret testStripeSignature stripped `shouldBe` VOk time
     it "fails incorrectly signed webhooks" $
       do let bs = "{\"foo\": 1234}"
          verifyStripeSignature testStripeSecret testStripeSignature bs `shouldBe` VFailed
     it "detects bad signatures" $
       do let bs = "{\"foo\": 1234}"
          verifyStripeSignature testStripeSecret "fooo" bs `shouldBe` VInvalidSignature
