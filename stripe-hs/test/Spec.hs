import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec
import Data.Time.Clock.POSIX
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Stripe.Client
import Stripe.Webhook.Verify

makeClient :: IO StripeClient
makeClient =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "STRIPE_KEY"
     pure (makeStripeClient apiKey manager)

forceSuccess :: (MonadFail m, Show a) => m (Either a b) -> m b
forceSuccess req =
  req >>= \res ->
  case res of
    Left err -> fail (show err)
    Right ok -> pure ok

main :: IO ()
main =
  hspec $
  do describe "api" apiTests
     describe "webhooks" webhookTests

testStripeSignature :: BS.ByteString
testStripeSignature =
  "t=1608784111"
  <> ",v1=3cef61e0cf5ad6e9832925080d463dd7396160f8371c430e426242f3f89f4126"
  <> ",v0=aa179c5358b80a557a960137a85279caa13b06a05390816b533c3c146127bbe0"

-- don't worry, i've long deleted this one :)
testStripeSecret :: WebhookSecret
testStripeSecret =
  "whsec_2FZJ5Kqdi6AF8fGCLOCP9lH8Hpw9DW6T"

webhookTests :: SpecWith ()
webhookTests =
  do it "accepts correctly signed webhooks" $ \() ->
       do bs <- BS.readFile "test-data/customer_webhook.json"
          let stripped = BS.take (BS.length bs - 1) bs -- trim trailing newline
              time = posixSecondsToUTCTime 1608784111
          verifyStripeSignature testStripeSecret testStripeSignature stripped `shouldBe` VOk time

apiTests :: SpecWith ()
apiTests =
  before makeClient $
  do describe "customers" $
       do it "creates a customer" $ \cli ->
            do cr <- forceSuccess $ createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               cEmail cr `shouldBe` Just "mail@athiemann.net"
          it "retrieves a customer" $ \cli ->
            do cr <- forceSuccess $ createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               cu <- forceSuccess $ retrieveCustomer cli (cId cr)
               cu `shouldBe` cr
          it "updates a customer" $ \cli ->
            do cr <- forceSuccess $ createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               cu <- forceSuccess $ updateCustomer cli (cId cr) (CustomerUpdate Nothing (Just "mail+2@athiemann.net"))
               cEmail cu `shouldBe` Just "mail+2@athiemann.net"
