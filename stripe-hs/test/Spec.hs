import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec
import Data.Time
import Data.Time.TimeSpan
import Data.Time.Clock.POSIX
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Vector as V

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

apiTests :: SpecWith ()
apiTests =
  before makeClient $
  do describe "events" $
       do it "lists events" $ \cli ->
            do _ <- forceSuccess $ createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               res <- forceSuccess $ listEvents cli Nothing
               V.null (slData res) `shouldBe` False
     describe "products" $
       do it "creates a product" $ \cli ->
            do res <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               prName res `shouldBe` "Test"
          it "retrieves a product" $ \cli ->
            do res <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               res2 <- forceSuccess $ retrieveProduct cli (prId res)
               res `shouldBe` res2
     describe "prices" $
       do it "creates a price" $ \cli ->
            do prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               res <-
                 forceSuccess $
                 createPrice cli $
                 PriceCreate "usd" (Just 1000) (prId prod) (Just "lk") True $
                 Just (PriceCreateRecurring "month" Nothing)
               pCurrency res `shouldBe` "usd"
               pUnitAmount res `shouldBe` Just 1000
               pType res `shouldBe` "recurring"
               pLookupKey res `shouldBe` Just "lk"
               pRecurring res `shouldBe` Just (PriceRecurring "month" 1)
          it "retrieves a price" $ \cli ->
            do prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               res <-
                 forceSuccess $
                 createPrice cli $
                 PriceCreate "usd" (Just 1000) (prId prod) Nothing False $
                 Just (PriceCreateRecurring "month" Nothing)
               res2 <- forceSuccess $ retrievePrice cli (pId res)
               res `shouldBe` res2
          it "lists by lookup_key" $ \cli ->
            do prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               price <-
                 forceSuccess $
                 createPrice cli $
                 PriceCreate "usd" (Just 1000) (prId prod) (Just "the_key") True $
                 Just (PriceCreateRecurring "month" Nothing)
               res <- forceSuccess $ listPrices cli (Just "the_key")
               pId (V.head (slData res)) `shouldBe` pId price
               res2 <- forceSuccess $ listPrices cli (Just "KEY_NOT_EXISTING_OK")
               V.null (slData res2) `shouldBe` True
     describe "subscriptions" $
       do it "allows creating a subscription" $ \cli ->
            do customer <-
                 forceSuccess $
                 createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               price <-
                 forceSuccess $
                 createPrice cli $
                 PriceCreate "usd" (Just 1000) (prId prod) Nothing False $
                 Just (PriceCreateRecurring "month" Nothing)
               trialEnd <- TimeStamp . addUTCTimeTS (hours 1) <$> getCurrentTime
               subscription <-
                 forceSuccess $
                 createSubscription cli $
                 SubscriptionCreate
                 { scCustomer = cId customer
                 , scItems = [SubscriptionCreateItem (pId price) (Just 1)]
                 , scCancelAtPeriodEnd = Just False
                 , scTrialEnd = Just trialEnd
                 }
               sCancelAtPeriodEnd subscription `shouldBe` False
               sCustomer subscription `shouldBe` cId customer
               let items = sItems subscription
               fmap siPrice items `shouldBe` pure price
               fmap siQuantity items `shouldBe` pure (Just 1)
               fmap siSubscription items `shouldBe` pure (sId subscription)
               sStatus subscription `shouldBe` "trialing"
     describe "customer portal" $
       do it "allows creating a customer portal (needs setup in dashboard)" $ \cli ->
            do customer <-
                 forceSuccess $
                 createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               portal <-
                 forceSuccess $
                 createCustomerPortal cli (CustomerPortalCreate (cId customer) (Just "https://athiemann.net/return"))
               cpCustomer portal `shouldBe` cId customer
               cpReturnUrl portal `shouldBe` Just "https://athiemann.net/return"
     describe "checkout" $
       do it "create and retrieves a checkout session" $ \cli ->
            do prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
               price <-
                 forceSuccess $
                 createPrice cli $
                 PriceCreate "usd" (Just 1000) (prId prod) Nothing False $
                 Just (PriceCreateRecurring "month" Nothing)
               customer <-
                 forceSuccess $
                 createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
               session <-
                 forceSuccess $
                 createCheckoutSession cli $
                 CheckoutSessionCreate
                 { cscCancelUrl = "https://athiemann.net/cancel"
                 , cscMode = "subscription"
                 , cscPaymentMethodTypes = ["card"]
                 , cscSuccessUrl = "https://athiemann.net/success"
                 , cscClientReferenceId = Just "cool"
                 , cscCustomer = Just (cId customer)
                 , cscLineItems = [CheckoutSessionCreateLineItem (pId price) 1]
                 }
               csClientReferenceId session `shouldBe` Just "cool"
               csCancelUrl session `shouldBe` "https://athiemann.net/cancel"
               csSuccessUrl session `shouldBe` "https://athiemann.net/success"
               csPaymentMethodTypes session `shouldBe` V.singleton "card"

               sessionRetrieved <-
                 forceSuccess $
                 retrieveCheckoutSession cli (csId session)
               sessionRetrieved `shouldBe` session

     describe "customers" $
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
