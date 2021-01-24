module ApiSpec (apiSpec) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec
import Data.Time
import Data.Time.TimeSpan
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Vector as V

import Stripe.Client

makeClient :: IO StripeClient
makeClient =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "STRIPE_KEY"
     pure (makeStripeClient apiKey manager 2)

forceSuccess :: (MonadFail m, Show a) => m (Either a b) -> m b
forceSuccess req =
  req >>= \res ->
  case res of
    Left err -> fail (show err)
    Right ok -> pure ok

apiSpec :: Spec
apiSpec =
  do describe "core api" apiTests
     describe "api" apiWorldTests

apiTests :: SpecWith ()
apiTests =
  beforeAll makeClient $
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

data StripeWorld
  = StripeWorld
  { swProduct :: Product
  , swPrice :: Price
  , swCustomer :: Customer
  } deriving (Show, Eq)

makeStripeWorld :: IO (StripeClient, StripeWorld)
makeStripeWorld =
  do cli <- makeClient
     customer <-
       forceSuccess $
       createCustomer cli (CustomerCreate Nothing (Just "mail@athiemann.net"))
     prod <- forceSuccess $ createProduct cli (ProductCreate "Test" Nothing)
     price <-
       forceSuccess $
       createPrice cli $
       PriceCreate "usd" (Just 1000) (prId prod) Nothing False $
       Just (PriceCreateRecurring "month" Nothing)
     pure (cli, StripeWorld prod price customer)

apiWorldTests :: SpecWith ()
apiWorldTests =
  beforeAll makeStripeWorld $
  do describe "subscriptions" $
       do it "allows creating a subscription" $ \(cli, sw) ->
            do trialEnd <- TimeStamp . addUTCTimeTS (hours 1) <$> getCurrentTime
               subscription <-
                 forceSuccess $
                 createSubscription cli $
                 SubscriptionCreate
                 { scCustomer = cId (swCustomer sw)
                 , scItems = [SubscriptionCreateItem (pId (swPrice sw)) (Just 1)]
                 , scCancelAtPeriodEnd = Just False
                 , scTrialEnd = Just trialEnd
                 }
               sCancelAtPeriodEnd subscription `shouldBe` False
               sCustomer subscription `shouldBe` cId (swCustomer sw)
               let items = sItems subscription
               fmap siPrice items `shouldBe` pure (swPrice sw)
               fmap siQuantity items `shouldBe` pure (Just 1)
               fmap siSubscription items `shouldBe` pure (sId subscription)
               sStatus subscription `shouldBe` "trialing"
     describe "customer portal" $
       do it "allows creating a customer portal (needs setup in dashboard)" $ \(cli, sw) ->
            do portal <-
                 forceSuccess $
                 createCustomerPortal cli (CustomerPortalCreate (cId (swCustomer sw)) (Just "https://athiemann.net/return"))
               cpCustomer portal `shouldBe` cId (swCustomer sw)
               cpReturnUrl portal `shouldBe` Just "https://athiemann.net/return"
     describe "checkout" $
       do it "create and retrieves a checkout session" $ \(cli, sw) ->
            do session <-
                 forceSuccess $
                 createCheckoutSession cli $
                 CheckoutSessionCreate
                 { cscCancelUrl = "https://athiemann.net/cancel"
                 , cscMode = "subscription"
                 , cscPaymentMethodTypes = ["card"]
                 , cscSuccessUrl = "https://athiemann.net/success"
                 , cscClientReferenceId = Just "cool"
                 , cscCustomer = Just (cId (swCustomer sw))
                 , cscAllowPromotionCodes = Just True
                 , cscLineItems = [CheckoutSessionCreateLineItem (pId (swPrice sw)) 1]
                 }
               csClientReferenceId session `shouldBe` Just "cool"
               csCancelUrl session `shouldBe` "https://athiemann.net/cancel"
               csSuccessUrl session `shouldBe` "https://athiemann.net/success"
               csPaymentMethodTypes session `shouldBe` V.singleton "card"
               csAllowPromotionCodes session `shouldBe` Just True

               sessionRetrieved <-
                 forceSuccess $
                 retrieveCheckoutSession cli (csId session)
               sessionRetrieved `shouldBe` session
