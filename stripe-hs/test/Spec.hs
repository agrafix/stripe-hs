import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec
import System.Environment (getEnv)
import qualified Data.Text as T

import Stripe.Client

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
