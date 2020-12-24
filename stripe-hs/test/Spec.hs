import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec
import System.Environment (getEnv)
import qualified Data.Text as T

import Stripe.Client

makeClient :: IO (Manager, ApiKey)
makeClient =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "STRIPE_KEY"
     pure (manager, apiKey)

main :: IO ()
main =
  hspec $
  before makeClient $
  do it "creates a customer" $ \(manager, key) ->
       do cr <- createCustomer key (CustomerCreate Nothing (Just "mail@athiemann.net")) manager
          fmap cEmail cr `shouldBe` Right (Just "mail@athiemann.net")
