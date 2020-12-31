import Test.Hspec

import ApiSpec
import WebhookSpec
import HelperSpec

main :: IO ()
main =
  hspec $
  do apiSpec
     helperSpec
     webhookSpec
