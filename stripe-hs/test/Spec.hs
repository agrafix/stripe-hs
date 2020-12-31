import Test.Hspec

import ApiSpec
import WebhookSpec

main :: IO ()
main =
  hspec $
  do apiSpec
     webhookSpec
