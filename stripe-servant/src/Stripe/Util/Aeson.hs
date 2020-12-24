-- |

module Stripe.Util.Aeson
  ( jsonOpts, deriveJSON, ToJSON, FromJSON )
where

import Data.Aeson
import Data.Aeson.TH
import Text.Casing (quietSnake)

jsonOpts :: Int -> Options
jsonOpts x =
  defaultOptions
  { fieldLabelModifier = quietSnake . drop x
  , constructorTagModifier = quietSnake
  }
