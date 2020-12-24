{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Stripe.Resources
  ( -- * Core Types
    TimeStamp(..), StripeList(..)
    -- * Customers
  , CustomerId(..), Customer(..), CustomerCreate(..), CustomerUpdate(..)
    -- * Events
  , EventId(..), Event(..), EventData(..)
  )
where

import Stripe.Util.Aeson

import Servant.API
import Data.Time.Clock.POSIX
import Web.FormUrlEncoded
import qualified Data.Text as T
import GHC.Generics
import Text.Casing (quietSnake)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Data.Time

formOptions :: Int -> FormOptions
formOptions x =
  FormOptions
  { fieldLabelModifier = quietSnake . drop x }

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp
  = TimeStamp { unTimeStamp :: UTCTime }
  deriving (Show, Eq)

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
    pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

-- | A 'V.Vector' wrapper with an indication is there are more items available through pagination.
data StripeList a
  = StripeList
  { slHasMore :: Bool
  , slData :: V.Vector a
  } deriving (Show, Eq)

newtype CustomerId
  = CustomerId { unCustomerId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Customer
  = Customer
  { cId :: CustomerId
  , cLivemode :: Bool
  , cCreated :: TimeStamp
  , cName :: Maybe T.Text
  , cEmail :: Maybe T.Text
  } deriving (Show, Eq)

data CustomerCreate
  = CustomerCreate
  { ccName :: Maybe T.Text
  , ccEmail :: Maybe T.Text
  } deriving (Show, Eq, Generic)

data CustomerUpdate
  = CustomerUpdate
  { cuName :: Maybe T.Text
  , cuEmail :: Maybe T.Text
  } deriving (Show, Eq, Generic)

newtype EventId
  = EventId { unEventId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Event
  = Event
  { eId :: EventId
  , eCreated :: TimeStamp
  , eLivemode :: Bool
  , eType :: T.Text
  , eApiVersion :: T.Text
  , eData :: EventData
  } deriving (Show, Eq)

data EventData
  = EventData
  { edObject :: A.Value
  } deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''StripeList)
$(deriveJSON (jsonOpts 1) ''Customer)
$(deriveJSON (jsonOpts 1) ''Event)
$(deriveJSON (jsonOpts 2) ''EventData)

instance ToForm CustomerCreate where
  toForm = genericToForm (formOptions 2)

instance ToForm CustomerUpdate where
  toForm = genericToForm (formOptions 2)
