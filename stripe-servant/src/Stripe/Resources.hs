{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Stripe.Resources
  ( -- * Core Types
    TimeStamp(..), StripeList(..)
    -- * Customers
  , CustomerId(..), Customer(..), CustomerCreate(..), CustomerUpdate(..)
    -- * Product catalog
  , ProductId(..), PriceId(..)
  , Product(..), ProductCreate(..)
  , Price(..), PriceRecurring(..), PriceCreate(..), PriceCreateRecurring(..)
    -- * Checkout
  , CheckoutSessionId(..), CheckoutSession(..), CheckoutSessionCreate(..), CheckoutSessionCreateLineItem(..)
    -- * Events
  , EventId(..), Event(..), EventData(..)
  )
where

import Stripe.Util.Aeson

import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import Servant.API
import Text.Casing (quietSnake)
import Web.FormUrlEncoded
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

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

newtype PriceId
  = PriceId { unPriceId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Price
  = Price
  { pId :: PriceId
  , pActive :: Bool
  , pCurrency :: T.Text
  , pNickname :: Maybe T.Text
  , pType :: T.Text -- TODO: make enum
  , pRecurring :: Maybe PriceRecurring
  , pUnitAmount :: Maybe Int
  , pProduct :: ProductId
  , pLookupKey :: Maybe T.Text
  } deriving (Show, Eq)

data PriceRecurring
  = PriceRecurring
  { prInterval :: T.Text -- TODO: make enum
  , prIntervalCount :: Int
  } deriving (Show, Eq)

data PriceCreate
  = PriceCreate
  { pcCurrency :: T.Text
  , pcUnitAmount :: Maybe Int
  , pcProduct :: ProductId
  , pcLookupKey :: Maybe T.Text
  , pcTransferLookupKey :: Bool
  , pcRecurring :: Maybe PriceCreateRecurring
  } deriving (Show, Eq, Generic)

data PriceCreateRecurring
  = PriceCreateRecurring
  { prcInterval :: T.Text -- TODO: make enum
  , prcIntervalCount :: Maybe Int
  } deriving (Show, Eq)

newtype ProductId
  = ProductId { unProductId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Product
  = Product
  { prId :: ProductId
  , prActive :: Bool
  , prName :: T.Text
  , prDescription :: Maybe T.Text
  } deriving (Show, Eq)

data ProductCreate
  = ProductCreate
  { prcName :: T.Text
  , prcDescription :: Maybe T.Text
  } deriving (Show, Eq, Generic)

newtype CheckoutSessionId
  = CheckoutSessionId { unCheckoutSessionId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data CheckoutSession
  = CheckoutSession
  { csId :: CheckoutSessionId
  , csClientReferenceId :: Maybe T.Text
  , csCancelUrl :: T.Text
  , csSuccessUrl :: T.Text
  , csPaymentMethodTypes :: V.Vector T.Text  -- TODO: make enum
  } deriving (Show, Eq)

data CheckoutSessionCreate
  = CheckoutSessionCreate
  { cscCancelUrl :: T.Text
  , cscMode :: T.Text  -- TODO: make enum
  , cscPaymentMethodTypes :: [T.Text]  -- TODO: make enum
  , cscSuccessUrl :: T.Text
  , cscClientReferenceId :: Maybe T.Text
  , cscCustomer :: Maybe CustomerId
  , cscLineItems :: [CheckoutSessionCreateLineItem]
  } deriving (Show, Eq, Generic)

data CheckoutSessionCreateLineItem
  = CheckoutSessionCreateLineItem
  { cscliPrice :: PriceId
  , cscliQuantity :: Integer
  } deriving (Show, Eq, Generic)

$(deriveJSON (jsonOpts 2) ''StripeList)
$(deriveJSON (jsonOpts 1) ''Customer)
$(deriveJSON (jsonOpts 1) ''Event)
$(deriveJSON (jsonOpts 2) ''EventData)
$(deriveJSON (jsonOpts 2) ''CheckoutSession)
$(deriveJSON (jsonOpts 1) ''Price)
$(deriveJSON (jsonOpts 2) ''PriceRecurring)
$(deriveJSON (jsonOpts 2) ''Product)

instance ToForm CustomerCreate where
  toForm = genericToForm (formOptions 2)

instance ToForm CustomerUpdate where
  toForm = genericToForm (formOptions 2)

instance ToForm ProductCreate where
  toForm = genericToForm (formOptions 3)

instance ToForm PriceCreate where
  toForm pc =
    let recurringPiece =
          case pcRecurring pc of
            Nothing -> []
            Just x ->
              [ ("recurring[interval]", [prcInterval x])
              , ("recurring[interval_count]", maybeToList $ fmap toUrlPiece $ prcIntervalCount x)
              ]
    in Form $ HM.fromList $
       [ ("currency", [pcCurrency pc])
       , ("product", [toUrlPiece $ pcProduct pc])
       , ("unit_amount", maybeToList $ fmap toUrlPiece $ pcUnitAmount pc)
       , ("lookup_key", maybeToList $ pcLookupKey pc)
       , ("transfer_lookup_key", [toUrlPiece $ pcTransferLookupKey pc])
       ] <> recurringPiece

instance ToForm CheckoutSessionCreate where
  toForm csc =
    let convertItem itm =
          [ ("line_items[0][price]", [toUrlPiece $ cscliPrice itm])
          , ("line_items[0][quantity]", [toUrlPiece $ cscliQuantity itm])
          ]
        lineItems =
          concatMap convertItem (cscLineItems csc)
    in Form $ HM.fromList $
       [ ("cancel_url", [cscCancelUrl csc])
       , ("success_url", [cscSuccessUrl csc])
       , ("payment_method_types", cscPaymentMethodTypes csc)
       , ("mode", [cscMode csc])
       , ("client_reference_id", maybeToList $ cscClientReferenceId csc)
       , ("customer", maybeToList $ fmap toUrlPiece $ cscCustomer csc)
       ] <> lineItems
