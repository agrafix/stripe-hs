{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Stripe.Resources
  ( -- * Core Types
    TimeStamp (..),
    StripeList (..),

    -- * Customers
    CustomerId (..),
    Customer (..),
    CustomerCreate (..),
    CustomerUpdate (..),

    -- * Product catalog
    ProductId (..),
    PriceId (..),
    Product (..),
    ProductCreate (..),
    Price (..),
    PriceRecurring (..),
    PriceCreate (..),
    PriceCreateRecurring (..),

    -- * Subscriptions
    SubscriptionId (..),
    SubscriptionItemId (..),
    Subscription (..),
    SubscriptionItem (..),
    SubscriptionCreate (..),
    SubscriptionCreateItem (..),

    -- * Customer Portal
    CustomerPortalId (..),
    CustomerPortal (..),
    CustomerPortalCreate (..),

    -- * Checkout
    CheckoutSessionId (..),
    CheckoutSession (..),
    CheckoutSessionCreate (..),
    CheckoutSessionCreateLineItem (..),

    -- * Events
    EventId (..),
    Event (..),
    EventData (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import GHC.Generics
import Servant.API
import Stripe.Util.Aeson
import Text.Casing (quietSnake)
import Web.FormUrlEncoded

formOptions :: Int -> FormOptions
formOptions x =
  FormOptions
    { fieldLabelModifier = quietSnake . drop x
    }

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving (Show, Eq)

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
      pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

instance ToHttpApiData TimeStamp where
  toUrlPiece x =
    let unix :: Int
        unix = round . utcTimeToPOSIXSeconds . unTimeStamp $ x
     in T.pack (show unix)

-- | A 'V.Vector' wrapper with an indication is there are more items available through pagination.
data StripeList a = StripeList
  { slHasMore :: Bool,
    slData :: V.Vector a
  }
  deriving (Show, Eq, Functor)

instance Semigroup (StripeList a) where
  (<>) a b = StripeList (slHasMore a || slHasMore b) (slData a <> slData b)

instance Monoid (StripeList a) where
  mempty = StripeList False mempty

instance Applicative StripeList where
  pure = StripeList False . pure
  (<*>) go x = StripeList (slHasMore go || slHasMore x) (slData go <*> slData x)

newtype CustomerId = CustomerId {unCustomerId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Customer = Customer
  { cId :: CustomerId,
    cLivemode :: Bool,
    cCreated :: TimeStamp,
    cName :: Maybe T.Text,
    cEmail :: Maybe T.Text
  }
  deriving (Show, Eq)

data CustomerCreate = CustomerCreate
  { ccName :: Maybe T.Text,
    ccEmail :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

data CustomerUpdate = CustomerUpdate
  { cuName :: Maybe T.Text,
    cuEmail :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

newtype EventId = EventId {unEventId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Event = Event
  { eId :: EventId,
    eCreated :: TimeStamp,
    eLivemode :: Bool,
    eType :: T.Text,
    eApiVersion :: T.Text,
    eData :: EventData
  }
  deriving (Show, Eq)

data EventData = EventData
  { edObject :: A.Value
  }
  deriving (Show, Eq)

newtype PriceId = PriceId {unPriceId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Price = Price
  { pId :: PriceId,
    pActive :: Bool,
    pCurrency :: T.Text,
    pNickname :: Maybe T.Text,
    pType :: T.Text, -- TODO: make enum
    pRecurring :: Maybe PriceRecurring,
    pUnitAmount :: Maybe Int,
    pProduct :: ProductId,
    pLookupKey :: Maybe T.Text
  }
  deriving (Show, Eq)

data PriceRecurring = PriceRecurring
  { prInterval :: T.Text, -- TODO: make enum
    prIntervalCount :: Int
  }
  deriving (Show, Eq)

data PriceCreate = PriceCreate
  { pcCurrency :: T.Text,
    pcUnitAmount :: Maybe Int,
    pcProduct :: ProductId,
    pcLookupKey :: Maybe T.Text,
    pcTransferLookupKey :: Bool,
    pcRecurring :: Maybe PriceCreateRecurring
  }
  deriving (Show, Eq, Generic)

data PriceCreateRecurring = PriceCreateRecurring
  { prcInterval :: T.Text, -- TODO: make enum
    prcIntervalCount :: Maybe Int
  }
  deriving (Show, Eq)

newtype ProductId = ProductId {unProductId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Product = Product
  { prId :: ProductId,
    prActive :: Bool,
    prName :: T.Text,
    prDescription :: Maybe T.Text
  }
  deriving (Show, Eq)

data ProductCreate = ProductCreate
  { prcName :: T.Text,
    prcDescription :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

newtype SubscriptionId = SubscriptionId {unSubscriptionId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Subscription = Subscription
  { sId :: SubscriptionId,
    sCancelAtPeriodEnd :: Bool,
    sCurrentPeriodEnd :: TimeStamp,
    sCurrentPeriodStart :: TimeStamp,
    sCustomer :: CustomerId,
    sItems :: StripeList SubscriptionItem,
    sStatus :: T.Text -- TODO: make enum
  }
  deriving (Show, Eq)

newtype SubscriptionItemId = SubscriptionItemId {unSubscriptionItemId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data SubscriptionItem = SubscriptionItem
  { siId :: SubscriptionItemId,
    siPrice :: Price,
    siQuantity :: Maybe Int,
    siSubscription :: SubscriptionId
  }
  deriving (Show, Eq)

data SubscriptionCreateItem = SubscriptionCreateItem
  { sciPrice :: PriceId,
    sciQuantity :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data SubscriptionCreate = SubscriptionCreate
  { scCustomer :: CustomerId,
    scItems :: [SubscriptionCreateItem],
    scCancelAtPeriodEnd :: Maybe Bool,
    scTrialEnd :: Maybe TimeStamp
  }
  deriving (Show, Eq, Generic)

newtype CheckoutSessionId = CheckoutSessionId {unCheckoutSessionId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data CheckoutSession = CheckoutSession
  { csId :: CheckoutSessionId,
    csLivemode :: Bool,
    csClientReferenceId :: Maybe T.Text,
    csCancelUrl :: T.Text,
    csSuccessUrl :: T.Text,
    csPaymentMethodTypes :: V.Vector T.Text, -- TODO: make enum
    csSubscription :: Maybe SubscriptionId,
    csAllowPromotionCodes :: Maybe Bool
  }
  deriving (Show, Eq)

data CheckoutSessionCreate = CheckoutSessionCreate
  { cscCancelUrl :: T.Text,
    cscMode :: T.Text, -- TODO: make enum
    cscPaymentMethodTypes :: [T.Text], -- TODO: make enum
    cscSuccessUrl :: T.Text,
    cscClientReferenceId :: Maybe T.Text,
    cscCustomer :: Maybe CustomerId,
    cscAllowPromotionCodes :: Maybe Bool,
    cscLineItems :: [CheckoutSessionCreateLineItem]
  }
  deriving (Show, Eq, Generic)

data CheckoutSessionCreateLineItem = CheckoutSessionCreateLineItem
  { cscliPrice :: PriceId,
    cscliQuantity :: Integer
  }
  deriving (Show, Eq, Generic)

newtype CustomerPortalId = CustomerPortalId {unCustomerPortalId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data CustomerPortal = CustomerPortal
  { cpId :: CustomerPortalId,
    cpLivemode :: Bool,
    cpCreated :: TimeStamp,
    cpCustomer :: CustomerId,
    cpReturnUrl :: Maybe T.Text,
    cpUrl :: T.Text
  }
  deriving (Show, Eq)

data CustomerPortalCreate = CustomerPortalCreate
  { cpcCustomer :: CustomerId,
    cpcReturnUrl :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (jsonOpts 2) ''StripeList)
$(deriveJSON (jsonOpts 1) ''Customer)
$(deriveJSON (jsonOpts 2) ''EventData)
$(deriveJSON (jsonOpts 1) ''Event)
$(deriveJSON (jsonOpts 2) ''CheckoutSession)
$(deriveJSON (jsonOpts 2) ''PriceRecurring)
$(deriveJSON (jsonOpts 1) ''Price)
$(deriveJSON (jsonOpts 2) ''Product)
$(deriveJSON (jsonOpts 2) ''SubscriptionItem)
$(deriveJSON (jsonOpts 1) ''Subscription)
$(deriveJSON (jsonOpts 2) ''CustomerPortal)

instance ToForm CustomerCreate where
  toForm = genericToForm (formOptions 2)

instance ToForm CustomerUpdate where
  toForm = genericToForm (formOptions 2)

instance ToForm CustomerPortalCreate where
  toForm = genericToForm (formOptions 3)

instance ToForm ProductCreate where
  toForm = genericToForm (formOptions 3)

instance ToForm PriceCreate where
  toForm pc =
    let recurringPiece =
          case pcRecurring pc of
            Nothing -> []
            Just x ->
              [ ("recurring[interval]", [prcInterval x]),
                ("recurring[interval_count]", maybeToList $ fmap toUrlPiece $ prcIntervalCount x)
              ]
     in Form $
          HM.fromList $
            [ ("currency", [pcCurrency pc]),
              ("product", [toUrlPiece $ pcProduct pc]),
              ("unit_amount", maybeToList $ fmap toUrlPiece $ pcUnitAmount pc),
              ("lookup_key", maybeToList $ pcLookupKey pc),
              ("transfer_lookup_key", [toUrlPiece $ pcTransferLookupKey pc])
            ]
              <> recurringPiece

instance ToForm SubscriptionCreate where
  toForm sc =
    let convertItem (idx, itm) =
          [ ("items[" <> toUrlPiece idx <> "][price]", [toUrlPiece $ sciPrice itm]),
            ("items[" <> toUrlPiece idx <> "][quantity]", maybeToList $ toUrlPiece <$> sciQuantity itm)
          ]
        lineItems =
          concatMap convertItem (zip ([0 ..] :: [Int]) (scItems sc))
     in Form $
          HM.fromList $
            [ ("customer", [toUrlPiece $ scCustomer sc]),
              ("cancel_at_period_end", maybeToList $ toUrlPiece <$> scCancelAtPeriodEnd sc),
              ("trial_end", maybeToList $ toUrlPiece <$> scTrialEnd sc)
            ]
              <> lineItems

instance ToForm CheckoutSessionCreate where
  toForm csc =
    let convertItem (idx, itm) =
          [ ("line_items[" <> toUrlPiece idx <> "][price]", [toUrlPiece $ cscliPrice itm]),
            ("line_items[" <> toUrlPiece idx <> "][quantity]", [toUrlPiece $ cscliQuantity itm])
          ]
        lineItems =
          concatMap convertItem (zip ([0 ..] :: [Int]) (cscLineItems csc))
        convertPmt (idx, pm) =
          ( "payment_method_types[" <> toUrlPiece idx <> "]",
            [pm]
          )
        pmt =
          map convertPmt (zip ([0 ..] :: [Int]) (cscPaymentMethodTypes csc))
     in Form $
          HM.fromList $
            [ ("cancel_url", [cscCancelUrl csc]),
              ("success_url", [cscSuccessUrl csc]),
              ("mode", [cscMode csc]),
              ("client_reference_id", maybeToList $ cscClientReferenceId csc),
              ("customer", maybeToList $ fmap toUrlPiece $ cscCustomer csc),
              ("allow_promotion_codes", maybeToList $ toUrlPiece <$> cscAllowPromotionCodes csc)
            ]
              <> lineItems
              <> pmt
