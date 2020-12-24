{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Stripe.Resources where

import Stripe.Util.Aeson

import Servant.API
import Web.FormUrlEncoded
import qualified Data.Text as T
import GHC.Generics
import Text.Casing (quietSnake)

formOptions :: Int -> FormOptions
formOptions x =
  FormOptions
  { fieldLabelModifier = quietSnake . drop x }

newtype CustomerId
  = CustomerId { unCustomerId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Customer
  = Customer
  { cId :: CustomerId
  , cLivemode :: Bool
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

$(deriveJSON (jsonOpts 1) ''Customer)

instance ToForm CustomerCreate where
  toForm = genericToForm (formOptions 2)

instance ToForm CustomerUpdate where
  toForm = genericToForm (formOptions 2)
