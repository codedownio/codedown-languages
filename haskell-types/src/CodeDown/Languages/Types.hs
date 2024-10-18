{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module CodeDown.Languages.Types where

import CodeDown.Languages.Deriving
import Data.Aeson
import Data.Text
import Flat


data SchemaItem target typ =
  SchemaItem {
    schemaItemTitle :: Text
    , schemaItemDescription :: Maybe Text
    , schemaItemTarget :: target
    , schemaItemHidden :: Maybe Bool

    -- Type-dependent
    , schemaItemDefaultValue :: typ
    }
  -- | SchemaItemString {
  --     schemaItemTitle :: Text
  --     , schemaItemDescription :: Maybe Text
  --     , schemaItemDefaultValue :: Text
  --     , schemaItemTarget :: a
  --     , schemaItemHidden :: Maybe Bool
  --     }
  deriving (Show, Eq, Ord, Flat, Generic)
deriveJTH defaultOptions 'defaultOptions ''SchemaItem
