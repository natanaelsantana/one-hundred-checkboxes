{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Api.Model where

import Data.Aeson
import GHC.Generics

data Checkbox = Checkbox {
    checkboxId :: Int,
    checked :: Bool
} deriving (Show, Generic)

instance FromJSON Checkbox where 
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \s -> 
        if s == "checkboxId" then "id" else s }

instance ToJSON Checkbox where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = \s -> 
        if s == "checkboxId" then "id" else s } 

data CheckboxUpdate = CheckboxUpdate {
    checkedValue :: Bool
} deriving (Show, Generic)

instance FromJSON CheckboxUpdate where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \s -> 
        if s == "checkedValue" then "checked" else s }

data CheckboxResponse = CheckboxResponse {
    checkboxes :: [Checkbox]
} deriving (Show, Generic)

instance ToJSON CheckboxResponse where
    toEncoding = genericToEncoding defaultOptions