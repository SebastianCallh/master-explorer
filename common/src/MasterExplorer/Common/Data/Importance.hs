{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Importance
  ( Importance(..)
  ) where

import           Control.Lens
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Importance = V | O | F | OV
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

makePrisms ''Importance
