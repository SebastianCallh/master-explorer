{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Importance
  ( Importance(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Importance = V | O | F | OV
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

