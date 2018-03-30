{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Block
  ( Block (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Block
  = One
  | Two
  | Three
  | Four
  | None
  deriving (Show, Read, Generic, ToJSON, FromJSON)
