{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.ExaminationType
  ( ExaminationType (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data ExaminationType
  = TEN
  | LAB
  | UPG
  | AUSK
  | OPPO
  | PROJ
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
