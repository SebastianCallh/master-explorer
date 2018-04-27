{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Level
  ( Level (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


data Level
  = G1
  | G2
  | A
  | A1
  | A2
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
