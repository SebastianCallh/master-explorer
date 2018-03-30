{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Slot
  ( Slot (..)
  ) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)

import           MasterExplorer.Common.Data.Block  (Block)
import           MasterExplorer.Common.Data.Period (Period)

data Slot = Slot
  { slotPeriod :: !Period
  , slotBlocks :: ![Block]
  } deriving (Show, Read, Generic, ToJSON, FromJSON)
