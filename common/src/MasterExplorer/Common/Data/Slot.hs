{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Slot
  ( Slot (..)
  ) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Ord                          (comparing)
import           GHC.Generics                      (Generic)

import           MasterExplorer.Common.Data.Block  (Block)
import           MasterExplorer.Common.Data.Period (Period)

data Slot = Slot
  { slotPeriod :: !Period
  , slotBlocks :: ![Block]
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Ord Slot where
  compare a b =
    case comparing slotPeriod a b of
      EQ -> comparing slotBlocks a b
      LT -> LT
      GT -> GT
