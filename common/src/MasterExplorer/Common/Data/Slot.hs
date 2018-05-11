{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Slot
  ( Slot (..)
  , masterSlots
  , slotsInPeriod
  ) where

import           Data.Semigroup                      ((<>))
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Ord                            (comparing)
import           GHC.Generics                        (Generic)

import           MasterExplorer.Common.Class.Pretty  (Pretty, pretty)
import           MasterExplorer.Common.Data.Block    (Block, allBlocks)
import           MasterExplorer.Common.Data.Period   (Period, allPeriods)
import           MasterExplorer.Common.Data.Semester (Semester, masterSemesters)

data Slot = Slot
  { slotSemester :: !Semester
  , slotPeriod   :: !Period
  , slotBlocks   :: !Block
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Ord Slot where
  compare a b =
    case comparing slotSemester a b of
      LT -> LT
      GT -> GT
      EQ -> case comparing slotPeriod a b of
        LT -> LT
        GT -> GT
        EQ -> comparing slotBlocks a b

instance Pretty Slot where
  pretty Slot{..} =
    pretty slotSemester <>
    pretty slotPeriod

masterSlots :: [Slot]
masterSlots = Slot <$>
  masterSemesters  <*>
  allPeriods       <*>
  allBlocks

slotsInPeriod :: Semester -> Period -> [Slot]
slotsInPeriod semester period =
  Slot semester period <$> allBlocks
  
