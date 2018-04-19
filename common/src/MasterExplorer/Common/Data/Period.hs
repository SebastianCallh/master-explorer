{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Period
  ( Period (..)
  , allPeriods
  ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT      (ToADTArbitrary,
                                                     genericArbitrary)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data Period = One | Two
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToADTArbitrary)

instance Ord Period where
  compare One Two = LT
  compare One One = EQ
  compare Two One = GT
  compare Two Two = EQ

instance Arbitrary Period where
  arbitrary = genericArbitrary

instance Pretty Period where
  pretty One = "1"
  pretty Two = "2"

allPeriods :: [Period]
allPeriods = [One, Two]
