{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Period
  ( Period (..)
  ) where

import           Data.Aeson                    (FromJSON, ToJSON)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

data Period = One | Two
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToADTArbitrary)

instance Arbitrary Period where
  arbitrary = genericArbitrary
