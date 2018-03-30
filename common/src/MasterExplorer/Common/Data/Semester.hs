{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Semester
  ( Semester (..)
  ) where

import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

data Semester
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  deriving (Show, Generic, ToADTArbitrary)

instance Arbitrary Semester where
  arbitrary = genericArbitrary
