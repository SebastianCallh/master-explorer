{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Semester where

import           Control.Lens
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT      (ToADTArbitrary,
                                                     genericArbitrary)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

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
  deriving (Show, Read, Ord, Eq, Generic, ToJSON, FromJSON, ToADTArbitrary)

instance Arbitrary Semester where
  arbitrary = genericArbitrary

instance Pretty Semester where
  pretty One   = "1Ht"
  pretty Two   = "2Vt"
  pretty Three = "3Ht"
  pretty Four  = "4Vt"
  pretty Five  = "5Ht"
  pretty Six   = "6Vt"
  pretty Seven = "7Ht"
  pretty Eight = "8Vt"
  pretty Nine  = "9Ht"
  pretty Ten   = "10Vt"

makePrisms ''Semester

masterSemesters :: [Semester]
masterSemesters = [Seven, Eight, Nine]

allSemesters :: [Semester]
allSemesters =
  [ One, Two, Three, Four, Five, Six
  , Seven, Eight, Nine, Ten]
