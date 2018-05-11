{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Specialization  where

import           Control.Lens
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary,
                                                genericArbitrary)

data Specialization
  = Algorithms
  | Communication
  | ComputerSystems
  | Electronics
  | Games
  | IndustrialEconomics
  | International
  | MachineLearning
  | MedicinalInformatics
  | None
  | SafeSystems
  | SignalProcessing
  | SoftwareEngineering
  | SystemsTechnology
  | SystemOnChip
  deriving (Show, Eq, Generic, ToADTArbitrary)

instance Arbitrary Specialization where
  arbitrary = genericArbitrary

makePrisms ''Specialization
