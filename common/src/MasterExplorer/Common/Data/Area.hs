{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Area
  ( Area(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Area
  = AppliedMaths
  | ComputerScience
  | ComputerEngineering
  | Electrotechnic
  | Engineering
  | EnergyEnvironment
  | Informatics
  | IndustrialEconomics
  | Maths
  | MedicinalEngineering
  | MediaEngineering
  | Law
  | Physics
  | ProductDevelopment
  | Programming
  | Science
  | Technical
  | TechnicalPhysics
  | Other
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
