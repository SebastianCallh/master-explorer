{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Area where

import           Control.Lens
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

makePrisms ''Area
