module CourseScalpel.Data.Specialization
  ( Specialization (..)
  , parseSpecialization
  ) where

import           Data.Text                                 (Text)

import           CourseScalpel.Web.Parsing                 (parseError)
import           MasterExplorer.Common.Data.Specialization (Specialization (..))

parseSpecialization :: Text -> Either Text Specialization
parseSpecialization "programmeringochalgoritmer"       = Right Algorithms
parseSpecialization "kommunikation"                    = Right Communication
parseSpecialization "datorsystem"                      = Right ComputerSystems
parseSpecialization "elektronik"                       = Right Electronics
parseSpecialization "spelprogrammering"                = Right Games
parseSpecialization "industriellekonomi"               = Right IndustrialEconomics
parseSpecialization "internationalsoftwareengineering" = Right International
parseSpecialization "aiochmaskininlärning"             = Right MachineLearning
parseSpecialization "medicinskinformatik"              = Right MedicinalInformatics
parseSpecialization "signal-ochbildbehandling"         = Right SignalProcessing
parseSpecialization "säkrasystem"                      = Right SafeSystems
parseSpecialization "storskaligmjukvaruutveckling"     = Right SoftwareEngineering
parseSpecialization "systemteknologi"                  = Right SystemsTechnology
parseSpecialization ""                                 = Right None
parseSpecialization x                                  = parseError x "Specialization"
