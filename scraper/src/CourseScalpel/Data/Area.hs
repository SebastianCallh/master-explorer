module CourseScalpel.Data.Area
  ( Area (..)
  , parseAreas
  ) where

import qualified Data.Text                       as T

import           Data.Text                       (Text)

import           CourseScalpel.Web.Parsing       (parseError)
import           MasterExplorer.Common.Data.Area (Area (..))

parseAreas :: Text -> Either Text [Area]
parseAreas = traverse parseArea . T.splitOn ","

parseArea :: Text -> Either Text Area
parseArea "Till\228mpad matematik"        = Right AppliedMaths
parseArea "Datavetenskap"                 = Right ComputerScience
parseArea "Datateknik"                    = Right ComputerEngineering
parseArea "Elektroteknik"                 = Right Electrotechnic
parseArea "Energi- och milj\246teknik"    = Right EnergyEnvironment
parseArea "Maskinteknik"                  = Right Engineering
parseArea "Informationsteknologi"         = Right Informatics
parseArea "Industriell ekonomi"           = Right IndustrialEconomics
parseArea "Matematik"                     = Right Maths
parseArea "Medicinsk teknik"              = Right MedicinalEngineering
parseArea "Fysik"                         = Right Physics
parseArea "Produktutveckling"             = Right ProductDevelopment
parseArea "Programmering"                 = Right Programming
parseArea "Naturvetenskapliga omr\229det" = Right Science
parseArea "Teknik"                        = Right Technical
parseArea "Teknisk fysik"                 = Right TechnicalPhysics
parseArea "Medieteknik "                  = Right MediaEngineering
parseArea "\214vriga \228mnen"            = Right Other
parseArea x                               = parseError x "Area"
