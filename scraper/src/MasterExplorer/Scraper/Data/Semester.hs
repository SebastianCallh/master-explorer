module MasterExplorer.Scraper.Data.Semester
  ( Semester (..)
  , parseSemester
  ) where

import           Data.Text                           (Text)

import           MasterExplorer.Common.Data.Semester (Semester (..))
import           MasterExplorer.Scraper.Web.Parsing  (parseError)

parseSemester :: Text -> Either Text Semester
parseSemester "Termin 1"  = Right One
parseSemester "Termin 2"  = Right Two
parseSemester "Termin 3"  = Right Three
parseSemester "Termin 4"  = Right Four
parseSemester "Termin 5"  = Right Five
parseSemester "Termin 6"  = Right Six
parseSemester "Termin 7"  = Right Seven
parseSemester "Termin 8"  = Right Eight
parseSemester "Termin 9"  = Right Nine
parseSemester "Termin 10" = Right Ten
parseSemester x           = parseError x "Semester"
