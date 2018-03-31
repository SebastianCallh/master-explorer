module MasterExplorer.Scraper.Data.Grading
  ( Grading (..)
  , parseGrading
  ) where

import qualified Data.Text                          as T

import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Grading (Grading (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

parseGrading :: Text -> Either Text Grading
parseGrading "U, 3, 4, 5"   = Right Scale
parseGrading "U,3,4,5"      = Right Scale
parseGrading "U, G"         = Right Binary
parseGrading "U,G"          = Right Binary
parseGrading "Deltagit (D)" = Right Presence
parseGrading "D"            = Right Presence
parseGrading ""             = Right Unspecified
parseGrading x              = parseError x "Grading"

