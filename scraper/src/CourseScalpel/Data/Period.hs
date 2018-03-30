module CourseScalpel.Data.Period
  ( Period (..)
  , parsePeriod
  ) where

import           Data.Text                         (Text)

import           CourseScalpel.Web.Parsing         (parseError)
import           MasterExplorer.Common.Data.Period (Period (..))

-- | Period 0 is for courses during nolle-p
parsePeriod :: Text -> Either Text Period
parsePeriod "Period 0" = Right One
parsePeriod "Period 1" = Right One
parsePeriod "Period 2" = Right Two
parsePeriod x          = parseError x "Period"
