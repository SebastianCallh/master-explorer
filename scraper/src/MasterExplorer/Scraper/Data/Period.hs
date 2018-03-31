module MasterExplorer.Scraper.Data.Period
  ( Period (..)
  , parsePeriod
  ) where

import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Period  (Period (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

-- | Period 0 is for courses during nolle-p
parsePeriod :: Text -> Either Text Period
parsePeriod "Period 0" = Right One
parsePeriod "Period 1" = Right One
parsePeriod "Period 2" = Right Two
parsePeriod x          = parseError x "Period"
