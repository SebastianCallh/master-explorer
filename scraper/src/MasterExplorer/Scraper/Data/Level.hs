module MasterExplorer.Scraper.Data.Level
  ( Level (..)
  , parseLevel
  ) where

import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Level   (Level (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

parseLevel :: Text -> Either Text Level
parseLevel "G1X" = Right G1
parseLevel "G2X" = Right G2
parseLevel "A"   = Right A
parseLevel "A1X" = Right A1
parseLevel "A2X" = Right A2
parseLevel  x    = parseError x "Level"

