module MasterExplorer.Scraper.Data.Prerequisites
  ( Prerequisites (..)
  , parsePrerequisites
  ) where

import           Data.Text                                (Text)

import           MasterExplorer.Common.Data.Prerequisites (Prerequisites (..))

parsePrerequisites :: Text -> Either Text (Maybe Prerequisites)
parsePrerequisites = pure . pure . Prerequisites
