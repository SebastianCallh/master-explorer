module MasterExplorer.Scraper.Data.CourseContent
  ( CourseContent (..)
  , parseContent
  ) where

import           Data.Text                                (Text)
import           MasterExplorer.Common.Data.CourseContent (CourseContent (..))

parseContent :: Text -> Either Text CourseContent
parseContent =  pure . CourseContent

