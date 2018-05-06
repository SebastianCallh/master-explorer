module MasterExplorer.Scraper.Data.Examinator
  ( Examinator (..)
  , parseExaminator
  ) where

import           Data.Text                             (Text)

import           MasterExplorer.Common.Data.Examinator (Examinator (..))

parseExaminator :: Text -> Either Text (Maybe Examinator)
parseExaminator = pure . pure . Examinator

