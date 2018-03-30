module CourseScalpel.Data.Importance
  ( Importance (..)
  , parseImportance
  ) where

import           Data.Text                             (Text)

import           CourseScalpel.Web.Parsing             (parseError)
import           MasterExplorer.Common.Data.Importance (Importance (..))

parseImportance :: Text -> Either Text Importance
parseImportance "V"   = Right V
parseImportance "O"   = Right O
parseImportance "F"   = Right F
parseImportance "O/V" = Right OV
parseImportance  x    = parseError x "Importance"
