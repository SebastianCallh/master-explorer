module CourseScalpel.Data.Field
  ( Field (..)
  , parseField
  ) where

import           Data.Text                        (Text)

import           CourseScalpel.Web.Parsing        (parseError)
import           MasterExplorer.Common.Data.Field (Field (..))

parseField :: Text -> Either Text Field
parseField "Humanistiska omr\229det"             = Right Humanities
parseField "Medicinska omr\229det"               = Right Medicine
parseField "Tekniska omr\229det"                 = Right Technical
parseField "Naturvetenskapliga omr\229det"       = Right Science
parseField "Samh\228llsvetenskapliga omr\229det" = Right Society
parseField x                                     = parseError x "Field"
