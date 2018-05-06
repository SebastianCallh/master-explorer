module MasterExplorer.Scraper.Data.Field
  ( Field (..)
  , parseFields
  ) where

import qualified Data.Text                          as T

import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Field   (Field (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

parseFields :: Text -> Either Text [Field]
parseFields = traverse parseField . T.splitOn ","

parseField :: Text -> Either Text Field
parseField "Humanistiska omr\229det"             = Right Humanities
parseField "Medicinska omr\229det"               = Right Medicine
parseField "Tekniska omr\229det"                 = Right Technical
parseField "Naturvetenskapliga omr\229det"       = Right Science
parseField "Samh\228llsvetenskapliga omr\229det" = Right Society
parseField "Juridiska området"                   = Right Law
parseField x                                     = parseError x "Field"
