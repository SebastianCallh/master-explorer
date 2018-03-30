module CourseScalpel.Web.Parsing
  ( Parser
  , parseError
  ) where

import           Data.Text       (Text)
import           Text.Megaparsec (ErrorFancy, Parsec)

type Parser = Parsec (ErrorFancy Text) Text

parseError :: Text -> Text -> Either Text a
parseError x t = Left $ mconcat ["Could not parse ", x, " as ", t]


