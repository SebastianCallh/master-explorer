module MasterExplorer.Scraper.Data.Hours
  ( Hours (..)
  , parseHours
  ) where

import           Control.Arrow                      ((***))
import           Control.Monad                      (join)
import           Data.Text                          (Text, strip)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           MasterExplorer.Common.Data.Hours   (Hours (..))
import           MasterExplorer.Scraper.Web.Parsing (Parser, parseError)

-- | Slightly different from the other parsers as it
--   parses both self study and scheduled time. They are in the
--   same section in the DOM so this is the most convenient way.
parseHours :: Text -> Either Text (Hours, Hours)
parseHours x = case parse parser "" (strip x) of
  Left  _err          -> parseError x "Hours"
  Right (sched, self) -> return $ join (***) (Hours . read) (sched, self)
  where
    parser :: Parser (String, String)
    parser = do
      scheduled <- string "Preliminär schemalagd tid: "    *> some digitChar <* string " h <br>"
      selfStudy <- string "Rekommenderad självstudietid: " *> some digitChar <* string " h"
      return (scheduled, selfStudy)

{-    chars = letterChar
            <|> char '\228'
            <|> char ':'
            <|> char '-'
            <|> spaceChar
-}
