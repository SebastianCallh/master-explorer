module CourseScalpel.Data.Credits
  ( Credits (..)
  , parseCredits
  ) where

import qualified Data.Text                          as T

import           Data.Text                          (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           CourseScalpel.Web.Parsing          (Parser, parseError)
import           MasterExplorer.Common.Data.Credits (Credits (..))

parseCredits :: Text -> Either Text Credits
parseCredits x = either errorOut mkCredit $ parse parser "" $ T.strip x
  where
    errorOut = const $ parseError x "Credits"
    mkCredit = pure . Credits . read
    parser :: Parser String
    parser = some floatChar <* optional (space *> string "hp")
    floatChar = digitChar <|> char '.'
