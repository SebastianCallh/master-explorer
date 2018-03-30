{-# LANGUAGE OverloadedStrings #-}

module CourseScalpel.Web.MarkupParser
  ( examText
  , parseSections
  , parse
  , semester
  , el
  , hours
  , h3text
  , h3
  , ul
  , li
  , table
  , tr
  , td
  , rntPadded
  ) where

import qualified Data.Map             as Map
import qualified Data.Text            as T
import qualified Text.Megaparsec      as MP (parse)

import           Data.Either          (rights)
import           Data.Map             (Map)
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import           Text.Megaparsec      hiding (parse)
import           Text.Megaparsec.Char

type Parser = Parsec (ErrorFancy Text) Text

parseSections :: [Text] -> Map Text Text
parseSections input = result
  where result  = Map.fromList <$> rights $ zipWith ($) parsers input
        parsers = repeat $ MP.parse h3text ""

parse :: Parser a -> Text -> Either Text a
parse parser input =
  case MP.parse parser "" input of
    Left err    -> Left . T.pack $ show err
    Right thing -> Right thing

-- Currently ignores all text except for the
-- table of examination parts
examText :: Parser ([[Text]], Text)
examText = do
  tbef  <- text
  t     <- table
  taft  <- text
  return (t, mconcat [tbef, T.pack " " , taft])

h3text :: Parser (Text, Text)
h3text = do
  _ <- crlf
  space
  section <- h3
  _ <- crlf
  space
  content <- T.pack <$> anyChar `manyTill` crlf
  return (section, content)

semester :: Parser Text
semester = T.pack <$> manyTill (alphaNumChar <|> spaceChar) (string " (")

-- TATA40 is -5 hours self study time
hours :: Parser Int
hours = many (string "-") *>
  (read <$> (many digitChar <* space <* char 'h'))

h3 :: Parser Text
h3 = T.pack <$> between open close p
  where open  = string "<h3 class=\"title\">"
        close = string "</h3>"
        p     = many someChar

text :: Parser Text
text = T.pack <$> many someChar

rntPadded :: Parser Text
rntPadded = T.pack <$> between (many rntChar) (many rntChar) (many $ noneOf ['\r', '\n', '\t'])

-- List

ul :: Parser [Text]
ul = mkup "ul" $ many li

li :: Parser Text
li = T.pack <$> mkup "li" (many someChar)

-- Table

table :: Parser [[Text]]
table = mkup "table" $ many tr

tr :: Parser [Text]
tr = mkup "tr" $ many td

td :: Parser Text
td = T.pack <$> mkup "td" (many someChar)

-- Parser builders

el :: String -> String -> Parser String
el open close = between (mkOpen open) (mkClose close) (many someChar)

mkup :: String -> Parser a -> Parser a
mkup e = between (mkOpen e) (mkClose e)

mkOpen :: String -> Parser Text
mkOpen e = do
  string . T.pack $ "<" <> e
  _ <- many someChar
  string . T.pack $ ">"

mkClose :: String -> Parser Text
mkClose e = do
  string . T.pack $ "</" <> e
  _ <- many someChar
  string . T.pack $ ">"

-- Char types

someChar :: Parser Char
someChar = letterChar
       <|> spaceChar
       <|> punctuationChar
       <|> digitChar
       <|> char '='

rntChar :: Parser Char
rntChar = char '\r'
      <|> char '\n'
      <|> char '\t'
