module MasterExplorer.Scraper.Data.Block
  ( Block (..)
  , parseBlocks
  ) where

import qualified Data.Text                          as T

import           Data.Char                          (isDigit)
import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Block   (Block (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

parseBlocks :: Text -> Either Text [Block]
parseBlocks x = traverse (parseBlock . T.singleton) digits
    where digits = T.unpack $ T.filter (\c -> isDigit c || c == '-') x

parseBlock :: Text -> Either Text Block
parseBlock "1" = Right One
parseBlock "2" = Right Two
parseBlock "3" = Right Three
parseBlock "4" = Right Four
parseBlock "-" = Right None
parseBlock  x  = parseError x "Block"
