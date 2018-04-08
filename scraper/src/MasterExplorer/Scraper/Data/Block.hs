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
    where digits = T.unpack $ T.filter isDigit x

parseBlock :: Text -> Either Text Block
parseBlock "1" = Right MasterExplorer.Common.Data.Block.One
parseBlock "2" = Right MasterExplorer.Common.Data.Block.Two
parseBlock "3" = Right MasterExplorer.Common.Data.Block.Three
parseBlock "4" = Right MasterExplorer.Common.Data.Block.Four
parseBlock "-" = Right MasterExplorer.Common.Data.Block.None
parseBlock  x  = parseError x "Block"
