module MasterExplorer.Scraper.Data.Block
  ( Block (..)
  , Blocks (..)
  , parseBlocks
  ) where

import qualified Data.Text                          as T

import           Data.Char                          (isDigit)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Block   (Block (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

newtype Blocks = Blocks { getBlocks :: [Block] }
  deriving (Show)

instance Monoid Blocks where
  mappend x y = Blocks $ getBlocks x <> getBlocks y
  mempty = Blocks []

parseBlocks :: Text -> Either Text Blocks
parseBlocks x = Blocks <$> traverse (parseBlock . T.singleton) digits
    where digits = T.unpack $ T.filter isDigit x

parseBlock :: Text -> Either Text Block
parseBlock "1" = Right MasterExplorer.Common.Data.Block.One
parseBlock "2" = Right MasterExplorer.Common.Data.Block.Two
parseBlock "3" = Right MasterExplorer.Common.Data.Block.Three
parseBlock "4" = Right MasterExplorer.Common.Data.Block.Four
parseBlock "-" = Right MasterExplorer.Common.Data.Block.None
parseBlock  x  = parseError x "Block"
