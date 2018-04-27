module MasterExplorer.Scraper.Data.BlockSpec where

import           Test.Hspec

import           MasterExplorer.Scraper.Data.Block  (Block (..), parseBlocks)
import           MasterExplorer.Scraper.Web.Parsing (parseError)

spec :: SpecWith ()
spec = describe "parseBlock" $ do
  it "parses single blocks correctly" $ do
    let eblocks = parseBlocks <$> ["1", "2", "3", "4", "-"]
    eblocks `shouldBe` (Right <$> [[One], [Two], [Three], [Four], [None]])

  it "parses multiple blocks correctly" $ do
    let eblocks = parseBlocks "3/4"
    eblocks `shouldBe` Right [Three, Four]

  it "fails to parse invalid block" $ do
    let input = "not a block"
    parseBlocks input `shouldBe` parseError input "Block"
