module MasterExplorer.Scraper.Data.UrlSpec where

import           Data.Text                       (pack)
import           Test.Hspec

import           MasterExplorer.Scraper.Data.Url (Url (..), parseUrls)

spec :: SpecWith ()
spec =
  describe "parseUrls" $ do
    it "parses an url" $ do
      input <- pack <$> readFile "markup/url.html"
      parseUrls input `shouldBe` Right [Url "http://www.isy.liu.se/en/edu/kurs/TSTE12/"]

    it "returns empty list on no urls" $
      parseUrls "" `shouldBe` Right []
