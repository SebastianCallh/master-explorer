module CourseScalpel.Data.UrlSpec where

import           Data.Text              (pack)
import           Test.Hspec

import           CourseScalpel.Data.Url (Url (..), parseUrls)

spec :: SpecWith ()
spec =
  describe "parseUrls" $ do
    it "parses an url" $ do
      input <- pack <$> readFile "markup/url.html"
      parseUrls input `shouldBe` (Right $ [Url "http://www.isy.liu.se/en/edu/kurs/TSTE12/"])

