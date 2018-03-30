module CourseScalpel.Data.HoursSpec where

import           Data.Text                 (pack)
import           Test.Hspec

import           CourseScalpel.Data.Hours  (Hours (..), parseHours)
import           CourseScalpel.Web.Parsing (parseError)


-- parsing hours fails parsing tste12, tbmi26, tdts21
-- parsing prereqs fails for tsks15/


spec :: SpecWith ()
spec =
  describe "parseHours" $ do
    it "parses correct markup" $ do
      input <- pack <$> readFile "markup/hours.html"
      parseHours input `shouldBe` Right (Hours 900, Hours 800)

    it "fails on non parsable input" $ do
      let input = "not hours"
      parseHours input `shouldBe` parseError input "Hours"
