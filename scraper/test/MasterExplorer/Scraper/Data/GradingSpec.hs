module MasterExplorer.Scraper.Data.GradingSpec where

import           Test.Hspec

import           MasterExplorer.Scraper.Data.Grading (Grading (..),
                                                      parseGrading)
import           MasterExplorer.Scraper.Web.Parsing  (parseError)

spec :: SpecWith ()
spec = describe "parseGrading" $ do
  it "parses valid values" $ do
    let input    = ["U, G", "U,G", "U, 3, 4, 5", "U,3,4,5", "Deltagit (D)", "D", ""]
    let result   = traverse parseGrading input
    result `shouldBe` Right [Binary, Binary, Scale, Scale, Presence, Presence, Unspecified]

  it "fails to parse non-valid values" $ do
    let input = "not a gradescale"
    parseGrading input `shouldBe` parseError input "Grading"

