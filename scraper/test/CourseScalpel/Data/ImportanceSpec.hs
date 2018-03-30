module CourseScalpel.Data.ImportanceSpec where

import           Test.Hspec

import           CourseScalpel.Data.Importance (Importance (..),
                                                parseImportance)
import           CourseScalpel.Web.Parsing     (parseError)

spec :: SpecWith ()
spec = describe "parseImportance" $ do
  it "parses valid values" $ do
    let valids = ["V", "O", "F", "O/V"]
    let evofs  = traverse parseImportance valids
    evofs `shouldBe` Right [V, O, F, OV]

  it "fails to parse non-valid values" $ do
    let input = "not importance"
    parseImportance input `shouldBe` parseError input "Importance"

