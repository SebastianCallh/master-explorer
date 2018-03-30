module CourseScalpel.Data.CreditsSpec where

import           Test.Hspec

import           CourseScalpel.Data.Credits (Credits (..), parseCredits)
import           CourseScalpel.Web.Parsing  (parseError)

spec :: SpecWith ()
spec = describe "parseCredits" $ do
  it "parses integer will trailing hp" $
    parseCredits "5 hp" `shouldBe` (Right $ Credits 5)

  it "parses decimal with trailing hp" $
    parseCredits "1.5 hp" `shouldBe` (Right $ Credits 1.5)

  it "fails to parse non-numeric value" $ do
    let input = "not credit"
    parseCredits input `shouldBe` parseError input "Credits"
