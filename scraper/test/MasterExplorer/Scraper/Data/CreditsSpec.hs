module MasterExplorer.Scraper.Data.CreditsSpec where

import           Test.Hspec

import           MasterExplorer.Scraper.Data.Credits (Credits (..),
                                                      parseCredits)
import           MasterExplorer.Scraper.Web.Parsing  (parseError)

spec :: SpecWith ()
spec = describe "parseCredits" $ do
  it "parses integer will trailing hp" $
    parseCredits "5 hp" `shouldBe` (Right $ Credits 5)

  it "parses decimal with trailing hp" $
    parseCredits "1.5 hp" `shouldBe` (Right $ Credits 1.5)

  it "fails to parse non-numeric value" $ do
    let input = "not credit"
    parseCredits input `shouldBe` parseError input "Credits"
