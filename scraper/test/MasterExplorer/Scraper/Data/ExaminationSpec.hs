module MasterExplorer.Scraper.Data.ExaminationSpec where

import qualified Data.Text                                   as T

import           Test.Hspec

import           MasterExplorer.Scraper.Data.Examination     (Examination (..),
                                                              parseExaminations)

import           MasterExplorer.Scraper.Data.Credits         (Credits (..))
import           MasterExplorer.Scraper.Data.ExaminationType (ExaminationType (..))
import           MasterExplorer.Scraper.Data.Grading         (Grading (..))
import           MasterExplorer.Scraper.Web.Parsing          (parseError)

spec :: SpecWith ()
spec =
  describe "parseExaminations" $ do
    it "parses expected markup" $ do
      input <- T.pack <$> readFile "markup/examination.html"
      let eexams = parseExaminations input
      eexams `shouldBe` Right [ Examination "TEN1" TEN  "Skriftlig tentamen" Scale    (Credits 4)
                              , Examination "UPG1" UPG  "Uppgifter"          Binary   (Credits 28)
                              , Examination "OPPO" OPPO "Opponering"         Binary   (Credits 2)
                              , Examination "AUSK" AUSK "Auskonsultation"    Presence (Credits 2)
                              , Examination "PRA1" PROJ "Project"            Presence (Credits 1.5)
                              , Examination "DAT1" DAT  "Datortentamen"      Binary   (Credits 5)
                              , Examination "HEM2" HEM  "Hemtentamen"        Scale    (Credits 6)
                              ]

    it "failes to parse" $ do
      let input = "not an examination"
      parseExaminations input `shouldBe` parseError input "Examinations"

    it "parses expected markup" $ do
      input <- T.pack <$> readFile "markup/examination-tams22.html"
      let eexams = parseExaminations input
      let expected1 = Examination "TEN1" TEN "En skriftlig tentamen (U,3,4,5)" Scale (Credits  5)
      let expected2 = Examination "LAB1" LAB "Obligatoriska inlämningsuppgifter (U,3,4,5)" Scale (Credits 1)
      eexams `shouldBe` Right [expected1, expected2]
