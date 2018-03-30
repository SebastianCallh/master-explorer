module CourseScalpel.Data.ExaminationSpec where

import qualified Data.Text                          as T

import           Test.Hspec

import           CourseScalpel.Data.Examination     (Examination (..),
                                                     parseExaminations)

import           CourseScalpel.Data.Credits         (Credits (..))
import           CourseScalpel.Data.ExaminationType (ExaminationType (..))
import           CourseScalpel.Data.Grading         (Grading (..))
import           CourseScalpel.Web.Parsing          (parseError)

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
                              ]

    it "failes to parse" $ do
      let input = "not an examination"
      parseExaminations input `shouldBe` parseError input "Examinations"
