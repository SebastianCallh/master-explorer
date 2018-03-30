module CourseScalpel.Data.AreaSpec where


import qualified Data.Text                 as T

import           Test.Hspec

import           CourseScalpel.Data.Area   (Area (..), parseAreas)
import           CourseScalpel.Web.Parsing (parseError)

spec :: SpecWith ()
spec =
  describe "parseArea" $ do
  it "parses valid values" $ do
    let input = T.intercalate ","
          [ "Till\228mpad matematik"
          , "Datavetenskap"
          , "Datateknik"
          , "Elektroteknik"
          , "Informationsteknologi"
          , "Matematik"
          , "Medicinsk teknik"
          , "Fysik"
          , "Naturvetenskapliga omr\229det"
          , "Teknik"
          , "Teknisk fysik"
          , "\214vriga \228mnen"
          ]

    let eareas = parseAreas input
    eareas `shouldBe` Right [ AppliedMaths
                            , ComputerScience
                            , ComputerEngineering
                            , Electrotechnic
                            , Informatics
                            , Maths
                            , MedicinalEngineering
                            , Physics
                            , Science
                            , Technical
                            , TechnicalPhysics
                            , Other
                            ]

  it "fails to parse non-valid values" $ do
    let input = "not an area"
    parseAreas input `shouldBe` parseError input "Area"

