module CourseScalpel.Data.ProgramSpec where

import qualified Data.Text                  as T

import           Test.Hspec

import           CourseScalpel.Data.Program (engD, engDPU, engED, engEMM, engI,
                                             engIInt, engIT, engKB, engKTS,
                                             engM, engMT, engMed, engTB, engU,
                                             engY, engYInt, parsePrograms)
import           CourseScalpel.Web.Parsing  (parseError)


spec :: SpecWith ()
spec = describe "parsePrograms" $ do
  it "parses correct engineering programs" $ do
    input <- T.pack <$> readFile "markup/programs.html"
    parsePrograms input `shouldBe` Right [ engD
                                         , engU
                                         , engIT
                                         , engI
                                         , engIInt
                                         , engTB
                                         , engKB
                                         , engY
                                         , engYInt
                                         , engMed
                                         , engM
                                         , engEMM
                                         , engDPU
                                         , engKTS
                                         , engMT
                                         , engED
                                         ]

  it "fails on invalid input" $ do
    let input = "not programs"
    parsePrograms input `shouldBe` parseError input "Programs"

