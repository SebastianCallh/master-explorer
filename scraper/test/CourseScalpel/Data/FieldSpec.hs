module CourseScalpel.Data.FieldSpec where


import           Test.Hspec

import           CourseScalpel.Data.Field  (Field (..), parseField)
import           CourseScalpel.Web.Parsing (parseError)


spec :: SpecWith ()
spec = describe "parseField" $ do
  it "parses expected input" $ do
    let input = [ "Humanistiska omr\229det"
                , "Medicinska omr\229det"
                , "Tekniska omr\229det"
                ,  "Naturvetenskapliga omr\229det"
                , "Samh\228llsvetenskapliga omr\229det"
                ]
    let result = traverse parseField input
    result `shouldBe` Right [Humanities, Medicine, Technical, Science, Society]

  it "fails to parse illegal values" $ do
    let input = "not a field"
    parseField input `shouldBe` parseError input "Field"
