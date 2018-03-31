module MasterExplorer.Scraper.Data.InstitutionSpec where

import           Test.Hspec

import           MasterExplorer.Scraper.Data.Institution (Institution (..),
                                                          parseInstitution)
import           MasterExplorer.Scraper.Web.Parsing      (parseError)

spec :: SpecWith ()
spec = describe "parseInstitution" $ do
  it "parses valid values" $ do
    let input = [ "Matematiska institutionen"
                , "Institutionen för Systemteknik"
                , "Institutionen f\246r  Datavetenskap"
                , "Institutionen f\246r Datavetenskap"
                , "Institutionen f\246r Medicinsk teknik"
                , "Institutionen f\246r Fysik, kemi och biologi"
                , "Institutionen f\246r  Fysik, kemi och biologi "
                , "Institutionen f\246r Ekonomisk och industriell utveckling"
                , "Tekniska fakultetskansliet"
                ]

    let result = traverse parseInstitution input
    result `shouldBe` Right [MAI, ISY, IDA, IDA, MED, IFM, IFM, IEE, TekFak]


  it "parses white space padded values" $ do
    let result = parseInstitution "Institutionen f\246r Fysik, kemi och biologi "
    result `shouldBe` Right IFM

  it "fails to parse non-valid values" $ do
    let input = "not an institution"
    parseInstitution input `shouldBe` parseError input "Institution"
