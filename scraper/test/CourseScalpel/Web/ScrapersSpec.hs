module CourseScalpel.Web.ScrapersSpec where


import           Data.Maybe                 (isJust)
import           Data.Text                  (pack)
import           Test.Hspec
import           Test.QuickCheck            (arbitrary, generate, property)
import           Text.HTML.Scalpel          (scrapeStringLike)

import           CourseScalpel.Data.Program (engD)
import           CourseScalpel.Web.Scrapers (listCoursesScraper,
                                             pageCourseScraper,
                                             programplanScraper,
                                             semesterScraper)

spec :: SpecWith ()
spec = describe "scrapers" $ do

  -- Course page

  it "scrapes tata65 correctly" $ property $ do
    markup <- pack <$> readFile "markup/tata65.html"
    let mcourse = scrapeStringLike markup pageCourseScraper
    isJust mcourse `shouldBe` True

  -- Program

  it "scrapes program 6cddd correctly" $ do
    markup <- pack <$> readFile "markup/program-6cddd.html"
    let mcourses = scrapeStringLike markup $ programplanScraper engD
    (length <$> mcourses) `shouldBe` Just 579

  --Courses listen on program page

  it "scrapes tr:s into ListCourse:s" $ property $ do
    markup  <- pack <$> readFile "markup/list-courses.html"
    scraper <- generate $
               listCoursesScraper <$>
               arbitrary <*>
               arbitrary <*>
               arbitrary <*>
               arbitrary

    let mcourses = scrapeStringLike markup scraper
    case mcourses of
      Nothing      -> failTest
      Just courses -> length courses `shouldBe` 3

  it "scrapes a program plan into a list of ListCourses" $ property $ do
    markup  <- pack <$> readFile "markup/semester.html"
    scraper <- generate $ semesterScraper <$> arbitrary
    let mcourses = scrapeStringLike markup scraper
    case mcourses of
      Nothing      -> failTest
      Just courses -> length courses `shouldBe` 336

failTest :: Expectation
failTest = expectationFailure "Could not parse courses"
