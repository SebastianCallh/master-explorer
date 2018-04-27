module MasterExplorer.Scraper.Web.ScrapersSpec where


import           Data.Either                                (lefts, rights)
import           Data.List                                  (nub, sort, (\\))
import           Data.Maybe                                 (isJust)
import           Data.Text                                  (Text, pack)
import           Test.Hspec
import           Test.QuickCheck                            (arbitrary,
                                                             generate, property)
import           Text.HTML.Scalpel

import qualified MasterExplorer.Scraper.Data.Block          as Block
import           MasterExplorer.Scraper.Data.CourseCode     (CourseCode (..),
                                                             getCode)
import           MasterExplorer.Scraper.Data.CourseName     (CourseName (..))
import           MasterExplorer.Scraper.Data.Credits        (Credits (..))
import           MasterExplorer.Scraper.Data.Importance     (Importance (V))
import           MasterExplorer.Scraper.Data.Level          (Level (G1))
import           MasterExplorer.Scraper.Data.ListCourse     (ListCourse (..))
import           MasterExplorer.Scraper.Data.Occasion       (Occasion (..))
import           MasterExplorer.Scraper.Data.Period         (Period (One, Two))
import           MasterExplorer.Scraper.Data.Program        (engD)
import           MasterExplorer.Scraper.Data.Semester       (Semester (Five, Seven))
import           MasterExplorer.Scraper.Data.Slot           (Slot (..))
import           MasterExplorer.Scraper.Data.Specialization (Specialization (None))
import           MasterExplorer.Scraper.Data.Url            (Url (..))
import           MasterExplorer.Scraper.Web.Scrapers        (coursesScraper,
                                                             pageCourseScraper,
                                                             programplanScraper,
                                                             semesterScraper)

spec :: SpecWith ()
spec = describe "Scrapers" $ do

  -- Course page

  it "scrapes tata65 correctly" $ property $ do
    markup <- pack <$> readFile "markup/tata65.html"
    let mcourse = scrapeStringLike markup pageCourseScraper
    isJust mcourse `shouldBe` True

  -- Program

  it "scrapes program 6cddd correctly" $ do
    markup <- pack <$> readFile "markup/program-6cddd.html"
    let meCourses = scrapeStringLike markup $ programplanScraper engD
    let mUniqueCodes = fmap nub $ scrapeStringLike markup $
          attrs "data-course-code" ("tr" @: [hasClass "main-row"])

    let fails = do
          courses   <- rights <$> meCourses
          let codes = getCode . lCourseCode <$> courses
          (codes \\) <$> mUniqueCodes

    filter (not . isKnownError) . lefts  <$> meCourses `shouldBe` pure []
    length <$> meCourses `shouldBe` length <$> mUniqueCodes

  it "scrapes tr:s into ListCourse:s" $ property $ do
    markup  <- pack <$> readFile "markup/list-courses.html"
    scraper <- generate $
               coursesScraper <$>
               arbitrary <*>
               arbitrary <*>
               arbitrary <*>
               arbitrary

    let mcourses = scrapeStringLike markup scraper
    case mcourses of
      Nothing      -> failTest
      Just courses -> length courses `shouldBe` 3

  it "scrapes a semester into a list of ListCourses" $ property $ do
    markup  <- pack <$> readFile "markup/semester.html"
    scraper <- generate $ semesterScraper <$> arbitrary
    let meCourses = scrapeStringLike markup scraper
    let mUniqueCodes = fmap nub $ scrapeStringLike markup $
          attrs "data-course-code" ("tr" @: [hasClass "main-row"])

    let fails = do
          courses   <- rights <$> meCourses
          let codes = getCode . lCourseCode <$> courses
          (codes \\) <$> mUniqueCodes

    filter (not . isKnownError) . lefts  <$> meCourses `shouldBe` pure []
    length <$> meCourses `shouldBe` length <$> mUniqueCodes

  it "scrapes a ListCourse running over two periods" $ property $ do
    markup  <- pack <$> readFile "markup/list-course-semigroup.html"
    scraper <- generate $ semesterScraper <$> arbitrary

    let mcourses = scrapeStringLike markup scraper
    case mcourses of
      Nothing      -> failTest
      Just courses -> do
        let course = head courses
        let expectedOccasions = [Occasion [Slot Seven One Block.Two, Slot Seven Two Block.Three]]
        lCourseOccasions <$> course `shouldBe` pure expectedOccasions

  it "scrapes course that has occasions in different semesters" $ property $ do
    markup  <- pack <$> readFile "markup/list-course-different-semesters.html"
    let scraper = programplanScraper engD

    let mCourses = scrapeStringLike markup scraper
    mCourses `shouldBe` pure [pure
      ListCourse
      { lCourseProgram   = engD
      , lCourseSpecs     = [None]
      , lCourseCode      = CourseCode "TFMT13"
      , lCourseName      = CourseName "Mätteknik"
      , lCourseCredits   = Credits 4
      , lCourseLevel     = G1
      , lCourseOccasions = [ Occasion [Slot Five  One Block.Two]
                           , Occasion [Slot Seven One Block.One]
                           ]
      , lCourseUrl        = Url "https://liu.se/studieinfo/kurs/tfmt13"
      , lCourseImportance = V
      }]

-- lCourseOccasions =
-- [Occasion {getOccasion = [Slot {slotSemester = Five, slotPeriod = One, slotBlocks = Two}]}]}

failTest :: Expectation
failTest = expectationFailure "Could not parse courses"


courseCodesWithoutUrl :: [Text]
courseCodesWithoutUrl = sort ["TDDD30XX","TDDE0001","TDDB84XX"]

-- | These errors appear because liu has no urls for the
--   specified courses.
isKnownError :: Text -> Bool
isKnownError err =
  err `elem`  [ "Couldn't parse url: Avancerad programvarudesign"
              , "Couldn't parse url: Programvarukvalitet"
              , "Couldn't parse url: Programvaruarkitekturer"
              , "Couldn't parse url: Sannolikhetsl\228ra och statistik, grundkurs"
              , "Couldn't parse url: Maskininl\228rning, planering och reglering f\246r autonoma farkoster"
              ,"Couldn't parse url: Signalbehandling f\246r kommunikation"
              ]
