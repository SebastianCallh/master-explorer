module CourseScalpel.Web.Scrapers
  ( pageCourseScraper
  , programplanScraper
  , semesterScraper
  , listCoursesScraper
  , testScraper
  ) where


import qualified Data.Map.Strict                   as M
import qualified Data.Text                         as T

import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import           Text.HTML.Scalpel                 (Scraper, attr, chroot,
                                                    chroots, hasClass,
                                                    innerHTML, innerHTMLs,
                                                    scrapeStringLike, text,
                                                    (@:))

import           CourseScalpel.Data.Block          (parseBlocks)
import           CourseScalpel.Data.CourseCode     (CourseCode (..))
import           CourseScalpel.Data.CourseName     (CourseName (..))
import           CourseScalpel.Data.Credits        (parseCredits)
import           CourseScalpel.Data.Level          (parseLevel)
import           CourseScalpel.Data.ListCourse     (ListCourse (..))
import           CourseScalpel.Data.PageCourse     (PageCourse (..),
                                                    fromPageSections)

import           CourseScalpel.Data.Importance     (parseImportance)
import           CourseScalpel.Data.Period         (Period, parsePeriod)
import           CourseScalpel.Data.Program        (Program)
import           CourseScalpel.Data.Semester       (Semester, parseSemester)
import           CourseScalpel.Data.Slot           (Slot, mkSlot)
import           CourseScalpel.Data.Specialization (Specialization,
                                                    parseSpecialization)
import           CourseScalpel.Data.Url            (Url (..))
import           CourseScalpel.Helpers             (maybeToEither)

type Scalpel = Scraper Text

testScraper :: Scalpel Text
testScraper = text "body"

pageCourseScraper :: Scalpel PageCourse
pageCourseScraper = fromPageSections . M.fromList <$>
    chroot ("section" @: [hasClass "studyguide-block"])
    (chroots "div" $ do
        title   <- innerHTML "h3"
        content <- sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
        return (title, content))

programplanScraper :: Program -> Scalpel [Either Text ListCourse]
programplanScraper program =
  chroot ("div" @: [hasClass "programplan"]) $
    semesterScraper program

semesterScraper :: Program -> Scalpel [Either Text ListCourse]
semesterScraper program =
  fmap concat $ chroots ("article" @: [hasClass "semester"]) $ do

    -- Semester string is on format 'Termin 8 (VT 2022)' so needs some processing
    semester <- parseSemester . T.strip . T.takeWhile (/= '(') <$> text "h3"

    fmap concat $ chroots ("div" @: [hasClass "specialization"]) $ do
      spec <- parseSpecialization <$> attr "data-specialization" ("div" @: [hasClass "specialization"])
      fmap concat $ chroots ("tbody" @: [hasClass "period"]) $ do

        period <- parsePeriod . sanitize <$> text "tr"
        let ecourses = listCoursesScraper program
                       <$> semester
                       <*> period
                       <*> spec

        either (fail . show) id ecourses

listCoursesScraper
  :: Program
  -> Semester
  -> Period
  -> Specialization
  -> Scalpel [Either Text ListCourse]
listCoursesScraper prog sem per spec = chroots ("tr" @: [hasClass "main-row"]) $ do
  tds <- fmap sanitize <$> innerHTMLs "td"
  if length tds /= 7 then
    return $ Left $ "Ill formed markup while parsing row: " <> T.pack (show tds)
  else do
    let [code, href, creds, lvl, bls, impSpan, _] = tds
    return $ do
      url         <- maybeToEither "Couldn't parse url" $ scrapeStringLike href $ attr "href" "a"
      name        <- maybeToEither "" $  scrapeStringLike href $ text "a"
      credits     <- parseCredits creds
      level       <- parseLevel lvl
      let mimp = sanitize <$> scrapeStringLike impSpan (innerHTML "span")
      importance  <- parseImportance =<< maybeToEither "" mimp

      return ListCourse
        { lCourseProgram    = prog
        , lCourseSems       = [sem]
        , lCoursePeriod     = [per]
        , lCourseSpecs      = [spec]
        , lCourseCode       = CourseCode code
        , lCourseName       = CourseName name
        , lCourseUrl        = Url url
        , lCourseCredits    = credits
        , lCourseLevel      = level
        , lCourseImportance = importance
        , lCourseSlots      = makeSlots per bls
        }

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])

makeSlots :: Period -> Text -> [Slot]
makeSlots period txt = either (const mempty) pure slots
  where slots = mkSlot period <$> parseBlocks txt
