module MasterExplorer.Scraper.Web.Scrapers
  ( pageCourseScraper
  , programplanScraper
  , semesterScraper
  , coursesScraper
  ) where

import qualified Data.Map.Strict                            as M
import qualified Data.Text                                  as T

import           Data.Either                                (partitionEithers)
import           Data.Map                                   (Map)
import           Data.Maybe                                 (fromMaybe)
import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text)
import           MasterExplorer.Scraper.Helpers             (eitherToMaybe)
import           Text.HTML.Scalpel                          (Scraper, attr,
                                                             chroot, chroots,
                                                             hasClass,
                                                             innerHTML,
                                                             innerHTMLs,
                                                             scrapeStringLike,
                                                             text, (@:))

import           MasterExplorer.Scraper.Data.Area           (Area, parseAreas)
import           MasterExplorer.Scraper.Data.Block          (Block, parseBlocks)
import           MasterExplorer.Scraper.Data.CourseCode     (CourseCode (..))
import           MasterExplorer.Scraper.Data.CourseContent  (CourseContent,
                                                             parseContent)
import           MasterExplorer.Scraper.Data.CourseName     (CourseName (..))
import           MasterExplorer.Scraper.Data.Credits        (Credits,
                                                             parseCredits)
import           MasterExplorer.Scraper.Data.Examination    (Examination,
                                                             parseExaminations)
import           MasterExplorer.Scraper.Data.Examinator     (Examinator (..))
import           MasterExplorer.Scraper.Data.Field          (Field, parseField)
import           MasterExplorer.Scraper.Data.Grading        (Grading,
                                                             parseGrading)
import           MasterExplorer.Scraper.Data.Hours          (Hours, parseHours)
import           MasterExplorer.Scraper.Data.Importance     (Importance,
                                                             parseImportance)
import           MasterExplorer.Scraper.Data.Institution    (Institution,
                                                             parseInstitution)
import           MasterExplorer.Scraper.Data.Level          (Level, parseLevel)
import           MasterExplorer.Scraper.Data.ListCourse     (ListCourse (..))
import           MasterExplorer.Scraper.Data.Occasion       (Occasion (..))
import           MasterExplorer.Scraper.Data.PageCourse     (PageCourse (..))
import           MasterExplorer.Scraper.Data.Period         (Period,
                                                             parsePeriod)
import           MasterExplorer.Scraper.Data.Prerequisites  (Prerequisites (..))
import           MasterExplorer.Scraper.Data.Program        (Program)
import           MasterExplorer.Scraper.Data.Program        (Program,
                                                             parsePrograms)
import           MasterExplorer.Scraper.Data.Semester       (Semester,
                                                             parseSemester)
import           MasterExplorer.Scraper.Data.Slot           (Slot (..))
import           MasterExplorer.Scraper.Data.Specialization (Specialization,
                                                             parseSpecialization)
import           MasterExplorer.Scraper.Data.Subject        (Subject,
                                                             parseSubjects)
import           MasterExplorer.Scraper.Data.Url            (Url (..))
import           MasterExplorer.Scraper.Data.Url            (Url (..),
                                                             parseUrls)
import           MasterExplorer.Scraper.Helpers             (maybeToEither)

type Scalpel = Scraper Text

pageCourseScraper :: Scalpel PageCourse
pageCourseScraper =
  fmap (makePageCourse . M.fromList) $
  chroot ("section" @: [hasClass "studyguide-block"]) $
    chroots "div" $ do
      title   <- innerHTML "h3"
      content <- sanitize . snd . T.breakOnEnd "</h3>" <$> innerHTML "div"
      return (title, content)

makePageCourse :: Map Text Text -> PageCourse
makePageCourse sections = PageCourse
  { pCourseAreas         = parseList parseAreas        "Huvudomr\229de"
  , pCourseInstitution   = parse parseInstitution      "Institution"
  , pCoursePrograms      = maybe [] parsePrograms $ M.lookup  "Kursen ges f\246r" sections
  , pCourseField         = parse parseField            "Utbildningsomr\229de"
  , pCoursePrerequisites = Prerequisites <$> M.lookup  "F\246rkunskapskrav" sections
  , pCourseGrading       = parse parseGrading          "Betygsskala"
  , pCourseExaminator    = Examinator <$> M.lookup     "Examinator" sections
  , pCourseExaminations  = parseList parseExaminations "Examination"
  , pCourseContent       = parse parseContent          "Kursinneh\229ll"
  , pCourseSubject       = parseList parseSubjects     "\196mnesomr\229de"
  , pCourseUrls          = parseList parseUrls         "Kurshemsida och andra l\228nkar"
  , pCourseScheduledTime = scheduledTime
  , pCourseSelfStudyTime = selfStudyTime
  }
  where
    parse :: (Text -> Either Text a) -> Text -> Maybe a
    parse parser key = maybe Nothing (eitherToMaybe . parser) $ M.lookup key sections
    parseList parser key = fromMaybe [] $ parse parser key
    (scheduledTime, selfStudyTime) =
      case parse parseHours "Undervisningstid" of
        Nothing     -> (Nothing, Nothing)
        Just (a, b) -> (Just a, Just b)

programplanScraper :: Program -> Scalpel [Either Text ListCourse]
programplanScraper program =
  chroot ("div" @: [hasClass "programplan"]) $
    semesterScraper program

semesterScraper :: Program -> Scalpel [Either Text ListCourse]
semesterScraper program =
  fmap (merge occasions . concat) $ chroots ("article" @: [hasClass "semester"]) $ do

    -- Semester string is on format 'Termin 8 (VT 2022)' so needs some processing
    esemester <- parseSemester . T.strip . T.takeWhile (/= '(') <$> text "h3"
    case esemester of
      Left err       -> return [Left err]
      Right semester -> specializationScraper program semester

  where
    occasions :: (ListCourse -> ListCourse -> ListCourse)
    occasions lc1 lc2 =
      lc1 { lCourseOccasions = lCourseOccasions lc1 <> lCourseOccasions lc2
          }

specializationScraper :: Program -> Semester -> Scalpel [Either Text ListCourse]
specializationScraper program semester =
    fmap (merge specs . concat) $ chroots ("div" @: [hasClass "specialization"]) $ do
      espec <- parseSpecialization <$> attr "data-specialization"
        ("div" @: [hasClass "specialization"])
      case espec of
        Left err   -> return [Left err]
        Right spec -> periodScraper program semester spec

    where
      specs :: (ListCourse -> ListCourse -> ListCourse)
      specs lc1 lc2 =
        lc1 { lCourseSpecs = lCourseSpecs lc1 <> lCourseSpecs lc2 }

periodScraper :: Program -> Semester -> Specialization -> Scalpel [Either Text ListCourse]
periodScraper program semester spec =
  fmap (merge occasion . concat) $ chroots ("tbody" @: [hasClass "period"]) $ do
    eperiod <- parsePeriod . sanitize <$> text "tr"
    case eperiod of
      Left err     -> return [Left err]
      Right period -> coursesScraper program semester spec period

  where
    occasion :: (ListCourse -> ListCourse -> ListCourse)
    occasion lc1 lc2 =
      lc1 { lCourseOccasions =
            zipWith (<>)
            (lCourseOccasions lc1)
            (lCourseOccasions lc2)
          }

coursesScraper
  :: Program
  -> Semester
  -> Specialization
  -> Period
  -> Scalpel [Either Text ListCourse]
coursesScraper prog sem spec per = chroots ("tr" @: [hasClass "main-row"]) $ do
  tds <- fmap sanitize <$> innerHTMLs "td"
  if length tds /= 7 then
    return $ Left $ "Ill formed markup while parsing row: " <> T.pack (show tds)
  else do
    let [code, href, creds, lvl, bls, impSpan, _] = tds
    return $ do
      let murl = scrapeStringLike href $ attr "href" "a"
      url         <- maybeToEither ("Couldn't parse url: " <> href) murl
      name        <- maybeToEither "" $  scrapeStringLike href $ text "a"
      credits     <- parseCredits creds
      level       <- parseLevel lvl
      let mimp = sanitize <$> scrapeStringLike impSpan (innerHTML "span")
      importance  <- parseImportance =<< maybeToEither "" mimp

      return ListCourse
        { lCourseProgram    = prog
        , lCourseSpecs      = [spec]
        , lCourseCode       = CourseCode code
        , lCourseName       = CourseName name
        , lCourseUrl        = Url url
        , lCourseCredits    = credits
        , lCourseLevel      = level
        , lCourseImportance = importance
        , lCourseOccasions  = makeOccasion sem per bls
        }

-- | Strips leading and trailing whitespace and removes
--   junk characters, typically from text inside tags.
sanitize :: Text -> Text
sanitize = T.strip . T.filter (not . isTrash)
  where isTrash = (`elem` ['\t', '\n', '\r'])

makeOccasion :: Semester -> Period -> Text -> [Occasion]
makeOccasion semester period txt = [Occasion slots]
  where
    slots  = either (const mempty) id eslots
    eslots = fmap (Slot semester period) <$> parseBlocks txt

merge :: (ListCourse -> ListCourse -> ListCourse)
      -> [Either Text ListCourse]
      -> [Either Text ListCourse]
merge f ecourses = (Right <$> x courses) <> (Left <$> errors)
  where
    (errors, courses) = partitionEithers ecourses
    x cs = fmap snd $ M.toList $ foldr insert M.empty cs
    insert c = M.insertWith f (lCourseCode c) c
