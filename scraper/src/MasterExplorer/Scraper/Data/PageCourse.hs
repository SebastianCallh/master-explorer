module MasterExplorer.Scraper.Data.PageCourse
  ( PageCourse(..)
  , fromPageSections
  ) where

-- Course info that was parsed from a course page

import qualified Data.Map                                  as M

import           Data.Map                                  (Map)
import           Data.Maybe                                (fromMaybe)
import           Data.Text                                 (Text)

import           MasterExplorer.Scraper.Data.Area          (Area, parseAreas)
import           MasterExplorer.Scraper.Data.CourseContent (CourseContent,
                                                            parseContent)
import           MasterExplorer.Scraper.Data.Examination   (Examination,
                                                            parseExaminations)
import           MasterExplorer.Scraper.Data.Examinator    (Examinator (..))
import           MasterExplorer.Scraper.Data.Field         (Field, parseField)
import           MasterExplorer.Scraper.Data.Grading       (Grading,
                                                            parseGrading)
import           MasterExplorer.Scraper.Data.Hours         (Hours, parseHours)
import           MasterExplorer.Scraper.Data.Institution   (Institution,
                                                            parseInstitution)
import           MasterExplorer.Scraper.Data.Prerequisites (Prerequisites (..))
import           MasterExplorer.Scraper.Data.Program       (Program,
                                                            parsePrograms)
import           MasterExplorer.Scraper.Data.Subject       (Subject,
                                                            parseSubjects)
import           MasterExplorer.Scraper.Data.Url           (Url (..), parseUrls)
import           MasterExplorer.Scraper.Helpers            (eitherToMaybe)

data PageCourse = PageCourse
  { pCourseAreas         :: ![Area]
  , pCourseInstitution   :: !(Maybe Institution)
  , pCoursePrograms      :: ![Program]
  , pCourseField         :: !(Maybe Field)
  , pCoursePrerequisites :: !(Maybe Prerequisites)
  , pCourseGrading       :: !(Maybe Grading)
  , pCourseExaminator    :: !(Maybe Examinator)
  , pCourseExaminations  :: ![Examination]
  , pCourseContent       :: !(Maybe CourseContent)
  , pCourseSubject       :: ![Subject]
  , pCourseUrls          :: ![Url]
  , pCourseSelfStudyTime :: !(Maybe Hours)
  , pCourseScheduledTime :: !(Maybe Hours)
  } deriving (Show)

fromPageSections :: Map Text Text -> PageCourse
fromPageSections sections = PageCourse
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
