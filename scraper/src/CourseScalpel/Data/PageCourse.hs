module CourseScalpel.Data.PageCourse
  ( PageCourse(..)
  , fromPageSections
  ) where

-- Course info that was parsed from a course page

import qualified Data.Map                         as M

import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)

import           CourseScalpel.Data.Area          (Area, parseAreas)
import           CourseScalpel.Data.CourseContent (CourseContent (..))
import           CourseScalpel.Data.Examination   (Examination,
                                                   parseExaminations)
import           CourseScalpel.Data.Examinator    (Examinator (..))
import           CourseScalpel.Data.Field         (Field, parseField)
import           CourseScalpel.Data.Grading       (Grading, parseGrading)
import           CourseScalpel.Data.Hours         (Hours, parseHours)
import           CourseScalpel.Data.Institution   (Institution,
                                                   parseInstitution)
import           CourseScalpel.Data.Prerequisites (Prerequisites (..))
import           CourseScalpel.Data.Program       (Program, parsePrograms)
import           CourseScalpel.Data.Subject       (Subject, parseSubjects)
import           CourseScalpel.Data.Url           (Url (..), parseUrls)
import           CourseScalpel.Helpers            (eitherToMaybe)

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

fromPageSections :: Map Text Text  -> PageCourse
fromPageSections sections = PageCourse
  { pCourseAreas         = parseList parseAreas        "Huvudomr\229de"
  , pCourseInstitution   = parse parseInstitution      "Institution"
  , pCoursePrograms      = parseList parsePrograms     "Kursen ges f\246r"
  , pCourseField         = parse parseField            "Utbildningsomr\229de"
  , pCoursePrerequisites = Prerequisites <$> M.lookup  "F\246rkunskapskrav" sections
  , pCourseGrading       = parse parseGrading          "Betygsskala"
  , pCourseExaminator    = Examinator <$> M.lookup     "Examinator" sections
  , pCourseExaminations  = parseList parseExaminations "Examination"
  , pCourseContent       = CourseContent <$> M.lookup  "Kursinneh\229ll" sections
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
