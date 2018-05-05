module MasterExplorer.Scraper.Data.PageCourse
  ( PageCourse(..)
  ) where

-- Course info that was parsed from a course page


import           MasterExplorer.Scraper.Data.Area          (Area)
import           MasterExplorer.Scraper.Data.CourseContent (CourseContent)
import           MasterExplorer.Scraper.Data.Examination   (Examination)
import           MasterExplorer.Scraper.Data.Examinator    (Examinator (..))
import           MasterExplorer.Scraper.Data.Field         (Field)
import           MasterExplorer.Scraper.Data.Grading       (Grading)
import           MasterExplorer.Scraper.Data.Hours         (Hours)
import           MasterExplorer.Scraper.Data.Institution   (Institution)
import           MasterExplorer.Scraper.Data.Prerequisites (Prerequisites (..))
import           MasterExplorer.Scraper.Data.Program       (Program)
import           MasterExplorer.Scraper.Data.Subject       (Subject)
import           MasterExplorer.Scraper.Data.Url           (Url (..))

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
