module MasterExplorer.Scraper.Data.Course
  ( Course (..)
  , fromPartials
  ) where

import           MasterExplorer.Common.Data.Course           (Course (..))
import           MasterExplorer.Scraper.Data.PageCourse      (PageCourse (..))
import           MasterExplorer.Scraper.Data.ListCourse      (ListCourse (..))

fromPartials :: ListCourse -> PageCourse -> Course
fromPartials ListCourse{..} PageCourse{..} = Course
  { _courseCode          = lCourseCode
  , _courseName          = lCourseName
  , _courseCredits       = lCourseCredits
  , _courseLevel         = lCourseLevel
  , _courseImportance    = lCourseImportance
  , _courseOccasions     = lCourseOccasions
  , _courseUrls          = pCourseUrls
  , _courseInstitution   = pCourseInstitution
  , _coursePrograms      = pCoursePrograms
  , _courseFields        = pCourseFields
  , _coursePrerequisites = pCoursePrerequisites
  , _courseGrades        = pCourseGrading
  , _courseExaminator    = pCourseExaminator
  , _courseExaminations  = pCourseExaminations
  , _courseContent       = pCourseContent
  , _courseSubjects      = pCourseSubject
  , _courseScheduledTime = pCourseScheduledTime
  , _courseSelfStudyTime = pCourseSelfStudyTime
  , _courseAreas         = pCourseAreas
  }
