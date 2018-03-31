module MasterExplorer.Scraper.Data.Course
  ( Course (..)
  , fromPartials
  ) where

import           MasterExplorer.Common.Data.Course  (Course (..))
import           MasterExplorer.Scraper.Data.PageCourse      (PageCourse (..))
import           MasterExplorer.Scraper.Data.ListCourse      (ListCourse (..))

fromPartials :: ListCourse -> PageCourse -> Course
fromPartials ListCourse{..} PageCourse{..} = Course
  { courseCode          = lCourseCode
  , courseName          = lCourseName
  , courseCredits       = lCourseCredits
  , courseLevel         = lCourseLevel
  , courseImportance    = lCourseImportance
  , courseSlots         = lCourseSlots
  , courseUrls          = pCourseUrls
  , courseInstitution   = pCourseInstitution
  , coursePrograms      = pCoursePrograms
  , courseField         = pCourseField
  , coursePrerequisites = pCoursePrerequisites
  , courseGrades        = pCourseGrading
  , courseExaminator    = pCourseExaminator
  , courseExaminations  = pCourseExaminations
  , courseContent       = pCourseContent
  , courseSubject       = pCourseSubject
  , courseScheduledTime = pCourseScheduledTime
  , courseSelfStudyTime = pCourseSelfStudyTime
  , courseAreas         = pCourseAreas
  }
