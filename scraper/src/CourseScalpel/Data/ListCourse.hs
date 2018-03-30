module CourseScalpel.Data.ListCourse
  ( ListCourse(..)
  ) where

-- Course info that was parsed from a program page

import           Data.Semigroup                    (Semigroup, (<>))

import           CourseScalpel.Data.CourseCode     (CourseCode)
import           CourseScalpel.Data.CourseName     (CourseName)
import           CourseScalpel.Data.Credits        (Credits)
import           CourseScalpel.Data.Importance     (Importance)
import           CourseScalpel.Data.Level          (Level)
import           CourseScalpel.Data.Period         (Period)
import           CourseScalpel.Data.Program        (Program)
import           CourseScalpel.Data.Semester       (Semester)
import           CourseScalpel.Data.Slot           (Slot)
import           CourseScalpel.Data.Specialization (Specialization)
import           CourseScalpel.Data.Url            (Url)

{- | Since a listcourse can be found under several
     semesters, periods, and consequently slots
     and different specializations both those fields are lists.
-}
data ListCourse = ListCourse
  { lCourseProgram    :: !Program
  , lCourseSems       :: ![Semester]
  , lCoursePeriod     :: ![Period]
  , lCourseSpecs      :: ![Specialization]
  , lCourseCode       :: !CourseCode
  , lCourseName       :: !CourseName
  , lCourseUrl        :: !Url
  , lCourseCredits    :: !Credits
  , lCourseLevel      :: !Level
  , lCourseImportance :: !Importance
  , lCourseSlots      :: ![Slot]
  } deriving (Show)

instance Semigroup ListCourse where
  x <> y =
    x { lCourseSems   = lCourseSems   x <> lCourseSems   y
      , lCoursePeriod = lCoursePeriod x <> lCoursePeriod y
      , lCourseSpecs  = lCourseSpecs  x <> lCourseSpecs  y
      , lCourseSlots  = lCourseSlots  x <> lCourseSlots  y
      }


