module MasterExplorer.Client.CourseEvent
  ( CourseEvent (..)
  ) where

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Occasion (Occasion)

data CourseEvent
  = CourseSelected    Course Occasion
  | CoursePreSelected Course
  | CourseDeselected  Course Occasion
