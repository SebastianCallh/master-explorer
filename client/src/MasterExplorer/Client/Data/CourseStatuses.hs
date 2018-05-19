module MasterExplorer.Client.Data.CourseStatuses where

import qualified Data.Map                              as M

import           Data.Map                              (Map)

import           MasterExplorer.Common.Data.Course     (Course)
import           MasterExplorer.Common.Data.CoursePlan (CoursePlan)
import qualified MasterExplorer.Common.Data.CoursePlan as CoursePlan

data CourseStatus
  = Available
  | InSelection
  | Selected

newtype CourseStatuses = CourseStatuses { getCourseStatuses :: Map Course CourseStatus }

empty :: CourseStatuses
empty = CourseStatuses M.empty

set :: CourseStatus -> Course -> CourseStatuses -> CourseStatuses
set status course =
  CourseStatuses . M.insert course status . getCourseStatuses

get :: Course -> CourseStatuses -> CourseStatus
get course =
  M.findWithDefault Available course . getCourseStatuses

fromCoursePlan :: CoursePlan -> CourseStatuses
fromCoursePlan = CourseStatuses . foldr (`M.insert` Selected) M.empty . CoursePlan.courses
