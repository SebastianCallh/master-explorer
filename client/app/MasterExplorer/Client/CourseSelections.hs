module MasterExplorer.Client.CourseSelections
( emptyCourseRepository
) where

import qualified Data.Set.Strict                   as S

import           Data.Set.Strict                   (Set)
import           Reflex.Dom

import           MasterExplorer.Client.Events      (SelectionChanged (..))
import           MasterExplorer.Common.Data.Course (Course)

data CourseRepository t = CourseRepository
  { _courseRepository_selectedCourses :: Dynamic (Set Course)
  }

emptyCourseRepository :: CourseRepository
emptyCourseRepository = CourseRepository
  { selections = S.empty
  }

