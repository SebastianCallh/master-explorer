module MasterExplorer.Scraper.Data.ListCourse
  ( ListCourse(..)
  ) where

-- Course info that was parsed from a program page

import           Data.Semigroup                             (Semigroup, (<>))
import           Data.Text                                  (Text)

import           MasterExplorer.Scraper.Data.Credits        (Credits)
import           MasterExplorer.Scraper.Data.Importance     (Importance)
import           MasterExplorer.Scraper.Data.Level          (Level)
import           MasterExplorer.Scraper.Data.Occasion       (Occasion)
import           MasterExplorer.Scraper.Data.Program        (Program)
import           MasterExplorer.Scraper.Data.Specialization (Specialization)
import           MasterExplorer.Scraper.Data.Url            (Url)

{- | Since a listcourse can be found under several
     semesters, periods, and consequently slots
     and different specializations both those fields are lists.
-}
data ListCourse = ListCourse
  { lCourseProgram    :: !Program
  , lCourseSpecs      :: ![Specialization]
  , lCourseCode       :: !Text
  , lCourseName       :: !Text
  , lCourseUrl        :: !Url
  , lCourseCredits    :: !Credits
  , lCourseLevel      :: !Level
  , lCourseImportance :: !Importance
  , lCourseOccasions  :: ![Occasion]
  } deriving (Show, Eq)

instance Semigroup ListCourse where
  x <> y =
    x { lCourseSpecs      = lCourseSpecs      x <> lCourseSpecs      y
      , lCourseOccasions  = lCourseOccasions  x <> lCourseOccasions  y
      }
