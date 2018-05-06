{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Course
  ( Course (..)
  , getCourseCode
  , getCourseName
  , getCourseSlots
  , getCourseContent
  , masterOccasions
  ) where

import qualified Data.Set                                 as S

import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)

import           MasterExplorer.Common.Class.FilterItem   (FilterItem (..))
import           MasterExplorer.Common.Class.ListItem     (ListItem (..))
import           MasterExplorer.Common.Data.Area          (Area)
import           MasterExplorer.Common.Data.CourseCode    (CourseCode (..))
import           MasterExplorer.Common.Data.CourseContent (CourseContent (..))
import           MasterExplorer.Common.Data.CourseName    (CourseName (..))
import           MasterExplorer.Common.Data.Credits       (Credits)
import           MasterExplorer.Common.Data.Examination   (Examination)
import           MasterExplorer.Common.Data.Examinator    (Examinator)
import           MasterExplorer.Common.Data.Field         (Field)
import           MasterExplorer.Common.Data.Grading       (Grading)
import           MasterExplorer.Common.Data.Hours         (Hours)
import           MasterExplorer.Common.Data.Importance    (Importance)
import           MasterExplorer.Common.Data.Institution   (Institution)
import           MasterExplorer.Common.Data.Level         (Level)
import           MasterExplorer.Common.Data.Occasion      (Occasion (..),
                                                           toMasterOccasions)
import           MasterExplorer.Common.Data.Prerequisites (Prerequisites)
import           MasterExplorer.Common.Data.Program       (Program)
import           MasterExplorer.Common.Data.Slot          (Slot)
import           MasterExplorer.Common.Data.Subject       (Subject)
import           MasterExplorer.Common.Data.Url           (Url)

data Course = Course
  { courseCode          :: !CourseCode
  , courseName          :: !CourseName
  , courseCredits       :: !Credits
  , courseLevel         :: !Level
  , courseOccasions     :: ![Occasion]
  , courseImportance    :: !Importance
  , courseAreas         :: ![Area]
  , courseInstitution   :: !Institution
  , coursePrograms      :: ![Program]
  , courseFields        :: ![Field]
  , coursePrerequisites :: !(Maybe Prerequisites)
  , courseGrades        :: !Grading
  , courseExaminator    :: !(Maybe Examinator)
  , courseExaminations  :: ![Examination]
  , courseContent       :: !CourseContent
  , courseSubjects      :: ![Subject]
  , courseUrls          :: ![Url]
  , courseScheduledTime :: !Hours
  , courseSelfStudyTime :: !Hours
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

instance ListItem Course where
  listItemText =  getCourseCode

instance FilterItem Course where
  filterFields course =
    [ getCourseCode
    , getCourseName
    ] <*> pure course

instance Eq Course where
  (==) a b = courseCode a == courseCode b

instance Ord Course where
  (<=) a b = courseCode a <= courseCode b

getCourseCode :: Course -> Text
getCourseCode = getCode . courseCode

getCourseName :: Course -> Text
getCourseName = getName . courseName

getCourseSlots :: Course -> [Slot]
getCourseSlots = concatMap getOccasion . courseOccasions

getCourseContent :: Course -> Text
getCourseContent = getContent . courseContent

-- | Since courses can be choosen every autumn/spring and not
--   just semester 5 or 6 or whatever the studieinfo says, all
--   courses are mapped onto their equivalent master semesters [7, 8, 9].
--   A set is used to avoid duplicates (courses can be both in semester
--   5 and 9 for instance).
masterOccasions :: Course -> [Occasion]
masterOccasions = S.toList . foldr insertMasterOccasions S.empty . courseOccasions
  where
    insertMasterOccasions o s = foldr S.insert s $ toMasterOccasions o

