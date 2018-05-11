{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Course  where

import qualified Data.Set                                 as S

import           Control.Lens                             hiding (Level)
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Ord                                 (comparing)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)

import           MasterExplorer.Common.Class.FilterItem   (FilterItem (..))
import           MasterExplorer.Common.Class.ListItem     (ListItem (..))
import           MasterExplorer.Common.Data.Area          (Area)
import           MasterExplorer.Common.Data.CourseContent (CourseContent (..))
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
  { _courseCode          :: !Text
  , _courseName          :: !Text
  , _courseCredits       :: !Credits
  , _courseLevel         :: !Level
  , _courseOccasions     :: ![Occasion]
  , _courseImportance    :: !Importance
  , _courseAreas         :: ![Area]
  , _courseInstitution   :: !Institution
  , _coursePrograms      :: ![Program]
  , _courseFields        :: ![Field]
  , _coursePrerequisites :: !(Maybe Prerequisites)
  , _courseGrades        :: !Grading
  , _courseExaminator    :: !(Maybe Examinator)
  , _courseExaminations  :: ![Examination]
  , _courseContent       :: !CourseContent
  , _courseSubjects      :: ![Subject]
  , _courseUrls          :: ![Url]
  , _courseScheduledTime :: !Hours
  , _courseSelfStudyTime :: !Hours
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

makeLenses ''Course

instance ListItem Course where
  listItemText =  _courseCode

instance FilterItem Course where
  filterFields course =
    [ _courseCode
    , _courseName
    ] <*> pure course

instance Eq Course where
  (==) a b = EQ == comparing _courseCode a b

instance Ord Course where
  (<=) a b = LT == comparing _courseCode a b

getCourseSlots :: Course -> [Slot]
getCourseSlots = concatMap getOccasion . _courseOccasions

getCourseContent :: Course -> Text
getCourseContent = getContent . _courseContent

-- | Since courses can be choosen every autumn/spring and not
--   just semester 5 or 6 or whatever the studieinfo says, all
--   courses are mapped onto their equivalent master semesters [7, 8, 9].
--   A set is used to avoid duplicates (courses can be both in semester
--   5 and 9 for instance).
masterOccasions :: Course -> [Occasion]
masterOccasions = S.toList . foldr insertMasterOccasions S.empty . _courseOccasions
  where
    insertMasterOccasions o s = foldr S.insert s $ toMasterOccasions o

