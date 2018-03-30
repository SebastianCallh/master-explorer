{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Course
  ( Course (..)
  ) where

import           Data.Aeson                               (FromJSON, ToJSON)
import           GHC.Generics                             (Generic)

import           MasterExplorer.Common.Data.Area          (Area)
import           MasterExplorer.Common.Data.CourseCode    (CourseCode)
import           MasterExplorer.Common.Data.CourseContent (CourseContent)
import           MasterExplorer.Common.Data.CourseName    (CourseName)
import           MasterExplorer.Common.Data.Credits       (Credits)
import           MasterExplorer.Common.Data.Examination   (Examination)
import           MasterExplorer.Common.Data.Examinator    (Examinator)
import           MasterExplorer.Common.Data.Field         (Field)
import           MasterExplorer.Common.Data.Grading       (Grading)
import           MasterExplorer.Common.Data.Hours         (Hours)
import           MasterExplorer.Common.Data.Importance    (Importance)
import           MasterExplorer.Common.Data.Institution   (Institution)
import           MasterExplorer.Common.Data.Level         (Level)
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
  , courseSlots         :: ![Slot]
  , courseImportance    :: !Importance
  , courseAreas         :: ![Area]
  , courseInstitution   :: !(Maybe Institution)
  , coursePrograms      :: ![Program]
  , courseField         :: !(Maybe Field)
  , coursePrerequisites :: !(Maybe Prerequisites)
  , courseGrades        :: !(Maybe Grading)
  , courseExaminator    :: !(Maybe Examinator)
  , courseExaminations  :: ![Examination]
  , courseContent       :: !(Maybe CourseContent)
  , courseSubject       :: ![Subject]
  , courseUrls          :: ![Url]
  , courseScheduledTime :: !(Maybe Hours)
  , courseSelfStudyTime :: !(Maybe Hours)
--  , courseParts         :: ![CoursePart]
  } deriving (Show, Read, Generic, ToJSON, FromJSON)