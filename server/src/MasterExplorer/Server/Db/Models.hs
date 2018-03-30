{-# LANGUAGE TemplateHaskell #-}

{- Contains orphan instances for sum types to be represented in the db.
   Not intended for imported outside MasterExplorer.Server.Db. -}
module MasterExplorer.Server.Db.Models
  ( Area
  , CourseCode
  , Course (..)
  , CourseContent
  , CourseName
  , CoursePart
  , Credits
  , Examination
  , Examinator
  , Field
  , Grading
  , Hours
  , Importance
  , Institution
  , Level
  , Prerequisites
  , Program (..)
  , ProgramCode
  , ProgramSlug
  , Slot
  , Subject
  , Url
  ) where

import           MasterExplorer.Common.Data.Area          (Area)
import           MasterExplorer.Common.Data.Course        (Course (..))
import           MasterExplorer.Common.Data.CourseCode    (CourseCode)
import           MasterExplorer.Common.Data.CourseContent (CourseContent)
import           MasterExplorer.Common.Data.CourseName    (CourseName)
import           MasterExplorer.Common.Data.CoursePart    (CoursePart)
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
import           MasterExplorer.Common.Data.Program       (Program (..))
import           MasterExplorer.Common.Data.ProgramCode   (ProgramCode)
import           MasterExplorer.Common.Data.ProgramSlug   (ProgramSlug)
import           MasterExplorer.Common.Data.Slot          (Slot)
import           MasterExplorer.Common.Data.Subject       (Subject)
import           MasterExplorer.Common.Data.Url           (Url)

import           Database.Persist.TH                      (derivePersistField)

derivePersistField "Area"
derivePersistField "CourseCode"
derivePersistField "CourseContent"
derivePersistField "CourseName"
derivePersistField "CoursePart"
derivePersistField "Credits"
derivePersistField "Examination"
derivePersistField "Examinator"
derivePersistField "Field"
derivePersistField "Grading"
derivePersistField "Hours"
derivePersistField "Importance"
derivePersistField "Institution"
derivePersistField "Level"
derivePersistField "Prerequisites"
derivePersistField "Program"
derivePersistField "ProgramCode"
derivePersistField "ProgramSlug"
derivePersistField "Slot"
derivePersistField "Subject"
derivePersistField "Url"
