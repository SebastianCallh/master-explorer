module MasterExplorer.Scraper.Data.Models where
{-  ( Area (..)
  , Block (..)
  , Blocks (..)
  , Code (..)
  , Course (..)
  , CourseContent (..)
--  , CoursePart
--  , CourseParts (..)
  , Credits (..)
  , Examination (..)
  , ExaminationType (..)
  , Examinator
  , Field (..)
  , Grading (..)
  , Hours (..)
  , Importance
  , Institution (..)
  , Level (..)
  , Name (..)
  , Period (..)
  , Prerequisites (..)
  , Program (..)
  , Semester (..)
  , Slot (..)
  , Specialization (..)
  , Subject (..)
  , Url (..)
  , programUrl
  , mkSlot
  , parseAreas
  , parsePrograms
  , parseInstitution
  , parseField
  , parseExaminations
  , parseSubjects
  , parseLinks
  , parseSelfStudyTime
  , parseScheduledTime
  , parseGrading
  , parseExaminator
  , parseSubject
  ) where

import qualified Data.Text                                  as T

import           Data.Char                                  (isDigit)
import           Data.Maybe                                 (fromMaybe, maybe)
import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text)
import           Text.HTML.Scalpel
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           MasterExplorer.Common.Class.HasText        (toText)
import           MasterExplorer.Common.Data.Code            (Code (..))
import           MasterExplorer.Common.Data.Course          (Course (..))
import           MasterExplorer.Common.Data.CourseContent   (CourseContent (..))
import           MasterExplorer.Common.Data.Credits         (Credits (..))
import           MasterExplorer.Common.Data.Examination     (Examination (..))
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType (..))
import           MasterExplorer.Common.Data.Examinator      (Examinator (..))
import           MasterExplorer.Common.Data.Field           (Field (..))
import           MasterExplorer.Common.Data.Grading         (Grading (..))
import           MasterExplorer.Common.Data.Hours           (Hours (..))
import           MasterExplorer.Common.Data.Importance      (Importance (..))
import           MasterExplorer.Common.Data.Institution     (Institution (..))
import           MasterExplorer.Common.Data.Level           (Level (..))
import           MasterExplorer.Common.Data.Name            (Name (..))
import           MasterExplorer.Common.Data.Period          (Period (..))
import           MasterExplorer.Common.Data.Prerequisites   (Prerequisites (..))
import           MasterExplorer.Common.Data.Program
import           MasterExplorer.Common.Data.Semester        (Semester (..))
import           MasterExplorer.Common.Data.Slot            (Slot (..))
import           MasterExplorer.Common.Data.Specialization  (Specialization (..))
import           MasterExplorer.Common.Data.Subject         (Subject (..))
import           MasterExplorer.Common.Data.Url             (Url (..))








--parseCourseParts :: Text -> Either Text CourseParts
--parseCourseParts x = traverse parseCoursePart x

{-case parse table x of
    Right rows -> CourseParts <$> traverse parseCoursePart rows
    Left  _err -> parseError x "Coursepart"-}
{-
parseCoursePart :: [Text] -> Either Text CoursePart
parseCoursePart [et, t] =
  CoursePart
  <$> parseCoursePartType et
  <*> parseHours t
parseCoursePart x = Left $
    mconcat ["Wrong number of arguments when parsing CoursePart: ", T.pack $ show x]

parseCoursePartType :: Text -> Either Text CoursePartType
parseCoursePartType "F\246rel\228sning" = Right Lecture
parseCoursePartType "Laboration"        = Right Laboration
parseCoursePartType "Lektion"           = Right Class
parseCoursePartType "Grupparbete"       = Right Group
parseCoursePartType "Projekt"           = Right Project
parseCoursePartType "Seminarium"        = Right Seminar
parseCoursePartType "Ospecificerad"     = Right MasterExplorer.Common.Data.CoursePartType.Unspecified
parseCoursePartType ""                  = Right MasterExplorer.Common.Data.CoursePartType.Unspecified
parseCoursePartType x                   = parseError x "CoursePartType"
-}
-}
