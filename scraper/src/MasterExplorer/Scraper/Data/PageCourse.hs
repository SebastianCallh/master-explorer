module MasterExplorer.Scraper.Data.PageCourse
  ( PageCourse(..)
  , fromPageSections
  ) where

-- Course info that was parsed from a course page

import qualified Data.Map                                  as M


import           Data.Bifunctor                            (bimap)
import           Data.Map                                  (Map)

import           Data.Semigroup                            ((<>))
import           Data.Text                                 (Text)

import           MasterExplorer.Scraper.Data.Area          (Area, parseAreas)
import           MasterExplorer.Scraper.Data.CourseContent (CourseContent,
                                                            parseContent)
import           MasterExplorer.Scraper.Data.Examination   (Examination,
                                                            parseExaminations)
import           MasterExplorer.Scraper.Data.Examinator    (Examinator (..),
                                                            parseExaminator)
import           MasterExplorer.Scraper.Data.Field         (Field, parseFields)
import           MasterExplorer.Scraper.Data.Grading       (Grading,
                                                            parseGrading)
import           MasterExplorer.Scraper.Data.Hours         (Hours, parseHours)
import           MasterExplorer.Scraper.Data.Institution   (Institution,
                                                            parseInstitution)
import           MasterExplorer.Scraper.Data.Prerequisites (Prerequisites (..),
                                                            parsePrerequisites)
import           MasterExplorer.Scraper.Data.Program       (Program,
                                                            parsePrograms)
import           MasterExplorer.Scraper.Data.Subject       (Subject,
                                                            parseSubjects)
import           MasterExplorer.Scraper.Data.Url           (Url (..), parseUrls)
import           MasterExplorer.Scraper.Data.Validation    (ValidationError,
                                                            valErr, (-:))

data PageCourse = PageCourse
  { pCourseAreas         :: ![Area]
  , pCourseInstitution   :: !Institution
  , pCoursePrograms      :: ![Program]
  , pCourseFields        :: ![Field]
  , pCoursePrerequisites :: !(Maybe Prerequisites)
  , pCourseGrading       :: !Grading
  , pCourseExaminator    :: !(Maybe Examinator)
  , pCourseExaminations  :: ![Examination]
  , pCourseContent       :: !CourseContent
  , pCourseSubject       :: ![Subject]
  , pCourseUrls          :: ![Url]
  , pCourseSelfStudyTime :: !Hours
  , pCourseScheduledTime :: !Hours
  } deriving (Show)

fromPageSections :: Map Text Text -> Either ValidationError PageCourse
fromPageSections sections = pure PageCourse
   -: parse parseAreas         Nothing        "Huvudomr\229de"
   -: parse parseInstitution   Nothing        "Institution"
   -: parse parsePrograms      Nothing        "Kursen ges f\246r"
   -: parse parseFields        Nothing        "Utbildningsomr\229de"
   -: parse parsePrerequisites (Just Nothing) "F\246rkunskapskrav"
   -: parse parseGrading       Nothing        "Betygsskala"
   -: parse parseExaminator    (Just Nothing) "Examinator"
   -: parse parseExaminations  Nothing        "Examination"
   -: parse parseContent       Nothing        "Kursinneh\229ll"
   -: parse parseSubjects      (Just [])      "\196mnesomr\229de"
   -: parse parseUrls          (Just [])      "Kurshemsida och andra l\228nkar"
   -: scheduledTime
   -: selfStudyTime
  where
    parse :: (Text -> Either Text a) -> Maybe a -> Text -> Either ValidationError a
    parse parser mdefault sectionKey =
      case M.lookup sectionKey sections of
        Nothing      -> case mdefault of
          Nothing -> Left . valErr $ "Could not find section " <> sectionKey
          Just a  -> pure a

        Just section -> bimap valErr id $ parser section

    (scheduledTime, selfStudyTime) =
      case parse parseHours Nothing "Undervisningstid" of
        Left e       -> (Left e, Left e)
        Right (a, b) -> (Right a, Right b)
