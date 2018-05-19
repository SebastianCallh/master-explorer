{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module MasterExplorer.Server.Db
  ( CourseProgram (..)
  , DbCourse (..)
  , DbProgram
  , SelectResult
  , migrateAll
  , selectCourse
  , selectCourses
  , updateCourse
  , updateCourses
  , updateProgram
  , insertCoursePrograms
  , selectCoursePlan
  , insertCoursePlan
  , fromDbCoursePlan
  , fromDbCourse
  , fromDbProgram
  , toDbProgram
  ) where

import qualified Data.Map                           as M


import           Data.Maybe                      (listToMaybe, mapMaybe)
import           Data.Map                           (Map)
import           Data.Text                       (Text)
import           Data.Traversable                (for)
import           Control.Applicative             (liftA2)
import           Control.Monad.IO.Class          (liftIO)           
import           Control.Monad.Reader            (ReaderT, asks)
import           Database.Persist.TH             (mkMigrate, mkPersist,
                                                  persistLowerCase, share,
                                                  sqlSettings)
import           Database.Esqueleto
import           Safe                            (headMay)

import           MasterExplorer.Server.Config    (Config (..), App)
import           MasterExplorer.Server.Db.Models 

--  | The field for unique code should be the only unique field
-- on Course or the upsert will fail :(
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbCourse
  code          Text
  name          Text
  urls          [Url]
  credits       Credits
  level         Level
  occasions     [Occasion]
  importance    Importance
  areas         [Area]
  institution   Institution
  fields        [Field]
  prerequisites Prerequisites Maybe
  grades        Grading
  subjects      [Subject]
  examinator    Examinator Maybe
  examinations  [Examination]
  content       CourseContent
  selfStudyTime Hours
  scheduledTime Hours
  UniqueDbCourse code

DbProgram
  code       ProgramCode
  slug       ProgramSlug
  
CourseProgram
  courseId  DbCourseId
  programId DbProgramId

DbCoursePlan
  courseSelections [CourseSelection]
  program          Program
|]

runDb :: ReaderT SqlBackend IO a -> App a
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

type SelectResult = (Entity DbCourse, Entity DbProgram)

selectCourse :: Text -> App (Maybe SelectResult)
selectCourse courseCode = do
  res <- runDb $
    select $ from $ \(c `InnerJoin` cp `InnerJoin` p) -> do
    on     $ p ?. DbProgramId  ==. cp ?. CourseProgramProgramId
    on     $ c ?. DbCourseId   ==. cp ?. CourseProgramCourseId
    where_ $ c ?. DbCourseCode ==. just (val courseCode)
    return (c, p)
  return $ headMay res >>= unwrap

selectCourses :: Program -> App [(Entity DbCourse, Entity DbProgram)]
selectCourses program = do
  res <- runDb $
    select $ from $ \(c `InnerJoin` cp `InnerJoin` p) -> do
    on     $ p ?. DbProgramId   ==. cp ?. CourseProgramProgramId
    on     $ c ?. DbCourseId    ==. cp ?. CourseProgramCourseId
    where_ $ p ?. DbProgramCode ==. just (val (_programCode program))
    return (c, p)
  return $ mapMaybe unwrap res

removeCourses :: App ()
removeCourses = do
  runDb $ delete $
    from $ \(_cp :: SqlExpr (Entity CourseProgram)) -> return ()
             
  runDb $ delete $
    from $ \(_c  :: SqlExpr (Entity DbCourse)) -> return ()

unwrap :: (Maybe a, Maybe b) -> Maybe (a, b)
unwrap = uncurry (liftA2 (,))

updateCourses :: [Course] -> App ()
updateCourses courses = do
  _ <- removeCourses
  let dbCourses = toDbCourse <$> courses
  coursesAndPrograms <- for dbCourses $ \(course, programs) -> do
        dbCourse   <- runDb $ insert course
        dbPrograms <- for programs $ \program -> 
          runDb $ select $ from $ \p -> do
            where_ (p ^. DbProgramCode ==. val (_programCode program))
            return p
        return (dbCourse, concat dbPrograms)

  let programCourses = foldMap (\(c, ps) -> CourseProgram c . entityKey <$> ps) coursesAndPrograms 
  _ <- runDb $ insertMany_ programCourses
  return ()

-- Assume the code unique field is first!
updateCourse :: DbCourse -> App (Entity DbCourse)
updateCourse dbcourse = runDb $ upsertBy (code dbcourse) dbcourse []
  where code = head . persistUniqueKeys

-- Assume the code unique field is first!
updateProgram :: DbProgram -> App (Entity DbProgram)
updateProgram dbprogram = 
  runDb $ upsertBy (code dbprogram) dbprogram []
  where code = head . persistUniqueKeys
  
insertCoursePrograms :: [CourseProgram] -> App [Key CourseProgram]
insertCoursePrograms = runDb . insertMany

-- Marshalling between db and otw program

toDbProgram :: Program -> DbProgram
toDbProgram Program{..} = DbProgram
  { dbProgramCode = _programCode
  , dbProgramSlug = _programSlug
  }

fromDbProgram :: DbProgram -> Program
fromDbProgram DbProgram{..} = Program
  { _programCode = dbProgramCode
  , _programSlug = dbProgramSlug
  }

-- Marshalling between db and otw course
  
toDbCourse :: Course -> (DbCourse, [Program])
toDbCourse Course{..} =
  (DbCourse
    { dbCourseCode          = _courseCode
    , dbCourseUrls          = _courseUrls
    , dbCourseName          = _courseName
    , dbCourseCredits       = _courseCredits
    , dbCourseLevel         = _courseLevel        
    , dbCourseOccasions     = _courseOccasions
    , dbCourseImportance    = _courseImportance
    , dbCourseAreas         = _courseAreas        
    , dbCourseInstitution   = _courseInstitution
    , dbCourseFields        = _courseFields
    , dbCoursePrerequisites = _coursePrerequisites
    , dbCourseGrades        = _courseGrades       
    , dbCourseExaminator    = _courseExaminator   
    , dbCourseExaminations  = _courseExaminations
    , dbCourseContent       = _courseContent      
    , dbCourseSubjects      = _courseSubjects
    , dbCourseSelfStudyTime = _courseSelfStudyTime
    , dbCourseScheduledTime = _courseScheduledTime
    }, _coursePrograms)

fromDbCourse :: DbCourse -> [Program] -> Course
fromDbCourse DbCourse{..} programs = Course
  { _courseCode          = dbCourseCode
  , _courseName          = dbCourseName
  , _courseUrls          = dbCourseUrls
  , _courseCredits       = dbCourseCredits
  , _courseLevel         = dbCourseLevel
  , _courseOccasions     = dbCourseOccasions
  , _courseImportance    = dbCourseImportance
  , _courseAreas         = dbCourseAreas 
  , _courseInstitution   = dbCourseInstitution
  , _coursePrograms      = programs
  , _courseFields        = dbCourseFields
  , _coursePrerequisites = dbCoursePrerequisites
  , _courseGrades        = dbCourseGrades
  , _courseExaminator    = dbCourseExaminator
  , _courseExaminations  = dbCourseExaminations
  , _courseContent       = dbCourseContent
  , _courseSubjects       = dbCourseSubjects
  , _courseSelfStudyTime = dbCourseSelfStudyTime
  , _courseScheduledTime = dbCourseScheduledTime
  } 

insertCoursePlan :: CoursePlan -> App Int
insertCoursePlan coursePlan = do
  let dbCoursePlan = toDbCoursePlan coursePlan --  slotMapToCourseSelections $ getSchedule schedule
  fromIntegral . fromSqlKey <$> runDb (insert dbCoursePlan)

selectCoursePlan :: Int -> App (Maybe (Entity DbCoursePlan))
selectCoursePlan coursePlanId =
  fmap listToMaybe $ runDb $
    select $ from $ \s -> do
      where_ $ s ^. DbCoursePlanId ==. val (toSqlKey $ fromIntegral coursePlanId)
      return s

-- Marshallig to and from db representation of CoursePlan

toDbCoursePlan :: CoursePlan -> DbCoursePlan
toDbCoursePlan CoursePlan{..} = DbCoursePlan
  { dbCoursePlanCourseSelections = slotMapToCourseSelections _coursePlanSchedule
  , dbCoursePlanProgram          = _coursePlanProgram
  }

fromDbCoursePlan :: DbCoursePlan -> CoursePlan
fromDbCoursePlan DbCoursePlan{..} =
  CoursePlan (slotMapFromCourseSelections dbCoursePlanCourseSelections) dbCoursePlanProgram

slotMapToCourseSelections :: Map Slot [Course] -> [CourseSelection]
slotMapToCourseSelections = M.foldMapWithKey insertCourseSlots
  where insertCourseSlots s cs = [CourseSelection s c | c <- cs]

slotMapFromCourseSelections :: [CourseSelection] -> Map Slot [Course]
slotMapFromCourseSelections = foldr insertSelection M.empty 
  where insertSelection cSel = M.insertWith (++)
                               (_courseSelectionSlot cSel)
                               [_courseSelectionCourse cSel]
