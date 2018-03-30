{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MasterExplorer.Server.Db
  ( CourseProgram (..)
  , DbCourse (..)
  , DbProgram
  , SelectResult
  , migrateAll
--  , insertCourse
  , selectCourse
  , selectCourses
  , updateCourse
  , updateCourses
  , updateProgram
  , insertCoursePrograms
  , fromDbCourse
  , fromDbProgram
  , toDbProgram
  ) where

import           Control.Applicative             (liftA2)
import           Control.Monad.IO.Class          (liftIO)           
import           Control.Monad.Reader            (ReaderT, asks)
import           Database.Persist.TH             (mkMigrate, mkPersist,
                                                  persistLowerCase, share,
                                                  sqlSettings)
import           Database.Esqueleto
import           Data.Maybe                      (mapMaybe)
import           Safe                            (headMay)


import           MasterExplorer.Server.Config        (Config (..), App)
import           MasterExplorer.Server.Db.Models

-- The field for unique code should be the only unique field
-- on Course or the upsert will fail :(
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbCourse
  code          CourseCode
  name          CourseName Maybe
  urls          [Url]
  credits       Credits Maybe
  level         Level Maybe
  slots         [Slot]
  importance    Importance Maybe
  areas         [Area]
  institution   Institution Maybe
  field         Field Maybe
  prerequisites Prerequisites Maybe
  grades        Grading Maybe
  subject       [Subject]
  examinator    Examinator Maybe
  examinations  [Examination]
  content       CourseContent Maybe
  selfStudyTime Hours Maybe
  scheduledTime Hours Maybe
  UniqueDbCourse code

DbProgram
  code       ProgramCode
  slug       ProgramSlug
  UniqueDbProgram code
  
CourseProgram
  courseId  DbCourseId
  programId DbProgramId
|]

  --  UniqueCourseProgram courseId programId

runDb :: ReaderT SqlBackend IO a -> App a
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

type SelectResult = (Entity DbCourse, Entity DbProgram)

selectCourse :: CourseCode -> App (Maybe SelectResult)
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
    where_ $ p ?. DbProgramCode ==. just (val (programCode program))
    return (c, p)
  return $ mapMaybe unwrap res

unwrap :: (Maybe a, Maybe b) -> Maybe (a, b)
unwrap = uncurry (liftA2 (,))

updateCourses :: [Course] -> App [Entity DbCourse]
updateCourses courses = do
  dbcourses <- traverse (updateCourse . fst . toDbCourse) courses

  let programs = foldMap coursePrograms courses
  dbprograms <- traverse (updateProgram . toDbProgram) programs
  
  let courseKeys  = entityKey <$> dbcourses
  let programKeys = entityKey <$> dbprograms
  let coursePrograms = zipWith CourseProgram courseKeys programKeys  
  _pcs <- insertCoursePrograms coursePrograms
  return dbcourses

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
  { dbProgramCode = programCode
  , dbProgramSlug = programSlug
  }

fromDbProgram :: DbProgram -> Program
fromDbProgram DbProgram{..} = Program
  { programCode = dbProgramCode
  , programSlug = dbProgramSlug
  }

-- Marshalling between db and otw course
  
toDbCourse :: Course -> (DbCourse, [Program])
toDbCourse Course{..} =
  (DbCourse
    { dbCourseCode          = courseCode
    , dbCourseUrls          = courseUrls
    , dbCourseName          = courseName
    , dbCourseCredits       = courseCredits
    , dbCourseLevel         = courseLevel        
    , dbCourseSlots         = courseSlots        
    , dbCourseImportance    = courseImportance   
    , dbCourseAreas         = courseAreas        
    , dbCourseInstitution   = courseInstitution  
    , dbCourseField         = courseField        
    , dbCoursePrerequisites = coursePrerequisites
    , dbCourseGrades        = courseGrades       
    , dbCourseExaminator    = courseExaminator   
    , dbCourseExaminations  = courseExaminations
    , dbCourseContent       = courseContent      
    , dbCourseSubject       = courseSubject      
    , dbCourseSelfStudyTime = courseSelfStudyTime
    , dbCourseScheduledTime = courseScheduledTime
    }, coursePrograms)

fromDbCourse :: DbCourse -> [Program] -> Course
fromDbCourse DbCourse{..} programs = Course
  { courseCode          = dbCourseCode
  , courseName          = dbCourseName
  , courseUrls          = dbCourseUrls
  , courseCredits       = dbCourseCredits
  , courseLevel         = dbCourseLevel
  , courseSlots         = dbCourseSlots
  , courseImportance    = dbCourseImportance
  , courseAreas         = dbCourseAreas 
  , courseInstitution   = dbCourseInstitution
  , coursePrograms      = programs
  , courseField         = dbCourseField
  , coursePrerequisites = dbCoursePrerequisites
  , courseGrades        = dbCourseGrades
  , courseExaminator    = dbCourseExaminator
  , courseExaminations  = dbCourseExaminations
  , courseContent       = dbCourseContent
  , courseSubject       = dbCourseSubject
  , courseSelfStudyTime = dbCourseSelfStudyTime
  , courseScheduledTime = dbCourseScheduledTime
  } 
