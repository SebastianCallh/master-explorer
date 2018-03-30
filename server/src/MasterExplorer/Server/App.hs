{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Server.App
    ( app
    , migrateDb
    , getCourses
    , updateCourses
    ) where

import qualified Data.Map.Strict                    as M
import qualified MasterExplorer.Server.Db           as Db

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks, runReaderT)
import           Data.List                          (sort)
import           Data.Maybe                         (fromJust, mapMaybe)
import           Database.Persist                   (Entity (..))
import           Database.Persist.Postgresql        (entityKey, entityVal,
                                                     runMigration,
                                                     runSqlPersistMPool)
import           MasterExplorer.Common.Api          (CourseAPI, courseApi)
import           MasterExplorer.Common.Data.Course  (Course (..))
import           MasterExplorer.Common.Data.Program (Program)
import           MasterExplorer.Server.Config       (App (..), Config (..))
import           Network.Wai                        (Application)
import           Network.Wai.Middleware.Cors        (simpleCors)
import           Safe                               (headMay)
import           Servant


app :: Config -> Application
app = simpleCors . serve courseApi . server

server :: Config -> Server CourseAPI
server conf = enter (appToHandler conf) appServerT

appToHandler :: Config -> App :~> Handler
appToHandler conf = NT $ appToHandler' conf

appToHandler' :: Config -> App a -> Handler a
appToHandler' conf = flip runReaderT conf . runApp

migrateDb :: Config -> IO ()
migrateDb config =
  flip runSqlPersistMPool (getPool config) $ runMigration Db.migrateAll

number :: App Int
number = return 666

appServerT :: ServerT CourseAPI App
appServerT = number :<|> getCourses :<|> updateCourses

getCourses :: Program -> App [Course]
getCourses prog = do
  results  <- Db.selectCourses prog
  return $ toCourses results
--  let (dbecourses, dbeprograms) = unzip results
--  let x = sort dbecourses
--  let y = foldr (\(c, p) -> M.insert (entityVal c) p) M.empty $ results
--  let coursMap = toCourses (headMay dbecourses) dbeprograms
--  let courses = _ x
--  liftIO $ print courses
--  return courses

updateCourses :: [Course] -> App Bool
updateCourses courses = do
  liftIO $ print $ mconcat ["Recieved ", show courses]

  -- fromjust because updateCourses assures the courses exist
  _ <- Db.updateCourses courses
  courses'  <- traverse (fmap fromJust . Db.selectCourse . courseCode) courses
  programs' <- traverse (Db.updateProgram . entityVal) $ snd <$> courses'
  return True

{-
toCourses :: Entity Db.DbCourse -> [Entity Db.DbProgram] -> Course
toCourses edbcours edbprogs =
  undefined
-}

newtype OrderedDbCourse = ODBC { getDbCourse :: Db.DbCourse }

instance Eq OrderedDbCourse where
  (==) a b = code a == code b

instance Ord OrderedDbCourse where
  (<=) a b = code a <= code b

code = Db.dbCourseCode . getDbCourse

-- (Entity DbCourse, Entity DbProgram)
toCourses :: [(Entity Db.DbCourse, Entity Db.DbProgram)] -> [Course] --M.Map Course [Program]
toCourses = M.elems . M.mapWithKey toCourse . foldr insert M.empty
  where
    insert (c, p) = M.insertWith (++) (ODBC $ entityVal c) [entityVal p]
    toCourse :: OrderedDbCourse -> [Db.DbProgram] -> Course
    toCourse odbc dbprogs = Db.fromDbCourse (getDbCourse odbc) (Db.fromDbProgram <$> dbprogs)

{-
toCourse :: Db.SelectResult -> Maybe Course
toCourse = fmap toCourse'
  where toCourse' (Entity _ dbcourse, Entity _ dbprogs) =
          Db.fromDbCourse dbcourse dbprogs
-}
  {-
  let programs = concat $ mapMaybe coursePrograms courses'
  programs' <- traverse (Db.updateProgram . Db.toDbProgram) programs
  let cps = zipWith Db.CourseProgram (entityKey <$> courses') (entityKey <$> programs')
  cps' <- Db.insertCoursePrograms cps
-}
{-  programs' <- traverse Db.insertPrograms $ mapMaybe coursePrograms courses'
  let coursePrograms = CourseProgram
  coursePrograms' <- Db.insertCourseProgram
  liftIO $ print "updated courses"
  return $ length courses' == length courses-}
