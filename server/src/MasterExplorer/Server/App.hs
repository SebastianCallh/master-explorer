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
import           Control.Monad.Reader               (runReaderT)
import           Data.Maybe                         (fromJust)
import           Data.Text                          (Text)
import           Database.Persist                   (Entity (..))
import           Database.Persist.Postgresql        (entityVal, runMigration,
                                                     runSqlPersistMPool)
import           MasterExplorer.Common.Api          (CourseAPI, courseApi)
import           MasterExplorer.Common.Data.Course
import           MasterExplorer.Common.Data.Program (Program)
import           MasterExplorer.Server.Config       (App (..), Config (..))
import           Network.Wai                        (Application)
import           Network.Wai.Middleware.Cors        (simpleCors)
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

updateCourses :: [Course] -> App Bool
updateCourses courses = do
  liftIO $ print $ mconcat ["Recieved ", show courses]

  -- fromjust because updateCourses assures the courses exist
  _ <- Db.updateCourses courses
  courses'   <- traverse (fmap fromJust . Db.selectCourse . _courseCode) courses
  _programs' <- traverse (Db.updateProgram . entityVal) $ snd <$> courses'
  return True

newtype OrderedDbCourse = ODBC { getDbCourse :: Db.DbCourse }

instance Eq OrderedDbCourse where
  (==) a b = code a == code b

instance Ord OrderedDbCourse where
  (<=) a b = code a <= code b

code :: OrderedDbCourse -> Text
code = Db.dbCourseCode . getDbCourse

toCourses :: [(Entity Db.DbCourse, Entity Db.DbProgram)] -> [Course]
toCourses = M.elems . M.mapWithKey toCourse . foldr insert M.empty
  where
    insert (c, p) = M.insertWith (++) (ODBC $ entityVal c) [entityVal p]
    toCourse :: OrderedDbCourse -> [Db.DbProgram] -> Course
    toCourse odbc dbprogs = Db.fromDbCourse (getDbCourse odbc) (Db.fromDbProgram <$> dbprogs)
