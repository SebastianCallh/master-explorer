{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Common.Client
 ( apiClient
 , getCourses
 , postCourses
 , getCoursePlan
 , postCoursePlan
 ) where

import qualified Data.Text                             as T

import           Control.Exception.Base                (displayException)
import           Data.Bifunctor                        (bimap)
import           Data.Text                             (Text)
import           MasterExplorer.Common.Api             (courseApi)
import           MasterExplorer.Common.Data.Course     (Course)
import           MasterExplorer.Common.Data.CoursePlan (CoursePlan)
import           MasterExplorer.Common.Data.Program    (Program)
import           Network.HTTP.Client                   (defaultManagerSettings,
                                                        newManager)
import           Servant
import           Servant.Client                        (BaseUrl (..),
                                                        ClientEnv (..), ClientM,
                                                        Scheme (..),
                                                        ServantError (..),
                                                        client, runClientM)
type Response a = IO (Either Text a)

getCourses :: ClientEnv -> Program -> Response [Course]
getCourses clientEnv program = runClient clientEnv $ getCourses' program

postCourses :: ClientEnv -> [Course] -> Response Bool
postCourses clientEnv courses = runClient clientEnv $ postCourses' courses

postCoursePlan :: ClientEnv -> CoursePlan -> Response Int
postCoursePlan clientEnv schedule = runClient clientEnv $ postCoursePlan' schedule

getCoursePlan :: ClientEnv -> Int -> Response (Maybe CoursePlan)
getCoursePlan clientEnv scheduleId = runClient clientEnv $ getCoursePlan' scheduleId

runClient :: ClientEnv -> ClientM a -> Response a
runClient env query = do
  eresult <- runClientM query env
  return $ bimap errorToText id eresult

errorToText :: ServantError -> Text
errorToText = T.pack . displayException

apiClient :: String -> Int -> IO ClientEnv
apiClient url port =
  ClientEnv
  <$> manager
  <*> pure localUrl
  where
    manager  = newManager defaultManagerSettings
    localUrl = BaseUrl Http url port ""

getCourses'     :: Program  -> ClientM [Course]
postCourses'    :: [Course] -> ClientM Bool
postCoursePlan' :: CoursePlan -> ClientM Int
getCoursePlan'  :: Int -> ClientM (Maybe CoursePlan)
getCourses' :<|> postCourses' :<|> postCoursePlan' :<|> getCoursePlan'= client courseApi
