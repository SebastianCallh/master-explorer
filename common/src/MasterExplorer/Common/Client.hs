{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Common.Client
 ( apiClient
 , getCourses
 , postCourses
 ) where

import qualified Data.Text                          as T

import           Control.Exception.Base             (displayException)
import           Data.Bifunctor                     (bimap)
import           Data.Text                          (Text)
import           MasterExplorer.Common.Api          (courseApi)
import           MasterExplorer.Common.Data.Course  (Course)
import           MasterExplorer.Common.Data.Program (Program)
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Servant
import           Servant.Client                     (BaseUrl (..),
                                                     ClientEnv (..), ClientM,
                                                     Scheme (..),
                                                     ServantError (..), client,
                                                     runClientM)
type Response a = IO (Either Text a)


getNumber :: ClientEnv -> Response Int
getNumber client = runClient client getNumber'

getCourses :: ClientEnv -> Program -> Response [Course]
getCourses client program = runClient client $ getCourses' program

postCourses :: ClientEnv -> [Course] -> Response Bool
postCourses client courses = runClient client $ postCourses' courses

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

getCourses'  :: Program  -> ClientM [Course]
postCourses' :: [Course] -> ClientM Bool
getNumber'   ::             ClientM Int
getNumber' :<|> getCourses' :<|> postCourses' = client courseApi
