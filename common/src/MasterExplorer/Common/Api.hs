{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Common.Api
    ( CourseAPI
    , courseApi
    ) where

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Data.Schedule (Schedule)
import           Servant                             ((:<|>), (:>), Capture,
                                                      Get, JSON, Post,
                                                      Proxy (..), ReqBody)

type CourseAPI = "getCourses"    :> Capture "program" Program   :> Get  '[JSON] [Course]
            :<|> "updateCourses" :> ReqBody '[JSON] [Course]    :> Post '[JSON] Bool
            :<|> "saveSchedule"  :> Capture "schedule" Schedule :> Post '[JSON] Int
            :<|> "loadSchedule"  :> Capture "scheduleId" Int    :> Post '[JSON] (Maybe Schedule)

courseApi :: Proxy CourseAPI
courseApi = Proxy
