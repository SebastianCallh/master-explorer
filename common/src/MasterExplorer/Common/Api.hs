{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Common.Api
    ( CourseAPI
    , courseApi
    ) where

import           MasterExplorer.Common.Data.Course     (Course)
import           MasterExplorer.Common.Data.CoursePlan (CoursePlan)
import           MasterExplorer.Common.Data.Program    (Program)
import           Servant                               ((:<|>), (:>), Capture,
                                                        Get, JSON, Post,
                                                        Proxy (..), ReqBody)

type CourseAPI = "getCourses"     :> Capture "program" Program       :> Get  '[JSON] [Course]
            :<|> "updateCourses"  :> ReqBody '[JSON] [Course]        :> Post '[JSON] Bool
            :<|> "saveCoursePlan" :> Capture "coursePlan" CoursePlan :> Post '[JSON] Int
            :<|> "loadCoursePlan" :> Capture "scheduleId" Int        :> Post '[JSON] (Maybe CoursePlan)

courseApi :: Proxy CourseAPI
courseApi = Proxy
