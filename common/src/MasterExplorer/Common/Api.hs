{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module MasterExplorer.Common.Api
    ( CourseAPI
    , courseApi
    ) where

import           MasterExplorer.Common.Data.Course  (Course)
import           MasterExplorer.Common.Data.Program (Program)
import           Servant                            ((:<|>), (:>), Capture, Get,
                                                     JSON, Post, Proxy (..),
                                                     ReqBody)

type CourseAPI = "number"                                     :> Get  '[JSON] Int
            :<|> "getCourses"    :> Capture "program" Program :> Get  '[JSON] [Course]
            :<|> "updateCourses" :> ReqBody '[JSON] [Course]  :> Post '[JSON] Bool

courseApi :: Proxy CourseAPI
courseApi = Proxy
