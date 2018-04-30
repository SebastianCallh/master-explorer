module MasterExplorer.Client.CourseInfo
  ( CourseInfo
  , CourseInfoEvent (..)
  , courseInfo
  ) where

import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.Course     (Course, getCourseName)

data CourseInfo = CourseInfo

data CourseInfoEvent = CourseInfoEvent

courseInfo :: forall t m.
  MonadWidget t m
  => Dynamic t Course
  -> m (Event t CourseInfoEvent)
courseInfo courseDyn = do
  dynText $ getCourseName <$> courseDyn
  e <- button "back"
  return $ CourseInfoEvent <$ e
