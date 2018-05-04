module MasterExplorer.Client.App
  ( app
  ) where

import           Reflex.Dom.Extended
import           Servant.Reflex                         (BaseUrl)

import           MasterExplorer.Common.Data.Program     (engPrograms)
import           MasterExplorer.Client.ProgramList      (programList)
import           MasterExplorer.Client.Content          (content)
import           MasterExplorer.Client.Api              (programCourses)
import           MasterExplorer.Client.CourseList       (courseList)

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn =
  divClass "container" $ do
    programSelectEv <- divClass "header" $
      programList engPrograms
  
    coursesEv  <- programCourses apiUrlDyn programSelectEv
    coursesDyn <- holdDyn [] coursesEv
  
    courseListEv <- divClass "sidebar" $
      courseList coursesDyn
    
    divClass "info-bar" $
      text "HP: 12"
  
    _ <- divClass "content" $
      content courseListEv

    divClass "footer" $ pure ()
  
    pure ()
