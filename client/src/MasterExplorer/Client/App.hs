module MasterExplorer.Client.App
  ( app
  ) where


import           Reflex.Dom.Extended
import           Servant.Reflex                         (BaseUrl)
import           Data.Text                              (Text)

import           MasterExplorer.Common.Data.Program     (engPrograms)
--import           MasterExplorer.Client.Data.AppState    (AppState) --FocusStatus (FocusStauts)
import           MasterExplorer.Client.ProgramList      (programList)
import           MasterExplorer.Client.Content          (content)
import           MasterExplorer.Client.Api              (programCourses)
import           MasterExplorer.Client.CourseList       (CourseList (..), courseList)

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn =
  divClass "container" $ do
    programSelectEv <- divClass "header" $ do
      divClass "logo" $ text "Master Explorer"        
      programList engPrograms

    coursesEv  <- programCourses apiUrlDyn programSelectEv
    coursesDyn <- holdDyn [] coursesEv
    
    cl <- divClass "sidebar" $
      courseList coursesDyn
      
    _ <- divClass "content" $
      content
        (_courseList_slots cl)
        (_courseList_onMouseEnter cl)
        (_courseList_onMouseLeave cl)

    divClass "footer" $ pure ()
  
    pure ()
