{-# LANGUAGE RecursiveDo #-}

module MasterExplorer.Client.App
  ( app
  ) where


import qualified Data.Map   as M

import           Control.Lens
import           Servant.Reflex                         (BaseUrl)
import           Reflex.Dom.Extended
import           MasterExplorer.Common.Data.Program     (Program, engPrograms)
import           MasterExplorer.Common.Data.Course      (Course)
import           MasterExplorer.Common.Data.Schedule    (Schedule (..))

import qualified MasterExplorer.Client.Api               as API
import qualified MasterExplorer.Client.EmptyContent      as EC
import qualified MasterExplorer.Client.Content           as C
import qualified MasterExplorer.Client.CourseList        as CL
import qualified MasterExplorer.Client.ProgramList       as PL
import qualified MasterExplorer.Client.ScheduleApiMenu   as SAM
import qualified MasterExplorer.Common.Data.Schedule     as Schedule

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn = do
  api <- API.widget apiUrlDyn
  divClass "container" $ do
    rec
      programList <- header api      
      courseList  <- sidebar programList        
      scheduleDyn <- content programList courseList api          
      return ()
          
    divClass "footer" $ pure ()
  
    pure ()
    
header :: forall t m.
  MonadWidget t m
  => API.Api t m  
  -> m (PL.ProgramList t)
header api =
  divClass "header" $ do
    divClass "logo" $ text "Master Explorer"
    let getCourses  = api ^. API.getProgramCourses
    let programsDyn = constDyn engPrograms
    PL.widget getCourses programsDyn

sidebar :: forall t m.
  MonadWidget t m
  => PL.ProgramList t
  -> m (CL.CourseList t)
sidebar programList =
  divClass "sidebar" $ 
    CL.widget  $ programList ^. PL.selectedCourses

content :: forall t m.
  MonadWidget t m
  => PL.ProgramList t
  -> CL.CourseList t
  -> API.Api t m
  -> m (Dynamic t Schedule)
content programList courseList api  =
  divClass "content" $ mdo --
    _ <- dyn $ ffor (programList ^. PL.selectedProgram) $ \case
      Nothing -> EC.widget
      Just _  -> C.widget
                 scheduleDyn
                 (courseList ^. CL.focusedCourse)
                     
    scheduleApiMenu <- SAM.widget
      (api ^. API.saveSchedule)
      (api ^. API.loadSchedule)
      scheduleDyn

    let setScheduleEv = const <$> scheduleApiMenu ^. SAM.onLoad

    -- Need to inject the output of the function into Maybe
    let addCourseToScheduleEv = fmap pure .
          uncurry Schedule.addSelection <$>
          (courseList ^. CL.onCourseSelect)
          
    scheduleDyn <- foldDynMaybe ($) (Schedule M.empty) $ leftmost
        [ addCourseToScheduleEv
        , setScheduleEv
        ]
  
    return scheduleDyn
