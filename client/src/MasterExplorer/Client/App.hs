{-# LANGUAGE RecursiveDo #-}

module MasterExplorer.Client.App
  ( app
  ) where

import           Control.Lens
import           Servant.Reflex                         (BaseUrl)
import           Reflex.Dom.Extended
import           MasterExplorer.Common.Data.Course      (Course)
import           MasterExplorer.Common.Data.Program     (Program, engPrograms)
import           MasterExplorer.Common.Data.Schedule    (Schedule (..))
import qualified MasterExplorer.Common.Data.Schedule       as Schedule

import           MasterExplorer.Client.Data.CourseStatuses (CourseStatuses)
import qualified MasterExplorer.Client.Api                  as API
import qualified MasterExplorer.Client.Content              as C
import qualified MasterExplorer.Client.CourseList           as CL
import qualified MasterExplorer.Client.ProgramList          as PL
import qualified MasterExplorer.Client.ScheduleApiMenu      as SAM
import qualified MasterExplorer.Client.Data.CourseStatuses  as CourseStatuses

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn = do
  api <- API.widget apiUrlDyn
  divClass "container" $ do
    rec
      programList <- header api
      courseList  <- sidebar availableCoursesDyn courseStatusesDyn
      (contentDyn, scheduleApiMenu) <- content
        scheduleDyn
        api
        (courseList ^. CL.focusedCourse)
        (updated $ programList ^. PL.selectedProgram)
      
      let availableCoursesDyn = programList ^. PL.selectedCourses
      let contentRemovedCourseEv = (switch . current $ view C.onCourseRemoved <$> contentDyn)
      
      courseStatusesDyn <- foldDyn ($) CourseStatuses.empty $ leftmost
        [ CourseStatuses.set CourseStatuses.Selected . snd <$> courseList ^. CL.onCourseSelect
        , CourseStatuses.set CourseStatuses.Available      <$> courseList ^. CL.onCourseDeselect
        , CourseStatuses.set CourseStatuses.InSelection    <$> courseList ^. CL.onCoursePreselect
        , CourseStatuses.set CourseStatuses.Available      <$> contentRemovedCourseEv
        , const . CourseStatuses.fromSchedule <$> scheduleSuccesfullyLoadedEv
        ]

      let scheduleSuccesfullyLoadedEv = fmapMaybe id $ scheduleApiMenu ^. SAM.onLoad
      
      scheduleDyn <- foldDyn ($) Schedule.empty $ leftmost
        [ uncurry Schedule.addSelection <$> courseList ^. CL.onCourseSelect
        , Schedule.removeSelection <$> courseList ^. CL.onCourseDeselect
        , const <$> scheduleSuccesfullyLoadedEv
        , Schedule.removeSelection <$> contentRemovedCourseEv
        ]
    
    divClass "footer" $ pure ()
    
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
  => Dynamic t [Course]

  -> Dynamic t CourseStatuses
  -> m (CL.CourseList t)
sidebar coursesDyn statusesDyn =
  divClass "sidebar" $ 
    CL.widget coursesDyn statusesDyn

content :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule
  -> API.Api t m
  -> Dynamic t (Maybe Course)
  -> Event t (Maybe Program)
  -> m (Dynamic t (C.Content t), SAM.ScheduleApiMenu t)
content scheduleDyn api focusedCourseDyn programSelectedEv =
  divClass "content" $ do
    contentDyn <- widgetHold C.empty $ ffor programSelectedEv $ \case
        Nothing -> C.empty
        Just _  -> C.widget
                   scheduleDyn
                   focusedCourseDyn
                   
    scheduleApiMenu <- SAM.widget
       (api ^. API.saveSchedule)
       (api ^. API.loadSchedule)
       scheduleDyn
  
    return (contentDyn, scheduleApiMenu)
