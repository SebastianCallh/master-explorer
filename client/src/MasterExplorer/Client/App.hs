{-# LANGUAGE RecursiveDo #-}

module MasterExplorer.Client.App
  ( app
  ) where

import qualified Data.Text as T
import qualified Data.Map  as M
import           Control.Lens
import           Reflex.Dom.Extended
import           Servant.Reflex                             (BaseUrl)
import           Text.Read                                  (readMaybe)

import           MasterExplorer.Common.Data.Course          (Course)
import           MasterExplorer.Common.Data.Program         (engPrograms)
import           MasterExplorer.Common.Data.CoursePlan      (CoursePlan (..))
import qualified MasterExplorer.Common.Data.CoursePlan      as CoursePlan

import qualified MasterExplorer.Client.Api                  as API
import qualified MasterExplorer.Client.Content              as C
import qualified MasterExplorer.Client.CourseList           as CL
import qualified MasterExplorer.Client.ProgramList          as PL
import qualified MasterExplorer.Client.Data.CourseStatuses  as CourseStatuses

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn = do
  api <- API.widget apiUrlDyn
  divClass "container" $ do
    rec
      (mCoursePlanHeaderDyn, mCourses) <-
        header api coursePlanListUpdateEv

      courseList <- divClass "sidebar" $ 
        CL.widget  mCourses courseStatusesDyn

      let coursePlanUpdatedSuccEv = fmapMaybe id $ updated mCoursePlanHeaderDyn
      courseStatusesDyn <- foldDyn ($) CourseStatuses.empty $ leftmost
        [ CourseStatuses.set CourseStatuses.Selected . snd <$> courseList ^. CL.onCourseSelect
        , CourseStatuses.set CourseStatuses.Available      <$> courseList ^. CL.onCourseDeselect
        , CourseStatuses.set CourseStatuses.InSelection    <$> courseList ^. CL.onCoursePreselect
        , CourseStatuses.set CourseStatuses.Available      <$> courseContentRemovedEv
        , const . CourseStatuses.fromCoursePlan            <$> coursePlanUpdatedSuccEv
        ]

      let coursePlanListUpdateEv = leftmost
            [ uncurry CoursePlan.addSelection <$> courseList ^. CL.onCourseSelect
            , CoursePlan.removeSelection      <$> courseList ^. CL.onCourseDeselect
            , CoursePlan.removeSelection      <$> courseContentRemovedEv
            ]
            
      let courseContentRemovedEv = switch . current $ view C.onCourseRemoved <$> content
      content <- widgetHold C.empty . ffor (updated mCoursePlanHeaderDyn) $ \case
        Nothing         -> C.empty
        Just coursePlan -> C.widget
          (courseList ^. CL.focusedCourse)
          (constDyn coursePlan)
          (api ^. API.saveCoursePlan)

    pure ()
  
header :: forall t m.
  MonadWidget t m
  => API.Api t m
  -> Event t (CoursePlan -> CoursePlan)
  -> m (Dynamic t (Maybe CoursePlan), Dynamic t [Course])
header api coursePlanListUpdateEv =
  divClass "header" $ do
    divClass "logo" $ text "Master Explorer"
    let getCourses  = api ^. API.getProgramCourses
    let programsDyn = constDyn engPrograms
    programList <- PL.widget getCourses programsDyn

    input <- textInput $ def &
             textInputConfig_attributes .~ constDyn (M.fromList
                                                     [ ("class", "courseplan-id-input")
                                                     , ("placeholder", "id")
                                                     ])
             
    let inputEv = input ^. textInput_input
    let scheduleIdInputEv = fmapMaybe (readMaybe . T.unpack) inputEv          
    scheduleIdDyn <- holdDyn 0 scheduleIdInputEv

    triggerEv <- button "Ladda"
    mCoursePlanLoadedEv <- api ^. API.loadCoursePlan $
      tag (current scheduleIdDyn) triggerEv
      
    let mCoursePlanLoadedSuccEv = fmapMaybe id mCoursePlanLoadedEv
    let mcoursePlanSelectedEv = CoursePlan.make <$> programList ^. PL.onProgramSelect
    mCoursePlanDyn <- foldDyn ($) Nothing $ leftmost
      [ const . pure  <$> mcoursePlanSelectedEv
      , const         <$> mCoursePlanLoadedEv
      , fmap          <$> coursePlanListUpdateEv
      ]

    let selectedProgramCoursesEv = updated $ programList ^. PL.selectedCourses
    loadedProgramCoursesEv <- api ^. API.getProgramCourses $
      view CoursePlan.coursePlanProgram <$> mCoursePlanLoadedSuccEv

    programCoursesDyn <- holdDyn [] $ leftmost
      [ selectedProgramCoursesEv
      , loadedProgramCoursesEv
      ]
       
    return (mCoursePlanDyn, programCoursesDyn) 
