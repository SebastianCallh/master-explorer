{-# LANGUAGE RecursiveDo #-}

module MasterExplorer.Client.App
  ( app
  ) where


import qualified Data.Map   as M

import           Control.Lens
import           Servant.Reflex                         (BaseUrl)
import           Reflex.Dom.Extended
import           MasterExplorer.Common.Data.Program     (engPrograms)
import           MasterExplorer.Common.Data.Schedule   

import qualified MasterExplorer.Client.Api               as API
import qualified MasterExplorer.Client.EmptyContent      as EC
import qualified MasterExplorer.Client.Content           as C
import qualified MasterExplorer.Client.CourseList        as CL
import qualified MasterExplorer.Client.ProgramList       as PL
import qualified MasterExplorer.Client.ScheduleApiMenu   as SAM

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn = do
  api <- API.widget apiUrlDyn
  divClass "container" $ do
    programList <- divClass "header" $ do
      divClass "logo" $ text "Master Explorer"
      let getCourses  = api ^. API.getProgramCourses
      let programsDyn = constDyn engPrograms
      PL.widget getCourses programsDyn

    courseList <- divClass "sidebar" $ do
      let coursesDyn = programList ^. PL.selectedCourses
      CL.widget coursesDyn

    _ <- divClass "content" $ do
      rec 
        content <- dyn $ ffor (programList ^. PL.selectedProgram) $ \case
          Nothing -> EC.widget
          Just _  -> C.widget scheduleDyn      
          
        scheduleApiMenu <- SAM.widget
          (api ^. API.saveSchedule)
          (api ^. API.loadSchedule)
          scheduleDyn
        
        scheduleDyn <- foldDynMaybe const (Schedule M.empty) $ leftmost
          [ Just . Schedule <$> updated (courseList ^. CL.slots)
          , scheduleApiMenu ^. SAM.onLoad
          ]
          
      return content
          
    divClass "footer" $ pure ()
  
    pure ()
