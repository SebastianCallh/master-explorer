{-# LANGUAGE RecursiveDo #-}

module MasterExplorer.Client.App
  ( app
  ) where


import qualified Data.Text  as T
import qualified Data.Map   as M

import           Control.Lens
import           Servant.Reflex                         (BaseUrl)
import           Reflex.Dom.Extended
import           MasterExplorer.Common.Data.Program     (engPrograms)
import           MasterExplorer.Common.Data.Schedule   
import           MasterExplorer.Client.ProgramList
import           MasterExplorer.Client.Content          (content)
import           MasterExplorer.Client.Api              (programCourses, saveSchedule, loadSchedule)
import           MasterExplorer.Client.CourseList
import           MasterExplorer.Client.EmptyContent     (emptyContent)
import           MasterExplorer.Client.ScheduleApiMenu

app :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl 
  -> m ()
app apiUrlDyn =
  divClass "container" $ do
    pl <- divClass "header" $ do
      divClass "logo" $ text "Master Explorer"
      let programEndpoint = programCourses apiUrlDyn
      let programsDyn     = constDyn engPrograms
      programList programEndpoint programsDyn

    cl <- divClass "sidebar" $ do
      let coursesDyn = pl ^. programList_selectedCourses
      courseList coursesDyn

    _ <- divClass "content" $ do
      rec 
        c <- dyn $ ffor (pl ^. programList_selectedProgram) $ \case
          Nothing -> emptyContent
          Just _  -> content scheduleDyn      
          
        sam <- scheduleApiMenu apiUrlDyn scheduleDyn
        scheduleDyn <- foldDynMaybe const (Schedule M.empty) $ leftmost
          [ Just . Schedule <$> updated (cl ^. courseList_slots)
          , sam ^. scheduleApiMenu_onLoad
          ]
          
      return c
          
    divClass "footer" $ pure ()
  
    pure ()
