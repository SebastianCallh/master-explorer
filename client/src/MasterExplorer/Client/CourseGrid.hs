{-# LANGUAGE RecordWildCards #-}

module MasterExplorer.Client.CourseGrid
  ( courseGrid
  ) where


import qualified Data.Map                   as M
import qualified Data.List                  as L

import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import           Data.Map                   (Map)
import           Reflex.Dom

import           MasterExplorer.Client.CourseList    (CourseListEvent (..))
import           MasterExplorer.Common.Data.Course   (Course, getCourseCode, courseSlots)
import           MasterExplorer.Common.Data.Slot     (Slot (..))
import           MasterExplorer.Common.Data.Block    (allBlocks)
import           MasterExplorer.Common.Data.Period   (Period, allPeriods)
import           MasterExplorer.Common.Data.Semester (Semester, masterSemesters)
import           MasterExplorer.Common.Class.Pretty  (Pretty, pretty)


data CourseGrid = CourseGrid
  { gridSlots :: !(Map Slot [Course])
  , gridFocus :: !(Map Slot (Map Text Text))
  }

emptyGrid :: CourseGrid
emptyGrid = CourseGrid
  { gridSlots = M.empty
  , gridFocus = M.empty
  }

courseGrid :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m (Event t Course)
courseGrid courseSelectedEv = do
  coursesGridDyn <- foldDyn update emptyGrid courseSelectedEv
  gridWidget coursesGridDyn
  where
    update :: CourseListEvent -> CourseGrid -> CourseGrid
    update (CourseSelected   c s) g = g { gridSlots = M.insertWith (++) s [c] $ gridSlots g }
    update (CourseDeselected c s) g = g { gridSlots = M.adjust (L.delete c) s $ gridSlots g }
    update (CourseMouseEnter c  ) g = g { gridFocus = foldr addFocus    (gridFocus g) $ courseSlots c }
    update (CourseMouseLeave c  ) g = g { gridFocus = foldr removeFocus (gridFocus g) $ courseSlots c }

    removeFocus = M.delete
    addFocus s  = M.insert s ("class" =: "grid-item focused")

data SemesterPeriod = SemesterPeriod
  { spSemester :: !Semester
  , spPeriod   :: !Period
  }

instance Pretty SemesterPeriod where
  pretty SemesterPeriod{..} =
    pretty spSemester <> pretty spPeriod

semesterPeriods :: [SemesterPeriod]
semesterPeriods =
  SemesterPeriod  <$>
  masterSemesters <*>
  allPeriods

semesterPeriodsSlots :: SemesterPeriod -> [Slot]
semesterPeriodsSlots SemesterPeriod{..} =
  Slot spSemester spPeriod <$> allBlocks
  
gridWidget :: forall t m.
  MonadWidget t m
  => Dynamic t CourseGrid
  -> m (Event t Course)
gridWidget grid = do
  eventsDyn <- divClass "course-grid" $ do
    
    _ <- divClass "grid-col" $ do
      let blocksDyn = constDyn allBlocks

      divClass "grid-header" $ pure ()
      simpleList blocksDyn $ \block ->
        divClass "row-header" $
          dynText $ pretty <$> block
    
    let rowData = (\sp -> (sp, semesterPeriodsSlots sp)) <$> semesterPeriods
    simpleList (constDyn rowData) $ gridCol grid
      
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

gridCol :: forall m t.
  MonadWidget t m
  => Dynamic t CourseGrid
  -> Dynamic t (SemesterPeriod, [Slot])
  -> m (Event t Course)
gridCol gridDyn periodSlotsDyn = do
  let slotsDyn       = snd <$> periodSlotsDyn
  let semesterPeriod = fst <$> periodSlotsDyn

  eventsDyn <- divClass "grid-col" $ do
    divClass "grid-header" $
      dynText $ pretty <$> semesterPeriod
    
    simpleList slotsDyn $ \slot -> do
      let courses = M.findWithDefault []                       <$> slot <*> (gridSlots <$> gridDyn)
      let style   = M.findWithDefault ("class" =: "grid-item") <$> slot <*> (gridFocus <$> gridDyn)
      gridItem style courses

  return $ switchPromptlyDyn $ leftmost <$> eventsDyn
    
gridItem :: forall m t.
  MonadWidget t m
  => Dynamic t (Map Text Text)
  -> Dynamic t [Course]
  -> m (Event t Course)
gridItem styleDyn coursesDyn = do
  eventsDyn <- elDynAttr "div" styleDyn $
    simpleList coursesDyn gridSlotItem
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

gridSlotItem :: forall m t.
  MonadWidget t m  
  => Dynamic t Course
  -> m (Event t Course)
gridSlotItem courseDyn =
  dyn $ widget <$> courseDyn
  where
    widget :: MonadWidget t m => Course -> m Course
    widget course = do
      _ <- link $ getCourseCode course
      return course
