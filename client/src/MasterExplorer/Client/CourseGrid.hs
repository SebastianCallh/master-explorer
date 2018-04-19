{-# LANGUAGE RecordWildCards #-}

module MasterExplorer.Client.CourseGrid
  ( courseGrid
  ) where

import qualified Data.Map                   as M
import qualified Data.List                  as L

import           Data.Semigroup             ((<>))
import           Data.Map                   (Map)
import           Reflex.Dom

import           MasterExplorer.Common.Data.Course   (Course, getCourseCode)
import           MasterExplorer.Common.Data.Slot     (Slot (..))
import           MasterExplorer.Common.Data.Block    (allBlocks)
import           MasterExplorer.Common.Data.Period   (Period, allPeriods)
import           MasterExplorer.Common.Data.Semester (Semester, masterSemesters)
import           MasterExplorer.Common.Class.Pretty  (Pretty, pretty)

courseGrid :: forall t m.
  MonadWidget t m
  => Event t (Course, Slot)
  -> m (Event t Course)
courseGrid courseSelectedEv = do
  coursesMapDyn <- foldDyn update M.empty courseSelectedEv
  grid coursesMapDyn
  where
    update :: (Course, Slot) -> Map Slot [Course] -> Map Slot [Course]
    update (course, slot) repo =
      if course `elem` M.findWithDefault [] slot repo 
      then M.adjust (L.delete course) slot repo
      else M.insertWith (++) slot [course] repo

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
  
grid :: forall t m.
  MonadWidget t m
  => Dynamic t (Map Slot [Course])
  -> m (Event t Course)
grid courseMap = do
  eventsDyn <- divClass "course-grid" $ do
    
    _ <- divClass "grid-col" $ do
      let blocksDyn = constDyn allBlocks

      divClass "grid-header" $ pure ()
      simpleList blocksDyn $ \block ->
        divClass "row-header" $
          dynText $ pretty <$> block
    
    let rowData = (\sp -> (sp, semesterPeriodsSlots sp)) <$> semesterPeriods
    simpleList (constDyn rowData) $ gridCol courseMap
      
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

gridCol :: forall m t.
  MonadWidget t m
  => Dynamic t (Map Slot [Course])
  -> Dynamic t (SemesterPeriod, [Slot])
  -> m (Event t Course)
gridCol courseMap periodSlotsDyn = do
  let slotsDyn       = snd <$> periodSlotsDyn
  let semesterPeriod = fst <$> periodSlotsDyn

  eventsDyn <- divClass "grid-col" $ do
    divClass "grid-header" $
      dynText $ pretty <$> semesterPeriod
    
    simpleList slotsDyn $ \slot -> do
      let courses = M.findWithDefault [] <$> slot <*> courseMap
      gridItem courses

  return $ switchPromptlyDyn $ leftmost <$> eventsDyn
    
gridItem :: forall m t.
  MonadWidget t m
  => Dynamic t [Course]
  -> m (Event t Course)
gridItem coursesDyn = do
  eventsDyn <- divClass "grid-item" $
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
      l <- link $ getCourseCode course
      return course
