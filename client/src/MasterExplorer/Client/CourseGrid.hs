module MasterExplorer.Client.CourseGrid
  ( courseGrid
  ) where

import qualified Data.Map                   as M
import qualified Data.List                  as L

import           Data.Map                   (Map)
import           Reflex.Dom

import           MasterExplorer.Common.Data.Course (Course, getCourseCode)
import           MasterExplorer.Common.Data.Slot   (Slot, allSlots)


courseGrid :: forall t m.
  (MonadWidget t m,
   MonadHold t m)
  => Event t (Course, Slot)
  -> m (Event t Course) -- Dynamic t (Map Slot [Course]))
courseGrid courseSelectedEv = do
  coursesMapDyn <- foldDyn update M.empty courseSelectedEv
  grid coursesMapDyn
  where
    update :: (Course, Slot) -> Map Slot [Course] -> Map Slot [Course]
    update (course, slot) repo =
      if course `elem` M.findWithDefault [] slot repo 
      then M.adjust (L.delete course) slot repo
      else M.insertWith (++) slot [course] repo

grid :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t (Map Slot [Course])
  -> m (Event t Course)
grid courseMap = do
  let coursesDyn = toList <$> courseMap
  eventsDyn  <- el "ul" $
    simpleList coursesDyn gridItem
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

  where
    toList :: Map Slot [Course] -> [[Course]]
    toList courseMap = flip (M.findWithDefault []) courseMap <$> L.sort allSlots  
  
gridItem :: forall m t.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Course]
  -> m (Event t Course)
gridItem coursesDyn = do
  eventsDyn <- el "ul" $
    simpleList coursesDyn gridSlotItem
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

gridSlotItem :: forall m t.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t Course
  -> m (Event t Course)
gridSlotItem courseDyn =
  dyn $ widget <$> courseDyn
  where
    widget :: MonadWidget t m => Course -> m Course
    widget course = do
      l <- link $ getCourseCode course
      return course
