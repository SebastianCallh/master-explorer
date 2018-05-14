{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseGrid
  ( CourseGrid
  , courseGrid_selections
  , courseGrid_slotsInFocus
  , courseGrid_onCourseSelected
  , courseGrid_onCourseRemoved
  , CourseGridEvent (..)
  , courseGrid
  ) where

import qualified Data.Map                   as M

import           Control.Lens
import           Data.Semigroup             ((<>))
import           Data.Map                   (Map)
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.Schedule   
import           MasterExplorer.Client.ColGrid         (colGrid)
import           MasterExplorer.Common.Data.Period     (Period)
import           MasterExplorer.Common.Data.Semester   (Semester)
import           MasterExplorer.Common.Data.Course     
import           MasterExplorer.Common.Data.Slot       (Slot (..), slotsInPeriod)
import           MasterExplorer.Common.Data.Occasion   (getOccasion)

data CourseGridEvent
  = CourseSelected Course
  | CourseRemoved Course

data CourseGrid t = CourseGrid
  { _courseGrid_selections       :: !(Dynamic t (Map Slot [Course]))
  , _courseGrid_slotsInFocus     :: !(Dynamic t [Slot])
  , _courseGrid_onCourseSelected :: !(Event t Course)
  , _courseGrid_onCourseRemoved  :: !(Event t Course)
  }
  
makeLenses ''CourseGrid

-- | Grid view of all master semesters and
--   currently selected courses.
courseGrid :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule
--  -> Event t Course
--  -> Event t Course
  -> m (CourseGrid t)
courseGrid scheduleDyn = do -- mouseEnterCourseEv mouseLeaveCourseEv = do
  let f c = const $ c ^. courseOccasions >>= getOccasion        
  let slotsInFocus = constDyn [] -- foldDyn f [] $ leftmost [mouseLeaveCourseEv, mouseEnterCourseEv]
  event        <- gridWidget scheduleDyn

  let onCourseSelected = fforMaybe event $ \case
        (CourseSelected c) -> Just c
        _                  -> Nothing

  let onCourseRemoved = fforMaybe event $ \case
        (CourseRemoved c) -> Just c
        _                    -> Nothing
     
  return CourseGrid
    { _courseGrid_selections       = getSchedule <$> scheduleDyn
    , _courseGrid_slotsInFocus     = slotsInFocus
    , _courseGrid_onCourseSelected = onCourseSelected
    , _courseGrid_onCourseRemoved  = onCourseRemoved
    }

gridWidget :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule
--  -> Dynamic t [Slot]
  -> m (Event t CourseGridEvent)
gridWidget scheduleDyn =
  colGrid "block-grid" $ gridCol <$> scheduleDyn

-- | Column of all blocks in a period in a semester.
gridCol :: forall m t.
  MonadWidget t m
  => Schedule -- ^ Current course selections. 
  -> (Semester, Period)  -- ^ The current column.
  -- Dynamic t [Slot]              -- ^ Slots in this column in ascending order.
  -> m (Event t CourseGridEvent)
gridCol schedule column = do
  let slotsDyn = constDyn $ uncurry slotsInPeriod column
  eventsDyn <- simpleList slotsDyn $ \slotDyn -> do
    ev <- dyn $ gridItem schedule <$> slotDyn
    switchPromptly never ev
  return . switch . current $ leftmost <$> eventsDyn

gridItem :: forall m t.
  MonadWidget t m
  => Schedule
--  -> [Slot]
  -> Slot
  -> m (Event t CourseGridEvent)
gridItem schedule slot = do
  let courses  = M.findWithDefault [] slot (getSchedule schedule)
  let adjustStyle = case courses of
        [] -> M.adjust ("grid-slot empty "     <>) "class"
        _  -> M.adjust ("grid-slot non-empty " <>) "class"

  let emptyStyle =  ("class" =: "")

  elAttr "div" (adjustStyle emptyStyle)  $ do
    let widgetMap = foldr makeWidgetMap M.empty courses 
    courseEv <- eventTabDisplay "course-list" "active-course" widgetMap
    return $ CourseSelected <$> courseEv
    
  where    
    makeWidgetMap c = M.insert (c ^. courseCode)
      (c ^. courseCode,
        divClass "visible-course" $ do
          l <- button $ c ^. courseName 
          return $ c <$ l)
