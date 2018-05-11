{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseGrid
  ( CourseGrid
  , courseGrid_slots
  , courseGrid_slotsInFocus
  , courseGrid_onCourseSelected
  , courseGrid_onCourseRemoved
  , CourseGridEvent (..)
  , courseGrid
  ) where

import qualified Data.Map                   as M
import qualified Data.Text as T

import           Data.Text                  (Text)
import           Control.Lens
import           Data.Semigroup             ((<>))
import           Data.Map                   (Map)
import           Reflex.Dom.Extended

import           MasterExplorer.Client.ColGrid         (colGrid)
import           MasterExplorer.Common.Data.Period     (Period)
import           MasterExplorer.Common.Data.Semester   (Semester)
import           MasterExplorer.Common.Data.Block      (allBlocks)
import           MasterExplorer.Common.Data.Course     
import           MasterExplorer.Common.Data.Slot       (Slot (..), slotsInPeriod)
import           MasterExplorer.Common.Data.Occasion   (getOccasion)

data CourseGridEvent
  = CourseSelected Course
  | CourseRemoved Course

data CourseGrid t = CourseGrid
  { _courseGrid_slots            :: !(Dynamic t (Map Slot [Course]))
  , _courseGrid_slotsInFocus     :: !(Dynamic t [Slot])
  , _courseGrid_onCourseSelected :: !(Event t Course)
  , _courseGrid_onCourseRemoved  :: !(Event t Course)
  }
  
makeLenses ''CourseGrid

-- | Grid view of all master semesters and
--   currently selected courses.
courseGrid :: forall t m.
  MonadWidget t m
  => Dynamic t (Map Slot [Course])
  -> Event t Course
  -> Event t Course
  -> m (CourseGrid t)
courseGrid selectionDyn mouseEnterCourseEv mouseLeaveCourseEv = do
  let f c = const $ c ^. courseOccasions >>= getOccasion        
  slotsInFocus <- foldDyn f [] $ leftmost $
    [traceEventWith (const "leaving!") $ mouseLeaveCourseEv,
     traceEventWith (const "entering!") $ mouseEnterCourseEv]
  event        <- gridWidget selectionDyn

  let onCourseSelected = fforMaybe event $ \case
        (CourseSelected c) -> Just c
        _                  -> Nothing

  let onCourseRemoved = fforMaybe event $ \case
        (CourseRemoved c) -> Just c
        _                    -> Nothing
     
  return CourseGrid
    { _courseGrid_slots            = selectionDyn
    , _courseGrid_slotsInFocus     = slotsInFocus
    , _courseGrid_onCourseSelected = onCourseSelected
    , _courseGrid_onCourseRemoved  = onCourseRemoved
    }

gridWidget :: forall t m.
  MonadWidget t m
  => Dynamic t (Map Slot [Course])
--  -> Dynamic t [Slot]
  -> m (Event t CourseGridEvent)
gridWidget selectionsDyn = -- slotsInFocusDyn =
  colGrid "block-grid" $ gridCol <$> selectionsDyn {- selectionsDyn
  gridItem
  <$> selectionsDyn
  <*> slotsInFocusDyn-}

-- | Column of all blocks in a period in a semester.
gridCol :: forall m t.
  MonadWidget t m
  => Map Slot [Course] -- ^ Current course selections. 
  -> (Semester, Period)  -- ^ The current column.
  -- Dynamic t [Slot]              -- ^ Slots in this column in ascending order.
  -> m (Event t CourseGridEvent)
gridCol selections column = do
  let slotsDyn = constDyn $ uncurry slotsInPeriod column
  eventsDyn <- simpleList slotsDyn $ \slotDyn -> do
    ev <- dyn $ gridItem selections <$> slotDyn
    switchPromptly never ev
  return . switch . current $ leftmost <$> eventsDyn
      
    {-\slotDyn -> do
      slotEvent <- divClass "" $
        dyn $ genDyn <*> slotDyn-}
    --


gridItem :: forall m t.
  MonadWidget t m
  => Map Slot [Course]
--  -> [Slot]
  -> Slot
  -> m (Event t CourseGridEvent)
gridItem selections slot = do
  let courses  = M.findWithDefault [] slot selections

--n  text $ if length slotsInFocus == 0
--         then "0"
--         else "non-zero"
-- This is the crashy boi
  {-  let inFocus = slot `elem` focusedSlots
  let style = if inFocus
        then "class" =: "focused"
        else ""      =: ""
-}

  let adjustStyle = case courses of
        [] -> M.adjust ("grid-slot empty "     <>) "class"
        _  -> M.adjust ("grid-slot non-empty " <>) "class"

  let emptyStyle =  ("class" =: "") --style

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
