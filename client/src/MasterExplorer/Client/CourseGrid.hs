{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseGrid  where

import qualified Data.Map                              as M
import qualified Data.Set                              as S

import           Control.Lens
import           Data.Semigroup                        ((<>))
import           Data.Map                              (Map)
import           Data.Set                              (Set)
import           Data.Text                             (Text)
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.CoursePlan (CoursePlan)
import           MasterExplorer.Common.Data.Period     (Period)
import           MasterExplorer.Common.Data.Semester   (Semester)
import           MasterExplorer.Common.Data.Course     (Course, courseSlots, courseCode, courseName)
import           MasterExplorer.Common.Data.Slot       (Slot (..), slotsInPeriod)
import qualified MasterExplorer.Common.Data.CoursePlan as CoursePlan

import qualified MasterExplorer.Client.ColGrid         as ColGrid

data CourseGridEvent
  = CourseSelected Course
  | CourseRemoved Course

data CourseGrid t = CourseGrid
  { _onCourseSelected :: !(Event t Course)
  , _onCourseRemoved  :: !(Event t Course)
  }
  
makeLenses ''CourseGrid

-- | Grid view of all master semesters and
--   currently selected courses.
widget :: forall t m.
  MonadWidget t m
  => Dynamic t CoursePlan        -- ^ Current course selections.
  -> Dynamic t (Maybe Course)  -- ^ Maybe a course slots should be in focus.
  -> m (CourseGrid t)
widget scheduleDyn mFocusedCourse = do
  let slotsInFocus = ffor mFocusedCourse $ \case
        Just course -> foldr S.insert S.empty $ courseSlots course
        Nothing     -> S.empty
        
  event <- markup scheduleDyn slotsInFocus

  let courseSelectedEv = fforMaybe event $ \case
        (CourseSelected c) -> Just c
        _                  -> Nothing

  let courseRemovedEv = fforMaybe event $ \case
        (CourseRemoved c) -> Just c
        _                    -> Nothing
     
  return CourseGrid
    { _onCourseSelected = courseSelectedEv
    , _onCourseRemoved  = courseRemovedEv
    }

markup :: forall t m.
  MonadWidget t m
  => Dynamic t CoursePlan
  -> Dynamic t (Set Slot)
  -> m (Event t CourseGridEvent)
markup scheduleDyn slotsInFocus =
  ColGrid.widget "block-grid" $
    gridCol <$> scheduleDyn <*> slotsInFocus

-- | Column of all blocks in a period in a semester.
gridCol :: forall m t.
  MonadWidget t m
  => CoursePlan            -- ^ Current course selections. 
  -> Set Slot            -- ^ Slots in focus.
  -> (Semester, Period)  -- ^ The current column.
  -> m (Event t CourseGridEvent)
gridCol schedule slotsInFocus column = do
  let slotsDyn = constDyn $ uncurry slotsInPeriod column
  eventsDyn <- simpleList slotsDyn $ \slotDyn -> do
    let styleDyn = ffor slotDyn $ \slot ->
          if slot `S.member` slotsInFocus
            then "class" =: "focused"
            else "class" =: ""
            
    ev <- dyn $ gridItem schedule <$> slotDyn <*> styleDyn
    switchPromptly never ev
        
  return . switch . current $ leftmost <$> eventsDyn

gridItem :: forall m t.
  MonadWidget t m
  => CoursePlan
  -> Slot
  -> Map Text Text
  -> m (Event t CourseGridEvent)
gridItem schedule slot style = do
  let courses = CoursePlan.getSlotCourses slot schedule
  let adjustStyle = case courses of
        [] -> M.adjust ("grid-slot empty "     <>) "class"
        _  -> M.adjust ("grid-slot non-empty " <>) "class"

  elAttr "div" (adjustStyle style) $ do
    let widgetMap = foldr makeWidgetMap M.empty courses 
    eventTabDisplay "course-list" "active-course" widgetMap    
    
  where    
    makeWidgetMap c = M.insert (c ^. courseCode)
      (c ^. courseCode,
        divClass "visible-course" $ do
          selectEv <- button $ c ^. courseName 
          removeEv <- button   "Ta bort"
          return $ leftmost
            [ CourseSelected c <$ selectEv
            , CourseRemoved  c <$ removeEv
            ]
      )
                      
