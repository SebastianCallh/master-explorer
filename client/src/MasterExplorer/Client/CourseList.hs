{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
module MasterExplorer.Client.CourseList
  ( CourseListEvent(..)
  , courseList
  ) where

import           Data.Maybe              (listToMaybe)
import           Data.Text               (pack)
import           Reflex.Dom

import MasterExplorer.Common.Data.Slot   (Slot)
import MasterExplorer.Common.Data.Course (Course (..), getCourseCode)
import MasterExplorer.Client.Elems       (filterList, dynLink)

data CourseListEvent
  = CourseSelected    Course Slot
  | CourseDeselected  Course Slot
  | CourseMouseEnter  Course
  | CourseMouseLeave  Course
  deriving Show

courseList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Course]
  -> m (Event t CourseListEvent)
courseList coursesDyn = 
  divClass "course-list" $
    filterList courseListItem coursesDyn

courseListItem :: forall t m.
  (MonadWidget t m,
  MonadSample t m)
  => Dynamic t Course
  -> m (Event t CourseListEvent)
courseListItem courseDyn = do
  let
    selectCourse ::  MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectCourse = Workflow $ do
      courseClickedEv <- divClass "available" $
        dynLink $ getCourseCode <$> courseDyn
      
       -- If there is only one slot availible, immediately choose it
      course <- sample . current $ courseDyn
      let next = if (length . courseSlots $ course) == 1
                 then (never, selectSlot <$ courseClickedEv)
                 else
                   let mslotBe = listToMaybe . courseSlots <$> current courseDyn
                       slotDyn = head . courseSlots <$> courseDyn
                       slotEv  = tagPromptlyDyn slotDyn courseClickedEv
                       event   = selectEvent courseDyn slotEv
                   in (event, deselectCourse mslotBe <$ courseClickedEv)

      pure next
      
    selectSlot :: MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectSlot = Workflow $ do
      clickEvs <- divClass "select-slot faded" $
        simpleList (courseSlots <$> courseDyn) template

      let slotEv = switchPromptlyDyn $ leftmost <$> clickEvs
      let event  = selectEvent courseDyn slotEv

      -- Behavior to let next step in workflow know what slot was selecter
      slotClickedBe <- hold Nothing $ Just <$> slotEv
      pure (event, deselectCourse slotClickedBe <$ slotEv)

    deselectCourse :: MonadWidget t m
      => Behavior t (Maybe Slot)
      -> Workflow t m (Event t CourseListEvent) 
    deselectCourse slotClickedBe = Workflow $ do
      clickEv <- divClass "selected" $
        dynLink $ getCourseCode <$> courseDyn

      let courseEv = tagPromptlyDyn courseDyn clickEv
      let makeTuple mslot course = (course,) <$> mslot
      let courseSlotEv = attachWithMaybe makeTuple slotClickedBe courseEv
      pure (uncurry CourseDeselected <$> courseSlotEv, selectCourse <$ clickEv)

    template :: MonadWidget t m
      => Dynamic t Slot
      -> m (Event t Slot)
    template slotDyn = do
      clickEv <- dynLink $ pack . show <$> slotDyn
      return $ tagPromptlyDyn slotDyn clickEv

    selectEvent :: MonadWidget t m
      => Dynamic t Course
      -> Event t Slot
      -> Event t CourseListEvent
    selectEvent selCourseDyn slotEv =
      uncurry CourseSelected <$> attachPromptlyDyn selCourseDyn slotEv

  (e, workflowEv) <- el' "li" $
    switchPromptlyDyn <$> workflow selectCourse
  
  let mouseEnterEv = CourseMouseEnter <$> tagPromptlyDyn courseDyn (domEvent Mouseenter e)
  let mouseLeaveEv = CourseMouseLeave <$> tagPromptlyDyn courseDyn (domEvent Mouseleave e)

  return $ leftmost [mouseLeaveEv, mouseEnterEv, workflowEv]
