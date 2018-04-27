{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}

module MasterExplorer.Client.CourseList
  ( CourseListEvent(..)
  , courseList
  ) where

import           Data.Maybe               (listToMaybe)
import           Reflex.Dom

import MasterExplorer.Common.Class.Pretty   (pretty)
import MasterExplorer.Common.Data.Occasion  (Occasion)
import MasterExplorer.Common.Data.Course    (Course (..), getCourseCode, masterOccasions)
import MasterExplorer.Client.Elems          (filterList, dynLink)

data CourseListEvent
  = CourseSelected    Course Occasion
  | CourseDeselected  Course Occasion
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
      let next = if (length . masterOccasions $ course) > 1
                 then (never, selectSlot <$ courseClickedEv)
                 else
                   let mslotBe = listToMaybe . courseOccasions <$> current courseDyn
                       slotDyn = head . courseOccasions <$> courseDyn
                       slotEv  = tagPromptlyDyn slotDyn courseClickedEv
                       event   = selectEvent courseDyn slotEv
                   in (event, deselectCourse mslotBe <$ courseClickedEv)

      pure next
      
    selectSlot :: MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectSlot = Workflow $ do
      clickEvs <- divClass "select-slot faded" $
        simpleList (masterOccasions <$> courseDyn) template

      let slotEv = switchPromptlyDyn $ leftmost <$> clickEvs
      let event  = selectEvent courseDyn slotEv

      -- Behavior to let next step in workflow know what slot was selecter
      slotClickedBe <- hold Nothing $ Just <$> slotEv
      pure (event, deselectCourse slotClickedBe <$ slotEv)

    deselectCourse :: MonadWidget t m
      => Behavior t (Maybe Occasion)
      -> Workflow t m (Event t CourseListEvent) 
    deselectCourse slotClickedBe = Workflow $ do
      clickEv <- divClass "selected" $
        dynLink $ getCourseCode <$> courseDyn

      let courseEv = tagPromptlyDyn courseDyn clickEv
      let makeTuple mslot course = (course,) <$> mslot
      let courseSlotEv = attachWithMaybe makeTuple slotClickedBe courseEv
      pure (uncurry CourseDeselected <$> courseSlotEv, selectCourse <$ clickEv)

    template :: MonadWidget t m
      => Dynamic t Occasion
      -> m (Event t Occasion)
    template occasionDyn = do
      clickEv <- dynLink $ pretty <$> occasionDyn
      return $ tagPromptlyDyn occasionDyn clickEv

    selectEvent :: MonadWidget t m
      => Dynamic t Course
      -> Event t Occasion
      -> Event t CourseListEvent
    selectEvent selCourseDyn occasionEv =
      uncurry CourseSelected <$> attachPromptlyDyn selCourseDyn occasionEv

  (e, workflowEv) <- el' "li" $
    switchPromptlyDyn <$> workflow selectCourse

  let mouseEnterEv = CourseMouseEnter <$> tagPromptlyDyn courseDyn (domEvent Mouseenter e)
  let mouseLeaveEv = CourseMouseLeave <$> tagPromptlyDyn courseDyn (domEvent Mouseleave e)

  return $ leftmost [mouseLeaveEv, mouseEnterEv, workflowEv]


-- TSKS10, semester 6, TAOP33, semester 5, TATA24 smester 3, TATA40 semester 3
