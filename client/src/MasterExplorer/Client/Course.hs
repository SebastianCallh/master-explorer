{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
module MasterExplorer.Client.Course
  ( courseList
  ) where

import           Data.Text               (pack)
import           Reflex.Dom

import MasterExplorer.Common.Data.Slot   (Slot)
import MasterExplorer.Common.Data.Course (Course (..), getCourseCode)
import MasterExplorer.Client.Elems       (filterList, dynLink)

courseList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Course]
  -> m (Event t (Course, Slot))
courseList coursesDyn = 
  divClass "course-list" $
    filterList courseListItem coursesDyn

courseListItem :: forall t m.
  MonadWidget t m
  => Dynamic t Course
  -> m (Event t (Course, Slot))
courseListItem courseDyn = do
  let
    selectCourse ::  MonadWidget t m
      => Workflow t m (Event t (Course, Slot))
    selectCourse = Workflow $ do      
      clickEv <- elClass "li" "available" $
        dynLink $ getCourseCode <$> courseDyn
      pure (never, selectSlot <$ clickEv)
    
    selectSlot :: MonadWidget t m
      => Workflow t m (Event t (Course, Slot))
    selectSlot = Workflow $ do      
      clickEvs <- elClass "li" "select-slot" $
        simpleList (courseSlots <$> courseDyn) template

      let slotClickedEv = switchPromptlyDyn $ leftmost <$> clickEvs      
      let taggedEv      = attachPromptlyDyn courseDyn slotClickedEv

      -- Behavior to let next step in workflow know what slot was selecter
      slotClickedBe <- hold Nothing $ Just <$> slotClickedEv
      pure (taggedEv, deselectCourse slotClickedBe <$ slotClickedEv)

    deselectCourse :: MonadWidget t m
      => Behavior t (Maybe Slot)
      -> Workflow t m (Event t (Course, Slot)) 
    deselectCourse slotClickedBe = Workflow $ do
      clickEv <- elClass "li" "selected" $
        dynLink $ getCourseCode <$> courseDyn

      let courseEv = tagPromptlyDyn courseDyn clickEv
      let makeTuple mslot course = (course,) <$> mslot
      let courseSlotEv = attachWithMaybe makeTuple slotClickedBe courseEv
      pure (courseSlotEv, selectCourse <$ clickEv)

    template :: MonadWidget t m
      => (Dynamic t Slot -> m (Event t Slot))
    template slotDyn = do
      clickEv <- dynLink $ pack . show <$> slotDyn
      return $ tagPromptlyDyn slotDyn clickEv
      
  workflowDyn <- workflow selectCourse
  return $ switchPromptlyDyn workflowDyn
