{-# LANGUAGE RankNTypes          #-}

module MasterExplorer.Client.Course
  ( courseList
  ) where

import           Reflex.Dom

import MasterExplorer.Common.Data.Slot   (Slot)
import MasterExplorer.Common.Data.Course (Course (..), getCourseCode)
import MasterExplorer.Client.Elems       (filterList, dynLink)

courseList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Course]
  -> m (Event t (Slot, Course))
courseList coursesDyn = 
  divClass "course-list" $
    filterList courseListItem coursesDyn

courseListItem :: forall t m.
  (DomBuilder t m,
   MonadSample t m,
   PostBuild t m)
  => Dynamic t Course
  -> m (Event t (Slot, Course))
courseListItem itemDyn =
  el "li" $ do
    ev <- dynLink $ getCourseCode <$> itemDyn
    return $ tagPromptlyDyn (tagWithSlot <$> itemDyn) ev
  where
    tagWithSlot :: Course -> (Slot, Course)
    tagWithSlot ev = (head $ courseSlots ev, ev)
