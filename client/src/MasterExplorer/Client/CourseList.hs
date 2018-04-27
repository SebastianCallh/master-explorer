{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}

module MasterExplorer.Client.CourseList
  ( CourseListEvent(..)
  , courseList
  ) where

import qualified Data.Text                as T

import           Data.Text                (Text)
import           Data.Semigroup           ((<>))
import           Data.Maybe               (listToMaybe)
import           Reflex.Dom

import MasterExplorer.Common.Class.Pretty   (pretty)
import MasterExplorer.Common.Data.Occasion  (Occasion, occasionSemester)
import MasterExplorer.Common.Data.Course    (Course (..), getCourseCode,
                                             getCourseName, masterOccasions)
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
      
    listItem style =
      elAttr' "div" ("class" =: ("list-item " <> style)) $ do
        dynText $ getCourseCode <$> courseDyn
        divClass "course-name" $ dynText $ shortName <$> courseDyn

    selectCourse ::  MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectCourse = Workflow $ do
        
      (e, _)  <- listItem "available" 
      let courseClickedEv = domEvent Click e

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
      clickEvs <- divClass "select-slot list-item" $
        simpleList (masterOccasions <$> courseDyn) $ \occasionDyn -> do
          clickEv <- dynLink $ pretty . occasionSemester <$> occasionDyn
          return $ tagPromptlyDyn occasionDyn clickEv

      let slotEv = switchPromptlyDyn $ leftmost <$> clickEvs
      let event  = selectEvent courseDyn slotEv

      -- Behavior to let next step in workflow know what slot was selecter
      slotClickedBe <- hold Nothing $ Just <$> slotEv
      pure (event, deselectCourse slotClickedBe <$ slotEv)

    deselectCourse :: MonadWidget t m
      => Behavior t (Maybe Occasion)
      -> Workflow t m (Event t CourseListEvent) 
    deselectCourse slotClickedBe = Workflow $ do

      (e, _)  <- listItem "selected"
      let courseClickedEv = domEvent Click e
      let courseEv = tagPromptlyDyn courseDyn courseClickedEv
      let makeTuple mslot course = (course,) <$> mslot
      let courseSlotEv = attachWithMaybe makeTuple slotClickedBe courseEv
      pure (uncurry CourseDeselected <$> courseSlotEv, selectCourse <$ courseClickedEv)


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

shortName :: Course -> Text
shortName course =
  if   (T.length . getCourseName $ course) > 12
  then (T.take 16 . getCourseName $ course) <> "..."
  else getCourseName course
