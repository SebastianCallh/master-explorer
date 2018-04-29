{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseList
  ( CourseListEvent(..)
  , courseList
  ) where

import qualified Data.Text                as T
import qualified Data.Map                 as M

import           Control.Lens
import           Data.Text                (Text)
import           Data.Map                 (Map)
import           Data.Semigroup           ((<>))
import           Data.Maybe               (listToMaybe)
import           Reflex.Dom.Extended

import MasterExplorer.Common.Class.Pretty   (pretty)
import MasterExplorer.Common.Data.Occasion  (Occasion, occasionSemester)
import MasterExplorer.Common.Data.Course    (Course (..), getCourseCode,
                                             getCourseName, masterOccasions)

data CourseStatus
  = Available
  | InSelection
  | Selected

data CourseList = CourseList
  { _courses    :: ![Course]
  , _selections :: !(Map Course CourseStatus)
  }

makeLenses ''CourseList

data CourseListEvent
  = CourseSelected    Course Occasion
  | CoursePreSelected Course
  | CourseDeselected  Course Occasion
  | CourseMouseEnter  Course
  | CourseMouseLeave  Course
  deriving Show


-- | The list to the left in the app for selecting courses. 
courseList :: forall t m.
  MonadWidget t m
  => Dynamic t [Course]
  -> m (Event t CourseListEvent)
courseList coursesDyn = do
  rec selections <- foldDyn updateCourseStatus M.empty event
      let list = CourseList <$> coursesDyn <*> selections
      event <- courseListWidget list
  return event

  where
    updateCourseStatus (CourseSelected      c _) = M.insert c Selected
    updateCourseStatus (CoursePreSelected   c  ) = M.insert c InSelection
    updateCourseStatus (CourseDeselected    c _) = M.insert c Available
    updateCourseStatus _                         = id

courseListWidget :: forall t m.
  MonadWidget t m
  => Dynamic t CourseList
  -> m (Event t CourseListEvent)
courseListWidget courseListDyn =
  divClass "course-list" $ 
    filterList (courseListItem courseListDyn) $
      view courses <$> courseListDyn

-- | Utilizes the Reflex Workflow for selecting courses.
--   Events propagate to the CourseList which keeps track of
--   the courses state to correctly render dynamic markup.
courseListItem :: forall t m.
  MonadWidget t m
  => Dynamic t CourseList
  -> Dynamic t Course
  -> m (Event t CourseListEvent)
courseListItem courseListDyn courseDyn = do
  let
    cListItem :: MonadWidget t m => Text -> m (Event t ())    
    cListItem style = do
      attrsDyn  <- itemAttrs courseListDyn courseDyn
      (e, _)    <- elDynAttr' "div" attrsDyn $ do
        dynText $ getCourseCode <$> courseDyn
        divClass "course-name" $ dynText $ shortName <$> courseDyn
      return $ domEvent Click e
      
    selectCourse :: MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectCourse = Workflow $ do
        
      courseClickedEv  <- cListItem "available" 
        
       -- If there is only one slot availible, immediately choose it
      course <- sample . current $ courseDyn
      let next =
            if (length . masterOccasions $ course) > 1
            then let 
              event = tag (current $ CoursePreSelected <$> courseDyn) courseClickedEv
            in (event, selectSlot <$ event)
            else
              let mslotBe = listToMaybe . courseOccasions <$> current courseDyn
                  slotDyn = head . courseOccasions <$> courseDyn
                  slotEv  = tag (current slotDyn) courseClickedEv
                  event   = selectEvent courseDyn slotEv
              in (event, deselectCourse mslotBe <$ event)

      pure next
      
    selectSlot :: MonadWidget t m
      => Workflow t m (Event t CourseListEvent)
    selectSlot = Workflow $ do
      attrsDyn <- itemAttrs courseListDyn courseDyn
      clickEvs <- elDynAttr "div" attrsDyn $
        simpleList (masterOccasions <$> courseDyn) $ \occasionDyn -> do
          clickEv <- dynLink $ pretty . occasionSemester <$> occasionDyn
          return $ tag (current occasionDyn) clickEv

      let slotEv = switch . current $ leftmost <$> clickEvs
      let event  = selectEvent courseDyn slotEv

      -- Behavior to let next step in workflow know what slot was selected
      slotClickedBe <- hold Nothing $ Just <$> slotEv
      pure (event, deselectCourse slotClickedBe <$ slotEv)

    deselectCourse :: MonadWidget t m
      => Behavior t (Maybe Occasion)
      -> Workflow t m (Event t CourseListEvent) 
    deselectCourse slotClickedBe = Workflow $ do

      courseClickedEv  <- cListItem "selected"
      let courseEv = tag (current courseDyn) courseClickedEv
      let makeTuple mslot course = (course,) <$> mslot
      let courseSlotEv = attachWithMaybe makeTuple slotClickedBe courseEv
      pure (uncurry CourseDeselected <$> courseSlotEv, selectCourse <$ courseClickedEv)

    selectEvent :: MonadWidget t m
      => Dynamic t Course
      -> Event t Occasion
      -> Event t CourseListEvent
    selectEvent selCourseDyn occasionEv =
      uncurry CourseSelected <$> attach (current selCourseDyn) occasionEv

  (e, workflowEv) <- el' "li" $
    switch . current <$> workflow selectCourse

  let mouseEnterEv = CourseMouseEnter <$> tag (current courseDyn) (domEvent Mouseenter e)
  let mouseLeaveEv = CourseMouseLeave <$> tag (current courseDyn) (domEvent Mouseleave e)

  return $ leftmost [mouseLeaveEv, mouseEnterEv, workflowEv]

-- | Dynamically calculates attributes for a course list item
--   depending on what state the course is in. Anchoring the state
--   in the workflow leads to in being based on index instead of course.
itemAttrs :: forall t m. 
  MonadWidget t m
  => Dynamic t CourseList
  -> Dynamic t Course
  -> m (Dynamic t (Map Text Text))
itemAttrs courseListDyn courseDyn =
  pure $ f <$> courseDyn <*> courseListDyn
 where
   f :: Course -> CourseList -> Map Text Text
   f c cl = case M.findWithDefault Available c (cl ^. selections) of
     Available   -> "class" =: "list-item available"
     InSelection -> "class" =: "list-item in-selection"
     Selected    -> "class" =: "list-item selected"
     
-- | Is used to "truncate" the name of a course
--   in the course list.
shortName :: Course -> Text
shortName course =
  if   (T.length . getCourseName $ course) > 12
  then (T.take 16 . getCourseName $ course) <> "..."
  else getCourseName course
