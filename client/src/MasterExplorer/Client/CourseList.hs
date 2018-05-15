{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

module MasterExplorer.Client.CourseList where

import qualified Data.Text                as T
import qualified Data.Map                 as M
import qualified Data.List                as L

import           Control.Lens
import           Data.Text                (Text)
import           Data.Map                 (Map)
import           Data.Semigroup           ((<>))
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Class.Pretty     (pretty)
import           MasterExplorer.Common.Data.Slot        (Slot)
import           MasterExplorer.Common.Data.Occasion    (Occasion, occasionSemester, getOccasion)
import           MasterExplorer.Common.Data.Course

data CourseStatus
  = Available
  | InSelection
  | Selected Occasion

data CourseList t = CourseList
  { _courses          :: !(Dynamic t [Course])
  , _statuses         :: !(Dynamic t (Map Course CourseStatus))
  , _focusedCourse    :: !(Dynamic t (Maybe Course))
  , _slots            :: !(Dynamic t (Map Slot [Course]))
  , _onCourseSelect   :: !(Event t (Course, Occasion))
  , _onCourseDeselect :: !(Event t Course)
  }

makeLenses ''CourseList

data CourseListEvent
  = CourseSelected    Course Occasion
  | CoursePreSelected Course
  | CourseDeselected  Course Occasion
  | CourseMouseEnter  Course
  | CourseMouseLeave  Course
  deriving Show

-- | The list to the left in the app for selecting courses
--   Internal events are bubbled up to this function which
--   then updates internal state. 
widget :: forall t m.
  MonadWidget t m
  => Dynamic t [Course]
  -> m (CourseList t)
widget coursesDyn = do
  rec statusesDyn   <- foldDynMaybe updateStatuses M.empty event
      slotsDyn      <- foldDynMaybe updateSlots    M.empty event
      courseInFocus <- foldDynMaybe updateHover    Nothing event
      event         <- markup statusesDyn coursesDyn

      let courseSelectedEv = fforMaybe event $ \case
            (CourseSelected c o) -> Just (c, o)
            _                    -> Nothing

      let courseDeselectedEv = fforMaybe event $ \case
            (CourseDeselected c _) -> Just c
            _                    -> Nothing
      
  return CourseList
    { _courses          = coursesDyn
    , _statuses         = statusesDyn
    , _focusedCourse    = courseInFocus
    , _slots            = slotsDyn
    , _onCourseSelect   = courseSelectedEv
    , _onCourseDeselect = courseDeselectedEv
    }

  where
    -- Events are mapped into Maybe to supress re-render firing on mouse events
    -- which causes infinite loops of MouseEnter.

    updateStatuses (CourseSelected      c o) = pure . M.insert c (Selected o)
    updateStatuses (CoursePreSelected   c  ) = pure . M.insert c InSelection
    updateStatuses (CourseDeselected    c _) = pure . M.insert c Available
    updateStatuses _                         = const Nothing

    updateHover (CourseMouseEnter c) _ = pure $ Just c 
    updateHover (CourseMouseLeave _) _ = pure   Nothing
    updateHover  _                   _ = Nothing

    updateSlots (CourseSelected c o) = \selections ->
      pure $ foldr (addToSlots c) selections (getOccasion o)
    updateSlots (CourseDeselected c o) = \selections ->
      pure $ foldr (removeFromSlots c) selections (getOccasion o)
    updateSlots  _                   = const Nothing

    addToSlots :: Course -> Slot -> Map Slot [Course] -> Map Slot [Course]
    addToSlots c s = M.insertWith (++) s [c]
    
    removeFromSlots :: Course -> Slot -> Map Slot [Course] -> Map Slot [Course]
    removeFromSlots c = M.adjust (L.delete c)

markup :: forall t m.
  MonadWidget t m
  => Dynamic t (Map Course CourseStatus)
  -> Dynamic t [Course]
  -> m (Event t CourseListEvent)
markup statusesDyn coursesDyn =
  divClass "course-list" $
    filterList (courseListItem statusesDyn) coursesDyn

-- | Maps the current state of the course to a
--   function that generates the markup and proper events.
courseListItem :: forall t m.
  MonadWidget t m
  => Dynamic t (Map Course CourseStatus)
  -> Dynamic t Course
  -> m (Event t CourseListEvent)
courseListItem statusesDyn courseDyn = do
  let statusDyn = M.findWithDefault Available <$> courseDyn <*> statusesDyn

  (e, event) <- el' "li" $ do
    event <- dyn $ ffor statusDyn $ \case
      Available   -> available
      InSelection -> inSelection
      Selected o  -> selected o

    switchPromptly never event

  let tagCourse = tag (current courseDyn)
  let mouseEnterEv = CourseMouseEnter <$> tagCourse (domEvent Mouseenter e)
  let mouseLeaveEv = CourseMouseLeave <$> tagCourse (domEvent Mouseleave e)
  return $ leftmost [mouseLeaveEv, mouseEnterEv, event]

  where
    available :: m (Event t CourseListEvent)
    available = do
      courseEv <- tag (current courseDyn) <$> item
      return $ ffor courseEv $ \course ->
        if (length . masterOccasions $ course) > 1
        then CoursePreSelected course
        else
          let occasion = head $ course ^. courseOccasions
          in CourseSelected course occasion

    inSelection :: m (Event t CourseListEvent)
    inSelection = do
      attrsDyn <- itemAttrs statusesDyn courseDyn
      event <- elDynAttr "div" attrsDyn $
        simpleList (masterOccasions <$> courseDyn) $ \occasionDyn -> do
          event <- dynLink $ pretty . occasionSemester <$> occasionDyn
          let courseListEv = CourseSelected <$> courseDyn <*> occasionDyn
          return $ tag (current courseListEv) event
          
      return . switch . current $ leftmost <$> event

    selected :: Occasion -> m (Event t CourseListEvent)
    selected occasion = do
      courseEv <- tag (current courseDyn) <$> item
      return $ ffor courseEv $ \course ->
        CourseDeselected course occasion

    item :: m (Event t ())
    item = do
      attrsDyn  <- itemAttrs statusesDyn courseDyn
      (e, _)    <- elDynAttr' "div" attrsDyn $ do
        dynText $ view courseCode <$> courseDyn
        divClass "course-name" $ dynText $ shortName <$> courseDyn
      return $ domEvent Click e

-- | Dynamically calculates attributes for a course list item
--   depending on what state the course is in. Anchoring the state
--   in the workflow leads to in being based on index instead of course.
itemAttrs :: forall t m. 
  MonadWidget t m
  => Dynamic t (Map Course CourseStatus)
  -> Dynamic t Course
  -> m (Dynamic t (Map Text Text))
itemAttrs statusesDyn courseDyn =
  pure $ f <$> courseDyn <*> statusesDyn
 where
   f :: Course -> Map Course CourseStatus -> Map Text Text
   f c s = case M.findWithDefault Available c s of
     Available   -> "class" =: "list-item available"
     InSelection -> "class" =: "list-item in-selection"
     Selected _  -> "class" =: "list-item selected"
     
-- | Is used to "truncate" the name of a course
--   in the course list.
shortName :: Course -> Text
shortName course =
  if   T.length name > 12
  then T.take 16 name <> "..."
  else name
  where
    name = course ^. courseName 
