{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

module MasterExplorer.Client.CourseList where

import qualified Data.Text                as T

import           Control.Lens
import           Data.Text                (Text)
import           Data.Map                 (Map)
import           Data.Semigroup           ((<>))
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Class.Pretty     (pretty)
import           MasterExplorer.Common.Data.Occasion    (Occasion, occasionSemester)
import           MasterExplorer.Common.Data.Course

import           MasterExplorer.Client.Data.CourseStatuses (CourseStatuses)
import qualified MasterExplorer.Client.Data.CourseStatuses as CourseStatuses

data CourseList t = CourseList
  { _focusedCourse     :: !(Dynamic t (Maybe Course))
  , _onCourseSelect    :: !(Event t (Occasion, Course))
  , _onCoursePreselect :: !(Event t Course)
  , _onCourseDeselect  :: !(Event t Course)
  }

makeLenses ''CourseList

data CourseListEvent
  = CourseSelected    Occasion Course 
  | CoursePreSelected Course
  | CourseDeselected  Course 
  | CourseMouseEnter  Course
  | CourseMouseLeave  Course
  deriving Show

-- | The list to the left in the app for selecting courses
--   Internal events are bubbled up to this function which
--   then updates internal state.
widget :: forall t m.
  MonadWidget t m
  => Dynamic t [Course]
  -> Dynamic t CourseStatuses
  -> m (CourseList t)
widget coursesDyn statusesDyn = do
  rec courseInFocus <- foldDynMaybe updateHover Nothing event
      event         <- markup statusesDyn coursesDyn 

      let courseSelectedEv = fforMaybe event $ \case
            (CourseSelected o c) -> Just (o, c)
            _                    -> Nothing

      let coursePreselectedEv = fforMaybe event $ \case
            (CoursePreSelected c) -> Just c
            _                     -> Nothing
      
      let courseDeselectedEv = fforMaybe event $ \case
            (CourseDeselected c) -> Just c
            _                    -> Nothing
      
  return CourseList
    { _focusedCourse     = courseInFocus
    , _onCourseSelect    = courseSelectedEv
    , _onCoursePreselect = coursePreselectedEv
    , _onCourseDeselect  = courseDeselectedEv
    }

  where
    -- Events are mapped into Maybe to supress re-render firing on mouse events
    -- which causes infinite loops of MouseEnter.    
    updateHover (CourseMouseEnter c) _ = pure $ Just c 
    updateHover (CourseMouseLeave _) _ = pure   Nothing
    updateHover  _                   _ = Nothing
    
markup :: forall t m.
  MonadWidget t m
  => Dynamic t CourseStatuses
  -> Dynamic t [Course]
  -> m (Event t CourseListEvent)
markup statusesDyn coursesDyn =
  divClass "course-list" $
    filterList (courseListItem statusesDyn) coursesDyn

-- | Maps the current state of the course to a
--   function that generates the markup and proper events.
courseListItem :: forall t m.
  MonadWidget t m
  => Dynamic t CourseStatuses
  -> Dynamic t Course
  -> m (Event t CourseListEvent)
courseListItem statusesDyn courseDyn = do  
  let statusDyn = CourseStatuses.get <$> courseDyn <*> statusesDyn

  (e, event) <- el' "li" $ do
    event <- dyn $ ffor statusDyn $ \case
      CourseStatuses.Available   -> available
      CourseStatuses.InSelection -> inSelection
      CourseStatuses.Selected    -> selected

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
          in CourseSelected occasion course

    inSelection :: m (Event t CourseListEvent)
    inSelection = do
      attrsDyn <- itemAttrs statusesDyn courseDyn
      event <- elDynAttr "div" attrsDyn $
        simpleList (masterOccasions <$> courseDyn) $ \occasionDyn -> do
          event <- dynLink $ pretty . occasionSemester <$> occasionDyn
          let courseListEv = CourseSelected <$> occasionDyn <*> courseDyn
          return $ tag (current courseListEv) event
          
      return . switch . current $ leftmost <$> event

    selected :: m (Event t CourseListEvent)
    selected = do
      courseEv <- tag (current courseDyn) <$> item
      return $ ffor courseEv CourseDeselected

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
  => Dynamic t CourseStatuses
  -> Dynamic t Course
  -> m (Dynamic t (Map Text Text))
itemAttrs statusesDyn courseDyn =
  pure $ f <$> courseDyn <*> statusesDyn
 where
   f :: Course -> CourseStatuses -> Map Text Text
   f c s = case CourseStatuses.get c s of
     CourseStatuses.Available   -> "class" =: "list-item available"
     CourseStatuses.InSelection -> "class" =: "list-item in-selection"
     CourseStatuses.Selected    -> "class" =: "list-item selected"
     
-- | Is used to "truncate" the name of a course
--   in the course list.
shortName :: Course -> Text
shortName course =
  if   T.length name > 12
  then T.take 16 name <> "..."
  else name
  where
    name = course ^. courseName 
