{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

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
import           Reflex.Dom.Extended

import MasterExplorer.Common.Class.Pretty   (pretty)
import MasterExplorer.Common.Data.Occasion  (Occasion, occasionSemester)
import MasterExplorer.Common.Data.Course    (Course (..), getCourseCode,
                                             getCourseName, masterOccasions)

data CourseStatus
  = Available
  | InSelection
  | Selected Occasion

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
  rec selections <- foldDynMaybe updateCourseStatus M.empty event
      let list = CourseList <$> coursesDyn <*> selections
      event <- courseListWidget list
  return event

  where
    -- | Map into Maybe to supress events firing on mouse events
    updateCourseStatus (CourseSelected      c o) = pure . M.insert c (Selected o)
    updateCourseStatus (CoursePreSelected   c  ) = pure . M.insert c InSelection
    updateCourseStatus (CourseDeselected    c _) = pure . M.insert c Available
    updateCourseStatus _                         = const Nothing

courseListWidget :: forall t m.
  MonadWidget t m
  => Dynamic t CourseList
  -> m (Event t CourseListEvent)
courseListWidget courseListDyn =
  divClass "course-list" $
    filterList (courseListItem courseListDyn) $
      view courses <$> courseListDyn
  
courseListItem :: forall t m.
  MonadWidget t m
  => Dynamic t CourseList
  -> Dynamic t Course
  -> m (Event t CourseListEvent)
courseListItem courseListDyn courseDyn = do
  let statusDyn =
        M.findWithDefault Available
        <$> courseDyn
        <*> (view selections <$> courseListDyn)

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
          let occasion = head $ courseOccasions course
          in CourseSelected course occasion

    inSelection :: m (Event t CourseListEvent)
    inSelection = do
      attrsDyn <- itemAttrs courseListDyn courseDyn
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
      attrsDyn  <- itemAttrs courseListDyn courseDyn
      (e, _)    <- elDynAttr' "div" attrsDyn $ do
        dynText $ getCourseCode <$> courseDyn
        divClass "course-name" $ dynText $ shortName <$> courseDyn
      return $ domEvent Click e

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
     Selected _  -> "class" =: "list-item selected"
     
-- | Is used to "truncate" the name of a course
--   in the course list.
shortName :: Course -> Text
shortName course =
  if   (T.length . getCourseName $ course) > 12
  then (T.take 16 . getCourseName $ course) <> "..."
  else getCourseName course
