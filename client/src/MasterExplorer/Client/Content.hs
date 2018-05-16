{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.Content where

import qualified Data.Map                               as M

import           Control.Lens
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.Schedule   
import           MasterExplorer.Common.Data.Course

import qualified MasterExplorer.Client.CourseGrid       as Grid
import qualified MasterExplorer.Client.CourseInfo       as Info 
import qualified MasterExplorer.Client.PlanStats        as Stats

{- The content view is either a schedule or a statistics view. Since the BlockView
   can be either the block schedule or a selected course and it also branches out. 
   It would be nice to extract BlockView to a new module but Content needs to preserve state
   from there during menu transitions so it requires some thinking.

   Content
   |
   |-- StatsView
   |
   |-- BlockView
       |
       |-- GridView
       |
       |-- CourseView
-}

data ContentViewWidget
  = BlockView
  | StatsView
  deriving (Show, Eq, Ord)

data BlockViewWidget
  = GridView
  | CourseView
  deriving (Show, Eq, Ord)

data Content t = Content
  { --_activeContentView :: !(Dynamic t ContentViewWidget)
--  , _activeBlockView   :: !(Dynamic t BlockViewWidget)
--  , _selectedCourse    :: !(Dynamic t (Maybe Course))
    _onCourseRemoved :: !(Event t Course)
  }

makeLenses ''Content

data ContentEvent
  = ContentViewSelected ContentViewWidget
  | BlockViewSelected   BlockViewWidget
  | CourseSelected      (Maybe Course)
  | CourseRemoved       Course

-- | The content makes up everything below the program menu
--   and to the left of the course list. 
widget :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule       -- ^ Current course selections.
  -> Dynamic t (Maybe Course) -- ^ Course which slots to highlight.
  -> m (Content t)
widget scheduleDyn mFocusCourse = do
  rec blockViewDyn       <- foldDyn updateBlockView GridView event
      selectedmCourseDyn <- foldDyn updateSelectedCourse Nothing event

      event <- eventTabDisplay "content-menu" "active-content" $ do
        let bv = blockView scheduleDyn selectedmCourseDyn mFocusCourse blockViewDyn
        let sv = statsView scheduleDyn
        M.fromList [ (BlockView, ("Blockschema", bv))
                   , (StatsView, ("Statistik",   sv))
                   ]

      let courseRemovedEv = fforMaybe event $ \case
            CourseRemoved c -> Just c
            _               -> Nothing
      
  return Content
    { _onCourseRemoved = courseRemovedEv
    }
 
  where
    updateSelectedCourse (CourseSelected mc) = const mc
    updateSelectedCourse _                   = id

    updateBlockView (BlockViewSelected v) = const v
    updateBlockView (CourseSelected _)    = const CourseView
    updateBlockView _                     = id
    
statsView :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule        -- ^ Current course selections.
  -> m (Event t ContentEvent)
statsView scheduleDyn =
  divClass "content-wrap" $ do
    planStats <- Stats.widget scheduleDyn
    return $ ContentViewSelected BlockView <$ (planStats ^. Stats.onClose)

blockView :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule        -- ^ Current course selections.
  -> Dynamic t (Maybe Course)  -- ^ Maybe a course is selected for the info view.
  -> Dynamic t (Maybe Course)  -- ^ Maybe a course slots should be in focus.
  -> Dynamic t BlockViewWidget -- ^ The sub widget that is currently selected.
  -> m (Event t ContentEvent)
blockView selectionsDyn mSelCourseDyn mFocusCourse activeWidget =
  divClass "content-wrap" $ do
    let gv = gridView selectionsDyn mFocusCourse
    let cv = courseView mSelCourseDyn
    let widgetMap = constDyn $ M.fromList
          [ (GridView,   gv)
          , (CourseView, cv)
          ]

    let makeWidget _key valDyn selectedDyn = do
          let dynAttrs = ffor selectedDyn $ \case
                True  -> ("class" =: "")
                False -> ("class" =: "hidden")

          elDynAttr "div" dynAttrs (dyn valDyn) >>=
            switchPromptly never

    event <- selectViewListWithKey activeWidget widgetMap makeWidget
    return $ snd <$> event
  
gridView :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule        -- ^ Current course selections.
  -> Dynamic t (Maybe Course)  -- ^ Maybe a course slots should be in focus.
  -> m (Event t ContentEvent)
gridView scheduleDyn mFocusCourse = do
  courseGrid <- Grid.widget scheduleDyn mFocusCourse
  return $ leftmost [ CourseSelected . pure <$> courseGrid ^. Grid.onCourseSelected
                    , CourseRemoved         <$> courseGrid ^. Grid.onCourseRemoved
                    ]

courseView :: forall t m.
  MonadWidget t m
  => Dynamic t (Maybe Course)
  -> m (Event t ContentEvent)
courseView mSelCourseDyn  = do
  events <- dyn $ ffor mSelCourseDyn $ \case
    Nothing     -> pure never
    Just course -> do
      ev <- Info.widget (constDyn course)
      return $ BlockViewSelected GridView <$ (ev  ^. Info.onClose)
  switchPromptly never events

-- | Content showed when no program is selected.
empty :: forall t m.
  MonadWidget t m
  => m (Content t)
empty = do
  divClass "content-empty" $
    el "h1" $
      text "Välj ett program för att börja!"
  return Content
    { _onCourseRemoved = never
    }
