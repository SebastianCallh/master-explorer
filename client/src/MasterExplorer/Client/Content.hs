{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.Content
  ( content
  ) where

import qualified Data.Map                          as M

import           Control.Lens
import           Data.Map                         (Map)
import           Reflex.Dom.Extended

import qualified MasterExplorer.Client.CourseGrid       as Grid
import qualified MasterExplorer.Client.CourseInfo       as Info 
import qualified MasterExplorer.Client.PlanStats        as Stats

import           MasterExplorer.Common.Data.Schedule   
import           MasterExplorer.Common.Data.Slot
import           MasterExplorer.Common.Data.Course
import           MasterExplorer.Common.Data.CourseSelection
import           MasterExplorer.Common.Data.Occasion

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
  { _content_activeContentView :: !(Dynamic t ContentViewWidget)
  , _content_activeBlockView   :: !(Dynamic t BlockViewWidget)
  , _content_selectedCourse    :: !(Dynamic t (Maybe Course))
--  , _content_schedule          :: !(Dynamic t Schedule)
  }

makeLenses ''Content

data ContentEvent
  = ContentViewSelected ContentViewWidget
  | BlockViewSelected   BlockViewWidget
  | CourseSelected      (Maybe Course)
  | CourseRemoved       Course

-- | The content makes up everything below the program menu
--   and to the left of the course list. 
content :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule
  {-(Event t Schedule -> m (Event t Int)) -- ^ Function to save schedule to server.
  -> Dynamic t Program                     -- ^ Current program selected.
  -> Dynamic t (Map Slot [Course])         -- ^ Current course selections.
  -> Event t Course                        -- ^ MouseEnter event for courses in course list.
  -> Event t Course                        -- ^ MouseLeave event for courses in course list.-}
  -> m ()
content scheduleDyn = do--saveSchedule selectionsDyn mouseEnterCourseEv mouseLeaveCourseEv = do
  rec contentViewDyn     <- foldDyn updateContentView BlockView event
      blockViewDyn       <- foldDyn updateBlockView GridView event
      selectedmCourseDyn <- foldDyn updateSelectedCourse Nothing event
--      let scheduleDyn = Schedule <$>  foldDyn updateSchedule (newSchedule event
      let content = Content contentViewDyn blockViewDyn selectedmCourseDyn

      event <- eventTabDisplay "content-menu" "active-content" $ do
        let bv = blockView scheduleDyn selectedmCourseDyn blockViewDyn
        let sv = statsView scheduleDyn
        M.fromList [ (BlockView, ("Blockschema", bv))
                   , (StatsView, ("Statistik",   sv))
                   ]
      
  return ()
 
  where
    slotsInFocus :: Maybe Course -> [Slot]
    slotsInFocus (Just c) = c ^. courseOccasions >>= getOccasion
    slotsInFocus Nothing  = []
     
    updateSelectedCourse (CourseSelected mc) = const mc
    updateSelectedCourse _                   = id

    updateContentView (ContentViewSelected v) = const v
    updateContentView _                       = id

    updateBlockView (BlockViewSelected v) = const v
    updateBlockView (CourseSelected _)    = const CourseView
    updateBlockView _                     = id

    updateSchedule _ = id
      
statsView :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule 
  -> m (Event t ContentEvent)
statsView scheduleDyn =
  divClass "content-wrap" $ do
    ev <- Stats.planStats scheduleDyn
    return $ ffor ev $ \case
      Stats.Ev -> ContentViewSelected BlockView

blockView :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule             -- ^ Selected courses.
  -> Dynamic t (Maybe Course)       -- ^ Maybe a course is selected for the info view.
--  -> Event t Course                 -- ^ Event for mouse entering a course in course list.
--  -> Event t Course                 -- ^ Event for mouse leaving a course in course list.
  -> Dynamic t BlockViewWidget      -- ^ The sub widget that is currently selected.
  -> m (Event t ContentEvent)
blockView selectionsDyn mSelCourseDyn
--  mouseEnterCourseEv mouseLeaveCourseEv
  activeWidget =
  divClass "content-wrap" $ do
    let gv = gridView selectionsDyn -- mouseEnterCourseEv mouseLeaveCourseEv
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
  => Dynamic t Schedule
--  -> Event t Course
--  -> Event t Course
  -> m (Event t ContentEvent)
gridView scheduleDyn = do -- mouseEnterCourseEv mouseLeaveCourseEv = do
  courseGrid <- Grid.courseGrid scheduleDyn -- mouseEnterCourseEv mouseLeaveCourseEv
  return $ leftmost [ CourseSelected . pure <$> courseGrid ^. Grid.courseGrid_onCourseSelected
                    , CourseRemoved         <$> courseGrid ^. Grid.courseGrid_onCourseRemoved
                    ]

courseView :: forall t m.
  MonadWidget t m
  => Dynamic t (Maybe Course)
  -> m (Event t ContentEvent)
courseView courseDyn = do
  events <- dyn $ ffor courseDyn $ \case
    Nothing     -> pure never
    Just course -> Info.courseInfo $ constDyn course

  event <- switchPromptly never events
  return $ ffor event $ \case
    Info.CourseInfoEvent -> BlockViewSelected GridView
