{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.Content
  ( content
  ) where

import qualified Data.Map                          as M

import           Control.Lens
import           Reflex.Dom.Extended

import qualified MasterExplorer.Client.CourseGrid  as Grid
import qualified MasterExplorer.Client.CourseInfo  as Info 
import           MasterExplorer.Client.CourseList  (CourseListEvent)
import           MasterExplorer.Common.Data.Course (Course)


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

data Content = Content
  { _activeContentView :: ContentViewWidget
  , _activeBlockView   :: BlockViewWidget
  , _selectedCourse    :: Maybe Course
  } deriving Show

makeLenses ''Content

defaultContent :: Content 
defaultContent = Content 
  { _activeContentView = BlockView
  , _activeBlockView   = GridView
  , _selectedCourse    = Nothing
  }
  
content :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m ()
content courseListEv = do
  rec content <- foldDyn ($) defaultContent event
      event   <- eventTabDisplay "content-menu" "active-content" $
        M.fromList [ (BlockView, ("Blockschema", blockView courseListEv content))
                   , (StatsView, ("Statistik",  statsView content))
                   ]

  return ()

statsView  :: forall t m.
  MonadWidget t m
  => Dynamic t Content
  -> m (Event t (Content -> Content))
statsView content = do
  text "o hai"
  return never

blockView :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> Dynamic t Content
  -> m (Event t (Content -> Content))
blockView courseListEv contentDyn = do
  let widgetMap = constDyn $ M.fromList
          [ (GridView,   gridView courseListEv)
          , (CourseView, courseView (view selectedCourse <$> contentDyn))
          ]

  let makeWidget _key valDyn selectedDyn = do
        let dynAttrs = ffor selectedDyn $ \case
              True  -> ("class" =: "content-wrap")
              False -> ("class" =: "hidden")

        elDynAttr "div" dynAttrs (dyn valDyn) >>=
          switchPromptly never

  let activeWidget = view activeBlockView <$> contentDyn
  
  event <- selectViewListWithKey activeWidget widgetMap makeWidget
  return $ snd <$> event
  
gridView :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m (Event t (Content -> Content))
gridView courseListEv = do
  gridEv <- Grid.courseGrid courseListEv
  return $ ffor gridEv $ \case
    Grid.CourseSelected course ->
      set activeBlockView CourseView .
      set selectedCourse (pure course)

courseView :: forall t m.
  MonadWidget t m
  => Dynamic t (Maybe Course)
  -> m (Event t (Content -> Content)) 
courseView courseDyn = do
  ev <- dyn $ ffor courseDyn $ \case
    Nothing     -> pure never
    Just course -> Info.courseInfo $ constDyn course
    
  ev' <- switchPromptly never ev
  return $ ffor ev' $ \case
    Info.CourseInfoEvent -> set activeBlockView GridView
