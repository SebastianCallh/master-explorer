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

data ContentView
  = Grid
  | Info 
  deriving (Eq, Ord)

data Content = Content
  { _activeView     :: ContentView
  , _selectedCourse :: Maybe Course
  }

makeLenses ''Content

makeContent
  :: ContentView
  -> Content 
makeContent activeView = Content 
  { _activeView     = activeView
  , _selectedCourse = Nothing
  }
  
content :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m ()
content courseListEv = do
  rec c <- foldDyn ($) (makeContent Grid) $ snd <$> activeEv
      let w = constDyn $ M.fromList
            [ (Grid, gridContent courseListEv)
            , (Info, infoContent (fmap (view selectedCourse) c))
            ]
              
      activeEv <- selectViewListWithKey (view activeView <$> c) w
        $ \_key valDyn selectedDyn -> do
          let dynAttrs = ffor selectedDyn $ \case
                True  -> ("class" =: "content-wrap")
                False -> ("class" =: "hidden")
          
          elDynAttr "div" dynAttrs (dyn valDyn) >>=
            switchPromptly never

  return ()            

gridContent :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m (Event t (Content -> Content))
gridContent courseListEv = do
  gridEv <- Grid.courseGrid courseListEv
  return $ ffor gridEv $ \case
    Grid.CourseSelected course ->
      set activeView Info .
      set selectedCourse (pure course)
  
infoContent :: forall t m.
  MonadWidget t m
  => Dynamic t (Maybe Course)
  -> m (Event t (Content -> Content)) 
infoContent courseDyn = do
  ev <- dyn $ ffor courseDyn $ \case
    Nothing     -> pure never
    Just course -> Info.courseInfo $ constDyn course
    
  ev' <- switchPromptly never ev 
  return $ ffor ev' $ \case
    Info.CourseInfoEvent -> set activeView Grid
