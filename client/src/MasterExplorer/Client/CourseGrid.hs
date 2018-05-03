{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseGrid
  ( CourseGrid
  , CourseGridEvent (..)
  , courseGrid
  ) where

import qualified Data.Map                   as M
import qualified Data.List                  as L

import           Control.Lens
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import           Data.Map                   (Map)
import           Reflex.Dom.Extended

import qualified MasterExplorer.Client.CourseList      as List
import           MasterExplorer.Client.CourseList      (CourseListEvent)
import           MasterExplorer.Common.Data.Course     (Course, getCourseName,
                                                        getCourseCode, masterOccasions)
import           MasterExplorer.Common.Data.Slot       (Slot (..))
import           MasterExplorer.Common.Data.Occasion   (getOccasion)
import           MasterExplorer.Common.Data.Block      (allBlocks)
import           MasterExplorer.Common.Data.Period     (Period, allPeriods)
import           MasterExplorer.Common.Data.Semester   (Semester, masterSemesters)
import           MasterExplorer.Common.Class.Pretty    (Pretty, pretty)

data CourseGridEvent
  = CourseSelected Course

data CourseGrid = CourseGrid
  { _slots :: !(Map Slot [Course])
  , _focus :: !(Map Slot (Map Text Text))
  }

makeLenses ''CourseGrid

emptyGrid :: CourseGrid
emptyGrid = CourseGrid
  { _slots = M.empty
  , _focus = M.empty
  }

-- | Grid view of all master semesters and
--   currently selected courses.
courseGrid :: forall t m.
  MonadWidget t m
  => Event t CourseListEvent
  -> m (Event t CourseGridEvent)
courseGrid courseSelectedEv = do
  coursesGridDyn <- foldDyn update emptyGrid courseSelectedEv
  gridWidget coursesGridDyn
  where
    update :: CourseListEvent -> CourseGrid -> CourseGrid
    update (List.CourseSelected   c o) g =
      g & slots .~ foldr (addToSlots c) (g ^. slots) (getOccasion o)

    update (List.CourseDeselected c o) g =
      g & slots .~ foldr (removeFromSlots c) (g ^. slots) (getOccasion o)

    update (List.CoursePreSelected _) g = g
    
    update (List.CourseMouseEnter c) g =
      g & focus .~ foldr addFocus (g ^. focus) (masterSlots c) 

    update (List.CourseMouseLeave c) g =
      g & focus .~ foldr removeFocus (g ^. focus) (masterSlots c)

    addToSlots :: Course -> Slot -> Map Slot [Course] -> Map Slot [Course]
    addToSlots c s = M.insertWith (++) s [c]
    
    removeFromSlots :: Course -> Slot -> Map Slot [Course] -> Map Slot [Course]
    removeFromSlots c = M.adjust (L.delete c)

    removeFocus = M.delete
    addFocus s  = M.insert s ("class" =: "focused")

    masterSlots :: Course -> [Slot]
    masterSlots = concatMap getOccasion . masterOccasions
    
data SemesterPeriod = SemesterPeriod
  { spSemester :: !Semester
  , spPeriod   :: !Period
  }

instance Pretty SemesterPeriod where
  pretty SemesterPeriod{..} =
    pretty spSemester <> pretty spPeriod

semesterPeriods :: [SemesterPeriod]
semesterPeriods =
  SemesterPeriod  <$>
  masterSemesters <*>
  allPeriods

semesterPeriodsSlots :: SemesterPeriod -> [Slot]
semesterPeriodsSlots SemesterPeriod{..} =
  Slot spSemester spPeriod <$> allBlocks
  
gridWidget :: forall t m.
  MonadWidget t m
  => Dynamic t CourseGrid
  -> m (Event t CourseGridEvent)
gridWidget grid = do
  eventsDyn <- divClass "course-grid" $ do
    
    _ <- divClass "grid-col" $ do
      let blocksDyn = constDyn allBlocks

      divClass "grid-header" $ pure ()
      simpleList blocksDyn $ \block ->
        divClass "row-header" $
          dynText $ pretty <$> block
    
    let rowData = (\sp -> (sp, semesterPeriodsSlots sp)) <$> semesterPeriods
    simpleList (constDyn rowData) $ gridCol grid

  return . switch . current $ leftmost <$> eventsDyn
  
gridCol :: forall m t.
  MonadWidget t m
  => Dynamic t CourseGrid
  -> Dynamic t (SemesterPeriod, [Slot])
  -> m (Event t CourseGridEvent)
gridCol gridDyn periodSlotsDyn = do
  let slotsDyn       = snd <$> periodSlotsDyn
  let semesterPeriod = fst <$> periodSlotsDyn

  eventsDyn <- divClass "grid-col" $ do
    divClass "grid-header" $
      dynText $ pretty <$> semesterPeriod

    simpleList slotsDyn $ \slotDyn ->
      gridItem gridDyn slotDyn

  return . switch . current $ leftmost <$> eventsDyn
  
gridItem :: forall m t.
  MonadWidget t m
  => Dynamic t CourseGrid
  -> Dynamic t Slot
  -> m (Event t CourseGridEvent)
gridItem gridDyn slotDyn = do
  let coursesDyn    = M.findWithDefault []              <$> slotDyn <*> (view slots <$> gridDyn)
  let focusStyleDyn = M.findWithDefault ("class" =: "") <$> slotDyn <*> (view focus <$> gridDyn)
  let styleAdjust   = ffor coursesDyn $ \case
        [] -> M.adjust ("grid-slot empty "     <>) "class"
        _  -> M.adjust ("grid-slot non-empty " <>) "class"

  elDynAttr "div" (styleAdjust <*> focusStyleDyn)  $ do
    event <- dyn $ ffor coursesDyn $ \courses -> do
      let widgetMap = foldr makeWidgetMap M.empty courses 
      courseEv <- eventTabDisplay "course-list" "active-course" widgetMap
      return $ CourseSelected <$> courseEv
    switchPromptly never event
    
  where    
    makeWidgetMap c = M.insert (getCourseCode c)
      (getCourseCode c,
        divClass "visible-course" $ do
          l <- button $ getCourseName c
          return $ c <$ l)
