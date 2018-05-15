module MasterExplorer.Client.ColGrid where

import           Data.Bifunctor                        (bimap)
import           Data.Text                             (Text)
import           Data.Semigroup                        ((<>))
import           Reflex.Dom.Extended
  
import           MasterExplorer.Common.Data.Semester   (Semester, masterSemesters)
import           MasterExplorer.Common.Data.Block      (allBlocks)
import           MasterExplorer.Common.Data.Period     (Period, allPeriods)
import           MasterExplorer.Common.Class.Pretty    (Pretty, pretty)

newtype Column = Column { getColumn :: (Semester, Period) }

instance Pretty Column where
  pretty = uncurry (<>) . bimap pretty pretty . getColumn
  
-- | A grid of all slots in the master semesters.
--   Returns events that fire in grid slots.
widget :: forall t m a.
  MonadWidget t m  
  => Dynamic t Text -- ^ Extra text to be added to wrapping divs class.
  -> Dynamic t ((Semester, Period) -> m (Event t a)) -- ^ Function that generates a column.
--  -> Dynamic t (Slot -> m (Event t a)) 
  -> m (Event t a)
widget dynStyle colGenDyn = do
  eventsDyn <- divClass "course-grid" $ do
    _ <- divClass "grid-col" $ do
      let blocksDyn = constDyn allBlocks      
      
      divClass "grid-header" $ pure ()
      simpleList blocksDyn $ \block ->
        divClass "row-header" $
          dynText $ pretty <$> block

    simpleList (constDyn gridCols) $ \colDyn -> do
      let attrsDyn = ("class" =:) . (<> " grid-col") <$> dynStyle
      colEv <- elDynAttr "div" attrsDyn $ do
        divClass "grid-header" $
          dynText $ pretty <$> colDyn
        dyn $ colGenDyn <*> (getColumn <$> colDyn)
      switchPromptly never colEv

  return . switch . current $ leftmost <$> eventsDyn

-- | Columns are defined by a semester and period.
--   Master semesters only.
gridCols :: [Column]
gridCols = Column <$> ((,) <$> masterSemesters <*> allPeriods)
