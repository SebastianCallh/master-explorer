{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo     #-}

module MasterExplorer.Client.ProgramList where

import           Reflex.Dom.Extended
import           Control.Lens

import           MasterExplorer.Common.Class.Pretty  (pretty)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Data.Course   (Course)

data ProgramListEvent
  = ProgramSelected   Program
  | ProgramDeselected Program

data ProgramList t = ProgramList
  { _programs        :: !(Dynamic t [Program])
  , _selectedProgram :: !(Dynamic t (Maybe Program))
  , _selectedCourses :: !(Dynamic t [Course])
  }

makeLenses ''ProgramList

widget :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => (Event t Program -> m (Event t [Course]))
  -> Dynamic t [Program]
  -> m (ProgramList t)
widget getCourses programsDyn = do
  rec
    selectedProg <- foldDyn updateSelection Nothing events
    progCourses <- holdDyn [] =<< getCourses (fmapMaybe id $ updated selectedProg)
    events  <- programListWidget programsDyn selectedProg

  return ProgramList
    { _programs        = programsDyn
    , _selectedProgram = selectedProg
    , _selectedCourses = progCourses
    }

  where
    updateSelection (ProgramSelected   p) = const $ pure p
    updateSelection (ProgramDeselected _) = const Nothing      
  
programListWidget :: forall t m.
  MonadWidget t m
  => Dynamic t [Program]
  -> Dynamic t (Maybe Program)
  -> m (Event t ProgramListEvent)
programListWidget programsDyn mSelectedProgramDyn = do
  events <- divClass "program-list" $
    el "ul" $
      simpleList programsDyn $
        programListItem mSelectedProgramDyn

  return . switch . current $ leftmost <$> events

programListItem :: forall t m.
  MonadWidget t m
  => Dynamic t (Maybe Program)
  -> Dynamic t Program
  -> m (Event t ProgramListEvent)
programListItem mSelectedProgramDyn programDyn = do
  event <- el "li" $ 
    dyn $ ffor programDyn $ \program -> do
      let tupleDyn = ffor mSelectedProgramDyn $ \selectedProg ->
            if pure program == selectedProg
            then (ProgramDeselected, "class" =: "selected")
            else (ProgramSelected, "class" =: "not-selected")

      let eventDyn = fst <$> tupleDyn <*> pure program
      let styleDyn = snd <$> tupleDyn
      
      (e, _) <- elDynAttr' "a" styleDyn $
        text $ pretty program
      
      return $ tag (current eventDyn) $ domEvent Click e

  switchPromptly never event
