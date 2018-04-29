{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo     #-}

module MasterExplorer.Client.ProgramList
  ( ProgramListEvent (..)
  , programList
  ) where

import           Reflex.Dom.Extended
import           Control.Lens

import           MasterExplorer.Common.Class.Pretty  (pretty)
import           MasterExplorer.Common.Data.Program  (Program)

data ProgramListEvent
  = ProgramSelected   Program
  | ProgramDeselected Program

data ProgramList = ProgramList
  { _programs   :: ![Program]
  , _selection  :: !(Maybe Program)
  }

makeLenses ''ProgramList

mkProgramList :: [Program] -> ProgramList
mkProgramList ps = ProgramList
  { _programs   = ps
  , _selection = Nothing 
  }

programList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => [Program]
  -> m (Event t ProgramListEvent)
programList ps = do
  rec list   <- foldDyn updateSelection (mkProgramList ps) events
      events <- programListWidget list      
  return events

  where
    updateSelection (ProgramSelected   p) pl = pl & selection .~ pure p
    updateSelection (ProgramDeselected _) pl = pl & selection .~ Nothing      
  
programListWidget :: forall t m.
  MonadWidget t m
  => Dynamic t ProgramList
  -> m (Event t ProgramListEvent)
programListWidget programListDyn = do
  events <- divClass "program-list" $
    el "ul" $
      simpleList (view programs <$> programListDyn) $
        programListItem programListDyn

  return . switch . current $ leftmost <$> events

programListItem :: forall t m.
  MonadWidget t m
  => Dynamic t ProgramList
  -> Dynamic t Program
  -> m (Event t ProgramListEvent)
programListItem programListDyn programDyn = do
  event <- el "li" $ 
    dyn $ ffor programDyn $ \program -> do
      let tupleDyn = ffor programListDyn $ \pl ->
            if pure program == pl ^. selection
            then (ProgramDeselected, "class" =: "selected")
            else (ProgramSelected, "class" =: "not-selected")

      let eventDyn = fst <$> tupleDyn <*> pure program
      let styleDyn = snd <$> tupleDyn
      
      (e, _) <- elDynAttr' "a" styleDyn $
        text $ pretty program
      
      return $ tag (current eventDyn) $ domEvent Click e

  switchPromptly never event
