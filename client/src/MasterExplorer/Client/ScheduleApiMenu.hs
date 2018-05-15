{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.ScheduleApiMenu where

import qualified Data.Text  as T

import           Control.Lens
import           Reflex.Dom.Extended
import           Text.Read                              (readMaybe)

import           MasterExplorer.Common.Data.Schedule   

data ScheduleApiMenu t = ScheduleApiMenu
  { _onSave :: Event t Int              -- ^ Carries the integer id of the schedule
  , _onLoad :: Event t (Maybe Schedule) -- ^ Carries the maybe existing schedule
  }

makeLenses ''ScheduleApiMenu

widget :: forall t m.
  MonadWidget t m
  => (Event t Schedule -> m (Event t Int))
  -> (Event t Int      -> m (Event t (Maybe Schedule)))
  -> Dynamic t Schedule
  -> m (ScheduleApiMenu t)
widget saveSchedule loadSchedule scheduleDyn = 
  divClass "schedule-api-menu" $ do
    saveEv  <- button "spara"
    savedEv <- saveSchedule $ tag (current scheduleDyn) saveEv
    
    loadEv  <- button "ladda"
    inputEv <- _textInput_input <$> textInput def
    let scheduleIdInputEv = fmapMaybe (readMaybe . T.unpack) inputEv          
    scheduleIdDyn <- holdDyn 0 scheduleIdInputEv
    loadedEv <- loadSchedule $ tag (current scheduleIdDyn) loadEv

    return ScheduleApiMenu
      { _onSave = savedEv
      , _onLoad = loadedEv
      }
