{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.ScheduleApiMenu where

import qualified Data.Text  as T

import           Control.Lens
import           Servant.Reflex                         (BaseUrl)
import           Reflex.Dom.Extended
import           Text.Read                              (readMaybe)

import           MasterExplorer.Client.Api              (saveSchedule, loadSchedule)
import           MasterExplorer.Common.Data.Schedule   


data ScheduleApiMenu t = ScheduleApiMenu
  { _scheduleApiMenu_onSave :: Event t Int              -- ^ Carries the integer id of the schedule
  , _scheduleApiMenu_onLoad :: Event t (Maybe Schedule) -- ^ Carries the maybe existing schedule
  }

makeLenses ''ScheduleApiMenu

scheduleApiMenu :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t Schedule
  -> m (ScheduleApiMenu t)
scheduleApiMenu apiUrlDyn scheduleDyn = 
  divClass "schedule-api-menu" $ do
    saveEv <- button "spara"
    savedEv <- saveSchedule apiUrlDyn $ tag (current scheduleDyn) saveEv
    
    loadEv <- button "ladda"
    inputEv  <- _textInput_input <$> textInput def
    let scheduleIdInputEv = fmapMaybe (readMaybe . T.unpack) inputEv          
    scheduleIdDyn <- holdDyn 0 scheduleIdInputEv
    loadedEv <- loadSchedule apiUrlDyn $ tag (current scheduleIdDyn) loadEv

    return ScheduleApiMenu
      { _scheduleApiMenu_onSave = savedEv
      , _scheduleApiMenu_onLoad = loadedEv
      }
