{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module MasterExplorer.Client.Api  where

import           Control.Lens
import           Data.Proxy        (Proxy (..))
import           Reflex.Dom
import           Servant.Reflex    
import           Servant.API       

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Data.Schedule (Schedule)
import           MasterExplorer.Common.Api           (courseApi)

data Api t m = Api
  { _getProgramCourses :: Event t Program  -> m (Event t [Course])
  , _saveSchedule      :: Event t Schedule -> m (Event t Int)
  , _loadSchedule      :: Event t Int      -> m (Event t (Maybe Schedule))
  }

makeLenses ''Api

widget :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl
  -> m (Api t m)
widget apiUrlDyn = do
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrlDyn
      (getCourses :<|> _ :<|> callSaveSchedule :<|> callLoadSchedule) = api
  
      getProgramCourses' programEv = do
        eprogram <- holdDyn (Left "-") (pure <$> programEv)
        let trigger = () <$ updated eprogram
        fmapMaybe reqSuccess <$> getCourses eprogram trigger

      saveSchedule' scheduleEv = do
        scheduleDyn <- holdDyn  (Left "-") (pure <$> scheduleEv)
        let trigger = () <$ scheduleEv
        fmapMaybe reqSuccess <$> callSaveSchedule scheduleDyn trigger

      loadSchedule' scheduleIdEv = do
        scheduleIdDyn <- holdDyn  (Left "-") (pure <$> scheduleIdEv)
        let trigger = () <$ scheduleIdEv
        fmapMaybe reqSuccess <$> callLoadSchedule scheduleIdDyn trigger

  return Api
    { _getProgramCourses = getProgramCourses'
    , _saveSchedule      = saveSchedule'
    , _loadSchedule      = loadSchedule'
    }
