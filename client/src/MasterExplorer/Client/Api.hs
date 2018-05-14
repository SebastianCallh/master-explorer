{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module MasterExplorer.Client.Api
  ( programCourses
  , saveSchedule
  , loadSchedule
  ) where

import           Data.Proxy        (Proxy (..))
import           Reflex.Dom
import           Servant.Reflex    
import           Servant.API       

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Data.Schedule (Schedule)
import           MasterExplorer.Common.Api           (courseApi)

programCourses :: forall t m.
  (SupportsServantReflex t m,
   MonadHold t m)
  => Dynamic t BaseUrl
  -> Event t Program
  -> m (Event t [Course])
programCourses url programEv = mdo
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url
  let (getCourses
       :<|> _updateCourses
       :<|> _saveSchedule
       :<|> _loadSchedule) = api
        
  eprogram <- holdDyn (Left "-") (pure <$> programEv)
  let trigger = () <$ updated eprogram
  fmapMaybe reqSuccess <$> getCourses eprogram trigger

saveSchedule :: forall t m.
  (SupportsServantReflex t m,
   MonadHold t m)
  => Dynamic t BaseUrl
  -> Event t Schedule
  -> m (Event t Int)
saveSchedule url scheduleEv = do
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url
  let (_getCourses
       :<|> _updateCourses
       :<|> saveSchedule'
       :<|> _loadSchedule) = api

  scheduleDyn <- holdDyn  (Left "-") (pure <$> scheduleEv)
  let trigger = () <$ scheduleEv
  fmapMaybe reqSuccess <$> saveSchedule' scheduleDyn trigger

loadSchedule :: forall t m.
  (SupportsServantReflex t m,
   MonadHold t m)
  => Dynamic t BaseUrl
  -> Event t Int
  -> m (Event t (Maybe Schedule))
loadSchedule url scheduleIdEv = do
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url
  let (_getCourses
       :<|> _updateCourses
       :<|> _saveSchedule
       :<|> loadSchedule') = api

  scheduleIdDyn <- holdDyn  (Left "-") (pure <$> scheduleIdEv)
  let trigger = () <$ scheduleIdEv
  fmapMaybe reqSuccess <$> loadSchedule' scheduleIdDyn trigger
