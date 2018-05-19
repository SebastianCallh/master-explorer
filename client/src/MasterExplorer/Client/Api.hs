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

import           MasterExplorer.Common.Data.Course     (Course)
import           MasterExplorer.Common.Data.Program    (Program)
import           MasterExplorer.Common.Data.CoursePlan (CoursePlan)
import           MasterExplorer.Common.Api             (courseApi)

data Api t m = Api
  { _getProgramCourses :: Event t Program    -> m (Event t [Course])
  , _saveCoursePlan    :: Event t CoursePlan -> m (Event t Int)
  , _loadCoursePlan    :: Event t Int        -> m (Event t (Maybe CoursePlan))
  }

makeLenses ''Api

widget :: forall t m.
  MonadWidget t m
  => Dynamic t BaseUrl
  -> m (Api t m)
widget apiUrlDyn = do
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrlDyn
      (getCourses :<|> _ :<|> callSaveCoursePlan :<|> callLoadCoursePlan) = api
  
      getProgramCourses' programEv = do
        eprogram <- holdDyn (Left "-") (pure <$> programEv)
        let trigger = () <$ programEv
        fmapMaybe reqSuccess <$> getCourses eprogram trigger

      saveCoursePlan' scheduleEv = do
        scheduleDyn <- holdDyn  (Left "-") (pure <$> scheduleEv)
        let trigger = () <$ scheduleEv
        fmapMaybe reqSuccess <$> callSaveCoursePlan scheduleDyn trigger

      loadCoursePlan' scheduleIdEv = do
        scheduleIdDyn <- holdDyn  (Left "-") (pure <$> scheduleIdEv)
        let trigger = () <$ scheduleIdEv
        fmapMaybe reqSuccess <$> callLoadCoursePlan scheduleIdDyn trigger


  return Api
    { _getProgramCourses = getProgramCourses'
    , _saveCoursePlan    = saveCoursePlan'
    , _loadCoursePlan    = loadCoursePlan'
    }
