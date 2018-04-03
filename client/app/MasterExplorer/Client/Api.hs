{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module MasterExplorer.Client.Api
  ( programCourses
  ) where

import           Data.Proxy        (Proxy (..))
import           Reflex.Dom
import           Servant.Reflex    
import           Servant.API       

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Api           (courseApi)

programCourses :: forall t m.
  (SupportsServantReflex t m,
   MonadHold t m)
  => Dynamic t BaseUrl
  -> Event t Program
  -> m (Event t [Course])
programCourses url programEv = mdo
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url
  let (_number :<|> getCourses :<|> _updateCourses) = api
  eprogram <- holdDyn (Left "-") (Right <$> programEv)
  let trigger = () <$ updated eprogram
  fmapMaybe reqSuccess <$> getCourses eprogram trigger
