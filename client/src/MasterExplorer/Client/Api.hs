{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module MasterExplorer.Client.Api
  ( programCourses
  ) where

import           Data.Text         (Text)
import           Data.Proxy        (Proxy (..))
import           Reflex.Dom
import           Servant.Reflex    
import           Servant.API       

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Api           (courseApi)
import           MasterExplorer.Client.ProgramList   (ProgramListEvent (..))

programCourses :: forall t m.
  (SupportsServantReflex t m,
   MonadHold t m)
  => Dynamic t BaseUrl
  -> Event t ProgramListEvent
  -> m (Event t [Course])
programCourses url programEv = mdo
  let api = client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url
  let (_number :<|> getCourses :<|> _updateCourses) = api
  eprogram <- holdDyn (Left "-") (unpackEvent <$> programEv)
  let trigger = () <$ updated eprogram
  fmapMaybe reqSuccess <$> getCourses eprogram trigger

unpackEvent :: ProgramListEvent -> Either Text Program
unpackEvent (ProgramSelected   p) = pure p
unpackEvent (ProgramDeselected p) = pure p
