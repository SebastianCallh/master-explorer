{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.Data.AppState
  (-- AppState (..)
  ) where
{-
import           Control.Lens
import           Data.Map                               (Map)
import           Reflex.Dom.Extended

import           MasterExplorer.Client.Data.FocusStatus (FocusStatus)
import           MasterExplorer.Common.Data.Course      (Course)
import           MasterExplorer.Common.Data.Slot        (Slot)

data AppState t = AppState
  { _slots :: !(Dynamic t (Map Slot [Course]))
  , _focus :: !(Dynamic t (Map Slot FocusStatus))
  }

makeLenses ''AppState
-}
