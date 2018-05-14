module MasterExplorer.Client.Data.SelectStatus
  ( SelectStatus (..)
  ) where

import           MasterExplorer.Common.Data.Occasion (Occasion)

data SelectStatus
  = Available
  | InSelection
  | Selected Occasion
