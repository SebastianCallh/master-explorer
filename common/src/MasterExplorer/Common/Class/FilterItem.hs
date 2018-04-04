module MasterExplorer.Common.Class.FilterItem
  ( FilterItem (..)
  ) where

import           Data.Text (Text)

class FilterItem a where
  filterFields :: a -> [Text]
