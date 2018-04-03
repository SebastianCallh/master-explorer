module MasterExplorer.Common.Class.ListItem
  ( ListItem
  , listItemText
  ) where

import           Data.Text (Text)

class ListItem a where
  listItemText :: a -> Text
