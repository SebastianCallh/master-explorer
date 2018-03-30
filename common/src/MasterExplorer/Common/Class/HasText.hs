module MasterExplorer.Common.Class.HasText where

import           Data.Text (Text)

class HasText a where
  toText   :: a    -> Text
  fromText :: Text -> Either Text a
