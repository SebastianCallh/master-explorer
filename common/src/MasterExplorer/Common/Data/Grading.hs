{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Grading
  ( Grading (..)
  ) where


import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Grading
  = Binary      -- U / G
  | Scale       -- U / 3 / 4 / 5
  | Presence    -- Mandatory presence
  | Unspecified -- There is a grading named "D". I do not know what it means.
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

