{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.CourseName
  ( CourseName (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Data.Text    (Text)

newtype CourseName = CourseName { getName :: Text }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
