{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.CoursePartType
  ( CoursePartType (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data CoursePartType
  = Class
  | Group
  | Laboration
  | Lecture
  | Project
  | Seminar
  | Unspecified
  deriving (Show, Read, Generic, ToJSON, FromJSON)
