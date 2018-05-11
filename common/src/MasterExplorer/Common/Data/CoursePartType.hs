{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.CoursePartType where

import           Control.Lens
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

makePrisms ''CoursePartType
