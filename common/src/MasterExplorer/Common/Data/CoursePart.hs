{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.CoursePart
  ( CoursePart (..)
  ) where

import           Data.Aeson                                (FromJSON, ToJSON)
import           GHC.Generics                              (Generic)

import           MasterExplorer.Common.Data.CoursePartType (CoursePartType)
import           MasterExplorer.Common.Data.Hours          (Hours)

data CoursePart = CoursePart
  { partType :: !CoursePartType
  , partTime :: !Hours
  } deriving (Show, Read, Generic, ToJSON, FromJSON)
