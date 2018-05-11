{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.CoursePart where

import           Control.Lens
import           Data.Aeson                                (FromJSON, ToJSON)
import           GHC.Generics                              (Generic)

import           MasterExplorer.Common.Data.CoursePartType (CoursePartType)
import           MasterExplorer.Common.Data.Hours          (Hours)

data CoursePart = CoursePart
  { partType :: !CoursePartType
  , partTime :: !Hours
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

makeLenses ''CoursePart
