{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.CourseSelection where

import           Control.Lens
import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           MasterExplorer.Common.Data.Course (Course)
import           MasterExplorer.Common.Data.Slot   (Slot)

data CourseSelection = CourseSelection
  { _courseSelectionSlot   :: !Slot
  , _courseSelectionCourse :: !Course
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

makeLenses ''CourseSelection
