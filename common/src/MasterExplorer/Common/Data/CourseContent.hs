{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.CourseContent
  ( CourseContent (..)
  ) where


import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype CourseContent = CourseContent { getCourseContent :: Text }
  deriving (Show, Read, Generic, ToJSON, FromJSON)
