{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Prerequisites
  ( Prerequisites (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype Prerequisites = Prerequisites { getPrerequisites :: Text }
  deriving (Show, Read, Generic, ToJSON, FromJSON)
