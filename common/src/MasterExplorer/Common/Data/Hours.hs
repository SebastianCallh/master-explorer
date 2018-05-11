{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Hours
  ( Hours(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

newtype Hours = Hours { getHours :: Int }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
