{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Url
  ( Url (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype Url = Url { getUrl :: Text }
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
