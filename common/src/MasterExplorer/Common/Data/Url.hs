{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Url
  ( Url (..)
  ) where


import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

newtype Url = Url { getUrl :: Text }
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

instance Pretty Url where
  pretty = getUrl
