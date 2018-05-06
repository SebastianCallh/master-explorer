{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Examinator
  ( Examinator (..)
  ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

newtype Examinator = Examinator { getExaminator :: Text }
    deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Pretty Examinator where
  pretty = getExaminator
