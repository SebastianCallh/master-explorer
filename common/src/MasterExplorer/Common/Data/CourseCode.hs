{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.CourseCode
  ( CourseCode (..)
  ) where


import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           Data.Text                          (Text)
import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

newtype CourseCode = CourseCode { getCode :: Text }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

instance Pretty CourseCode where
  pretty = getCode
