{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Field
  ( Field (..)
  ) where


import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Field
  = Humanities
  | Medicine
  | Technical
  | Science
  | Society
  | Law
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
