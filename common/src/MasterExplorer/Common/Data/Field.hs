{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Field
  ( Field (..)
  ) where


import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data Field
  = Humanities
  | Law
  | Medicine
  | Science
  | Society
  | Technical
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Pretty Field where
  pretty Humanities = "Humanistiska"
  pretty Law        = "Medicinska"
  pretty Medicine   = "Tekniska"
  pretty Science    = "Naturvetenskapliga"
  pretty Society    = "Samhällsvetenskapliga"
  pretty Technical  = "Juridiska"
