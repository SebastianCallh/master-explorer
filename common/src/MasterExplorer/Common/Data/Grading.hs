{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Grading  where


import           Control.Lens
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data Grading
  = Binary      -- U / G
  | Scale       -- U / 3 / 4 / 5
  | Presence    -- Mandatory presence
  | Unspecified -- There is a grading named "D". I do not know what it means.
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Pretty Grading where
   pretty Binary      = "U/G"
   pretty Scale       = "U/3/4/5"
   pretty Presence    = "Närvaro"
   pretty Unspecified = "Övrig betygsättning"

makePrisms ''Grading
