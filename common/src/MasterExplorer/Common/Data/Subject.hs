{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Subject
  ( Subject (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Subject
  = ComputerScience
  | Electrotechnics
  | EnvironmentProtection
  | English
  | French
  | German
  | History
  | Informatics
  | Leadership
  | Matematik
  | MediaCommunication
  | Organisation
  | OtherMedicine
  | OtherTechnical
  | Physics
  | Spanish
  deriving (Show, Read, Generic, ToJSON, FromJSON)

