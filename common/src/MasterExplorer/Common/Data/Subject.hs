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
  | Electronics
  | EnvironmentProtection
  | English
  | French
  | German
  | History
  | Informatics
  | Law
  | LeadershipOrganisation
  | Leadership
  | Matematik
  | MediaCommunication
  | MediaProduction
  | Organisation
  | OtherMedicine
  | OtherTechnical
  | Physics
  | Philosophy
  | Spanish
  | Other
  deriving (Show, Read, Generic, ToJSON, FromJSON)

