{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Subject  where

import           Control.Lens
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data Subject
  = ComputerScience
  | Electronics
  | Electrotechnics
  | English
  | EnvironmentProtection
  | French
  | German
  | History
  | Informatics
  | Law
  | Leadership
  | LeadershipOrganisation
  | Matematik
  | MediaCommunication
  | MediaProduction
  | Organisation
  | Other
  | OtherMedicine
  | OtherTechnical
  | Philosophy
  | Physics
  | Spanish
  deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Pretty Subject where
  pretty ComputerScience        = "Datateknik"
  pretty Electronics            = "Elektronik"
  pretty Electrotechnics        = "Elektroteknik"
  pretty English                = "Engelska"
  pretty EnvironmentProtection  = "Milj\246v\229rd och milj\246skydd"
  pretty French                 = "Franska"
  pretty German                 = "Tyska"
  pretty History                = "Historia"
  pretty Informatics            = "Informatik/Data- och systemvetenskap"
  pretty Law                    ="Juridik och rättsvetenskap"
  pretty Leadership             = "Ledarskap"
  pretty LeadershipOrganisation = "Ledarskap, organisation och styrning"
  pretty Matematik              = "Matematik"
  pretty MediaCommunication     = "Medie- o kommunikationsvetenskap"
  pretty MediaProduction        = "Medieproduktion"
  pretty Organisation           = "Industriell ekonomi och organisation"
  pretty Other                  = "Övriga ämnen"
  pretty OtherMedicine          = "\214vrigt inom medicin"
  pretty OtherTechnical         = "\214vriga tekniska \228mnen"
  pretty Philosophy             = "Filosofi"
  pretty Physics                = "Fysik"
  pretty Spanish                = "Spanska"

makePrisms ''Subject
