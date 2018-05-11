{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.ExaminationType where

import           Control.Lens
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data ExaminationType
  = TEN
  | LAB
  | UPG
  | AUSK
  | OPPO
  | PROJ
  | KTR
  | MUN
  | ANN
  | MOM
  | BAS
  | DAT
  | HEM
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Pretty ExaminationType where
  pretty TEN  = "Tentamen"
  pretty LAB  = "Laboration"
  pretty AUSK = "Auskultation"
  pretty OPPO = "Opponering"
  pretty PROJ = "Projekt"
  pretty KTR  = "Kontrollskrivning"
  pretty MUN  = "Muntri"
  pretty ANN  = "Aktiv närvaro"
  pretty UPG  = "Övrigt moment"
  pretty MOM  = "Övrigt moment"
  pretty BAS  = "Basgruppsarbete"
  pretty DAT  = "Datortentamen"
  pretty HEM  = "Hemtentamen"

makePrisms ''ExaminationType
