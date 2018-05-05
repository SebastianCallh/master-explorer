{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.ExaminationType
  ( ExaminationType (..)
  ) where

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
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Pretty ExaminationType where
  pretty TEN  = "Tentamen"
  pretty LAB  = "Laboration"
  pretty UPG  = "Övrig Uppgift"
  pretty AUSK = "Auskultation"
  pretty OPPO = "Opponering"
  pretty PROJ = "Projekt"
  pretty KTR  = "Kontrollskrivning"

