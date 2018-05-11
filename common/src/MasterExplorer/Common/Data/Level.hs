{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Level where

import           Control.Lens                       hiding (Level)
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

data Level
  = G1
  | G2
  | A
  | A1
  | A2
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Pretty Level where
  pretty G1 = "Grundläggande nivå 1"
  pretty G2 = "Grundläggande nivå 2"
  pretty A  = "Avancerad nivå"
  pretty A1 = "Avancerad nivå 1"
  pretty A2 = "Avancerad nivå 2"


makePrisms ''Level
