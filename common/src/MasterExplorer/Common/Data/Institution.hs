{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Institution
  ( Institution (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Institution
  = MAI
  | IDA
  | ISY
  | MED
  | IFM
  | IEE
  | ITN
  | TekFak
  | Tema
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
