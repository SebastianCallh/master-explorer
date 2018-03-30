{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Credits
  ( Credits (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

newtype Credits = Credits { getCredits :: Float }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
