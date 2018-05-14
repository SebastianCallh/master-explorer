{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Credits
  ( Credits (..)
  ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Semigroup                     ((<>))
import           Data.Text                          (pack)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

newtype Credits = Credits { getCredits :: Float }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)


-- |
instance Pretty Credits where
  pretty c =
    let roundedCredits = fromIntegral . round $ getCredits c in
      if fromInteger roundedCredits == getCredits c
      then pack (show roundedCredits) <> " hp"
      else pack (show $ getCredits c) <> " hp"
