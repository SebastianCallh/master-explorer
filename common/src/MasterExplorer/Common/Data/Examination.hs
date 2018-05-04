{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Examination
  ( Examination (..)
  ) where

import           Data.Aeson                                 (FromJSON, ToJSON)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           MasterExplorer.Common.Data.Credits         (Credits)
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType)
import           MasterExplorer.Common.Data.Grading         (Grading)

data Examination = Examination
  { examCode        :: !Text
  , examType        :: !ExaminationType
  , examDescription :: !Text
  , examGrading     :: !Grading
  , examCredits     :: !Credits
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

