{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.ExaminationPart  where

import           Control.Lens
import           Data.Aeson                                 (FromJSON, ToJSON)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           MasterExplorer.Common.Data.Credits         (Credits)
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType)
import           MasterExplorer.Common.Data.Grading         (Grading)

data ExaminationPart = ExaminationPart
  { partCode        :: !Text
  , partTypes       :: !ExaminationType
  , partDescription :: !Text
  , partGrading     :: !Grading
  , partCredits     :: !Credits
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

makePrisms ''ExaminationPart
