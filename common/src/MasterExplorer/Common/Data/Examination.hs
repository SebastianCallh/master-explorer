{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Examination where

import           Control.Lens
import           Data.Aeson                                 (FromJSON, ToJSON)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           MasterExplorer.Common.Data.Credits         (Credits)
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType)
import           MasterExplorer.Common.Data.Grading         (Grading)

data Examination = Examination
  { _examinationCode        :: !Text
  , _examinationType        :: !ExaminationType
  , _examinationDescription :: !Text
  , _examinationGrading     :: !Grading
  , _examinationCredits     :: !Credits
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

makeLenses ''Examination
