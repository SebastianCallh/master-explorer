{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Schedule
  ( Schedule (..)
  ) where

import qualified Data.Text                         as T

import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Bifunctor                    (bimap)
import           Data.Map                          (Map)
import           GHC.Generics                      (Generic)
import           Servant.API                       (FromHttpApiData,
                                                    ToHttpApiData,
                                                    parseUrlPiece, toUrlPiece)
import           Text.Read                         (readEither)

import           MasterExplorer.Common.Data.Course (Course)
import           MasterExplorer.Common.Data.Slot   (Slot)

-- | A schedule with a set of selected courses.
newtype Schedule = Schedule { getSchedule :: Map Slot [Course] }
  deriving (Show, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData Schedule where
  parseUrlPiece t =
    bimap T.pack id . readEither $ T.unpack t

instance ToHttpApiData Schedule where
  toUrlPiece = T.pack . show
