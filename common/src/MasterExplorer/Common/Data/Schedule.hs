{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Schedule where

import qualified Data.List                           as L
import qualified Data.Map                            as M
import qualified Data.Text                           as T

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bifunctor                      (bimap)
import           Data.Map                            (Map)
import           GHC.Generics                        (Generic)
import           Servant.API                         (FromHttpApiData,
                                                      ToHttpApiData,
                                                      parseUrlPiece, toUrlPiece)
import           Text.Read                           (readEither)

import           MasterExplorer.Common.Data.Course   (Course)
import           MasterExplorer.Common.Data.Occasion (Occasion (..))
import           MasterExplorer.Common.Data.Slot     (Slot)

-- | A schedule with a set of selected courses.
newtype Schedule = Schedule { getSchedule :: Map Slot [Course] }
  deriving (Show, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData Schedule where
  parseUrlPiece t =
    bimap T.pack id . readEither $ T.unpack t

instance ToHttpApiData Schedule where
  toUrlPiece = T.pack . show

addSelection :: Course -> Occasion -> Schedule -> Schedule
addSelection course occasion schedule =
  Schedule $ foldr (addToSlots course) (getSchedule schedule) (getOccasion occasion)
  where
    addToSlots c s = M.insertWith (++) s [c]

removeSelection :: Course -> Occasion -> Schedule -> Schedule
removeSelection course occasion schedule =
  Schedule $ foldr (removeFromSlots course) (getSchedule schedule) (getOccasion occasion)
  where
    removeFromSlots c = M.adjust (L.delete c)
