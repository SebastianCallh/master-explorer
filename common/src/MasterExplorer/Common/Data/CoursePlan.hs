{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.CoursePlan where

import qualified Data.List                           as L
import qualified Data.Map                            as M
import qualified Data.Text                           as T

import           Control.Lens
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bifunctor                      (bimap)
import           Data.Map                            (Map)
import           GHC.Generics                        (Generic)
import           Servant.API                         (FromHttpApiData,
                                                      ToHttpApiData,
                                                      parseUrlPiece, toUrlPiece)
import           Text.Read                           (readEither)

import           MasterExplorer.Common.Data.Course   (Course, courseSlots)
import           MasterExplorer.Common.Data.Occasion (Occasion (..))
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Common.Data.Slot     (Slot)

-- | A schedule with a set of selected courses.
data CoursePlan = CoursePlan
  { _coursePlanSchedule :: !(Map Slot [Course])
  , _coursePlanProgram  :: !Program
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData CoursePlan where
  parseUrlPiece t =
    bimap T.pack id . readEither $ T.unpack t

instance ToHttpApiData CoursePlan where
  toUrlPiece = T.pack . show

makeLenses ''CoursePlan

make :: Program -> CoursePlan
make program = CoursePlan
  { _coursePlanSchedule = M.empty
  , _coursePlanProgram  = program
  }

getSlotCourses :: Slot -> CoursePlan -> [Course]
getSlotCourses slot =
  M.findWithDefault [] slot . _coursePlanSchedule

addSelection :: Occasion -> Course -> CoursePlan -> CoursePlan
addSelection occasion course coursePlan =
  coursePlan & coursePlanSchedule %~ addOccasion
  where
    addOccasion = flip (foldr $ addToSlots course) $ getOccasion occasion
    addToSlots c s  = M.insertWith (++) s [c]

removeSelection :: Course -> CoursePlan -> CoursePlan
removeSelection course coursePlan =
  coursePlan & coursePlanSchedule %~ removeOccasion
  where
    removeOccasion = flip (foldr $ removeFromSlots course) $ courseSlots course
    removeFromSlots c = M.adjust (L.delete c)

courses :: CoursePlan -> [Course]
courses = foldMap snd . M.toList . _coursePlanSchedule

