{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Occasion
  ( Occasion (..)
  , toMasterOccasions
  , occasionSemester
  ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Text                           (intercalate)
import           GHC.Generics                        (Generic)

import           Data.Semigroup                      (Semigroup, (<>))

import qualified MasterExplorer.Common.Data.Semester as Semester

import           MasterExplorer.Common.Class.Pretty  (Pretty, pretty)
import           MasterExplorer.Common.Data.Semester (Semester)
import           MasterExplorer.Common.Data.Slot     (Slot (..))

newtype Occasion = Occasion { getOccasion :: [Slot] }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

instance Semigroup Occasion where
  (<>) x y = Occasion $ getOccasion x <> getOccasion y

instance Pretty Occasion where
  pretty = intercalate "-" . fmap pretty .  getOccasion

toMasterOccasions :: Occasion -> [Occasion]
toMasterOccasions occasion
  | semester == Semester.One   = oddSem
  | semester == Semester.Two   = evenSem
  | semester == Semester.Three = oddSem
  | semester == Semester.Four  = evenSem
  | semester == Semester.Five  = oddSem
  | semester == Semester.Six   = evenSem
  | semester == Semester.Seven = oddSem
  | semester == Semester.Eight = evenSem
  | semester == Semester.Nine  = oddSem
  | semester == Semester.Ten   = evenSem
  | otherwise                          = []
  where
    oddSem  = [ moveToSemester Semester.Seven occasion
              , moveToSemester Semester.Nine occasion
              ]
    evenSem = [ moveToSemester Semester.Eight occasion ]

    -- Assumes all slots in occasion has the same semester
    semester = slotSemester . head $ getOccasion occasion

moveToSemester :: Semester -> Occasion -> Occasion
moveToSemester semester =
  Occasion . fmap setSemester . getOccasion
  where
    setSemester s = s { slotSemester = semester }

occasionSemester :: Occasion -> Semester
occasionSemester = slotSemester . head . getOccasion
