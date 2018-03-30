module CourseScalpel.Data.Slot
  ( Slot (..)
  , mkSlot
  ) where

import           CourseScalpel.Data.Block        (Blocks (..))
import           CourseScalpel.Data.Period       (Period)
import           MasterExplorer.Common.Data.Slot (Slot (..))

mkSlot :: Period -> Blocks -> Slot
mkSlot periods blocks = Slot periods $ getBlocks blocks
