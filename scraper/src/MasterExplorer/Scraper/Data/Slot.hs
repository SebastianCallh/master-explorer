module MasterExplorer.Scraper.Data.Slot
  ( Slot (..)
  , mkSlot
  ) where

import           MasterExplorer.Common.Data.Slot    (Slot (..))
import           MasterExplorer.Scraper.Data.Block  (Blocks (..))
import           MasterExplorer.Scraper.Data.Period (Period)

mkSlot :: Period -> Blocks -> Slot
mkSlot periods blocks = Slot periods $ getBlocks blocks
