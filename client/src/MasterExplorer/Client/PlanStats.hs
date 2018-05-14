module MasterExplorer.Client.PlanStats
  ( PlanStatsEvent (..)
  , planStats
  ) where

import qualified Data.Map                   as M
import qualified Data.Text as T

import           Control.Lens
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.Schedule
import           MasterExplorer.Common.Data.Course          
import           MasterExplorer.Client.ColGrid              (colGrid)
import           MasterExplorer.Common.Data.Examination     
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType(..))
import           MasterExplorer.Common.Data.Period          (Period)
import           MasterExplorer.Common.Data.Semester        (Semester)
import           MasterExplorer.Common.Data.Slot            (slotsInPeriod)

data PlanStatsEvent
  = Ev

planStats  :: forall t m.
  MonadWidget t m
  => Dynamic t Schedule
  -> m (Event t PlanStatsEvent)
planStats scheduleDyn = 
  colGrid "stats-column" $ gridCol <$> scheduleDyn

gridCol :: forall m t.
  MonadWidget t m
  => Schedule            -- ^ Current course selections. 
  -> (Semester, Period)  -- ^ The current column.
  -> m (Event t PlanStatsEvent)
gridCol schedule column = do
  let slots   = uncurry slotsInPeriod column
  let courses = slots   >>= flip (M.findWithDefault []) (getSchedule schedule)
  let exams   = courses >>= view courseExaminations
  let writtenExams = filter ((==TEN) . view examinationType) exams
  
  el "h2" $
    text $ mconcat [ "Tentor: "
                   , T.pack . show $ length writtenExams
                   ]
    
  return never
