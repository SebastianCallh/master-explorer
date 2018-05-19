{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.PlanStats where

import qualified Data.Text as T

import           Control.Lens
import           Reflex.Dom.Extended

import           MasterExplorer.Common.Data.CoursePlan      (CoursePlan)
import           MasterExplorer.Common.Data.Course          
import           MasterExplorer.Common.Data.Examination     
import           MasterExplorer.Common.Data.ExaminationType (ExaminationType(..))
import           MasterExplorer.Common.Data.Period          (Period)
import           MasterExplorer.Common.Data.Semester        (Semester)
import           MasterExplorer.Common.Data.Slot            (slotsInPeriod)
import qualified MasterExplorer.Common.Data.CoursePlan      as CP

import qualified MasterExplorer.Client.ColGrid              as ColGrid

data PlanStats t = PlanStats
  { _onClose :: Event t ()
  }

makeLenses ''PlanStats

widget :: forall t m.
  MonadWidget t m
  => Dynamic t CoursePlan
  -> m (PlanStats t)
widget coursePlanDyn = do
  closeEv <- ColGrid.widget "stats-column" $ gridCol <$> coursePlanDyn
  return PlanStats
    { _onClose = closeEv
    }
    
gridCol :: forall m t.
  MonadWidget t m
  => CoursePlan          -- ^ Current course selections. 
  -> (Semester, Period)  -- ^ The current column.
  -> m (Event t ())
gridCol coursePlan column = do
  let slots   = uncurry slotsInPeriod column
  let courses = foldMap (`CP.getSlotCourses` coursePlan) slots
  let exams   = courses >>= view courseExaminations
  let writtenExams = filter ((==TEN) . view examinationType) exams
  
  el "h2" $
    text $ mconcat [ "Tentor: "
                   , T.pack . show $ length writtenExams
                   ]
    
  return never
