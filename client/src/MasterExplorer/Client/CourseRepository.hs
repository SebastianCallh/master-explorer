module MasterExplorer.Client.CourseRepository
  ( courseRepository
  ) where

import qualified Data.Map                   as M
import qualified Data.List                  as L

import           Data.Map                   (Map)
import           Reflex.Dom

import           MasterExplorer.Common.Data.Course (Course)
import           MasterExplorer.Common.Data.Slot   (Slot)

courseRepository :: forall t m.
  (MonadWidget t m,
   MonadHold t m)
  => Event t (Slot, Course)
  -> m (Dynamic t (Map Slot [Course]))
courseRepository = foldDyn update M.empty
  where
    update :: (Slot, Course) -> Map Slot [Course] -> Map Slot [Course]
    update (slot, course) repo =
      if course `elem` M.findWithDefault [] slot repo 
      then M.adjust (L.delete course) slot repo
      else M.insertWith (++) slot [course] repo
    
