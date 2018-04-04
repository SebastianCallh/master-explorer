module MasterExplorer.Client.CourseRepository
  ( courseRepository
  ) where

import qualified Data.Set                   as S

import           Data.Set                   (Set)
import           Reflex.Dom

import           MasterExplorer.Common.Data.Course (Course)

courseRepository :: forall t m.
  (MonadWidget t m,
   MonadHold t m)
  => Event t Course
  -> m (Dynamic t (Set Course))
courseRepository = foldDyn update S.empty 
  where
    update course repo =
      if course `S.member` repo
      then S.delete course repo
      else S.insert course repo
