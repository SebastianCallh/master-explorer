{-# LANGUAGE RankNTypes          #-}

module MasterExplorer.Client.Course
  ( courseList
  ) where

import           Reflex.Dom

import MasterExplorer.Common.Data.Course (Course)
import MasterExplorer.Client.Elems       (itemList)

courseList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Course]
  -> m (Event t Course)
courseList coursesDyn = 
  divClass "course-list" $
    itemList coursesDyn
