module MasterExplorer.Client.CourseSelections
  ( SelectionChanged (..)
  ) where

data SelectionChanged t a
  = Selected t a
  | Deselected t a

