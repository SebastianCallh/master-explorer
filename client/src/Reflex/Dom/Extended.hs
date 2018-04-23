module Reflex.Dom.Extended
  ( module Reflex.Dom
  , ListItem (..)
  , FilterItem (..)
  , dynLink
  , itemList
  , filterList
  ) where

import qualified Data.Text as T
import qualified Data.Map  as M

import           Data.Text   (Text)
import           Reflex.Dom

dynLink :: forall t m.
  (DomBuilder t m,
   PostBuild t m)
  => Dynamic t Text
  -> m (Event t ())
dynLink textDyn = do
  (e, _) <- elDynAttr' "a" (M.empty <$ textDyn) $
    dynText textDyn
  return $ domEvent Click e

-- Item list --

class ListItem a where
  listItemText :: a -> Text

itemList :: forall t m a b.
  (MonadWidget t m,
  ListItem a)
  => Dynamic t [a]
  -> (Dynamic t a -> m (Event t b))
  -> m (Event t b)
itemList itemsDyn template  = do
  eventsDyn <- el "ul" $
    simpleList itemsDyn template
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

listItem :: forall t m a.
  (MonadWidget t m,
  ListItem a)
  => Dynamic t a
  -> m (Event t a)
listItem itemDyn =
  el "li" $ do
    ev <- dynLink $ listItemText <$> itemDyn
    return $ tagPromptlyDyn itemDyn ev

-- Filter list --
    
class (ListItem a) => FilterItem a where
  filterFields :: a -> [Text]

filterList :: forall t m a b.
  (MonadWidget t m,
   FilterItem a)
  => Dynamic t [a]
  -> (Dynamic t a -> m (Event t b))
  -> m (Event t b)
filterList itemsDyn template  = do
  filterDyn <- _textInput_value <$> textInput def  
  let filteredCourses = filterItems <$> filterDyn <*> itemsDyn
  divClass "filter-list" $
    itemList filteredCourses template

filterItems :: forall a.
  FilterItem a
  => T.Text
  -> [a]
  -> [a]
filterItems query = filter prefixFilter
  where
    prefixFilter c = any (\f -> T.toLower query `T.isPrefixOf` T.toLower f)  (filterFields c) 
