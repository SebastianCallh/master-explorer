module Reflex.Dom.Extended
  ( module Reflex.Dom
  , dynLink
  ) where

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

itemList :: forall t m a b.
  (MonadWidget t m,
   DomBuilder t m,
   MonadSample t m,
   ListItem a)
  => (Dynamic t a -> m (Event t b))
  -> Dynamic t [a]
  -> m (Event t b)
itemList template itemsDyn = do
  eventsDyn <- el "ul" $
    simpleList itemsDyn template
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

listItem :: forall t m a.
  (DomBuilder t m,
   MonadSample t m,
   PostBuild t m,
   ListItem a)
  => Dynamic t a
  -> m (Event t a)
listItem itemDyn =
  el "li" $ do
    ev <- dynLink $ listItemText <$> itemDyn
    return $ tagPromptlyDyn itemDyn ev

-- Filter list --
    
filterList :: forall t m a b.
  (MonadWidget t m,
   DomBuilder t m,
   ListItem a,
   FilterItem a)
  => (Dynamic t a -> m (Event t b))
  -> Dynamic t [a]
  -> m (Event t b)
filterList template itemsDyn = do
  filterDyn <- _textInput_value <$> textInput def  
  let filteredCourses = filterItems <$> filterDyn <*> itemsDyn
  divClass "filter-list" $
    itemList template filteredCourses 

filterItems :: forall a.
  FilterItem a
  => T.Text
  -> [a]
  -> [a]
filterItems query = filter prefixFilter
  where
    prefixFilter c = any (\f -> T.toLower query `T.isPrefixOf` T.toLower f)  (filterFields c) 
