{-# LANGUAGE RecursiveDo     #-}

module Reflex.Dom.Extended
  ( module Reflex.Dom
  , itemList
  , listItem
  , filterList
  , dynLink
  , eventTabDisplay
  ) where

import qualified Data.Map                               as M
import qualified Data.Text                              as T

import           Control.Lens                           (iforM, imapM)
import           Data.Maybe                             (listToMaybe)
import           Data.Map                               (Map)
import           Data.Text                              (Text)
import           Reflex.Dom

import           MasterExplorer.Common.Class.ListItem   (ListItem, listItemText)
import           MasterExplorer.Common.Class.FilterItem (FilterItem, filterFields)

dynLink :: forall t m.
  (DomBuilder t m,
   PostBuild t m)
  => Dynamic t Text
  -> m (Event t ())
dynLink textDyn = do
  (e,_) <- elDynAttr' "a" (M.empty <$ textDyn) $
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


-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
eventTabDisplay :: forall t m k a. (MonadWidget t m, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, m (Event t a)) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m (Event t a) --a
eventTabDisplay ulClass activeClass tabItems = do
  let t0 = listToMaybe $ M.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass) $ do
        tabClicksList :: [Event t k] <- M.elems <$> imapM (\k (s,_) -> 
          headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
        
  el "div" $ do
    aMap <- iforM tabItems $ \k (_, w) -> do
          let isSelected = demuxed currentTab $ Just k
              attrs = ffor isSelected $ \s ->
                if s then M.empty else M.singleton "style" "display:none;"
          elDynAttr "div" attrs w
    
    return . leftmost $ snd <$> M.toList aMap

  where
    headerBarLink :: Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap (\b -> if b then M.singleton "class" activeClass else M.empty) isSelected
      elDynAttr "li" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)
