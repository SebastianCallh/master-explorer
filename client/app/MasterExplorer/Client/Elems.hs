module MasterExplorer.Client.Elems
  ( itemList
  , filterList
  , okButton
  , cancelButton
  , customButton
  ) where

import qualified Data.Text as T

import           Data.Text                              (Text)
import           Reflex.Dom
                        
import           MasterExplorer.Common.Class.ListItem   (ListItem, listItemText)
import           MasterExplorer.Common.Class.FilterItem (FilterItem, filterFields)

customButton :: MonadWidget t m => Text -> m (Event t ())
customButton txt = divClass "custom-button" $ button txt

okButton :: MonadWidget t m => m (Event t ())
okButton = divClass "ok-button" $ button "Ok"

cancelButton :: MonadWidget t m => m (Event t ())
cancelButton = divClass "cancel-button" $ button "Cancel"

cssClass :: Text
cssClass = "course-list"
 
itemList :: forall t m a.
  (MonadWidget t m,
   DomBuilder t m,
   MonadSample t m,
   ListItem a)
  => Dynamic t [a]
  -> m (Event t a)
itemList itemsDyn = do
  eventsDyn <- elClass "ul" cssClass $
    simpleList itemsDyn listItem
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
    dynText $ listItemText <$> itemDyn
    ev <- _link_clicked <$> link "Select"
    return $ tagPromptlyDyn itemDyn ev
    
filterList :: forall t m a.
  (MonadWidget t m,
   DomBuilder t m,
   ListItem a,
   FilterItem a)
  => Dynamic t [a]
  -> m (Event t a)
filterList itemsDyn = do
  filterDyn <- _textInput_value <$> textInput def
  let filteredCourses = filterItems <$> filterDyn <*> itemsDyn
  itemList filteredCourses

filterItems :: forall a.
  FilterItem a
  => T.Text
  -> [a]
  -> [a]
filterItems query = filter prefixFilter
  where
    prefixFilter c = any (\f -> T.toLower query `T.isPrefixOf` T.toLower f)  (filterFields c) 
