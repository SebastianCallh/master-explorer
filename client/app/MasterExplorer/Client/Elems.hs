module MasterExplorer.Client.Elems
  ( itemList
  , okButton
  , cancelButton
  , customButton
  ) where

import           Data.Text  (Text)
import           Reflex.Dom
import           Reflex.Dynamic (switchPromptlyDyn)
                        
import           MasterExplorer.Common.Class.ListItem (ListItem, listItemText)
  
customButton :: MonadWidget t m => Text -> m (Event t ())
customButton txt = divClass "custom-button" $ button txt

okButton :: MonadWidget t m => m (Event t ())
okButton = divClass "ok-button" $ button "Ok"

cancelButton :: MonadWidget t m => m (Event t ())
cancelButton = divClass "cancel-button" $ button "Cancel"

itemList :: forall t m a.
  (MonadWidget t m,
   DomBuilder t m,
   ListItem a)
  => Dynamic t [a]
  -> m (Event t a)
itemList coursesDyn = do
  eventsDyn <- elClass "ul" "course-list" $
    simpleList coursesDyn listItem
  return $ switchPromptlyDyn $ leftmost <$> eventsDyn

listItem :: forall t m a.
  (DomBuilder t m,
   MonadSample t m,
   ListItem a)
  => Dynamic t a
  -> m (Event t a)
listItem courseDyn =
  el "li" $ do
    course <- sample . current $ courseDyn
    click  <- _link_clicked <$> link (listItemText course)
    return $ course <$ click


  --return $ _ links
--    course <- sample $ current $ listItemText <$> courseDyn
