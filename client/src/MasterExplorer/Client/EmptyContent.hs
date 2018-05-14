module MasterExplorer.Client.EmptyContent where

import           Reflex.Dom.Extended

emptyContent :: forall t m.
  MonadWidget t m
  => m ()
emptyContent =
  divClass "content-empty" $
    el "h1" $
      text "Välj ett program för att börja!"
