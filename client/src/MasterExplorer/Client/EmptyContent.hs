module MasterExplorer.Client.EmptyContent where

import           Reflex.Dom.Extended

-- | Content showed when no program is selected.
widget :: forall t m.
  MonadWidget t m
  => m ()
widget =
  divClass "content-empty" $
    el "h1" $
      text "Välj ett program för att börja!"
