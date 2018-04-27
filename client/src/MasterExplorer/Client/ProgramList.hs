module MasterExplorer.Client.ProgramList
  ( programList
  ) where

import           Reflex.Dom

import           MasterExplorer.Common.Class.Pretty  (pretty)
import           MasterExplorer.Common.Data.Program  (Program)
import           MasterExplorer.Client.Elems         (dynLink)

programList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Program]
  -> m (Event t Program)
programList programsDyn =
  divClass "program-list" $ do
  eventsDyn <- el "ul" $
    simpleList programsDyn $ \programDyn ->
    el "li" $ do
      ev <- dynLink $ pretty <$> programDyn
      return $ tagPromptlyDyn programDyn ev

  return $ switchPromptlyDyn $ leftmost <$> eventsDyn
