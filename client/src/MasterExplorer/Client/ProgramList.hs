module MasterExplorer.Client.ProgramList
  ( programList
  ) where

import qualified Data.Map.Strict                     as M

import           Reflex.Dom.Extended
import           Data.Maybe                          (fromJust)
import           Data.Text                           (Text, unpack)
import           Data.Map.Strict                     (Map)

import           MasterExplorer.Common.Class.HasText (toText,fromText)
import           MasterExplorer.Common.Class.Pretty  (pretty)
import           MasterExplorer.Common.Data.Program  (Program)

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

toProgram :: Map Int Text -> Int -> Program
toProgram progs i = either (error . unpack) id $ fromText selection
  where
    selection :: Text
    selection = fromJust $ M.lookup i progs

toMap :: [Program] -> Map Int Text
toMap =  M.fromList . zip [1..] . fmap toText
