module MasterExplorer.Client.Program
  ( programList
  , programDropDown
  ) where

import qualified Data.Map.Strict                     as M

import           Data.Maybe                          (fromJust)
import           Data.Text                           (Text, unpack)
import           Data.Map.Strict                     (Map)
import           Reflex.Dom

import           MasterExplorer.Client.Elems         (itemList)
import           MasterExplorer.Common.Class.HasText (toText,fromText)
import           MasterExplorer.Common.Data.Program  (Program, engPrograms)

programList :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => Dynamic t [Program]
  -> m (Event t Program)
programList programsDyn =
  divClass "program-list" $ 
    itemList programsDyn

programDropDown :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)
  => m (Dynamic t Program)
programDropDown =
  divClass "program-dropdown" $ do
    let programs = toMap engPrograms
    dd <- dropdown 1 (constDyn programs) def    
    return $ toProgram programs <$> value dd

toProgram :: Map Int Text -> Int -> Program
toProgram progs i = either (error . unpack) id $ fromText selection
  where
    selection :: Text
    selection = fromJust $ M.lookup i progs

toMap :: [Program] -> Map Int Text
toMap =  M.fromList . zip [1..] . fmap toText
