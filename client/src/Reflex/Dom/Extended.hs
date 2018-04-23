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
