{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Data.Text as T

import           Control.Monad.Fix (MonadFix)
import           Data.Monoid
import           Data.Proxy (Proxy (..))
import           Reflex.Dom
import           Servant.Reflex
import           Servant.API

import           MasterExplorer.Common.Data.Program
import           MasterExplorer.Common.Api          (courseApi)

--let client = apiClient "http://localhost" 8080
--let res = getCourses <$> client <*> pure prog
                   
main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  input <- textInput def
  let keypressEvent = T.pack . show <$> _textInput_keypress input
  keypressDyn <- holdDyn "None" keypressEvent
  dynText keypressDyn
  send <- button "Send"
  let prog = engD
  thing
  return ()

thing :: forall t m.
  (SupportsServantReflex t m,
   DomBuilder t m,
   DomBuilderSpace m ~ GhcjsDomSpace,
   MonadFix m,
   PostBuild t m,
   MonadHold t m)
  => m ()
thing = mdo
  let url = constDyn $ BasePath "http://localhost:8080"
{-  let tweakRequest = ClientOptions $ \r -> do
        putStrLn ("Got req: " ++ show r)
        return $ r & withCredentials .~ True-}
        
  let (number :<|> getCourses :<|> updateCourses) = 
        client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url -- tweakRequest        
  fireBtn <- button "kör"
  btn2 <- button "kurser"

  coursesResponse <- (getCourses $ constDyn . pure $ engD) btn2
  
  intResponse  <- number fireBtn
  let textInt = fmap (T.pack . show) <$> intResponse
  dynText  =<<
    holdDyn "(no errors)" (fmapMaybe reqFailure (coursesResponse))
    
  divClass "double-result" $ el "p" $ dynText =<<
    holdDyn "No number yet" (fmap (T.pack . show) $
                              fmapMaybe reqSuccess (coursesResponse))
  return () --fireBtn

--  resDyn <- holdDyn _ intResponse
--  dynText $ (T.pack . show) <$> resDyn

result :: Show a => Maybe a -> T.Text
result (Just x) = "Received: " <> T.pack (show x)
result Nothing = "Response is Nothing"

--main :: IO ()
--main = mainWidget $ text "hi"
