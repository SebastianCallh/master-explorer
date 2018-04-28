{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Reflex.Dom                       as Dom
import qualified Reflex.Dom.Core                  as Core

import           Data.FileEmbed                   (embedFile)
import           Language.Javascript.JSaddle.Warp
import           Servant.Reflex

import           MasterExplorer.Client.App        (app)

data Mode
  = Development
  | Production

main :: IO ()
main = runApp Development

runApp :: Mode -> IO ()
runApp Development =
  run 3911 $ Core.mainWidgetWithCss css $ app apiUrl
  where css    = $(embedFile "css/style.css")
        apiUrl = Dom.constDyn $ BasePath "http://localhost:8080"

runApp Production =
  Dom.mainWidgetWithCss css $ app apiUrl
  where css    = $(embedFile "css/style.css")
        apiUrl = Dom.constDyn $ BasePath "http://localhost:8080"
