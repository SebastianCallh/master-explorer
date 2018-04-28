{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Language.Javascript.JSaddle.Warp
import           Reflex.Dom                      hiding (mainWidgetWithCss, run)
import           Reflex.Dom.Core                        (mainWidgetWithCss)
import           Servant.Reflex
import           Data.FileEmbed                         (embedFile)

import           MasterExplorer.Common.Data.Program     (engPrograms)
import           MasterExplorer.Client.ProgramList      (programList)
import           MasterExplorer.Client.CourseGrid       (courseGrid)
import           MasterExplorer.Client.Api              (programCourses)
import           MasterExplorer.Client.CourseList       (courseList)

main :: IO ()
main = let css = $(embedFile "css/style.css") in
         run 3911 $ mainWidgetWithCss css app
--main = mainWidget body
--let url = constDyn $ BasePath "http://localhost:8080"
-- neat font <link href="https://fonts.googleapis.com/css?family=Abel" rel="stylesheet">

app :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)  
  => m ()
app =
  divClass "container" $ do
    programSelectEv <- divClass "header" $
      programList $ constDyn engPrograms
  
    let url = constDyn $ BasePath "http://localhost:8080"
    coursesEv  <- programCourses url programSelectEv
    coursesDyn <- holdDyn [] coursesEv
  
    courseSelectEv <- divClass "sidebar" $
      courseList coursesDyn

    divClass "info-bar" $
      text "HP: 12"
  
    _courseClicks <- divClass "content" $
      courseGrid courseSelectEv

    
    divClass "footer" $ pure ()
  
    pure ()
    

