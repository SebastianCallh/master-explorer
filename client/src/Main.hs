{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Data.Text as T

import            Language.Javascript.JSaddle.Warp
import            Reflex.Dom.Core (mainWidget)
import            Reflex.Dom hiding (mainWidget, run)
import            Servant.Reflex                       

import           MasterExplorer.Client.Program          (programList)
import           MasterExplorer.Common.Data.Program     (engPrograms)
import           MasterExplorer.Client.Api              (programCourses)
import           MasterExplorer.Client.Course           (courseList)
import           MasterExplorer.Client.CourseRepository (courseRepository)


main :: IO ()
main = run 3911 $ mainWidget app
--main = mainWidget body
--  let url = constDyn $ BasePath "http://localhost:8080"

app :: forall t m.
  (MonadWidget t m,
   DomBuilder t m)  
  => m ()
app = divClass "header" $ do
  programSelectEv <- programList $ constDyn engPrograms
  
  let url = constDyn $ BasePath "http://localhost:8080"
  coursesEv      <- programCourses url programSelectEv
  coursesDyn     <- holdDyn [] coursesEv
  courseSelectEv <- courseList coursesDyn

  selectedCourses <- courseRepository courseSelectEv
  dynText $ T.pack . show <$> selectedCourses

--  return ()
