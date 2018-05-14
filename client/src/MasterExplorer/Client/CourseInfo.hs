{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Client.CourseInfo where

import           Control.Lens
import           Data.Semigroup                         ((<>))
import           Reflex.Dom.Extended 

import           MasterExplorer.Common.Data.Examination
import           MasterExplorer.Common.Data.Course      
import           MasterExplorer.Common.Class.Pretty     (pretty)

data CourseInfo t = CourseInfo
  { _onClose :: Event t ()
  }

makeLenses ''CourseInfo

data CourseInfoEvent = CourseInfoEvent

widget :: forall t m.
  MonadWidget t m
  => Dynamic t Course
  -> m (CourseInfo t)
widget courseDyn = do
  closeEv <- markup courseDyn
  return CourseInfo
    { _onClose = closeEv
    }


markup :: forall t m.
  MonadWidget t m
  => Dynamic t Course
  -> m (Event t ())
markup courseDyn = 
  divClass "course-info" $ do
    divClass "course-header" $ do
      el "h1" $ dynText $
        mconcat [ view courseCode <$> courseDyn
                , pure " - "
                , view courseName <$> courseDyn
                ]
      
      el "p" $ dynText $
        mconcat [ pretty . view courseCredits <$> courseDyn
                , pure " - "
                , pretty . view courseLevel <$> courseDyn
                ]
        
    el "h2" $ text "Kursinnehåll"
    _ <- divClass "course-section content" $
      dynText $ getCourseContent <$> courseDyn

    el "h2" $ text "Examinationsmoment" 
    _ <- divClass "course-section examinations" $
      el "table" $ do
        el "thead" $
          el "tr" $ do
            el "th" $ text "Kod"
            el "th" $ text "Beskrivning"
            el "th" $ text "Betygsättning"
            el "th" $ text "Hp"
        el "tbody" $     
          simpleList (view courseExaminations <$> courseDyn) $ \examinationDyn -> do
            let examTd f = el "td" $ dynText (f <$> examinationDyn)    
            el "tr" $ do
              examTd (view examinationCode)
              examTd (view examinationDescription)
              examTd (pretty . view examinationGrading)
              examTd (pretty . view examinationCredits)

    el "h2" $ text "Huvudområde" 
    _ <-divClass "course-section field" $
        el "ul" $
          simpleList (view courseFields <$> courseDyn) $ \fieldDyn ->
            el "li" $
              dynText $ pretty <$> fieldDyn

    el "h2" $ text "Examinator" 
    _ <-divClass "course-section examinator" $
        dyn $ ffor (view courseExaminator <$> courseDyn) $ \case
          Nothing         -> text "-"
          Just examinator -> text $ pretty examinator            

    el "h2" $ text "Ämnesområde" 
    _ <- divClass "course-section subject" $
      el "ul" $
        simpleList (view courseSubjects <$> courseDyn) $ \subjectDyn ->
          el "li" $
            dynText $ pretty <$> subjectDyn            
      
    el "h2" $ text "Länkar" 
    _ <- divClass "course-section links" $
      el "ul" $
        simpleList (view courseUrls <$> courseDyn) $ \dynUrl -> do
          let pUrl = pretty <$> dynUrl
          let attrs = ffor pUrl $ \url ->
                ("target" =: "_blank") <>
                ("href"   =: url)
                
          el "li" $ 
            elDynAttr "a" attrs $ dynText pUrl

    divClass "button-menu" $ do
      (e,_) <- elClass' "a" "button" $
        text "Tillbaka"
      return $ domEvent Click e
