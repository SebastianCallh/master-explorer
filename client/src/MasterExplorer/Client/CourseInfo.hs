module MasterExplorer.Client.CourseInfo
  ( CourseInfo
  , CourseInfoEvent (..)
  , courseInfo
  ) where

import           Data.Text                              (Text, pack)
import           Data.Semigroup                         ((<>))
import           Reflex.Dom.Extended 

import           MasterExplorer.Common.Data.Examination (Examination (..))
import           MasterExplorer.Common.Data.Course      (Course (..), getCourseCode,
                                                         getCourseName, getCourseContent)
import           MasterExplorer.Common.Class.Pretty     (pretty)

data CourseInfo = CourseInfo

data CourseInfoEvent = CourseInfoEvent

courseInfo :: forall t m.
  MonadWidget t m
  => Dynamic t Course
  -> m (Event t CourseInfoEvent)
courseInfo courseDyn =
  divClass "course-info" $ do

    divClass "course-header" $ do
      el "h1" $ dynText $
        mconcat [ getCourseCode <$> courseDyn
                , pure " - "
                , getCourseName <$> courseDyn
                ]
      
      el "p" $ dynText $
        mconcat [ pretty . courseCredits <$> courseDyn
                , pure " - "
                , pretty . courseLevel <$> courseDyn
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
          simpleList (courseExaminations <$> courseDyn) $ \examinationDyn -> do
            let examTd f = el "td" $ dynText (f <$> examinationDyn)    
            el "tr" $ do
              examTd examCode 
              examTd examDescription
              examTd (pretty . examGrading)
              examTd (pretty . examCredits)

    el "h2" $ text "Huvudområde" 
    _ <-divClass "course-section field" $
        el "ul" $
          simpleList (courseFields <$> courseDyn) $ \fieldDyn ->
            el "li" $
              dynText $ pretty <$> fieldDyn

    el "h2" $ text "Examinator" 
    _ <-divClass "course-section examinator" $
        dyn $ ffor (courseExaminator <$> courseDyn) $ \case
          Nothing         -> text "-"
          Just examinator -> text $ pretty examinator            

    el "h2" $ text "Ämnesområde" 
    _ <- divClass "course-section subject" $
      el "ul" $
        simpleList (courseSubjects <$> courseDyn) $ \subjectDyn ->
          el "li" $
            dynText $ pretty <$> subjectDyn            
      
    el "h2" $ text "Länkar" 
    _ <- divClass "course-section links" $
      el "ul" $
        simpleList (courseUrls <$> courseDyn) $ \dynUrl -> do
          let pUrl = pretty <$> dynUrl
          let attrs = ffor pUrl $ \url ->
                ("target" =: "_blank") <>
                ("href"   =: url)
                
          el "li" $ 
            elDynAttr "a" attrs $ dynText pUrl

    divClass "button-menu" $ do
      (e,_) <- elClass' "a" "button" $
        text "Tillbaka"
      return $ CourseInfoEvent <$ domEvent Click e
