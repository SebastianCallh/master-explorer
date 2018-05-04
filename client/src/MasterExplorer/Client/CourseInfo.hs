module MasterExplorer.Client.CourseInfo
  ( CourseInfo
  , CourseInfoEvent (..)
  , courseInfo
  ) where

import qualified Data.Map.Strict                        as M

import           Data.Text                              (Text, pack)
import           Reflex.Dom.Extended 
import           Text.HTML.Scalpel                      hiding (text)

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
    _ <- divClass "course-content" $
      dyn $ ffor (getCourseContent <$> courseDyn) $ \case
        Nothing      -> text ""
        Just content -> text content
                                                          

    el "h2" $ text "Examinationsmoment" 
    _ <- divClass "course-examinations" $
      el "table" $ do
        el "thead" $
          el "tr" $ do
            el "th" $ text "Code"
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
            
            
          {-simpleList (courseExaminations <$> courseDyn) $ \examinationDyn -> 
--        dynText $ pretty <$> examinationDyn
        tableDynAttr "Examinations-Table"        
        [("Code", \_k _rDyn -> dynText $ examCode <$> examinationDyn)]
        (constDyn (M.empty :: M.Map Int Bool))
        (const (pure $ constDyn M.empty))
        -}
{-ableDynAttr :: forall t m r k v. (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
-}
    divClass "button-menu" $ do
      (e,_) <- elClass' "a" "button" $
        text "Tillbaka"
      return $ CourseInfoEvent <$ domEvent Click e

getField :: Int -> Examination -> Text
getField _ _ = "hej"



    {-_ <- divClass "course-content" $ do

      
      let contentParser = (chroot "p" $
            chroot "ul" $
              texts "li") :: Scraper Text [Text]

      ev <- dyn $ ffor (getCourseContent <$> courseDyn) $ \mContent -> do
        text $ case mContent of
                 Just x -> pack $ show x
                 Nothing -> "nothing"
                 
        case mContent >>= flip scrapeStringLike contentParser of
          Nothing -> text "nothing"
          Just cs -> do
            simpleList (constDyn cs) display
            text "het"

      return ""
-}
--      undefined
      {-
      simpleList (parseDynContent <$> courseDyn) $ \case
          Nothing      -> text "Inget kursinnehåll tillgängligt :("
          Just content -> do
            return ()
-}
