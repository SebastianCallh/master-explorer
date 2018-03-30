module CourseScalpel.Web.CourseScraper
  ( scrapePageCourse
  ) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Text.HTML.Scalpel              (Scraper, anySelector, chroot,
                                                 chroots, hasClass, innerHTML,
                                                 scrapeURL, (@:))

import           CourseScalpel.Data.PageCourse  (PageCourse (..),
                                                 fromPageSections)
import           CourseScalpel.Web.MarkupParser (parseSections)

scrapePageCourse :: Text -> IO (Either Text PageCourse)
scrapePageCourse url = do
  msections <- scrapeURL (T.unpack url) courseScraper
  return $ maybe errorMsg toPageCourse msections
  where
    toPageCourse = Right . fromPageSections . parseSections
    errorMsg  = Left $ mconcat [ "Error when parsing url "
                               , url
                               , ": "
                               ]

    courseScraper :: Scraper Text [Text]
    courseScraper = chroot ("section" @: [hasClass "syllabus-main"]) . chroots "div" $
      innerHTML anySelector
