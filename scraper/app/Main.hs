{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict                        as M

import           Control.Monad                          (forM)
import           Data.Either                            (partitionEithers)
import           Data.Map.Strict                        (Map)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (Text, unpack)
import           Text.HTML.Scalpel                      (scrapeURL)

import           MasterExplorer.Common.Client           (apiClient, postCourses)
import           MasterExplorer.Scraper.Data.Course     (fromPartials,
                                                         getCourseCode)
import           MasterExplorer.Scraper.Data.CourseCode (CourseCode)
import           MasterExplorer.Scraper.Data.ListCourse (ListCourse (..))
import           MasterExplorer.Scraper.Data.PageCourse (PageCourse)
import           MasterExplorer.Scraper.Data.Program    (engD, programUrl)
import           MasterExplorer.Scraper.Data.Url        (getUrl)
import           MasterExplorer.Scraper.Helpers         (maybeToEither)
import           MasterExplorer.Scraper.Web.Scrapers    (pageCourseScraper,
                                                         programplanScraper)

main :: IO ()
main = do
  print "Scraping courses..."
  let url = unpack . getUrl $ programUrl engD
  listCourses <- scrapeURL url $ programplanScraper engD
  case listCourses of
    Nothing        -> fail $ "Failed to get page of " <> url
    Just elCourses -> do
      let (lErrors, lCourses) = partitionEithers elCourses

      -- Since courses can be in several periods/profiles they
      -- need to be merged in a meaningful way
      let lCoursesMap = foldr insert M.empty lCourses
      epCourses <- forM (M.toList lCoursesMap) (scrape . snd)
      let (pErrors, pCourses) = partitionEithers epCourses

      let errors  = lErrors <> pErrors
      let courses = zipWith fromPartials (snd <$> M.toList lCoursesMap) pCourses

      print $ mconcat [ show . length $ errors
                      , " errors: "
                      , show $ unpack <$> errors
                      ]

      print $ mconcat [ show . length $ courses
                      , " courses: "
                      , show $  (unpack . getCourseCode) <$> courses
                      ]

      client   <- apiClient "localhost" 8080
      response <- postCourses client courses

      print $ show [ "Response: "
                   , show response
                   ]

insert :: ListCourse
       -> Map CourseCode ListCourse
       -> Map CourseCode ListCourse
insert lCourse = M.insertWith (<>) (lCourseCode lCourse) lCourse

scrape :: ListCourse -> IO (Either Text PageCourse)
scrape lc = do
  let url = getUrl $ lCourseUrl lc
  res <- scrapeURL (unpack url) pageCourseScraper
  return $ maybeToEither ("Failed to get page of " <> url) res
