{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict               as M

import           Control.Monad                 (forM)
import           Data.Either                   (partitionEithers)
import           Data.Map.Strict               (Map)
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text, unpack)
import           Text.HTML.Scalpel             (scrapeURL)

import           CourseScalpel.Data.Course     (fromPartials)
import           CourseScalpel.Data.CourseCode (CourseCode)
import           CourseScalpel.Data.ListCourse (ListCourse (..))
import           CourseScalpel.Data.PageCourse (PageCourse)
import           CourseScalpel.Data.Program    (engD, programUrl)
import           CourseScalpel.Data.Url        (getUrl)
import           CourseScalpel.Helpers         (maybeToEither)
import           CourseScalpel.Web.Scrapers    (pageCourseScraper,
                                                programplanScraper)
import           MasterExplorer.Common.Client  (apiClient, postCourses)

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
      let courses = zipWith fromPartials lCourses pCourses

      print $ mconcat [ show . length $ errors
                      , " errors: "
                      , show errors
                      ]

      print $ mconcat [ show . length $ courses
                      , " courses: "
                      , show courses
                      ]

      client   <- apiClient "localhost" 8080
      response <- postCourses client courses

      print $ show [ "Response: "
                   , show response
                   ]

insert :: ListCourse
       -> Map CourseCode ListCourse
       -> Map CourseCode ListCourse
insert lCourse =
  M.insertWith (<>) (lCourseCode lCourse) lCourse

scrape :: ListCourse -> IO (Either Text PageCourse)
scrape lc = do
  let url = getUrl $ lCourseUrl lc
  res <- scrapeURL (unpack url) pageCourseScraper
  return $ maybeToEither ("Failed to get page of " <> url) res
