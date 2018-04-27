{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                          (forM)
import           Data.Either                            (partitionEithers)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (Text, unpack)
import           Text.HTML.Scalpel                      (scrapeURL)

import           MasterExplorer.Common.Client           (apiClient, postCourses)
import           MasterExplorer.Scraper.Data.Course     (fromPartials,
                                                         getCourseCode)

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

      epCourses <- forM lCourses scrape
      let (pErrors, pCourses) = partitionEithers epCourses
      let courses = zipWith fromPartials lCourses pCourses
      let errors  = lErrors <> pErrors

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

scrape :: ListCourse
       -> IO (Either Text PageCourse)
scrape lc = do
  let url = getUrl $ lCourseUrl lc
  res <- scrapeURL (unpack url) pageCourseScraper
  return $ maybeToEither ("Failed to get page of " <> url) res
