{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                          (forM, forM_)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Logger                   (LogLevel (..),
                                                         logWithoutLoc)
import           Control.Monad.Reader                   (asks)
import           Data.Either                            (partitionEithers)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (unpack)
import           Data.Time.Clock                        (getCurrentTime)
import           Text.HTML.Scalpel                      (scrapeURL)

import           MasterExplorer.Common.Class.Pretty     (pretty)
import           MasterExplorer.Common.Client           (apiClient, postCourses)
import           MasterExplorer.Scraper.Data.Course     (Course, fromPartials)

import           MasterExplorer.Scraper.Config          (App, Config (..),
                                                         runScraper)
import           MasterExplorer.Scraper.Data.ListCourse (ListCourse (..))
import           MasterExplorer.Scraper.Data.PageCourse (fromPageSections)
import           MasterExplorer.Scraper.Data.Program    (Program, engD,
                                                         engPrograms,
                                                         programUrl)
import           MasterExplorer.Scraper.Data.Url        (Url, getUrl)
import           MasterExplorer.Scraper.Data.Validation (ValidationError (..),
                                                         valErr)

import           MasterExplorer.Scraper.Helpers         (maybeToEither)
import           MasterExplorer.Scraper.Web.Scrapers    (pageCourseScraper,
                                                         programplanScraper)
main :: IO ()
main = do
  putStrLn "Scraping courses..."

  let logPath = "log.txt"
  let config = Config
        { configLogOutput         = logPath
        , configSupportedPrograms = [engD]
        }

  courses <- runScraper app config
  putStrLn $ mconcat [ show $ length courses
                  , " courses scraped. "
                  , " Errors can be read in "
                  , logPath
                  ]

  putStrLn "Sending to server..."
  client   <- apiClient "localhost" 8080
  response <- postCourses client courses
  putStrLn $ "Response: " <> show response


app :: App [Course]
app = do
  startTime <- liftIO getCurrentTime
  logWithoutLoc "Main" LevelInfo $
    "Running scraper - " ++ show startTime

  courses <- asks configSupportedPrograms
    >>= scrapeProgramCourses

  endTime <- liftIO getCurrentTime
  logWithoutLoc "Main" LevelInfo $
    "Scraper finished - " ++ show endTime

  return courses

scrapeProgramCourses :: [Program] -> App [Course]
scrapeProgramCourses programs =
  fmap concat $ forM programs $ \program -> do
    let pUrl = unpack . getUrl $ programUrl program
    lCourses <- liftIO $ scrapeURL pUrl $ programplanScraper program
    case lCourses of
      Nothing -> do
        logWithoutLoc "Main" LevelError $
          "Could not fetch Url" <> pUrl
        return []

      Just elCourses -> do
        let (lErrors, lCourses) = partitionEithers elCourses
        forM_ lErrors $ logWithoutLoc "Main" LevelError

        eCourses <- liftIO $ forM lCourses $ \lCourse -> do
          let cUrl = getUrl $ lCourseUrl lCourse
          mfields <- scrapeURL (unpack cUrl) pageCourseScraper
          return $ case mfields of
            Nothing -> Left . valErr $ "Could not fetch Url" <> cUrl
            Just x  ->
              case fromPageSections x of
                Right pCourse -> pure $ fromPartials lCourse pCourse
                Left e -> Left . valErr $ mconcat
                          [ "Error scraping "
                          , pretty (lCourseCode lCourse)
                          , " "
                          , getError e
                          ]

        let (errors, courses) = partitionEithers eCourses
        forM_ errors $ logWithoutLoc "Main" LevelError . getError
        return courses



