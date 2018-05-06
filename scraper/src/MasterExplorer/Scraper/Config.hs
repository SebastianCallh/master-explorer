{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MasterExplorer.Scraper.Config
  ( App
  , runScraper
  , Config (..)
  ) where

import           Control.Monad.Logger                (LoggingT, MonadLogger,
                                                      runFileLoggingT)
import           Control.Monad.Reader                (MonadIO, MonadReader,
                                                      ReaderT, runReaderT)

import           MasterExplorer.Scraper.Data.Program (Program)

data Config = Config
  { configLogOutput         :: FilePath
  , configSupportedPrograms :: [Program]
  }

newtype App a = App { runApp :: ReaderT Config (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadLogger)

runScraper :: App a -> Config -> IO a
runScraper app config = logRunner (runReaderT (runApp app) config)
  where
    logRunner = runFileLoggingT (configLogOutput config)
