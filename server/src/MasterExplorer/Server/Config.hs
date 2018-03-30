{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module MasterExplorer.Server.Config (Config (..), App (..), Environment (..)
              , getPort, getLogger, getConfig
              , getEnvironment, conStr
              ) where

import qualified Data.ByteString.Char8                as BS (pack)

import           Control.Monad.Except                 (MonadError)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadReader, ReaderT)
import           Control.Monad.Trans.Maybe
import           Data.ByteString                      (ByteString)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text, pack)
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Safe                                 (readMay)
import           Servant                              (Handler, ServantErr)
import           System.Environment                   (lookupEnv)

--import           Paths_server                         (getDataFileName)
--import           CoursePlanner.Data.Program           (ProgramAbbr)
--import           Util.Exam.Parser                     (ExamReq, ProgReqs, parse)

newtype App a = AppT { runApp :: ReaderT Config Handler a }
  deriving ( Functor, Applicative, Monad, MonadReader Config
           , MonadIO, MonadError ServantErr)

data Environment
  = Development
  | Production
  | Test
  deriving (Read, Show)

data Config = Config
   { getEnv  :: Environment
   , getPool :: ConnectionPool
   , getKey  :: Text
   }

lookupVar :: String ->  IO String
lookupVar env = do
  maybeEnv <- lookupEnv env
  case maybeEnv of
    Just envVar -> return envVar
    Nothing -> return . error $ mconcat
      [ "Env variable "
      , env
      , " does not exist!"
      ]

{-
getSettings :: Environment -> IO Settings
getSettings env = do
    port <- getPort
    return $ (case env of
          Development -> setPort port . setFdCacheDuration 0
          _           -> setPort port) defaultSettings
-}

getConfig :: IO Config
getConfig = do
  env  <- getEnvironment
  pool <- makePool env
  key  <- getApiKey
  return Config
    { getEnv  = env
    , getPool = pool
    , getKey  = key
    }

getLogger :: Environment -> Middleware
getLogger Development = logStdoutDev
getLogger Production  = logStdout
getLogger Test        = id

getEnvironment :: IO Environment
getEnvironment = read <$> lookupVar "ENV"

getApiKey :: IO Text
getApiKey = pack <$> lookupVar "APIKEY"

getPort :: IO Port
getPort = lookupSetting "PORT" 8080

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing  ->  return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat
      [ "Failed to read [["
      , str
      , "]] for environment variable "
      , env
      ]

makePool :: Environment -> IO ConnectionPool

makePool Test = runNoLoggingT $
  createPostgresqlPool (conStr "_test") (poolSize Test)

makePool Development = runStdoutLoggingT $
  createPostgresqlPool (conStr "") (poolSize Development)

makePool Production = do
  pool <- runMaybeT $ do
    let keys = [ "host="
               , " dbname="
               , " user="
               , " password="
               , " port="
               ]
        envs = [ "PGHOST"
               , "PGDATABASE"
               , "PGUSER"
               , "PGPASS"
               , "PGPORT"
               ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let conS = mconcat . zipWith (++) keys $ envVars
    runStdoutLoggingT $ createPostgresqlPool (BS.pack conS) $ poolSize Production
  case pool of
    Nothing -> error "Couldn't read postgres env variables"
    Just a  -> return a

conStr :: ByteString -> ConnectionString
conStr suffix = "host=localhost dbname=master_explorer" <> suffix <>
                " user=master_explorer_user password=handmina port=5432"

poolSize :: Environment -> Int
poolSize env = case env of
                   Development -> 1
                   Production  -> 8
                   Test        -> 1
