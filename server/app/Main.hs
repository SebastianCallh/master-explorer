module Main where

import           MasterExplorer.Server.App    (app, migrateDb)
import           MasterExplorer.Server.Config (getConfig, getPort)
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setLogger, setPort)
import           Network.Wai.Logger           (withStdoutLogger)

main :: IO ()
main = do
  port <- getPort
  cfg  <- getConfig
  putStrLn "Running migrations"
  migrateDb cfg

  putStrLn $ mconcat
    [ "Starting app at port "
    , show port
    , "!"
    ]

  withStdoutLogger $ \applogger -> do
    let settings = setPort port $ setLogger applogger defaultSettings
    runSettings settings $ app cfg
