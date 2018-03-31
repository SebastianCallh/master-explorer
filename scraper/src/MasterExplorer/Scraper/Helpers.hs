module MasterExplorer.Scraper.Helpers
  ( eitherToMaybe
  , maybeToEither
  ) where

import           Data.Text (Text)

eitherToMaybe :: Either l a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left  e



