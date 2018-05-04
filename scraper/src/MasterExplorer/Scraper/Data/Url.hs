module MasterExplorer.Scraper.Data.Url
  ( Url (..)
  , parseUrls
  ) where

import           Data.Text                      (Text)
import           Text.HTML.Scalpel              (attrs, scrapeStringLike)

import           MasterExplorer.Common.Data.Url (Url (..))

parseUrls :: Text -> Either Text [Url]
parseUrls x = Right $ maybe [] (fmap Url) hrefs
  where
    hrefs = scrapeStringLike x $ attrs "href" "a"
