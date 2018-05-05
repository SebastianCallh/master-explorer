module MasterExplorer.Scraper.Data.ExaminationType
  ( ExaminationType (..)
  , parseExaminationType
  ) where

import qualified Data.Text                                  as T

import           Data.Text                                  (Text)

import           MasterExplorer.Common.Data.ExaminationType (ExaminationType (..))
import           MasterExplorer.Scraper.Web.Parsing         (parseError)


parseExaminationType :: Text -> Either Text ExaminationType
parseExaminationType x
  | "TEN"  `T.isPrefixOf` x = Right TEN
  | "LAB"  `T.isPrefixOf` x = Right LAB
  | "UPG"  `T.isPrefixOf` x = Right UPG
  | "OPPO" `T.isPrefixOf` x = Right OPPO
  | "AUSK" `T.isPrefixOf` x = Right AUSK
  | "PRA"  `T.isPrefixOf` x = Right PROJ
  | "KTR"  `T.isPrefixOf` x = Right KTR
  | otherwise = parseError x "ExaminationType"

