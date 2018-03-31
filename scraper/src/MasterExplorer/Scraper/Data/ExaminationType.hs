module MasterExplorer.Scraper.Data.ExaminationType
  ( ExaminationType (..)
  , parseExaminationType
  ) where

import qualified Data.Text                                  as T

import           Data.Text                                  (Text)

import           MasterExplorer.Common.Data.ExaminationType (ExaminationType (..))
import           MasterExplorer.Scraper.Web.Parsing         (parseError)

--parseExaminationTypes :: Text -> Either Text [ExaminationType]
--parseExaminationTypes = undefined

parseExaminationType :: Text -> Either Text ExaminationType
parseExaminationType x
  | "TEN"  `T.isPrefixOf` x = Right TEN
  | "LAB"  `T.isPrefixOf` x = Right LAB
  | "UPG"  `T.isPrefixOf` x = Right UPG
  | "OPPO" `T.isPrefixOf` x = Right OPPO
  | "AUSK" `T.isPrefixOf` x = Right AUSK
  | "PRA"  `T.isPrefixOf` x = Right PROJ
  | otherwise = parseError x "ExaminationType"

