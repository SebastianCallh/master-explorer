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
  | "ANN"  `T.isPrefixOf` x = Right ANN
  | "AUSK" `T.isPrefixOf` x = Right AUSK
  | "BAS"  `T.isPrefixOf` x = Right BAS
  | "DAT"  `T.isPrefixOf` x = Right DAT
  | "HEM"  `T.isPrefixOf` x = Right HEM
  | "KTR"  `T.isPrefixOf` x = Right KTR
  | "LAB"  `T.isPrefixOf` x = Right LAB
  | "MOM"  `T.isPrefixOf` x = Right MOM
  | "MUN"  `T.isPrefixOf` x = Right MUN
  | "OPPO" `T.isPrefixOf` x = Right OPPO
  | "PRA"  `T.isPrefixOf` x = Right PROJ
  | "TEN"  `T.isPrefixOf` x = Right TEN
  | "UPG"  `T.isPrefixOf` x = Right UPG
  | otherwise = parseError x "ExaminationType"

