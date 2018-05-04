module MasterExplorer.Scraper.Data.Examination
  ( Examination (..)
  , parseExaminations
  ) where

import qualified Data.Text                                   as T

import           Data.Text                                   (Text)
import           Text.HTML.Scalpel                           (chroot, chroots,
                                                              scrapeStringLike,
                                                              texts)

import           MasterExplorer.Common.Data.Examination      (Examination (..))
import           MasterExplorer.Scraper.Data.Credits         (parseCredits)
import           MasterExplorer.Scraper.Data.ExaminationType (parseExaminationType)
import           MasterExplorer.Scraper.Data.Grading         (parseGrading)
import           MasterExplorer.Scraper.Web.Parsing          (parseError)

parseExaminations :: Text -> Either Text [Examination]
parseExaminations x = maybe (parseError x "Examinations") sequence eexams
  where
    eexams = scrapeStringLike x $ chroot "table" $ chroots "tr" $ do
      txts <- texts "td"
      if length txts /= 4
        then return $ Left $
             mconcat [ "Only "
                     , T.pack . show $ length txts
                     , " fields when parsing Examination. 4 are required"
                     ]
        else do
        let [code, desc, grading, credits] = txts
        return $ Examination code
          <$> parseExaminationType code
          <*> pure desc
          <*> parseGrading grading
          <*> parseCredits credits
