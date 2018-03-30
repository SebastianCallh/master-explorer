module CourseScalpel.Data.Examination
  ( Examination (..)
  , parseExaminations
  ) where

import qualified Data.Text                              as T

import           Data.Text                              (Text)
import           Text.HTML.Scalpel                      (chroot, chroots,
                                                         scrapeStringLike,
                                                         texts)

import           CourseScalpel.Data.Credits             (parseCredits)
import           CourseScalpel.Data.ExaminationType     (parseExaminationType)
import           CourseScalpel.Data.Grading             (parseGrading)
import           CourseScalpel.Web.Parsing              (parseError)
import           MasterExplorer.Common.Data.Examination (Examination (..))

parseExaminations :: Text -> Either Text [Examination]
parseExaminations x = maybe (parseError x "Examinations") sequence eexams
  where
    eexams = scrapeStringLike x $chroot "tbody" $ chroots "tr" $ do
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
