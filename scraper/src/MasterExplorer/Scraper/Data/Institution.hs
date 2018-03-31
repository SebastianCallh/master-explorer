module MasterExplorer.Scraper.Data.Institution
  ( Institution (..)
  , parseInstitution
  ) where

import           Data.Text                              (Text, strip)

import           MasterExplorer.Common.Data.Institution (Institution (..))
import           MasterExplorer.Scraper.Web.Parsing     (parseError)

parseInstitution :: Text -> Either Text Institution
parseInstitution = parseInstitution' . strip

parseInstitution' :: Text -> Either Text Institution
parseInstitution' "Matematiska institutionen"                                 = Right MAI
parseInstitution' "Institutionen för Systemteknik"                            = Right ISY
  -- typo in sh
parseInstitution' "Institutionen f\246r  Datavetenskap"                       = Right IDA
parseInstitution' "Institutionen f\246r Datavetenskap"                        = Right IDA
parseInstitution' "Institutionen f\246r Medicinsk teknik"                     = Right MED

   -- typo in sh
parseInstitution' "Institutionen f\246r Fysik, kemi och biologi"              = Right IFM
parseInstitution' "Institutionen f\246r  Fysik, kemi och biologi"            = Right IFM
parseInstitution' "Institutionen f\246r Ekonomisk och industriell utveckling" = Right IEE
parseInstitution' "Institutionen f\246r Tema"                                 = Right Tema
parseInstitution' "Institutionen för Teknik och naturvetenskap"               = Right ITN
parseInstitution' "Tekniska fakultetskansliet"                                = Right TekFak

parseInstitution' x = parseError x "Institution"
