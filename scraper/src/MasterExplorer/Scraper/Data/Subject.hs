module MasterExplorer.Scraper.Data.Subject
  ( Subject (..)
  , parseSubjects
  ) where


import qualified Data.Text                          as T

import           Data.Text                          (Text)

import           MasterExplorer.Common.Data.Subject (Subject (..))
import           MasterExplorer.Scraper.Web.Parsing (parseError)

parseSubjects :: Text -> Either Text [Subject]
parseSubjects = traverse parseSubject . T.splitOn ","

parseSubject :: Text -> Either Text Subject
parseSubject "Datateknik"                           = Right ComputerScience
parseSubject "Elektroteknik"                        = Right Electrotechnics
parseSubject "Milj\246v\229rd och milj\246skydd"    = Right EnvironmentProtection
parseSubject "Engelska"                             = Right English
parseSubject "Franska"                              = Right French
parseSubject "Tyska"                                = Right German
parseSubject "Historia"                             = Right History
parseSubject "Informatik/Data- och systemvetenskap" = Right Informatics
parseSubject "Ledarskap, organisation och styrning" = Right LeadershipOrganisation
parseSubject "Ledarskap"                            = Right Leadership
parseSubject "Matematik"                            = Right Matematik
parseSubject "Medie- o kommunikationsvetenskap"     = Right MediaCommunication
parseSubject "Industriell ekonomi och organisation" = Right Organisation
parseSubject "\214vrigt inom medicin"               = Right OtherMedicine
parseSubject "\214vriga tekniska \228mnen"          = Right OtherTechnical
parseSubject "Fysik"                                = Right Physics
parseSubject "Filosofi"                             = Right Philosophy
parseSubject "Spanska"                              = Right Spanish
parseSubject "Elektronik"                           = Right Electronics
parseSubject "Övriga ämnen"                         = Right Other
parseSubject "Juridik och rättsvetenskap"           = Right Law
parseSubject "Medieproduktion"                      = Right MediaProduction
parseSubject x = parseError x "Subject"
