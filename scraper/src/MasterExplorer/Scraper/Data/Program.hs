module MasterExplorer.Scraper.Data.Program
  ( Program (..)
  , parsePrograms
  , programUrl

  -- the programs below are exported for unit testing
  , engD
  , engU
  , engIT
  , engI
  , engIInt
  , engTB
  , engKB
  , engY
  , engYInt
  , engMed
  , engM
  , engEMM
  , engDPU
  , engKTS
  , engMT
  , engED
  ) where

import           Data.Text                           (Text)
import           Text.HTML.Scalpel

import           MasterExplorer.Common.Class.HasText (toText)
import           MasterExplorer.Common.Data.Program
import           MasterExplorer.Common.Data.Url      (Url (..))
import           MasterExplorer.Scraper.Web.Parsing  (parseError)

parsePrograms :: Text -> Either Text [Program]
parsePrograms x = maybe errorMsg (traverse parseProgram) mitems
  where
    mitems    = scrapeStringLike x . chroot "ul" $ texts "li"
    errorMsg  = parseError x "Programs"

parseProgram :: Text -> Either Text Program
parseProgram "Civilingenjör i medicinsk teknik"                      = Right engMed
parseProgram "Civilingenjör i medieteknik"                           = Right engMT
parseProgram "Civilingenjör i informationsteknologi"                 = Right engIT
parseProgram "Civilingenjör i elektronikdesign"                      = Right engED
parseProgram "Civilingenjör i kommunikation, transport och samhälle" = Right engKTS
parseProgram "Civilingenjör i maskinteknik"                          = Right engM
parseProgram "Civilingenjör i industriell ekonomi"                   = Right engI
parseProgram "Civilingenjör i industriell ekonomi - internationell"  = Right engIInt
parseProgram "Civilingenjör i energi - miljö - management"           = Right engEMM
parseProgram "Civilingenjör i teknisk fysik och elektroteknik - internationell" = Right engYInt
parseProgram "Civilingenjör i teknisk biologi"                       = Right engTB
parseProgram "Civilingenjör i mjukvaruteknik"                        = Right engU
parseProgram "Civilingenjör i design och produktutveckling"          = Right engDPU
parseProgram "Civilingenjör i kemisk biologi - med valbar utgång till naturvetenskaplig kandidat" = Right engKB
parseProgram "Civilingenjör i datateknik"                            = Right engD
parseProgram "Civilingenjör i teknisk fysik och elektroteknik"       = Right engY

parseProgram "Biologi, kandidatprogram"                              = Right bachBio
parseProgram "Kandidatprogrammet i Experimentell och industriell biomedicin" = Right bachExp
parseProgram "Kulturvetenskap, kandidatprogram"                      = Right bachCult
parseProgram "Kandidatprogrammet i samhällsplanering"                = Right bachSoc
parseProgram "Systemvetenskap, kandidatprogram"                      = Right bachSys
parseProgram "Kemisk biologi kandidatprogram"                        = Right bachChem
parseProgram "Kognitionsvetenskap, kandidatprogram"                  = Right bachCog
parseProgram "Kultur, samhälle, mediegestaltning, kandidatprogram"   = Right bachKSM
parseProgram "Innovativ programmering, kandidatprogram"              = Right bachIP
parseProgram "Flygtransport och logistik, kandidatprogram"           = Right bachAer
parseProgram "Samhällets logistik, kandidatprogram"                  = Right bachLog
parseProgram "Kemi - molekylär design, kandidatprogram"              = Right bachMol
parseProgram "Fysik och nanovetenskap, kandidatprogram"              = Right bachNano
parseProgram "Matematik, kandidatprogram"                            = Right bachMath
parseProgram "Grafisk design och kommunikation, kandidatprogram"     = Right bachGDK
parseProgram "Samhälls- och kulturanalys, kandidatprogram"           = Right bachSC
parseProgram "Politices kandidatprogrammet"                          = Right bachPol
parseProgram "Statistik och dataanalys, kandidatprogram"             = Right bachStat
parseProgram "Miljövetare, kandidatprogram"                          = Right bachEnv
parseProgram "Affärsjuridiska programmet, kandidatprogram"           = Right bachLaw

parseProgram "Högskoleingenjör i datateknik"                         = Right bachD
parseProgram "Masterprogram i Samhällsgestaltning"                   = Right masterSoc
parseProgram "Protein Science, masterprogram"                        = Right masterPS
parseProgram "Computer Science, masterprogram"                        = Right masterCS
parseProgram "Intelligent Transport Systems and Logistics, masterprogram"  = Right masterITSL
parseProgram "Mechanical Engineering, masterprogram"                 = Right masterM
parseProgram "Human Resource Management and Development, masterprogram" = Right masterHR
parseProgram "Industrial Engineering and Management, masterprogram"  = Right masterI
parseProgram "Datavetenskap, masterprogram"                          = Right masterDAV
parseProgram "Statistics and Machine Learning, Master´s Programme"   = Right masterStat
parseProgram "Masterprogram i Etnicitet och migration"               = Right masterEth
parseProgram "Communication Systems, masterprogram"                  = Right masterCom
parseProgram "Aeronautical Engineering, masterprogram"               = Right masterAer
parseProgram "Ecology and the Environment, masterprogram"            = Right masterEco
parseProgram "Materials Science and Nanotechnology, masterprogram"   = Right masterNano
parseProgram "Electronics Engineering, masterprogram"                = Right masterElec
parseProgram "Fysik och nanovetenskap, masterprogram"                = Right masterPhys
parseProgram "Applied Ethology and Animal Biology, masterprogram"    = Right masterAnim
parseProgram "Internationellt masterprogram i vuxnas lärande och globala förändringar" = Right masterLearn
--parseProgram "Applied Ethics, Master´s Programme"                    = Right masterAeth
parseProgram "Kognitionsvetenskap, masterprogram"                    = Right masterCog
parseProgram "Masterprogram i arbetsterapi/folkhälsovetenskap/fysioterapi/logopedi/medicinsk pedagogik/omvårdnadsvetenskap" = Right masterMed
parseProgram "Masterprogram i Intersektionell genusvetenskap som förändringsarbete"
    = Right masterGen
--parseProgram "Child Studies, Master´s Programme"                     = Right masterChild
parseProgram "Design, masterprogram"                                 = Right masterDes
parseProgram "Matematik, masterprogram"                              = Right masterMath
parseProgram "Sustainability Engineering and Management, masterprogram" = Right  masterSus
parseProgram x = parseError x "Program"

programUrl :: Program -> Url
programUrl =
  Url . mappend "https://liu.se/studieinfo/program/" . toText . programSlug
