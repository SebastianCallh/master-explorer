{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.ProgramCode
  ( ProgramCode (..)
  ) where

import qualified Data.Text                           as T

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Maybe                          (maybe)
import           GHC.Generics                        (Generic)
import           Servant.API                         (FromHttpApiData,
                                                      ToHttpApiData,
                                                      parseUrlPiece, toUrlPiece)
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT       (genericArbitrary)
import           Text.Read                           (readMaybe)

import           MasterExplorer.Common.Class.HasText (HasText, fromText, toText)

data ProgramCode
  = EngD         -- Datateknik
  | EngU         -- Mjukvaruteknik
  | EngI         -- Industriell ekonomi
  | EngIInt      -- Industriell ekonomi - Internationell
  | EngIT        -- Informationsteknologi
  | EngY         -- Teknisk fysik och elektroteknik
  | EngYInt      -- Teknisk fysik och elektroteknik - Internationell
  | EngMed       -- Medicinsk teknik
  | EngED        -- Elektronikdesign
  | EngMT        -- Mediateknik
  | EngKTS       -- Kommunikation, transport och samhälle
  | EngM         -- Maskinteknik
  | EngEMM       -- Energi, miljö och management
  | EngTB        -- Teknisk biologi
  | EngDPU       -- Design och produktutveckling
  | EngKB        -- Kemisk biologi
  | BachD        -- Datateknik
  | BachElec     -- Eelektronik
  | BachConst    -- Byggnadsteknik
  | BachKA       -- Kemisk analysteknik
  | BachM        -- Maskinteknik
  | BachBio      -- Biologi
  | BachExp      -- Experimentell och industriell biomedicin
  | BachCult     -- Kulturvetenskap
  | BachSoc      -- Samhällsplanering
  | BachSys      -- Systemvetenskap
  | BachChem     -- Kemisk biologi
  | BachCog      -- Kognitionsvetenskap
  | BachKSM      -- Kultur, samhhälle, mediagestalltning
  | BachIP       -- Innovativ programmering
  | BachAer      -- Flygtransport och logistik
  | BachLog      -- Samhällets logistik
  | BachMol      -- Kemi - molekylär design
  | BachNano     -- Fysik och nanovetenskap
  | BachMath     -- Matematik
  | BachGDK      -- Grafisk design och kommunikation
  | BachStat     -- Statistik och dataanalys
  | BachSC       -- Samhälls- och kulturanalys
  | BachPol      -- Policies
  | BachEnv      -- Miljövetare
  | BachLaw      -- Affärsjuridik
  | MasterDAV    -- Datavetenskap
  | MasterCS     -- Computer Science
  | MasterStat   -- Statistics and machine learning
  | MasterSoc    -- Samhällsgestaltning
  | MasterPS     -- Protein Science
  | MasterITSL   -- Intelligent Transport Systems and Logistics
  | MasterM      -- Mechanical Engineering
  | MasterHR     -- Human Resource Management and Development
  | MasterI      -- Industrial Engineering and Management
  | MasterEth    -- Masterprogram i Etnicitet och migration
  | MasterCom    -- Communication Systems
  | MasterAer    -- Aeronautical Engineering
  | MasterEco    -- Ecology and the Environment
  | MasterNano   -- Materials Science and Nanotechnology
  | MasterElec   -- Electronics Engineering
  | MasterPhys   -- Fysik och nanovetenskap
  | MasterAnim   -- Applied Ethology and Animal Biology
  | MasterLearn  -- Internationellt masterprogram i vuxnas lärande och...
  | MasterAeth   -- Applied ethics
  | MasterCog    -- Kognitionsvetenskap
  | MasterMed    -- Masterprogram i arbetsterapi/folkhälsovetenskap/...
  | MasterGen    -- Masterprogram i Intersektionell genusvetenskap som förändringsarbete
  | MasterChild  -- Child studies
  | MasterDes    -- Design
  | MasterMath   -- Matematik
  | MasterSus    -- Sustainability Engineering and Management
  | MasterBio    -- Biomedical Engineering
  | MasterEnv    -- Masterprogram i miljö- och utomhuspedagogik och friluftsliv
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary ProgramCode where
  arbitrary = genericArbitrary

instance HasText ProgramCode where
  toText = T.pack . show
  fromText t = maybe err Right (readMaybe $ T.unpack t)
    where err = Left $ mconcat ["Could not parse ", t, " as ProgramCode."]

instance FromHttpApiData ProgramCode where
  parseUrlPiece   = fromText

instance ToHttpApiData ProgramCode where
  toUrlPiece   = toText
