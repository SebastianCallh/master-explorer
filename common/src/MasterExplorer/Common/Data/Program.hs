{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterExplorer.Common.Data.Program where
-- Just export everything because it is so terribly much

import qualified Data.Text                              as T

import           Control.Lens
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Semigroup                         ((<>))
import           GHC.Generics                           (Generic)
import           Servant.API                            (FromHttpApiData,
                                                         ToHttpApiData,
                                                         parseUrlPiece,
                                                         toUrlPiece)
import           Test.QuickCheck                        (Arbitrary, arbitrary)

import           MasterExplorer.Common.Class.HasText    (HasText, fromText,
                                                         toText)
import           MasterExplorer.Common.Class.ListItem   (ListItem, listItemText)
import           MasterExplorer.Common.Class.Pretty     (Pretty, pretty)
import           MasterExplorer.Common.Data.ProgramCode (ProgramCode (..))
import           MasterExplorer.Common.Data.ProgramSlug (ProgramSlug (..))

data Program = Program
  { _programCode :: !ProgramCode
  , _programSlug :: !ProgramSlug
  } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

instance Arbitrary Program where
  arbitrary = Program
    <$> arbitrary
    <*> arbitrary

instance FromHttpApiData Program where
  parseUrlPiece t = do
    [ecode, eslug] <- T.splitOn sep <$> pure t
    code <- parseUrlPiece ecode
    slug <- parseUrlPiece eslug
    return $ Program code slug

instance ToHttpApiData Program where
  toUrlPiece p = mconcat [code, sep, slug]
    where code = toUrlPiece $ _programCode p
          slug = toUrlPiece $ _programSlug p

instance ListItem Program where
  listItemText = toText . _programCode

instance HasText Program where
  toText p = toText (_programCode p)
             <> sep
             <> toText (_programSlug p)

  fromText t = do
    let [ecode, eslug] = T.splitOn sep t
    Program <$> fromText ecode <*> fromText eslug

instance Pretty Program where
  pretty = pretty . _programCode

sep :: T.Text
sep = ":"

makeLenses ''Program

bachD = Program BachD       P6IDAT
bachElec =  Program BachElec    P6IELK
bachConst = Program BachConst   P6IBYG
bachKA =  Program BachKA      P6IKEA
bachM = Program BachM       P6IMAS
bachBio  = Program BachBio     P6KBIO
bachExp = Program BachExp     PMGMB2
bachCult = Program BachCult    PF7KKU
bachSoc = Program BachSoc     PF7KSP
bachSys = Program BachSys     PF7KSY
bachChem = Program BachChem    P6KKEB
bachCog = Program BachCog     PF7KKO
bachKSM = Program BachKSM     PF7KKM
bachIP  = Program BachIP      P6KIPR
bachAer = Program BachAer     P6KFTL
bachLog = Program BachLog     P6KLOG
bachMol = Program BachMol     P6KKEM
bachNano = Program BachNano    P6KFYN
bachMath  = Program BachMath    P6KMAT
bachGDK  = Program BachGDK     P6KGDK
bachSC  = Program BachSC      PF7KSK
bachPol = Program BachPol     PF7KPO
bachStat  = Program BachStat    PF7KSA
bachEnv  = Program BachEnv     PF7KMO
bachLaw  = Program BachLaw     PF7KAA

  -- Engineering
engD  = Program EngD        P6CDDD
engU  = Program EngU        P6CMJU
engI  = Program EngI        P6CIII
engIInt = Program EngIInt     P6CIEI
engIT   = Program EngIT       P6CITE
engY    = Program EngY        P6CYYY
engYInt = Program EngYInt     P6CYYI
engMed  = Program EngMed      P6CMED
engMT   = Program EngMT       P6CMEN
engED   = Program EngED       P6CIEN
engKTS  = Program EngKTS      P6CKTS
engM    = Program EngM        P6CMMM
engEMM  = Program EngEMM      P6CEMM
engTB   = Program EngTB       P6CTBI
engDPU  = Program EngDPU      P6CDPU
engKB   = Program EngKB       P6CKEB

  -- Master programs
masterCS = Program MasterCS    P6MICS
masterDAV = Program MasterDAV   P6MDAV
masterStat = Program MasterStat PF7MSL
masterSoc = Program  MasterSoc   PF7MSA
masterPS  = Program MasterPS    P6MPRO
masterITSL = Program MasterITSL  P6MTSL
masterM   = Program MasterM     P6MMEC
masterHR  = Program MasterHR    PF7MHR
masterI  = Program MasterI     PF7MHR
masterEth = Program MasterEth   PF7MEM
masterCom  = Program MasterCom   P6MCSY
masterAer = Program MasterAer   P6MAER
masterEco  = Program MasterEco   P6MECO
masterNano  = Program MasterNano  P6MMSN
masterElec = Program MasterElec  P6MELE
masterPhys  = Program MasterPhys  P6MFYS
masterAnim  = Program MasterAnim  P6METH
masterLearn = Program MasterLearn PL7MLG
masterCog  = Program MasterCog   PF7MKS
masterMed  = Program MasterMed   PMMIN1
masterGen  = Program MasterGen   PF7MGE
masterDes  = Program MasterDes   P6MDES
masterMath = Program MasterMath  P6MMAT
masterSus  = Program MasterSus   P6MSUS
masterBio  = Program MasterBio   P6MBME
masterEnv  = Program MasterEnv   PL7MOS



allPrograms :: [Program]
allPrograms =
  masterPrograms <>
  bachPrograms   <>
  engPrograms

engPrograms :: [Program]
engPrograms =
  [ engD
  , engU
  , engI
  , engIInt
  , engIT
  , engY
  , engYInt
  , engMed
  , engMT
  , engED
  , engKTS
  , engM
  , engEMM
  , engTB
  , engDPU
  , engKB
  ]

bachPrograms :: [Program]
bachPrograms =
    [ bachD
  , bachElec
  , bachConst
  , bachKA
  , bachM
  , bachBio
  , bachExp
  , bachCult
  , bachSoc
  , bachSys
  , bachChem
  , bachCog
  , bachKSM
  , bachIP
  , bachAer
  , bachLog
  , bachMol
  , bachNano
  , bachMath
  , bachGDK
  , bachSC
  , bachPol
  , bachStat
  , bachEnv
  , bachLaw
  ]

masterPrograms :: [Program]
masterPrograms =
  [ masterCS
  , masterDAV
  , masterStat
  , masterSoc
  , masterPS
  , masterITSL
  , masterM
  , masterHR
  , masterI
  , masterEth
  , masterCom
  , masterAer
  , masterEco
  , masterNano
  , masterElec
  , masterPhys
  , masterAnim
  , masterLearn
  , masterCog
  , masterMed
  , masterGen
  , masterDes
  , masterMath
  , masterSus
  , masterBio
  , masterEnv
  ]

