{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.ProgramSlug
  ( ProgramSlug (..)
  ) where

import qualified Data.Text                           as T

import           Data.Aeson                          (FromJSON, ToJSON)
import           GHC.Generics                        (Generic)
import           Servant.API                         (FromHttpApiData,
                                                      ToHttpApiData,
                                                      parseUrlPiece, toUrlPiece)
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.ADT       (genericArbitrary)
import           Text.Read                           (readMaybe)

import           MasterExplorer.Common.Class.HasText (HasText, fromText, toText)

data ProgramSlug
  = P6IDAT
  | P6IELK
  | P6IBYG
  | P6IKEA
  | P6IMAS
  | P6KBIO
  | PMGMB2
  | PF7KKU
  | PF7KSP
  | PF7KSY
  | P6KKEB
  | PF7KKO
  | PF7KKM
  | P6KIPR
  | P6KFTL
  | P6KLOG
  | P6KKEM
  | P6KFYN
  | P6KMAT
  | P6KGDK
  | PF7KSK
  | PF7KPO
  | PF7KSA
  | PF7KMO
  | PF7KAA

  -- Engineering
  | P6CDDD
  | P6CMJU
  | P6CIII
  | P6CIEI
  | P6CITE
  | P6CYYY
  | P6CYYI
  | P6CMED
  | P6CMEN
  | P6CIEN
  | P6CKTS
  | P6CMMM
  | P6CEMM
  | P6CTBI
  | P6CDPU
  | P6CKEB

  -- Master programs
  | P6MICS
  | P6MDAV
  | PF7MSL
  | PF7MSA
  | P6MPRO
  | P6MTSL
  | P6MMEC
  | PF7MHR
  | PF7MIO
  | PF7MEM
  | P6MCSY
  | P6MAER
  | P6MECO
  | P6MMSN
  | P6MELE
  | P6MFYS
  | P6METH
  | PL7MLG
  | PF7MKS
  | PMMIN1
  | PF7MGE
  | P6MDES
  | P6MMAT
  | P6MSUS
  | P6MBME
  | PL7MOS
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- Uses P as prefix since num characters can't be first in Haskell
instance HasText ProgramSlug where
  toText     = T.pack . tail . show
  fromText p = maybe err Right (readMaybe . T.unpack $ T.cons 'P' p)
    where err = Left $ mconcat ["Could not parse ", p, " as ProgramSlug."]

instance Arbitrary ProgramSlug where
  arbitrary = genericArbitrary

instance FromHttpApiData ProgramSlug where
  parseUrlPiece = fromText

instance ToHttpApiData ProgramSlug where
  toUrlPiece = toText
