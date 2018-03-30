module MasterExplorer.Common.Data.ProgramCodeSpec where

import           Data.Either                            (either)
import           Servant.API                            (parseUrlPiece,
                                                         toUrlPiece)
import           Test.Hspec
import           Test.QuickCheck

import           MasterExplorer.Common.Class.HasText    (fromText, toText)
import           MasterExplorer.Common.Data.ProgramCode (ProgramCode)

aProgramCode :: Gen ProgramCode
aProgramCode = arbitrary

spec :: Spec
spec =
  describe "ProgramCode instances" $ do
    it "has an isomorphic HttpApiData instance" $ property $ do
      prog <- aProgramCode
      let progCode' = parseUrlPiece $ toUrlPiece prog
      return $ either (const False) (== prog) progCode'

    it "has an isomorphic HasText instance" $ property $ do
      prog <- aProgramCode
      let progCode' = fromText $ toText prog
      return $ either (const False) (== prog) progCode'
