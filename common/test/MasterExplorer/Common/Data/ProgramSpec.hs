module MasterExplorer.Common.Data.ProgramSpec where

import           Servant.API                        (parseUrlPiece, toUrlPiece)
import           Test.Hspec
import           Test.QuickCheck

import           MasterExplorer.Common.Data.Program (Program (..))

aProgram :: Gen Program
aProgram = arbitrary

spec :: Spec
spec =
  describe "Program instances" $
    it "has an isomorphic HttpApiData instance" $ property $ do
      prog <- aProgram
      let prog' = parseUrlPiece $ toUrlPiece prog
      return $ either (const False) (== prog) prog'
