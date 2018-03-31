module MasterExplorer.Scraper.Data.ProgramConf
  ( ProgramConf(..)
  , mkProgramConf
  , progConfs
  ) where

import           Data.Text (Text)

data ProgramConf = ProgramConf
  { confCode :: !Text
  , confName :: !Text
  }

mkProgramConf :: Text -> Text -> ProgramConf
mkProgramConf = ProgramConf

progConfs :: [ProgramConf]
progConfs =
  [mkProgramConf "6cddd" "D"]
