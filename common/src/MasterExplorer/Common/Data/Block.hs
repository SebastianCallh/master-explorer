{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Block
  ( Block (..)
  , allBlocks
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Block
  = One
  | Two
  | Three
  | Four
  | None
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance Ord Block where
  compare None None   = EQ
  compare None One    = LT
  compare None Two    = LT
  compare None Three  = LT
  compare None Four   = LT
  compare One None    = GT
  compare One One     = EQ
  compare One Two     = LT
  compare One Three   = LT
  compare One Four    = LT
  compare Two None    = GT
  compare Two One     = GT
  compare Two Two     = EQ
  compare Two Three   = LT
  compare Two Four    = LT
  compare Three None  = GT
  compare Three One   = GT
  compare Three Two   = GT
  compare Three Three = EQ
  compare Three Four  = LT
  compare Four None   = GT
  compare Four One    = GT
  compare Four Two    = GT
  compare Four Three  = GT
  compare Four Four   = EQ

allBlocks :: [Block]
allBlocks = [None, One, Two, Three, Four]
