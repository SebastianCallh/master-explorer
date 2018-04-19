{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MasterExplorer.Common.Data.Block
  ( Block (..)
  , allBlocks
  ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)

import           MasterExplorer.Common.Class.Pretty (Pretty, pretty)

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

instance Pretty Block where
  pretty One   = "1"
  pretty Two   = "2"
  pretty Three = "3"
  pretty Four  = "4"
  pretty None  = "-"

allBlocks :: [Block]
allBlocks = [None, One, Two, Three, Four]
