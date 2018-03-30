{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Wtf
    ( someFunc
    ) where

import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
  bar String

Person
    name String
    age Int Maybe
    deriving Show
|]

p :: Person
p = Person "Hej" (Just 5)

someFunc :: IO ()
someFunc = putStrLn "someFunc" *>
           print p
