module MasterExplorer.Scraper.Data.Validation
  ( ValidationError (..)
  , valErr
  , (-:)
  ) where

import           Data.Semigroup (Semigroup, (<>))
import           Data.Text      (Text)

newtype ValidationError = ValidationError { getError :: Text }

instance Semigroup ValidationError where
  (<>) a b = ValidationError $ getError a <> "\n" <> getError b

validate :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
validate (Left a)  (Left b)  = Left $ a <> b
validate (Left a)  (Right _) = Left a
validate (Right _) (Left b)  = Left b
validate (Right f) (Right a) = Right $ f a

(-:) :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
(-:) = validate

valErr :: Text -> ValidationError
valErr = ValidationError
