{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Process.Minizinc.Extra where

import Data.Aeson (FromJSON(..), ToJSON, withArray, (.:), withObject)
import Control.Applicative ((<|>))
import Control.Monad.Fail (fail)
import Control.Monad (when)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Data.Vector as V

-- | A containter to represent sets to/from-JSON in a MiniZinc-compatible way.
-- Note that support for MiniZinc set varies depending on the var/par status of
-- variables (e.g., var set of float is not supported)
data MznSet a = MznSet { set :: [ a ] }
  deriving (Show, Eq, Ord, Generic, Functor)
instance Hashable a => Hashable (MznSet a)
instance ToJSON a => ToJSON (MznSet a)
instance (Enum a, FromJSON a) => FromJSON (MznSet a) where
  parseJSON = do
    withObject "MznSet" $ \v ->
       MznSet . flatten <$> (v .: "set")

data BoundsOrVal a = Bounds !a !a | Val !a

flatten :: (Enum a) => [BoundsOrVal a] -> [a]
flatten = concatMap f
  where
    f (Val a) = [a]
    f (Bounds v0 v1) = [v0..v1]

instance (FromJSON a) => FromJSON (BoundsOrVal a) where
  parseJSON x = bounds x <|> (Val <$> parseJSON x)
    where
      bounds = withArray "Bounds" $ \xs -> do
         when (length xs /= 2) $ fail "bounds syntax for sets should have 2 values"
         Bounds <$> parseJSON (xs V.! 0) <*> parseJSON (xs V.! 1)
