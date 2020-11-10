{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable(..))
import Control.Monad.IO.Class (liftIO)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Process.Minizinc

data Input001 = Input001 { input001X :: Int }
instance Hashable Input001 where
  hashWithSalt s (Input001 x) = hashWithSalt s x
instance ToJSON Input001 where
  toJSON (Input001 x) = Aeson.object [ "x" .= x ]
data Output001 = Output001 { output001Y :: Int }
instance FromJSON Output001 where
  parseJSON = Aeson.withObject "output001" $ \v -> Output001 <$> v .: "y"

prop_mzncall_t001 :: Property
prop_mzncall_t001 =
  property $ do
    x <- forAll $ Gen.integral (Range.linear (-100) 100)
    let mzn = simpleMiniZinc @Input001 @Output001 "modes/test001.mzn" 1000 Gecode
    outy <- liftIO $ runLastMinizincJSON mzn (Input001 x)
    Just x === fmap output001Y outy
    

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = do
  tests
  pure ()
