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
import Test.Hspec (describe, it, hspec)
import Test.Hspec.Hedgehog (hedgehog)

import Process.Minizinc

data Input001 = Input001 { input001X :: Int }
  deriving (Show,Eq,Ord)
instance Hashable Input001 where
  hashWithSalt s (Input001 x) = hashWithSalt s x
instance ToJSON Input001 where
  toJSON (Input001 x) = Aeson.object [ "x" .= x ]
data Output001 = Output001 { output001Y :: Int }
  deriving (Show,Eq,Ord)
instance FromJSON Output001 where
  parseJSON = Aeson.withObject "output001" $ \v -> Output001 <$> v .: "y"

solver :: MonadGen m => m Solver
solver = Gen.element [ Chuffed, COIN_BC, Gecode ]

mzncall_t001_01 = do
    x <- forAll $ Gen.integral (Range.linear (-100) 100)
    s <- forAll solver
    let mzn = simpleMiniZinc @Input001 @Output001 "models/test001_01.mzn" 1000 s
    let input = Input001 x
    outy <- liftIO $ runLastMinizincJSON mzn input
    liftIO $ cleanTmpFile mzn input
    Just x === fmap output001Y outy

mzncall_t001_02 = do
    x <- forAll $ Gen.integral (Range.linear (-100) 100)
    s <- forAll solver
    let mzn = simpleMiniZinc @Input001 @Output001 "models/test001_02.mzn" 1000 s
    let input = Input001 x
    outy <- liftIO $ runLastMinizincJSON mzn input
    liftIO $ cleanTmpFile mzn input
    Nothing === outy

prop_mzncall_t001_01 :: Property
prop_mzncall_t001_01 =
  property mzncall_t001_01

prop_mzncall_t001_02 :: Property
prop_mzncall_t001_02 =
  property mzncall_t001_02

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = hspec $ do
  describe "output-input json reading" $ do
    it "should solve trivial problems" $ hedgehog $ do
      mzncall_t001_01
    it "should not solve unsatisfiable problems" $ hedgehog $ do
      mzncall_t001_02
