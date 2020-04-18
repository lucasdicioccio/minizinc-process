{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import Process.Minizinc
import Process.Minizinc.Inspect

data Input = Input {x :: Int}
  deriving (Generic)

instance ToJSON Input

instance Hashable Input

data Output = Output {y :: Int}
  deriving (Show, Generic)

instance FromJSON Output

main :: IO ()
main = do
  inspect "models/example001.mzn" >>= print
  inspect "models/example002.mzn" >>= print
  let mzn = simpleMiniZinc @Input @Output "models/example001.mzn" 1000 Gecode
  let problem = Input 10
  runLastMinizincJSON mzn problem >>= print
