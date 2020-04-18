{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | A set of types and functions to help calling Minizinc as an external binary.
--
-- Current strategy is to use JSON encode/decoding for passing in inputs and
-- reading outputs.
-- At this time, only a primitive output parser is supported.
module Process.Minizinc (
    MiniZinc(..)
  , simpleMiniZinc
  , Solver(..)
  , SolverName
  , MilliSeconds
  , runLastMinizincJSON
  ) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON, FromJSON, decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Search.DFA (split)
import Data.Hashable (Hashable, hash)
import qualified Data.List as List
import System.Process.ByteString (readProcessWithExitCode)

-- | Type alias asking for milliseconds.
type MilliSeconds a = Int

-- | Name of a solver to be passed to the minizinc binary.
type SolverName = String

-- | Supported solvers or 'Other'.
data Solver = Chuffed | COIN_BC | CPLEX | Gecode | Gurobi | SCIP | Xpress | Other SolverName

-- | An object helping to run MiniZinc.
data MiniZinc input answer
  = MiniZinc
      { model :: FilePath
      -- ^ a file path to a model
      , mkTmpDataPath :: input -> FilePath
      -- ^ a file path to hold, must be writable and readable
      , mkTimeLimit :: input -> MilliSeconds Int
      -- ^ a timelimit in seconds (an Int)
      , mkSolver :: input -> Solver
      -- ^ the solver to use (see `minizinc --solvers`)
      , mkExtraArgs :: input -> [String]
      -- ^ other arguments that get appended before the path to the model and data paths
      }

-- | A constructor for MiniZinc object for simple situations.
simpleMiniZinc
  :: Hashable input
  => FilePath
  -> MilliSeconds Int
  -> Solver
  -> MiniZinc input answer
simpleMiniZinc path timeout solver =
  MiniZinc
    path
    (\obj -> show (hash obj) ++ ".json")
    (const timeout)
    (const solver)
    (const [])

-- | Runs MiniZinc on the input and parses output for the last answer.
--
-- The parser for now is primitive and all the parsing occurs after processing
-- with no guarantee to run on bounded-memory. This matters if your MiniZinc
-- model returns so many solutions that the output is large.
runLastMinizincJSON
  :: (ToJSON input, FromJSON answer)
  => MiniZinc input answer
  -> input
  -> IO (Maybe answer)
runLastMinizincJSON minizinc obj = do
  LByteString.writeFile fullPath $ encode obj
  (_, out, err) <- readProcessWithExitCode "minizinc" args ""
  seq (ByteString.length err) $ pure $ locateLastAnswer out
  where
    fullPath :: FilePath
    fullPath = mkTmpDataPath minizinc obj

    locateLastAnswer :: FromJSON answer => ByteString -> Maybe answer
    locateLastAnswer = locateLastOutput >=> decode . fromStrict

    args :: [String]
    args =
      [ "--time-limit",
        show (mkTimeLimit minizinc obj),
        "--solver",
        showSolver (mkSolver minizinc obj),
        "--output-mode",
        "json"
      ] ++ (mkExtraArgs minizinc obj)
        ++
      [
        model minizinc,
        fullPath
      ]

showSolver :: Solver -> String
showSolver = \case
  Chuffed -> "Chuffed"
  COIN_BC -> "COIN-BC"
  CPLEX -> "CPLEX"
  Gecode -> "Gecode"
  Gurobi -> "Gurobi"
  SCIP -> "SCIP"
  Xpress -> "Xpress"
  Other n -> n

locateLastOutput :: ByteString -> Maybe ByteString
locateLastOutput =
  safehead
    . reverse
    . List.filter (ByteString.isPrefixOf openCurlybrace)
    . split resultSeparator
  where
    safehead [] = Nothing
    safehead xs = Just $ head xs
    resultSeparator = "\n----------\n"
    openCurlybrace = "{"
