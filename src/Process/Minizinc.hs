{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

-- | A set of types and functions to help calling Minizinc as an external binary.
--
-- Current strategy is to use JSON encode/decoding for passing in inputs and
-- reading outputs.
-- At this time, only a primitive output parser is supported.
module Process.Minizinc
  ( MiniZinc (..),
    simpleMiniZinc,
    withArgs,
    Solver (..),
    SolverName,
    MilliSeconds,
    runLastMinizincJSON,
    SearchState(..),
    ResultHandler(..),
    runMinizincJSON,
  )
where

import Control.Monad ((>=>), void)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, parse, IResult(..))
import Data.Attoparsec.Combinator (try)
import Data.Aeson (FromJSON, fromJSON, ToJSON, decode, encode, Value, Result(..))
import Data.Aeson.Parser.Internal (json')
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Search.DFA (split)
import Data.Hashable (Hashable, hash)
import qualified Data.List as List
import System.Process.ByteString (readProcessWithExitCode)
import System.Process (createProcess, proc, StdStream(CreatePipe), std_out)
import GHC.IO.Handle (Handle, hClose, hIsEOF)

-- | Type alias asking for milliseconds.
type MilliSeconds a = Int

-- | Name of a solver to be passed to the minizinc binary.
type SolverName = String

-- | Supported solvers or 'Other'.
data Solver = Chuffed | COIN_BC | CPLEX | Gecode | Gurobi | SCIP | Xpress | Other SolverName

-- | An object helping to run MiniZinc.
data MiniZinc input answer
  = MiniZinc
      { -- | a file path to a model
        model :: FilePath,
        -- | a file path to hold, must be writable and readable
        mkTmpDataPath :: input -> FilePath,
        -- | a timelimit in seconds (an Int)
        mkTimeLimit :: input -> MilliSeconds Int,
        -- | the solver to use (see `minizinc --solvers`)
        mkSolver :: input -> Solver,
        -- | other arguments that get appended before the path to the model and data paths
        mkExtraArgs :: input -> [String]
      }

-- | A constructor for MiniZinc object for simple situations.
simpleMiniZinc ::
  Hashable input =>
  FilePath ->
  MilliSeconds Int ->
  Solver ->
  MiniZinc input answer
simpleMiniZinc path timeout solver =
  MiniZinc
    path
    (\obj -> show (hash obj) ++ ".json")
    (const timeout)
    (const solver)
    (const [])

-- | Helper to set arguments.
withArgs :: [String] -> MiniZinc input answer -> MiniZinc input answer
withArgs args mzn = mzn { mkExtraArgs = const args }

-- | Runs MiniZinc on the input and parses output for the last answer.
--
-- The parser for now is primitive and all the parsing occurs after processing
-- with no guarantee to run on bounded-memory. This matters if your MiniZinc
-- model returns so many solutions that the output is large.
runLastMinizincJSON ::
  (ToJSON input, FromJSON answer) =>
  MiniZinc input answer ->
  input ->
  IO (Maybe answer)
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
      ]
        ++ (mkExtraArgs minizinc obj)
        ++ [ model minizinc,
             fullPath
           ]

data SearchState a
  = Exhausted a
  | Incomplete a
  | Unsatisfiable
  | InternalError String
  deriving (Show, Eq, Ord, Functor)

reduce :: FromJSON a => SearchState Value -> SearchState a
reduce Unsatisfiable = Unsatisfiable
reduce (InternalError s) = InternalError s
reduce (Exhausted val) = case fromJSON val of
  Success obj -> Exhausted obj
  Error err -> InternalError err
reduce (Incomplete val) = case fromJSON val of
  Success obj -> Incomplete obj
  Error err -> InternalError err

data ResultHandler obj b
  = ResultHandler
  { handleNext :: b -> SearchState obj -> IO (b, Maybe (ResultHandler obj b))
  }

runMinizincJSON ::
  forall input answer b.
  (ToJSON input, FromJSON answer) =>
  MiniZinc input answer ->
  input ->
  b ->
  ResultHandler answer b ->
  IO b
runMinizincJSON minizinc obj v0 resultHandler = do
  LByteString.writeFile fullPath $ encode obj
  (_, Just out, _, _) <- createProcess (proc "minizinc" args){ std_out = CreatePipe }
  vRet <- go out (parse oneResult) "" v0 resultHandler
  hClose out
  pure vRet
  where
    go :: Handle
       -> (ByteString -> IResult ByteString (SearchState Value))
       -> ByteString
       -> b
       -> ResultHandler answer b
       -> IO b
    go out parsebuf buf v1 handler
      | ByteString.null buf = do
           eof <- hIsEOF out
           if eof
           then
             inputFinished v1
           else do
             dat <- ByteString.hGetLine out
             go out parsebuf dat v1 handler
      | otherwise = do
           case parsebuf buf of
             Done remainderBuf stateVal -> do
               (v2, nextHandler) <- (handleNext handler) v1 (reduce stateVal)
               case nextHandler of
                 Nothing -> userFinished v2
                 Just resultHandler -> go out (parse oneResult) remainderBuf v2 resultHandler
             Fail _ _ err -> do
               (v2,_) <- (handleNext handler) v1 (InternalError err)
               finalizeFailure v2
             Partial f -> go out f "" v1 handler

    inputFinished = pure
    userFinished = pure
    finalizeFailure = pure

    fullPath :: FilePath
    fullPath = mkTmpDataPath minizinc obj
    args :: [String]
    args =
      [ "--time-limit",
        show (mkTimeLimit minizinc obj),
        "--solver",
        showSolver (mkSolver minizinc obj),
        "--output-mode",
        "json"
      ]
        ++ (mkExtraArgs minizinc obj)
        ++ [ model minizinc,
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


-- | NOTE: the parser is fed with hGetLine (stripping EOL markers)
-- this assumption simplifies the grammar below
oneResult :: Parser (SearchState Value)
oneResult =
    try unsat
    <|> sat
  where
    unsat =  unsatMark *> pure Unsatisfiable

    sat = reverseAp <$> json' <*> searchstate
    reverseAp val constructor = constructor val

    searchstate =
      try (resultMark *> exhaustiveMark *> pure Exhausted)
      <|> try (resultMark *> pure Incomplete)

    resultMark = "----------"
    exhaustiveMark = "=========="
    unsatMark = "=====UNSATISFIABLE====="
