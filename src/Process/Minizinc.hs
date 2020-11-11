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
    SearchState(..),
    result,
    ResultHandler(..),
    runMinizincJSON,
    collectResults,
    keepLast,
    runLastMinizincJSON,
    cleanTmpFile,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Data.Attoparsec.ByteString (Parser, parse, IResult(..))
import Data.Attoparsec.Combinator (try)
import Data.Aeson (FromJSON, fromJSON, ToJSON, encode, Value, Result(..))
import Data.Aeson.Parser.Internal (json')
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Hashable (Hashable, hash)
import System.Directory (removeFile)
import System.Process (createProcess, proc, StdStream(CreatePipe), std_out, cleanupProcess)
import GHC.IO.Handle (Handle, hIsEOF)

-- | Type alias asking for milliseconds.
type MilliSeconds a = Int

-- | Name of a solver to be passed to the minizinc binary.
type SolverName = String

-- | Supported solvers or 'Other'.
data Solver = Chuffed | COIN_BC | CPLEX | Gecode | Gurobi | SCIP | Xpress | Other SolverName
  deriving (Show)

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

-- | Removes the temporary data file created as input before running minizinc.
cleanTmpFile :: MiniZinc input a -> input -> IO ()
cleanTmpFile mzn = removeFile . mkTmpDataPath mzn

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
    (\obj -> "minizinc-process-" ++ show (hash obj) ++ ".json")
    (const timeout)
    (const solver)
    (const [])

-- | Helper to set arguments.
withArgs :: [String] -> MiniZinc input answer -> MiniZinc input answer
withArgs args mzn = mzn { mkExtraArgs = const args }

-- | Runs MiniZinc on the input and parses output for the last answer.
runLastMinizincJSON ::
  (ToJSON input, FromJSON answer) =>
  MiniZinc input answer ->
  input ->
  IO (Maybe answer)
runLastMinizincJSON minizinc obj = do
  fmap adapt $ runMinizincJSON minizinc obj Nothing keepLast
  where
    adapt :: Maybe (SearchState a) -> Maybe a
    adapt x = x >>= result

data SearchState a
  = Exhausted a
  | Incomplete a
  | Unsatisfiable
  | InternalError String
  deriving (Show, Eq, Ord, Functor)

result :: SearchState a -> Maybe a
result (Incomplete a) = Just a
result (Exhausted a) = Just a
result _ = Nothing

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

-- | Collect all results in memory.
-- The resulting list is in reverse order (best are first elements in case of optimizations).
collectResults :: ResultHandler obj [SearchState obj]
collectResults = ResultHandler go
  where
    go xs x = seq x $ pure (x:xs, Just $ collectResults)

-- | Keep only the latest result in memory.
keepLast :: ResultHandler obj (Maybe (SearchState obj))
keepLast = ResultHandler go
  where
    go _ x = seq x $ pure (Just x, Just $ keepLast)


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
  let start = createProcess (proc "minizinc" args){ std_out = CreatePipe }
  bracket
    start
    cleanupProcess
    (\(_, Just out, _, _) -> go out (parse oneResult) "" v0 resultHandler)
  where
    go :: Handle
       -> (ByteString -> IResult ByteString (SearchState Value))
       -> ByteString
       -> b
       -> ResultHandler answer b
       -> IO b
    go out parsebuf buf v1 handler = do
           case parsebuf buf of
             Done remainderBuf stateVal -> do
               (v2, nextHandler) <- (handleNext handler) v1 (reduce stateVal)
               case nextHandler of
                 Nothing -> userFinished v2
                 Just resultHandler -> go out (parse oneResult) remainderBuf v2 resultHandler
             Fail _ _ err -> do
               (v2,_) <- (handleNext handler) v1 (InternalError err)
               finalizeFailure v2
             Partial f -> do
               -- NOTE: on End-Of-File we attempt to feed an empty byte to
               -- conclude the parse.
               -- Thus the real "final marked" is when the Handle is at EOF and
               -- the input chunk is empty.
               -- A more explicit final-result marker could benefit the
               -- implementation of ResultHandler.
               eof <- hIsEOF out
               case (eof, ByteString.null buf) of
                 (True, True) -> inputFinished v1
                 (True, False) -> go out f "" v1 handler
                 _             -> do
                     dat <- ByteString.hGetLine out
                     go out f dat v1 handler

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
