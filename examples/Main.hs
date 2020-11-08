{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Process.Minizinc
import Process.Minizinc.Inspect

import Paths_minizinc_process

main :: IO ()
main = do
  putStrLn "*>> example 1 (trivial)"
  example001
  putStrLn "*>> example 1 (trivial, streaming)"
  example001Stream
  putStrLn "*>> example 1 (trivial, collecting 100ms of results [truncated to 10])"
  example001Collect
  putStrLn "*>> example 2 (inspection)"
  example002
  putStrLn "*>> example 3 (realistic)"
  example003
  putStrLn "*>> example 3bis (streaming realistic)"
  example003bis
  putStrLn "*>> example 4 (unsat)"
  example004

-- Example001: some trivial input/output.
data Input001 = Input001 {x :: Int}
  deriving (Show, Generic)

instance ToJSON Input001

instance Hashable Input001

data Output001 = Output001 {y :: Int}
  deriving (Show, Generic)

instance FromJSON Output001

example001 :: IO ()
example001 = do
  path <- getDataFileName "models/example001.mzn"
  let mzn = simpleMiniZinc @Input001 @Output001 path 1000 Gecode
  let solve = runLastMinizincJSON mzn
  traverse_ (\i -> print i >> solve i >>= print) [Input001 x | x <- [0..4] ]

example001Stream :: IO ()
example001Stream = do
  path <- getDataFileName "models/example001.mzn"
  let mzn = simpleMiniZinc @Input001 @Output001 path 1000 Gecode
  let solve input = runMinizincJSON mzn input () handler
  traverse_ (\i -> print i >> solve i >>= print) [Input001 x | x <- [0..4] ]
  where
    handler :: ResultHandler Output001 ()
    handler = ResultHandler handle
    handle v o = print (v,o) >> pure ((), Just handler)

example001Collect :: IO ()
example001Collect = do
  path <- getDataFileName "models/example001.mzn"
  let mzn = simpleMiniZinc @Input001 @Output001 path 100 Gecode & withArgs [ "-a" ]
  runMinizincJSON mzn (Input001 0) [] collectResults >>= print . take 10


-- Example002: shows model inspection result for a model with a bit of every
-- MiniZinc types.
example002 :: IO ()
example002 = do
  inspect "models/example001.mzn" >>= print

-- Example003: demonstrates how to work with some domain model a diverging from
-- the constraint model (which uses multi-dimensional array from User to User).
--
--  We attribute users to rooms. Each user has some preference to be in the
--  same room as others. Rooms have limited size.

data User = User { name :: Text , likeUser :: User -> Int }

userFromPairs :: Text -> [(Text, Int)] -> Int -> User
userFromPairs n pairs defaultlikeness =
  User n f
  where
    f user = case lookup (name user) pairs of Nothing -> defaultlikeness ; Just v -> v

data Room = Room { label :: Text, roomSize :: Int }
  deriving (Show)

data Input003 = Input003 { nUsers :: Int , nRooms :: Int , attirance :: [[Int]] , size :: [Int] }
  deriving (Generic)
instance ToJSON Input003

instance Hashable Input003

data Output003 = Output003 { room :: [Int] }
  deriving (Show, Generic)

instance FromJSON Output003

translate003 :: [Room] -> [User] -> (Input003, Output003 -> [(User, Room)])
translate003 rooms users =
    let input003 = Input003 (length users) (length rooms) prefs sizes
        f output003 = zip users (fmap unsafeLookupRoom (room output003))
    in (input003, f)
  where
    prefs = [ [likeUser u1 u2 | u2 <- users] | u1 <- users ]
    sizes = [roomSize r | r <- rooms]
    unsafeLookupRoom roomnbr = rooms !! (roomnbr - 1)

example003 :: IO ()
example003 = do
  path <- getDataFileName "models/example003.mzn"
  let (input003,translateBack) = translate003 rooms users
  let mzn = simpleMiniZinc @Input003 @Output003 path 10000 Gecode
  output <- runMinizincJSON mzn input003 Nothing keepLast
  case output >>= result of
    Nothing -> print "no solutions"
    Just pairs -> print [ (name u, label r) | (u,r) <- translateBack pairs ]
  where
    users = [albert, paul, philip, sarah, sylvia, zoe]
    rooms = [large, medium, small]

    albert, paul, philip, sarah, sylvia, zoe :: User
    albert = userFromPairs "albert" [("zoe", 8), ("paul", 2)] (-1)
    paul   = userFromPairs "paul"   [("philip", 10), ("sarah", 3), ("sylvia", -5)] 0
    philip = userFromPairs "philip" [("philip", 5), ("sarah", 5), ("sylvia", 3)] 1
    sarah  = userFromPairs "sarah"  [("sylvia", 5), ("paul", -4)] 0
    sylvia = userFromPairs "sylvia" [("paul", -5), ("sarah", 10), ("philip", 10)] 3
    zoe    = userFromPairs "zoe"    [] 0

    large, medium, small :: Room
    large  = Room "large"  4
    medium = Room "medium" 3
    small  = Room "small"  2


example003bis :: IO ()
example003bis = do
  path <- getDataFileName "models/example003bis.mzn"
  let (input003,translateBack) = translate003 rooms users
  let mzn = simpleMiniZinc @Input003 @Output003 path 10000 Gecode
          & withArgs ["-a"]
  n <- runMinizincJSON mzn input003 (0 :: Int) handler
  putStrLn $ "number of solutions: " <> show n
  where
    handler = ResultHandler handle
    handle n searchstate = do
      print searchstate
      pure $ (n+1, Just handler)
    users = [albert, paul, philip, sarah, sylvia, zoe]
    rooms = [large, medium, small]

    albert, paul, philip, sarah, sylvia, zoe :: User
    albert = userFromPairs "albert" [("zoe", 8), ("paul", 2)] (-1)
    paul   = userFromPairs "paul"   [("philip", 10), ("sarah", 3), ("sylvia", -5)] 0
    philip = userFromPairs "philip" [("philip", 5), ("sarah", 5), ("sylvia", 3)] 1
    sarah  = userFromPairs "sarah"  [("sylvia", 5), ("paul", -4)] 0
    sylvia = userFromPairs "sylvia" [("paul", -5), ("sarah", 10), ("philip", 10)] 3
    zoe    = userFromPairs "zoe"    [] 0

    large, medium, small :: Room
    large  = Room "large"  4
    medium = Room "medium" 3
    small  = Room "small"  2

type Input004 = Input001
mkInput004 = Input001
type Output004 = Output001

example004 :: IO ()
example004 = do
  path <- getDataFileName "models/example004.mzn"
  let mzn = simpleMiniZinc @Input004 @Output004 path 10000 Gecode
  runMinizincJSON mzn (mkInput004 5) () handler
  where
    handler = ResultHandler handle
    handle v0 searchstate = do
      print searchstate
      pure $ (v0, Just handler)
